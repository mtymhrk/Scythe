#include <string.h>
#include <limits.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "chashtbl.h"
#include "api.h"
#include "earray.h"
#include "iseq.h"

ScmTypeInfo SCM_ISEQ_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmISeq),
  .gc_ini_func         = scm_iseq_gc_initialize,
  .gc_fin_func         = scm_iseq_gc_finalize,
  .gc_accept_func      = scm_iseq_gc_accept,
  .gc_accept_func_weak = NULL
};

struct {
  const char *mne;
  int code;
} mnemonic2opcod_tbl[] = {
  { "nop"          , SCM_OPCODE_NOP },
  { "stop"         , SCM_OPCODE_STOP },
  { "call"         , SCM_OPCODE_CALL },
  { "return"       , SCM_OPCODE_RETURN },
  { "frame"        , SCM_OPCODE_FRAME },
  { "immval"       , SCM_OPCODE_IMMVAL },
  { "push"         , SCM_OPCODE_PUSH },
  { "push_primval" , SCM_OPCODE_PUSH_PRIMVAL },
  { "gref"         , SCM_OPCODE_GREF },
  { "gdef"         , SCM_OPCODE_GDEF },
  { "gset"         , SCM_OPCODE_GSET },
  { "jmp"          , SCM_OPCODE_JMP },
  { "label"        , SCM_ISEQ_PI_LABEL },
  { "asm"          , SCM_ISEQ_PI_ASM }
};


static size_t
scm_iseq_asm_hash(ScmCHashTblKey key)
{
  size_t hash;

  hash = 0;
  for (const char *p = (char *)key; *p != '\0'; p++)
    hash = (hash << 5) - hash + (unsigned char)(*p);

  return hash;
}

static bool
scm_iseq_asm_cmp(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return (strcmp((char *)key1, (char *)key2) == 0) ? true : false;
}

static int
scm_iseq_asm_mne2opcode(const char *mne)
{
  /* TODO: use quick sort */
  scm_assert(mne != NULL);

  for (size_t i = 0;
       i < sizeof(mnemonic2opcod_tbl)/sizeof(mnemonic2opcod_tbl[0]);
       i++) {
    if (strcmp(mnemonic2opcod_tbl[i].mne, mne) == 0)
      return mnemonic2opcod_tbl[i].code;
  }

  return -1;
}

static ScmLabelInfo *
scm_iseq_asm_new_label_rec(void)
{
  int rslt;
  ScmLabelInfo *rec;

  rec = scm_capi_malloc(sizeof(*rec));
  if (rec == NULL) return NULL; /* [ERR]: [through] */

  rslt = eary_init(&rec->ref, sizeof(size_t), 8);
  if (rslt < 0) {
    scm_capi_free(rec);
    return NULL;                /* [ERR]: [through] */
  };

  rec->label[0] = '\0';
  rec->idx = 0;
  rec->defined_p = false;

  return rec;
}

static void
scm_iseq_asm_del_label_rec(ScmLabelInfo *rec)
{
  scm_assert(rec != NULL);

  eary_fin(&rec->ref);
  scm_capi_free(rec);
}

static int
scm_iseq_asm_reg_label_ref_idx(ScmCHashTbl *tbl, EArray *labels,
                               ScmObj label, size_t ref_idx)
{
  int rslt;
  bool found;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  char label_name[SCM_ISEQ_LABEL_NAME_MAX];
  ssize_t name_sz;

  scm_assert(tbl != 0);
  scm_assert(scm_capi_symbol_p(label));

  name_sz = scm_capi_symbol_bytesize(label);
  if (name_sz < 0) return -1;   /* [ERR]: [through] */
  if ((size_t)name_sz > sizeof(label_name) - 1) return -1; /* [ERR]: iseq: label
 name is too long */

  name_sz = scm_capi_symbol_to_cstr(label, label_name, sizeof(label_name));
  if (name_sz < 0) return -1;   /* [ERR]: [through] */

  rslt = scm_chash_tbl_get(tbl, SCM_CHASH_TBL_KEY(label_name), &val, &found);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (found) {
    rec = (ScmLabelInfo *)val;
  }
  else {
    rec = scm_iseq_asm_new_label_rec();
    if (rec == NULL) return -1; /* [ERR]: [through] */

    memcpy(rec->label, label_name, (size_t)name_sz + 1);
    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_iseq_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_iseq_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }
  }

  EARY_PUSH(&rec->ref, size_t, ref_idx, rslt);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return 0;
}

static int
scm_iseq_asm_reg_label_def_idx(ScmCHashTbl *tbl, EArray *labels,
                               ScmObj label, size_t idx)
{
  int rslt;
  bool found;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  char label_name[SCM_ISEQ_LABEL_NAME_MAX];
  ssize_t name_sz;

  scm_assert(tbl != NULL);
  scm_assert(labels != NULL);
  scm_assert(scm_capi_symbol_p(label));

  name_sz = scm_capi_symbol_bytesize(label);
  if (name_sz < 0) return -1;   /* [ERR]: [through] */
  if ((size_t)name_sz > sizeof(label_name) - 1) return -1; /* [ERR]: iseq: label name is too long */

  name_sz = scm_capi_symbol_to_cstr(label, label_name, sizeof(label_name));
  if (name_sz < 0) return -1;   /* [ERR]: [through] */

  rslt = scm_chash_tbl_get(tbl, SCM_CHASH_TBL_KEY(label_name), &val, &found);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (found) {
    rec = (ScmLabelInfo *)val;
  }
  else {
    rec = scm_iseq_asm_new_label_rec();
    if (rec == NULL) return -1; /* [ERR]: [through] */

    memcpy(rec->label, label_name, (size_t)name_sz + 1);

    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_iseq_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_iseq_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [thorugh] */
    }
  }

  if (rec->defined_p) return -1; /* [ERR]: iseq: already defined */

  rec->idx = idx;
  rec->defined_p = true;

  return 0;
}

static int
scm_iseq_asm_sym2opcode(ScmObj op)
{
  scm_assert(scm_obj_not_null_p(op));

  if (scm_capi_fixnum_p(op)) {
    long l = scm_capi_fixnum_to_clong(op);
    if (l >= INT_MAX) return -1; /* [ERR]: iseq: unkown opcode */
    return (int)l;
  }
  else if (scm_capi_symbol_p(op)) {
    char mne[32];
    ssize_t rslt = scm_capi_symbol_to_cstr(op, mne, sizeof(mne));
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    rslt = scm_iseq_asm_mne2opcode(mne);
    if (rslt < 0) return -1;    /* [ERR]: iseq: unknown mnemonic */
    return (int)rslt;
  }
  else {
    return -1;                  /* [ERR]: iseq: unknown op */
  }
}

static ssize_t
scm_iseq_asm_inst_noarg_op(ScmObj iseq, size_t idx, int opcode)
{
  scm_inst_t i;
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx < (size_t)SSIZE_MAX - 1);
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);

  i.plain.op = (uint8_t)opcode;
  i.plain.arg = 0;
  rslt = scm_iseq_set_word(iseq, idx++, i.iword);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

static ssize_t
scm_iseq_asm_inst_unary_op(ScmObj iseq, size_t idx, int opcode, ScmObj arg)
{
  scm_inst_t i;
  int immv_idx, rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx < (size_t)SSIZE_MAX - 1);
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_obj_not_null_p(arg));

  immv_idx = scm_iseq_set_immval(iseq, arg);
  if (immv_idx < 0) return -1;

  i.immv1.op = (uint8_t)opcode;
  i.immv1.imm_idx = immv_idx;
  rslt = scm_iseq_set_word(iseq, idx++, i.iword);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

static ssize_t
scm_iseq_asm_inst_primval_op(ScmObj iseq, size_t idx, int opcode, ScmObj arg)
{
  scm_inst_t i;
  long primval;
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx  < (size_t)SSIZE_MAX - 1);
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_fixnum_p(arg));

  primval = scm_capi_fixnum_to_clong(arg);
  if (primval < SCM_INST_PRIMVAL_MIN || SCM_INST_PRIMVAL_MAX < primval)
    return -1;                  /* [ERR]: iseq: operand is out of range */

  i.primv.op = (uint8_t)opcode;
  i.primv.primval = primval;
  rslt = scm_iseq_set_word(iseq, idx++, i.iword);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

static ssize_t
scm_iseq_asm_inst_ref_label_op(ScmObj iseq, size_t idx,
                               int opcode, ScmObj label,
                               ScmCHashTbl *label_tbl, EArray *labels)
{
  scm_inst_t i;
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx < (size_t)SSIZE_MAX - 1);
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(label));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  rslt = scm_iseq_asm_reg_label_ref_idx(label_tbl, labels, label, idx);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  i.primv.op = (uint8_t)opcode;
  i.primv.primval = 0;    /* dummy */;
  rslt = scm_iseq_set_word(iseq, idx++, i.iword);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

static ssize_t
scm_iseq_asm_inst(ScmObj iseq, ScmObj inst,
                  size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj op = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  int opcode, rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &inst, &op, &arg);

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(!scm_obj_null_p(inst));
  scm_assert(idx < (size_t)SSIZE_MAX - 1);
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  if (scm_capi_pair_p(inst)) {
    op = scm_api_car(inst);
    if (scm_obj_null_p(op)) return -1; /* [ERR]: [thorugh] */

    arg = scm_api_cdr(inst);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */
  }
  else {
    op = inst;
    arg = scm_api_nil();
  }

  opcode = scm_iseq_asm_sym2opcode(op);
  if (opcode < 0) return -1;    /* [ERR]: [through] */

  switch (opcode) {
  case SCM_OPCODE_NOP:          /* fall through */
  case SCM_OPCODE_STOP:         /* fall through */
  case SCM_OPCODE_CALL:         /* fall through */
  case SCM_OPCODE_RETURN:       /* fall through */
  case SCM_OPCODE_FRAME:        /* fall through */
  case SCM_OPCODE_PUSH:
    return scm_iseq_asm_inst_noarg_op(iseq, idx, opcode);
    break;
  case SCM_OPCODE_GREF:         /* fall through */
  case SCM_OPCODE_GDEF:         /* fall through */
  case SCM_OPCODE_GSET:         /* fall through */
  case SCM_OPCODE_IMMVAL:
    if (!scm_capi_pair_p(arg)) return -1; /* [ERR]: iseq: operands is not exist */

    arg = scm_api_car(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */

    return scm_iseq_asm_inst_unary_op(iseq, idx, opcode, arg);
    break;
  case SCM_OPCODE_PUSH_PRIMVAL:
    if (!scm_capi_pair_p(arg)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg = scm_api_car(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */

    return scm_iseq_asm_inst_primval_op(iseq, idx, opcode, arg);
    break;
  case SCM_OPCODE_JMP:
    if (!scm_capi_pair_p(arg)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg = scm_api_car(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */
    if (!scm_capi_symbol_p(arg)) return -1; /* [ERR]: iseq: operand is not symbol */

    return scm_iseq_asm_inst_ref_label_op(iseq, idx,
                                          opcode, arg, label_tbl, labels);
    break;
  case SCM_ISEQ_PI_LABEL:
    if (!scm_capi_pair_p(arg)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg = scm_api_car(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */
    if (!scm_capi_symbol_p(arg)) return -1; /* [ERR]: iseq: operand is not symbol */

    rslt = scm_iseq_asm_reg_label_def_idx(label_tbl, labels, arg, idx);
    return (rslt < 0) ?  -1 : (ssize_t)idx;
    break;
  case SCM_ISEQ_PI_ASM:
    if (!scm_capi_pair_p(arg)) return -1;  /* [ERR]: iseq: operands is not exist */
    arg = scm_api_car(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */
    if (!scm_capi_pair_p(arg)) return -1; /* [ERR]: operand is not list */

    arg = scm_iseq_list_to_iseq(arg);
    if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */

    return scm_iseq_asm_inst_unary_op(iseq, idx, SCM_OPCODE_IMMVAL, arg);
    break;
  default:
    scm_assert(false);
    break;
  }

  return -1;
}
static int
scm_iseq_asm_label_resolv(ScmObj iseq, ScmCHashTbl *label_tbl, EArray *labels)
{
  size_t idx1, idx2;
  char **lbl;
  int rslt;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  bool found, deleted;
  size_t *inst_idx;
  scm_inst_t iword;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  EARY_FOR_EACH(labels, idx1, lbl) {
    rslt = scm_chash_tbl_get(label_tbl, SCM_CHASH_TBL_KEY(*lbl), &val, &found);
    if (rslt < 0) return -1;    /* [ERR]: [through] */
    if (!found) goto err_free_rec; /* [ERR]: iseq: inner error occured*/

    rec = (ScmLabelInfo *)val;
    EARY_FOR_EACH(&rec->ref, idx2,  inst_idx) {
      EARY_GET(SCM_ISEQ_EARY_SEQ(iseq), scm_iword_t,
               *inst_idx, iword.iword);
      if ((ssize_t)rec->idx
          < SCM_INST_PRIMVAL_MIN + (ssize_t)*inst_idx + 1)
        goto err_free_rec;      /* [ERR]: iseq: operand is underflow */
      else if ((ssize_t)*inst_idx - 1
               > SCM_INST_PRIMVAL_MAX - (ssize_t)rec->idx)
        goto err_free_rec;      /* [ERR]: iseq: operand is overflow */
      iword.primv.primval = rec->idx - *inst_idx - 1;
      scm_iseq_set_word(iseq, *inst_idx, iword.iword);
    }

    rslt = scm_chash_tbl_delete(label_tbl, SCM_CHASH_TBL_KEY(*lbl), NULL, NULL);
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    scm_iseq_asm_del_label_rec(rec);
  }

  return 0;

 err_free_rec:
  EARY_FOR_EACH(labels, idx1, lbl) {
    rslt = scm_chash_tbl_delete(label_tbl, SCM_CHASH_TBL_KEY(*lbl),
                                &val, &deleted);
    if (rslt < 0) return -1;
    if (deleted) {
      rec = (ScmLabelInfo *)val;
      scm_iseq_asm_del_label_rec(rec);
    }
  }

  return -1;
}

static int
scm_iseq_assemble(ScmObj iseq,
                  ScmCHashTbl *label_tbl, EArray *labels, ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;
  ssize_t idx;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &lst, &cur, &inst);

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);
  scm_assert(scm_capi_pair_p(lst));

  idx = 0;
  for (cur = lst;
       scm_obj_not_null_p(cur) && !scm_capi_nil_p(cur);
       cur = scm_api_cdr(cur)) {
    inst = scm_api_car(cur);
    idx = scm_iseq_asm_inst(iseq, inst, (size_t)idx, label_tbl, labels);
    if (idx < 0) return -1;     /* [ERR]: [through] */
  }

  rslt = scm_iseq_asm_label_resolv(iseq, label_tbl, labels);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return 0;
}

int
scm_iseq_initialize(ScmObj iseq) /* GC OK */
{
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  rslt = eary_init(SCM_ISEQ_EARY_SEQ(iseq),
                   sizeof(scm_iword_t), SCM_ISEQ_DEFAULT_SEQ_SIZE);
  if (rslt != 0)
    return -1;                           /* TODO: error handling; [ERR]: [through] */

  rslt = eary_init(SCM_ISEQ_EARY_IMMVS(iseq),
                   sizeof(ScmObj), SCM_ISEQ_DEFAULT_IMMVS_SIZE);
  if (rslt != 0)
    return -1;                           /* TODO: error handling; [ERR]: [through] */

  return 0;
}

ScmObj
scm_iseq_new(SCM_MEM_TYPE_T mtype) /* GC OK */
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  iseq = scm_capi_mem_alloc(&SCM_ISEQ_TYPE_INFO, mtype);
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_iseq_initialize(iseq) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return iseq;
}

void
scm_iseq_finalize(ScmObj obj) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_fin(SCM_ISEQ_EARY_SEQ(obj));
  eary_fin(SCM_ISEQ_EARY_IMMVS(obj));
}

int
scm_iseq_set_immval(ScmObj iseq, ScmObj val) /* GC OK */
{
  size_t idx;
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  idx = SCM_ISEQ_VEC_LENGTH(iseq);
  if (idx >= SCM_ISEQ_IMMVS_MAX) return -1; /* [ERR]: iseq: immediate value area over flow */

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), idx, val, iseq, err);

  if(err != 0) return -1;       /* [ERR]: [through] */

  return (int)idx;
}

int
scm_iseq_update_immval(ScmObj iseq, int idx, ScmObj val)
{
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(scm_obj_not_null_p(val));

  if ((size_t)idx >= SCM_ISEQ_VEC_LENGTH(iseq)) return -1; /* [ERR]: iseq: argument out of range */

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), (size_t)idx, val, iseq, err);

  if (err != 0) return -1;      /* [ERR]: [through] */

  return idx;
}

int
scm_iseq_set_word(ScmObj iseq, size_t index, scm_iword_t word) /* GC OK */
{
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(index <= SSIZE_MAX);

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_iword_t, index, word, err);
  if (err != 0) return -1;/* [ERR]: [through] */

  return 0;
}

ssize_t
scm_iseq_push_word(ScmObj iseq, scm_iword_t word)
{
  int err;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) < SSIZE_MAX - 1);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));
  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_iword_t, idx, word, err);
  if (err != 0) return -1;

  return (ssize_t)idx;
}

int
scm_iseq_get_word(ScmObj iseq, size_t index, scm_iword_t *word)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(index <= SSIZE_MAX);
  scm_assert(word != NULL);

  if (index >= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)))
    return -1;

  EARY_GET(SCM_ISEQ_EARY_SEQ(iseq), scm_iword_t, index, *word);

  return 0;
}

ScmObj
scm_iseq_list_to_iseq(ScmObj lst)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmCHashTbl *label_tbl = NULL;
  EArray labels;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &lst);

  scm_assert(scm_capi_pair_p(lst));

  rslt = eary_init(&labels, sizeof(char *), 32);
  if (rslt) return SCM_OBJ_NULL; /* [ERR]: [through] */

  iseq = scm_iseq_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(iseq)) goto err; /* [ERR]: [through] */

  label_tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 32,
                                SCM_CHASH_TBL_CVAL, SCM_CHASH_TBL_CVAL,
                                scm_iseq_asm_hash, scm_iseq_asm_cmp);
  if (label_tbl == NULL) goto err; /* [ERR]: [through] */

  rslt = scm_iseq_assemble(iseq, label_tbl, &labels, lst);
  if (rslt < 0) goto err;       /* [ERR]: [through] */

  eary_fin(&labels);
  scm_chash_tbl_end(label_tbl);

  return iseq;

 err:
  eary_fin(&labels);
  if (label_tbl != NULL) scm_chash_tbl_end(label_tbl);
  return SCM_OBJ_NULL;
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_init(SCM_ISEQ_EARY_SEQ(obj), 0, 0);
  eary_init(SCM_ISEQ_EARY_IMMVS(obj), 0, 0);
}

void
scm_iseq_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  for (size_t i = 0; i < SCM_ISEQ_VEC_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_ISEQ_IMMVAL_VEC(obj)[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt))
      return rslt;
  }

  return rslt;
}
