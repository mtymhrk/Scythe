#include "object.h"
#include "api.h"
#include "chashtbl.h"
#include "earray.h"
#include "assembler.h"

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
  { "gref"         , SCM_OPCODE_GREF },
  { "gdef"         , SCM_OPCODE_GDEF },
  { "gset"         , SCM_OPCODE_GSET },
  { "jmp"          , SCM_OPCODE_JMP },
  { "label"        , SCM_ASM_PI_LABEL },
  { "asm"          , SCM_ASM_PI_ASM }
};


static size_t
scm_asm_hash(ScmCHashTblKey key)
{
  size_t hash;

  hash = 0;
  for (const char *p = (char *)key; *p != '\0'; p++)
    hash = (hash << 5) - hash + (unsigned char)(*p);

  return hash;
}

static bool
scm_asm_cmp(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return (strcmp((char *)key1, (char *)key2) == 0) ? true : false;
}

static int
scm_asm_mne2opcode(const char *mne)
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
scm_asm_new_label_rec(void)
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
scm_asm_del_label_rec(ScmLabelInfo *rec)
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
    rec = scm_asm_new_label_rec();
    if (rec == NULL) return -1; /* [ERR]: [through] */

    memcpy(rec->label, label_name, (size_t)name_sz + 1);
    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }
  }

  EARY_PUSH(&rec->ref, size_t, ref_idx, rslt);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return 0;
}

static int
scm_asm_reg_label_def_idx(ScmCHashTbl *tbl, EArray *labels,
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
    rec = scm_asm_new_label_rec();
    if (rec == NULL) return -1; /* [ERR]: [through] */

    memcpy(rec->label, label_name, (size_t)name_sz + 1);

    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [through] */
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_asm_del_label_rec(rec);
      return -1;                /* [ERR]: [thorugh] */
    }
  }

  if (rec->defined_p) return -1; /* [ERR]: iseq: already defined */

  rec->idx = idx;
  rec->defined_p = true;

  return 0;
}

static int
scm_asm_sym2opcode(ScmObj op)
{
  scm_assert(!scm_capi_null_value_p(op));

  if (scm_capi_fixnum_p(op)) {
    long l = scm_capi_fixnum_to_clong(op);
    if (l >= INT_MAX) return -1; /* [ERR]: iseq: unkown opcode */
    return (int)l;
  }
  else if (scm_capi_symbol_p(op)) {
    char mne[32];
    ssize_t rslt = scm_capi_symbol_to_cstr(op, mne, sizeof(mne));
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    rslt = scm_asm_mne2opcode(mne);
    if (rslt < 0) return -1;    /* [ERR]: iseq: unknown mnemonic */
    return (int)rslt;
  }
  else {
    return -1;                  /* [ERR]: iseq: unknown op */
  }
}

static ssize_t
scm_asm_inst_noarg_op(ScmObj iseq, int opcode)
{
  ssize_t idx;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);

  idx = scm_capi_iseq_push_op(iseq, (uint8_t)opcode);
  if (idx < 0) return -1;      /* [ERR]: [through] */

  return idx + 2;
}

static ssize_t
scm_asm_inst_unary_op(ScmObj iseq, int opcode, ScmObj arg)
{
  ssize_t idx;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(!scm_capi_null_value_p(arg));

  idx = scm_capi_iseq_push_op_immval(iseq, opcode, arg);
  if (idx < 0) return -1;

  return idx + 6;
}

static ssize_t
scm_asm_inst_cval_op(ScmObj iseq, int opcode, ScmObj arg)
{
  long cval;
  ssize_t idx;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_fixnum_p(arg));

  cval = scm_capi_fixnum_to_clong(arg);
  if (cval < INT32_MIN || INT32_MAX < cval)
    return -1;                  /* [ERR]: iseq: operand is out of range */

  idx = scm_capi_iseq_push_op_cval(iseq, opcode, (uint32_t)cval);
  if (idx < 0) return -1;

  return idx + 6;
}

static ssize_t
scm_asm_inst_cval_cval_op(ScmObj iseq, int opcode,
                          ScmObj arg1, ScmObj arg2)
{
  long cval1, cval2;
  ssize_t idx;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_fixnum_p(arg1));
  scm_assert(scm_capi_fixnum_p(arg2));

  cval1 = scm_capi_fixnum_to_clong(arg1);
  if (cval1 < INT32_MIN || INT32_MAX < cval1)
    return -1;                  /* [ERR]: iseq: operand is out of range */

  cval2 = scm_capi_fixnum_to_clong(arg2);
  if (cval2 < INT32_MIN || INT32_MAX < cval2)
    return -1;                  /* [ERR]: iseq: operand is out of range */

  idx = scm_capi_iseq_push_op_cval_cval(iseq, opcode,
                                        (uint32_t)cval1, (uint32_t)cval2);
  if (idx < 0) return -1;

  return idx + 10;
}

static ssize_t
scm_asm_inst_ref_label_op(ScmObj iseq, int opcode, ScmObj label,
                          ScmCHashTbl *label_tbl, EArray *labels)
{
  ssize_t idx, rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(label));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  idx = scm_capi_iseq_push_op_cval(iseq, opcode, 0);
  if (idx < 0) return -1;      /* [ERR]: [through] */

  rslt = scm_iseq_asm_reg_label_ref_idx(label_tbl,
                                        labels, label, (size_t)idx + 2);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx + 6;
}

static ssize_t
scm_asm_inst(ScmObj iseq, ScmObj inst, size_t idx,
             ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj op = SCM_OBJ_INIT;
  ScmObj args = SCM_OBJ_INIT, arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  int opcode, rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &inst, &op, &args, &arg1, &arg2);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(!scm_obj_null_p(inst));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  if (scm_capi_pair_p(inst)) {
    op = scm_api_car(inst);
    if (scm_obj_null_p(op)) return -1; /* [ERR]: [thorugh] */

    args = scm_api_cdr(inst);
    if (scm_obj_null_p(args)) return -1; /* [ERR]: [through] */
  }
  else {
    op = inst;
    args = scm_api_nil();
  }

  opcode = scm_asm_sym2opcode(op);
  if (opcode < 0) return -1;    /* [ERR]: [through] */

  switch (opcode) {
  case SCM_OPCODE_NOP:          /* fall through */
  case SCM_OPCODE_STOP:         /* fall through */
  case SCM_OPCODE_FRAME:        /* fall through */
  case SCM_OPCODE_PUSH:
    return scm_asm_inst_noarg_op(iseq, opcode);
    break;
  case SCM_OPCODE_GREF:         /* fall through */
  case SCM_OPCODE_GDEF:         /* fall through */
  case SCM_OPCODE_GSET:         /* fall through */
  case SCM_OPCODE_IMMVAL:
    if (!scm_capi_pair_p(args)) return -1; /* [ERR]: iseq: operands is not exist */

    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */

    return scm_asm_inst_unary_op(iseq, opcode, arg1);
    break;
  case SCM_OPCODE_CALL:         /* fall through */
  case SCM_OPCODE_RETURN:       /* fall through */
    if (!scm_capi_pair_p(args)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */

    return scm_asm_inst_cval_op(iseq, opcode, arg1);
    break;
  case SCM_OPCODE_TAIL_CALL:
    if (!scm_capi_pair_p(args)) return -1;  /* [ERR]: iseq: operands is not exist */
    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */

    arg2 = scm_api_car(scm_api_cdr(args));
    if (scm_obj_null_p(arg2)) return -1; /* [ERR]: [through] */

    return scm_asm_inst_cval_cval_op(iseq, opcode, arg1, arg2);
    break;
  case SCM_OPCODE_JMP:
    if (!scm_capi_pair_p(args)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */
    if (!scm_capi_symbol_p(arg1)) return -1; /* [ERR]: iseq: operand is not symbol */

    return scm_asm_inst_ref_label_op(iseq, opcode,
                                          arg1, label_tbl, labels);
    break;
  case SCM_ASM_PI_LABEL:
    if (!scm_capi_pair_p(args)) return -1;  /* [ERR]: iseq: operands is not exist */

    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */
    if (!scm_capi_symbol_p(arg1)) return -1; /* [ERR]: iseq: operand is not symbol */

    rslt = scm_asm_reg_label_def_idx(label_tbl, labels, arg1, idx);
    return (rslt < 0) ?  -1 : (ssize_t)idx;
    break;
  case SCM_ASM_PI_ASM:
    if (!scm_capi_pair_p(args)) return -1;  /* [ERR]: iseq: operands is not exist */
    arg1 = scm_api_car(args);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */
    if (!scm_capi_pair_p(arg1)) return -1; /* [ERR]: iseq: operand is not pair */
    arg1 = scm_asm_assemble(arg1);
    if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [through] */

    return scm_asm_inst_unary_op(iseq, SCM_OPCODE_IMMVAL, arg1);
    break;
  default:
    scm_assert(false);
    break;
  }

  return -1;
}
static int
scm_asm_label_resolv(ScmObj iseq, ScmCHashTbl *label_tbl, EArray *labels)
{
  size_t idx1, idx2;
  char **lbl;
  int rslt;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  bool found, deleted;
  size_t *ref_idx;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  EARY_FOR_EACH(labels, idx1, lbl) {
    rslt = scm_chash_tbl_get(label_tbl, SCM_CHASH_TBL_KEY(*lbl), &val, &found);
    if (rslt < 0) return -1;    /* [ERR]: [through] */
    if (!found) goto err_free_rec; /* [ERR]: iseq: inner error occured*/

    rec = (ScmLabelInfo *)val;
    EARY_FOR_EACH(&rec->ref, idx2,  ref_idx) {
      if ((ssize_t)rec->idx < INT32_MIN + (ssize_t)*ref_idx + 4)
        goto err_free_rec;      /* [ERR]: iseq: operand is underflow */
      else if ((ssize_t)*ref_idx - 4 > INT32_MAX - (ssize_t)rec->idx)
        goto err_free_rec;      /* [ERR]: iseq: operand is overflow */
      scm_capi_iseq_set_cval(iseq, *ref_idx,
                             (uint32_t)((ssize_t)rec->idx
                                        - (ssize_t)*ref_idx - 4));
    }

    rslt = scm_chash_tbl_delete(label_tbl, SCM_CHASH_TBL_KEY(*lbl), NULL, NULL);
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    scm_asm_del_label_rec(rec);
  }

  return 0;

 err_free_rec:
  EARY_FOR_EACH(labels, idx1, lbl) {
    rslt = scm_chash_tbl_delete(label_tbl, SCM_CHASH_TBL_KEY(*lbl),
                                &val, &deleted);
    if (rslt < 0) return -1;
    if (deleted) {
      rec = (ScmLabelInfo *)val;
      scm_asm_del_label_rec(rec);
    }
  }

  return -1;
}

static int
scm_asm_assemble_aux(ScmObj iseq,
                     ScmCHashTbl *label_tbl, EArray *labels, ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;
  ssize_t idx;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &lst, &cur, &inst);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);
  scm_assert(scm_capi_pair_p(lst));

  idx = 0;
  for (cur = lst;
       scm_obj_not_null_p(cur) && !scm_capi_nil_p(cur);
       cur = scm_api_cdr(cur)) {
    inst = scm_api_car(cur);
    idx = scm_asm_inst(iseq, inst, (size_t)idx, label_tbl, labels);
    if (idx < 0) return -1;     /* [ERR]: [through] */
  }

  rslt = scm_asm_label_resolv(iseq, label_tbl, labels);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return 0;
}

ScmObj
scm_asm_assemble(ScmObj lst)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmCHashTbl *label_tbl = NULL;
  EArray labels;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &lst);

  scm_assert(scm_capi_pair_p(lst));

  rslt = eary_init(&labels, sizeof(char *), 32);
  if (rslt) return SCM_OBJ_NULL; /* [ERR]: [through] */

  iseq = scm_api_make_iseq();
  if (scm_obj_null_p(iseq)) goto err; /* [ERR]: [through] */

  label_tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 32,
                                SCM_CHASH_TBL_CVAL, SCM_CHASH_TBL_CVAL,
                                scm_asm_hash, scm_asm_cmp);
  if (label_tbl == NULL) goto err; /* [ERR]: [through] */

  rslt = scm_asm_assemble_aux(iseq, label_tbl, &labels, lst);
  if (rslt < 0) goto err;       /* [ERR]: [through] */

  eary_fin(&labels);
  scm_chash_tbl_end(label_tbl);

  return iseq;

 err:
  eary_fin(&labels);
  if (label_tbl != NULL) scm_chash_tbl_end(label_tbl);
  return SCM_OBJ_NULL;
}
