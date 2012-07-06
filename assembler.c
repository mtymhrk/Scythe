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
  { "halt"         , SCM_OPCODE_HALT },
  { "call"         , SCM_OPCODE_CALL },
  { "tcall"        , SCM_OPCODE_TAIL_CALL },
  { "return"       , SCM_OPCODE_RETURN },
  { "frame"        , SCM_OPCODE_FRAME },
  { "immval"       , SCM_OPCODE_IMMVAL },
  { "push"         , SCM_OPCODE_PUSH },
  { "gref"         , SCM_OPCODE_GREF },
  { "gdef"         , SCM_OPCODE_GDEF },
  { "gset"         , SCM_OPCODE_GSET },
  { "sref"         , SCM_OPCODE_SREF },
  { "sset"         , SCM_OPCODE_SSET },
  { "cref"         , SCM_OPCODE_CREF },
  { "cset"         , SCM_OPCODE_CSET },
  { "jmp"          , SCM_OPCODE_JMP },
  { "jmpf"         , SCM_OPCODE_JMPF },
  { "raise"        , SCM_OPCODE_RAISE },
  { "box"          , SCM_OPCODE_BOX },
  { "unbox"        , SCM_OPCODE_UNBOX },
  { "close"        , SCM_OPCODE_CLOSE },
  { "label"        , SCM_ASM_PI_LABEL },
  { "asm"          , SCM_ASM_PI_ASM },
  { "asm-close"    , SCM_ASM_PI_ASM_CLOSE }
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

  if ((size_t)name_sz > sizeof(label_name) - 1) {
    scm_capi_error("Assembler: label name is too long", 1, label);
    return -1;
  }

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

  if ((size_t)name_sz > sizeof(label_name) - 1) {
    scm_capi_error("Assember: label name is too long", 1, label);
    return -1;
  }

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

  if (rec->defined_p) {
    scm_capi_error("Assembler: label is already defined", 1, label);
    return -1;
  }

  rec->idx = idx;
  rec->defined_p = true;

  return 0;
}

static int
scm_asm_sym2opcode(ScmObj op)
{
  scm_assert(!scm_capi_null_value_p(op));

  if (scm_capi_integer_p(op)) {
    scm_sword_t cd;
    int r = scm_capi_num_to_sword(op, &cd);
    if (r < 0) return -1;        /* [ERR]: [through] */

    if (cd >= UINT8_MAX) {
      scm_capi_error("Assembler: invalid opcode", 1, op);
      return -1;
    }
    return (int)cd;
  }
  else if (scm_capi_symbol_p(op)) {
    char mne[32];
    ssize_t rslt = scm_capi_symbol_to_cstr(op, mne, sizeof(mne));
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    rslt = scm_asm_mne2opcode(mne);
    if (rslt < 0) {
      scm_capi_error("Assembler: unknown mnemonic", 1, op);
      return -1;
    }

    return (int)rslt;
  }
  else {
    scm_capi_error("Assembler: invalid opcode", 1, op);
    return -1;                  /* [ERR]: iseq: unknown op */
  }
}

static ssize_t
scm_asm_inst_noarg(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                   size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ssize_t nr_arg;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands);

  nr_arg = scm_capi_length(operands);
  if (nr_arg != 0) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  return scm_capi_iseq_push_opfmt_noarg(iseq, (uint8_t)opcode);
}

static ssize_t
scm_asm_inst_obj(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_arg;

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1; /* [ERR]: [throughg] */

  return scm_capi_iseq_push_opfmt_obj(iseq, opcode, arg);
}

static ssize_t
scm_asm_inst_si(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  scm_sword_t val;
  ssize_t nr_arg;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1; /* [ERR]: [throughg] */

  if (!scm_capi_integer_p(arg)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg);
    return -1;
  }

  rslt = scm_capi_num_to_sword(arg, &val);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (val < INT32_MIN || INT32_MAX < val) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg);
    return -1;
  }

  return scm_capi_iseq_push_opfmt_si(iseq, opcode, (int32_t)val);
}

static ssize_t
scm_asm_inst_si_si(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                   size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  scm_sword_t val1, val2;
  ssize_t nr_arg;
  int rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg1, &arg2);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 2) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 2) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [throughg] */

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1; /* [ERR]: [throughg] */

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_integer_p(arg2)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg2);
    return -1;
  }

  rslt = scm_capi_num_to_sword(arg1, &val1);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (val1 < INT32_MIN || INT32_MAX < val1) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  rslt = scm_capi_num_to_sword(arg2, &val2);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (val2 < INT32_MIN || INT32_MAX < val2) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg2);
    return -1;
  }

  return scm_capi_iseq_push_opfmt_si_si(iseq, opcode,
                                        (int32_t)val1, (int32_t)val2);
}

static ssize_t
scm_asm_inst_si_obj(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                    size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  scm_sword_t val;
  ssize_t nr_arg;
  int rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg1, &arg2);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 2) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 2) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [throughg] */

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1; /* [ERR]: [throughg] */

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg1);
    return -1;
  }

  rslt = scm_capi_num_to_sword(arg1, &val);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (val < INT32_MIN || INT32_MAX < val) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  return scm_capi_iseq_push_opfmt_si_obj(iseq, opcode, (int32_t)val, arg2);
}

static ssize_t
scm_asm_inst_iof(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj label = SCM_OBJ_INIT;
  ssize_t i, nr_arg, rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(0 <= opcode && opcode <= UINT8_MAX);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  label = scm_api_car(operands);
  if (scm_obj_null_p(label)) return -1; /* [ERR]: [through] */

  if (!scm_capi_symbol_p(label)) {
    scm_capi_error("Assembler: operands is not symbol", 2, operator, label);
    return -1;
  }

  i = scm_capi_iseq_push_opfmt_iof(iseq, opcode, 0);
  if (i < 0) return -1;      /* [ERR]: [through] */

  rslt = scm_iseq_asm_reg_label_ref_idx(label_tbl,
                                        labels, label, (size_t)i - 4);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return i;
}

static ssize_t
scm_asm_inst_label(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                          size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_arg;
  int rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1; /* [ERR]: [throughg] */


  if (!scm_capi_symbol_p(arg)) {
    scm_capi_error("Assembler: operand is not symbol", 2, operator, arg);
    return -1;
  }

  rslt = scm_asm_reg_label_def_idx(label_tbl, labels, arg, idx);
  return (rslt < 0) ?  -1 : (ssize_t)idx;
}

static ssize_t
scm_asm_inst_asm(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_arg;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1; /* [ERR]: [throughg] */

  if (!scm_capi_pair_p(arg) && !scm_capi_nil_p(arg)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg);
    return -1;
  }

  arg = scm_asm_assemble(arg);
  if (scm_obj_null_p(arg)) return -1; /* [ERR]: [through] */

  return scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, arg);
}

static ssize_t
scm_asm_inst_asm_close(ScmObj iseq, int opcode,
                       ScmObj operator, ScmObj operands,
                       size_t idx, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  scm_sword_t val;
  ssize_t nr_arg;
  int rslt;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  SCM_STACK_FRAME_PUSH(&iseq, &operator, &operands,
                       &arg1, &arg2);

  nr_arg = scm_capi_length(operands);
  if (nr_arg < 2) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_arg > 2) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1; /* [ERR]: [throughg] */

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1; /* [ERR]: [throughg] */

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_pair_p(arg2) && !scm_capi_nil_p(arg2)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg2);
    return -1;
  }

  rslt = scm_capi_num_to_sword(arg1, &val);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  if (val < INT32_MIN || INT32_MAX < val) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  arg2 = scm_asm_assemble(arg2);
  if (scm_obj_null_p(arg2)) return -1; /* [ERR]: [through] */

  return scm_capi_iseq_push_opfmt_si_obj(iseq, SCM_OPCODE_CLOSE,
                                         (int32_t)val, arg2);
}

static ssize_t
scm_asm_inst(ScmObj iseq, ScmObj inst, size_t idx,
             ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj operator = SCM_OBJ_INIT, operands = SCM_OBJ_INIT;
  int opcode;

  SCM_STACK_FRAME_PUSH(&iseq, &inst,
                       &operator, &operands);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(!scm_obj_null_p(inst));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  if (scm_capi_pair_p(inst)) {
    operator = scm_api_car(inst);
    if (scm_obj_null_p(operator)) return -1; /* [ERR]: [thorugh] */

    operands = scm_api_cdr(inst);
    if (scm_obj_null_p(operands)) return -1; /* [ERR]: [through] */
  }
  else {
    operator = inst;
    operands = scm_api_nil();
  }

  opcode = scm_asm_sym2opcode(operator);
  if (opcode < 0) return -1;    /* [ERR]: [through] */

  if (opcode < SCM_ASM_PI_START) {
    int fmtid = scm_capi_opcode_to_opfmt(opcode);
    if (fmtid < 0) return -1;    /* [ERR]: [through] */

    switch (fmtid) {
    case SCM_OPFMT_NOARG:
      return scm_asm_inst_noarg(iseq, opcode, operator, operands,
                                idx, label_tbl, labels);
      break;
    case SCM_OPFMT_OBJ:
      return scm_asm_inst_obj(iseq, opcode, operator, operands,
                              idx, label_tbl, labels);
      break;
    case SCM_OPFMT_SI:
      return scm_asm_inst_si(iseq, opcode, operator, operands,
                             idx, label_tbl, labels);
      break;
    case SCM_OPFMT_SI_SI:
      return scm_asm_inst_si_si(iseq, opcode, operator, operands,
                                idx, label_tbl, labels);
      break;
    case SCM_OPFMT_SI_OBJ:
      return scm_asm_inst_si_obj(iseq, opcode, operator, operands,
                                 idx, label_tbl, labels);
      break;
    case SCM_OPFMT_IOF:
      return scm_asm_inst_iof(iseq, opcode, operator, operands,
                              idx, label_tbl, labels);
      break;
    default:
      scm_assert(false);
      break;
    }
  }
  else {
    switch (opcode) {
    case SCM_ASM_PI_LABEL:
      return scm_asm_inst_label(iseq, opcode, operator, operands,
                                idx, label_tbl, labels);
      break;
    case SCM_ASM_PI_ASM:
      return scm_asm_inst_asm(iseq, opcode, operator, operands,
                              idx, label_tbl, labels);
      break;
    case SCM_ASM_PI_ASM_CLOSE:
      return scm_asm_inst_asm_close(iseq, opcode, operator, operands,
                                    idx, label_tbl, labels);
      break;
    default:
      scm_assert(false);
      break;
    }
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

    if (!found) {
      scm_capi_fatal("Assember: inner error occured");
      goto err_free_rec;
    }

    rec = (ScmLabelInfo *)val;
    EARY_FOR_EACH(&rec->ref, idx2,  ref_idx) {
      if ((ssize_t)rec->idx < INT32_MIN + (ssize_t)*ref_idx + 4) {
        scm_capi_error("Assember: operand is underflow", 0);
        goto err_free_rec;
      }
      else if ((ssize_t)*ref_idx - 4 > INT32_MAX - (ssize_t)rec->idx) {
        scm_capi_error("Assember: operand is overflow", 0);
        goto err_free_rec;
      }

      scm_capi_iseq_set_si(iseq, *ref_idx,
                           (int32_t)((ssize_t)rec->idx
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
