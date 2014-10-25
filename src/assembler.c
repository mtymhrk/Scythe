#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/chashtbl.h"
#include "scythe/earray.h"
#include "scythe/vminst.h"
#include "scythe/assembler.h"

static struct {
  int code;
  const char *mne;
} opcode2mnemonic_tbl[] = {
  { SCM_OPCODE_NOP,         "nop" },
  { SCM_OPCODE_HALT,        "halt" },
  { SCM_OPCODE_UNDEF,       "undef" },
  { SCM_OPCODE_UNINIT,      "uninit" },
  { SCM_OPCODE_CFRAME,      "cframe" },
  { SCM_OPCODE_EFRAME,      "eframe" },
  { SCM_OPCODE_EPOP,        "epop" },
  { SCM_OPCODE_ESHIFT,      "eshift" },
  { SCM_OPCODE_IMMVAL,      "immval" },
  { SCM_OPCODE_PUSH,        "push" },
  { SCM_OPCODE_MVPUSH,      "mvpush" },
  { SCM_OPCODE_RETURN,      "return" },
  { SCM_OPCODE_PCALL,       "pcall" },
  { SCM_OPCODE_CALL,        "call" },
  { SCM_OPCODE_TAIL_CALL,   "tcall" },
  { SCM_OPCODE_GREF,        "gref" },
  { SCM_OPCODE_GDEF,        "gdef" },
  { SCM_OPCODE_GSET,        "gset" },
  { SCM_OPCODE_SREF,        "sref" },
  { SCM_OPCODE_SSET,        "sset" },
  { SCM_OPCODE_JMP,         "jmp" },
  { SCM_OPCODE_JMPT,        "jmpt" },
  { SCM_OPCODE_JMPF,        "jmpf" },
  { SCM_OPCODE_BOX,         "box" },
  { SCM_OPCODE_CLOSE,       "close" },
  { SCM_OPCODE_DEMINE,      "demine" },
  { SCM_OPCODE_EMINE,       "emine" },
  { SCM_OPCODE_EDEMINE,     "edemine" },
  { SCM_OPCODE_MRVC,        "mrvc" },
  { SCM_OPCODE_MRVE,        "mrve" },
  { SCM_ASM_PI_LABEL,       "label" },
  { SCM_ASM_PI_ASM,         "asm" },
  { SCM_ASM_PI_ASM_CLOSE,   "asm-close" },
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

static ScmLabelInfo *
scm_asm_new_label_rec(void)
{
  int rslt;
  ScmLabelInfo *rec;

  rec = scm_capi_malloc(sizeof(*rec));
  if (rec == NULL) return NULL;

  rslt = eary_init(&rec->ref, sizeof(size_t), 8);
  if (rslt < 0) {
    scm_capi_free(rec);
    return NULL;
  };

  rec->label[0] = '\0';
  rec->offset = 0;
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
scm_asm_reg_label_ref(ScmCHashTbl *tbl, EArray *labels,
                      ScmObj label, size_t offset)
{
  int rslt;
  bool found;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  char label_name[SCM_ISEQ_LABEL_NAME_MAX], *p;
  ssize_t name_sz;

  SCM_REFSTK_INIT_REG(&label);

  scm_assert(tbl != 0);
  scm_assert(scm_capi_symbol_p(label));

  name_sz = scm_capi_symbol_bytesize(label);
  if (name_sz < 0) return -1;

  if ((size_t)name_sz > sizeof(label_name) - 1) {
    scm_capi_error("Assembler: label name is too long", 1, label);
    return -1;
  }

  p = scm_capi_symbol_to_cstr(label, label_name, sizeof(label_name));
  if (p == NULL) return -1;

  rslt = scm_chash_tbl_get(tbl, SCM_CHASH_TBL_KEY(label_name), &val, &found);
  if (rslt < 0) return -1;

  if (found) {
    rec = (ScmLabelInfo *)val;
  }
  else {
    rec = scm_asm_new_label_rec();
    if (rec == NULL) return -1;

    memcpy(rec->label, label_name, (size_t)name_sz + 1);
    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_asm_del_label_rec(rec);
      return -1;
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_asm_del_label_rec(rec);
      return -1;
    }
  }

  EARY_PUSH(&rec->ref, size_t, offset, rslt);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_reg_label_def(ScmCHashTbl *tbl, EArray *labels,
                      ScmObj label, size_t offset)
{
  int rslt;
  bool found;
  ScmCHashTblVal val;
  ScmLabelInfo *rec;
  char label_name[SCM_ISEQ_LABEL_NAME_MAX], *p;
  ssize_t name_sz;

  SCM_REFSTK_INIT_REG(&label);

  scm_assert(tbl != NULL);
  scm_assert(labels != NULL);
  scm_assert(scm_capi_symbol_p(label));

  name_sz = scm_capi_symbol_bytesize(label);
  if (name_sz < 0) return -1;

  if ((size_t)name_sz > sizeof(label_name) - 1) {
    scm_capi_error("Assember: label name is too long", 1, label);
    return -1;
  }

  p = scm_capi_symbol_to_cstr(label, label_name, sizeof(label_name));
  if (p == NULL) return -1;

  rslt = scm_chash_tbl_get(tbl, SCM_CHASH_TBL_KEY(label_name), &val, &found);
  if (rslt < 0) return -1;

  if (found) {
    rec = (ScmLabelInfo *)val;
  }
  else {
    rec = scm_asm_new_label_rec();
    if (rec == NULL) return -1;

    memcpy(rec->label, label_name, (size_t)name_sz + 1);

    rslt = scm_chash_tbl_insert(tbl, SCM_CHASH_TBL_KEY(rec->label),
                                SCM_CHASH_TBL_VAL(rec));
    if (rslt < 0) {
      scm_asm_del_label_rec(rec);
      return -1;
    }

    EARY_PUSH(labels, char *, rec->label, rslt);
    if (rslt < 0) {
      scm_chash_tbl_delete(tbl, SCM_CHASH_TBL_KEY(rec->label), NULL, NULL);
      scm_asm_del_label_rec(rec);
      return -1;
    }
  }

  if (rec->defined_p) {
    scm_capi_error("Assembler: label is already defined", 1, label);
    return -1;
  }

  rec->offset = offset;
  rec->defined_p = true;

  return 0;
}

static int
scm_asm_sym2opcode(ScmObj op)
{
  SCM_REFSTK_INIT_REG(&op);

  scm_assert(!scm_capi_null_value_p(op));

  if (scm_capi_integer_p(op)) {
    scm_sword_t cd;
    int r = scm_capi_integer_to_sword(op, &cd);
    if (r < 0) return -1;

    if (cd > SCM_BYTE_MAX) {
      scm_capi_error("Assembler: invalid opcode", 1, op);
      return -1;
    }
    return (int)cd;
  }
  else if (scm_capi_symbol_p(op)) {
    ssize_t rslt;
    char mne[32];
    char *p = scm_capi_symbol_to_cstr(op, mne, sizeof(mne));
    if (p == NULL) return -1;

    rslt = scm_asm_mnemonic2opcode(mne);
    if (rslt < 0) {
      scm_capi_error("Assembler: unknown mnemonic", 1, op);
      return -1;
    }

    return (int)rslt;
  }
  else {
    scm_capi_error("Assembler: invalid opcode", 1, op);
    return -1;
  }
}

static ssize_t
scm_asm_inst_noopd(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                   size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ssize_t nr_opd;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd != 0) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  return scm_capi_iseq_push_inst(iseq, opcode);
}

static ssize_t
scm_asm_inst_obj(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_opd;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1;

  return scm_capi_iseq_push_inst(iseq, opcode, arg);
}

static ssize_t
scm_asm_inst_obj_obj(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                     size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  ssize_t nr_opd;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg1, &arg2);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 2) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 2) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  return scm_capi_iseq_push_inst(iseq, opcode, arg1, arg2);
}

static ssize_t
scm_asm_inst_si(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  scm_sword_t val;
  ssize_t nr_opd;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1;

  if (!scm_capi_integer_p(arg)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg, &val);
  if (rslt < 0) return -1;

  if (val < INT_MIN || INT_MAX < val) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg);
    return -1;
  }

  return scm_capi_iseq_push_inst(iseq, opcode, (int)val);
}

static ssize_t
scm_asm_inst_si_si(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                   size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  scm_sword_t val1, val2;
  ssize_t nr_opd;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg1, &arg2);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 2) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 2) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_integer_p(arg2)) {
    scm_capi_error("Assembler: operand is not integer", 2, operator, arg2);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg1, &val1);
  if (rslt < 0) return -1;

  if (val1 < INT_MIN || INT_MAX < val1) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg2, &val2);
  if (rslt < 0) return -1;

  if (val2 < INT_MIN || INT_MAX < val2) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg2);
    return -1;
  }

  return scm_capi_iseq_push_inst(iseq, opcode, (int)val1, (int)val2);
}

static ssize_t
scm_asm_inst_si_si_obj(ScmObj iseq, int opcode,
                       ScmObj operator, ScmObj operands,
                       size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT, arg3 = SCM_OBJ_INIT;
  scm_sword_t val1, val2;
  ssize_t nr_opd;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg1, &arg2, &arg3);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 3) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 3) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  arg3 = scm_capi_list_ref(operands, 2);
  if (scm_obj_null_p(arg3)) return -1;

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_integer_p(arg2)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg2);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg1, &val1);
  if (rslt < 0) return -1;

  if (val1 < INT_MIN || INT_MAX < val1) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg2, &val2);
  if (rslt < 0) return -1;

  if (val2 < INT_MIN || INT_MAX < val2) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg2);
    return -1;
  }

  return scm_capi_iseq_push_inst(iseq, opcode, (int)val1, (int)val2, arg3);
}

static ssize_t
scm_asm_inst_iof(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj label = SCM_OBJ_INIT;
  ssize_t ret, nr_opd, rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &label);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  label = scm_api_car(operands);
  if (scm_obj_null_p(label)) return -1;

  if (!scm_capi_symbol_p(label)) {
    scm_capi_error("Assembler: operands is not symbol", 2, operator, label);
    return -1;
  }

  ret = scm_capi_iseq_push_inst(iseq, opcode, 0);
  if (ret < 0) return -1;

  rslt = scm_asm_reg_label_ref(label_tbl, labels, label, offset);
  if (rslt < 0) return -1;

  return ret;
}

static ssize_t
scm_asm_inst_label(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                   size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_opd;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1;


  if (!scm_capi_symbol_p(arg)) {
    scm_capi_error("Assembler: operand is not symbol", 2, operator, arg);
    return -1;
  }

  rslt = scm_asm_reg_label_def(label_tbl, labels, arg, offset);
  return (rslt < 0) ?  -1 : (ssize_t)offset;
}

static ssize_t
scm_asm_inst_asm(ScmObj iseq, int opcode, ScmObj operator, ScmObj operands,
                 size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg = SCM_OBJ_INIT;
  ssize_t nr_opd;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 1) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 1) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg = scm_api_car(operands);
  if (scm_obj_null_p(arg)) return -1;

  if (!scm_capi_pair_p(arg) && !scm_capi_nil_p(arg)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg);
    return -1;
  }

  arg = scm_asm_assemble(arg, SCM_OBJ_NULL);
  if (scm_obj_null_p(arg)) return -1;

  return scm_capi_iseq_push_inst(iseq, SCM_OPCODE_IMMVAL, arg);
}

static ssize_t
scm_asm_inst_asm_close(ScmObj iseq, int opcode,
                       ScmObj operator, ScmObj operands,
                       size_t offset, ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT, arg3 = SCM_OBJ_INIT;
  scm_sword_t val1, val2;
  ssize_t nr_opd;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &operator, &operands,
                      &arg1, &arg2, &arg3);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_capi_symbol_p(operator));
  scm_assert(scm_capi_nil_p(operands) || scm_capi_pair_p(operands));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  nr_opd = scm_capi_length(operands);
  if (nr_opd < 3) {
    scm_capi_error("Assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if (nr_opd > 3) {
    scm_capi_error("Assembler: too many operands", 2, operator, operands);
    return -1;
  }

  arg1 = scm_capi_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_capi_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  arg3 = scm_capi_list_ref(operands, 2);
  if (scm_obj_null_p(arg3)) return -1;

  if (!scm_capi_integer_p(arg1)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_integer_p(arg2)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg1);
    return -1;
  }

  if (!scm_capi_pair_p(arg3) && !scm_capi_nil_p(arg3)) {
    scm_capi_error("Assembler: operand is not list", 2, operator, arg2);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg1, &val1);
  if (rslt < 0) return -1;

  if (val1 < INT_MIN || INT_MAX < val1) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg1);
    return -1;
  }

  rslt = scm_capi_integer_to_sword(arg2, &val2);
  if (rslt < 0) return -1;

  if (val2 < INT_MIN || INT_MAX < val2) {
    scm_capi_error("Assembler: operand is out of range", 2, operator, arg2);
    return -1;
  }

  arg3 = scm_asm_assemble(arg3, SCM_OBJ_NULL);
  if (scm_obj_null_p(arg3)) return -1;

  return scm_capi_iseq_push_inst(iseq,
                                 SCM_OPCODE_CLOSE, (int)val1, (int)val2, arg3);
}

static ssize_t
scm_asm_inst(ScmObj iseq, ScmObj inst, size_t offset,
             ScmCHashTbl *label_tbl, EArray *labels)
{
  ScmObj operator = SCM_OBJ_INIT, operands = SCM_OBJ_INIT;
  int opcode;

  SCM_REFSTK_INIT_REG(&iseq, &inst,
                      &operator, &operands);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(!scm_obj_null_p(inst));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  if (scm_capi_pair_p(inst)) {
    operator = scm_api_car(inst);
    if (scm_obj_null_p(operator)) return -1;

    operands = scm_api_cdr(inst);
    if (scm_obj_null_p(operands)) return -1;
  }
  else {
    operator = inst;
    operands = SCM_NIL_OBJ;
  }

  opcode = scm_asm_sym2opcode(operator);
  if (opcode < 0) return -1;

  if (opcode < SCM_ASM_PI_START) {
    int fmtid = scm_opfmt_table[opcode];
    if (fmtid < 0) return -1;

    switch (fmtid) {
    case SCM_OPFMT_NOOPD:
      return scm_asm_inst_noopd(iseq, opcode, operator, operands,
                                offset, label_tbl, labels);
      break;
    case SCM_OPFMT_OBJ:
      return scm_asm_inst_obj(iseq, opcode, operator, operands,
                              offset, label_tbl, labels);
      break;
    case SCM_OPFMT_OBJ_OBJ:
      return scm_asm_inst_obj_obj(iseq, opcode, operator, operands,
                                  offset, label_tbl, labels);
      break;
    case SCM_OPFMT_SI:
      return scm_asm_inst_si(iseq, opcode, operator, operands,
                             offset, label_tbl, labels);
      break;
    case SCM_OPFMT_SI_SI:
      return scm_asm_inst_si_si(iseq, opcode, operator, operands,
                                offset, label_tbl, labels);
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      return scm_asm_inst_si_si_obj(iseq, opcode, operator, operands,
                                    offset, label_tbl, labels);
      break;
    case SCM_OPFMT_IOF:
      return scm_asm_inst_iof(iseq, opcode, operator, operands,
                              offset, label_tbl, labels);
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
                                offset, label_tbl, labels);
      break;
    case SCM_ASM_PI_ASM:
      return scm_asm_inst_asm(iseq, opcode, operator, operands,
                              offset, label_tbl, labels);
      break;
    case SCM_ASM_PI_ASM_CLOSE:
      return scm_asm_inst_asm_close(iseq, opcode, operator, operands,
                                    offset, label_tbl, labels);
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
  size_t *ref_offset;

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);

  EARY_FOR_EACH(labels, idx1, lbl) {
    rslt = scm_chash_tbl_get(label_tbl, SCM_CHASH_TBL_KEY(*lbl), &val, &found);
    if (rslt < 0) return -1;

    if (!found) {
      scm_capi_fatal("Assember: inner error occured");
      goto err_free_rec;
    }

    rec = (ScmLabelInfo *)val;
    EARY_FOR_EACH(&rec->ref, idx2, ref_offset) {
      if ((ssize_t)rec->offset
          < INT_MIN + (ssize_t)*ref_offset + (ssize_t)SCM_OPFMT_INST_SZ_IOF) {
        scm_capi_error("Assember: operand is underflow", 0);
        goto err_free_rec;
      }
      else if (-((ssize_t)*ref_offset + (ssize_t)SCM_OPFMT_INST_SZ_IOF)
               > INT_MAX - (ssize_t)rec->offset) {
        scm_capi_error("Assember: operand is overflow", 0);
        goto err_free_rec;
      }

      scm_capi_iseq_update_oprand_iof(iseq,
                                      *ref_offset,
                                      (int)rec->offset
                                      - ((int)*ref_offset
                                         + (int)SCM_OPFMT_INST_SZ_IOF));
    }

    rslt = scm_chash_tbl_delete(label_tbl, SCM_CHASH_TBL_KEY(*lbl), NULL, NULL);
    if (rslt < 0) return -1;

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
  ssize_t offset;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &lst, &cur, &inst);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(label_tbl != NULL);
  scm_assert(labels != NULL);
  scm_assert(scm_capi_pair_p(lst) || scm_capi_nil_p(lst));

  offset = scm_capi_iseq_length(iseq);
  if (offset < 0) return -1;

  for (cur = lst;
       scm_obj_not_null_p(cur) && !scm_capi_nil_p(cur);
       cur = scm_api_cdr(cur)) {
    inst = scm_api_car(cur);
    offset = scm_asm_inst(iseq, inst, (size_t)offset, label_tbl, labels);
    if (offset < 0) return -1;
  }

  rslt = scm_asm_label_resolv(iseq, label_tbl, labels);
  if (rslt < 0) return -1;

  return 0;
}

ScmObj
scm_asm_assemble(ScmObj lst, ScmObj iseq)
{
  ScmCHashTbl *label_tbl = NULL;
  EArray labels;
  int rslt;

  SCM_REFSTK_INIT_REG(&lst, &iseq);

  scm_assert(scm_capi_pair_p(lst) || scm_capi_nil_p(lst));
  scm_assert(scm_obj_null_p(iseq) || scm_capi_iseq_p(iseq));

  rslt = eary_init(&labels, sizeof(char *), 32);
  if (rslt) return SCM_OBJ_NULL;

  if (scm_obj_null_p(iseq)) {
    iseq = scm_api_make_iseq();
    if (scm_obj_null_p(iseq)) goto err;
  }

  label_tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 32,
                                SCM_CHASH_TBL_CVAL, SCM_CHASH_TBL_CVAL,
                                scm_asm_hash, scm_asm_cmp);
  if (label_tbl == NULL) goto err;

  rslt = scm_asm_assemble_aux(iseq, label_tbl, &labels, lst);
  if (rslt < 0) goto err;

  eary_fin(&labels);
  scm_chash_tbl_end(label_tbl);

  return iseq;

 err:
  eary_fin(&labels);
  if (label_tbl != NULL) scm_chash_tbl_end(label_tbl);
  return SCM_OBJ_NULL;
}

int
scm_asm_mnemonic2opcode(const char *mne)
{
  scm_assert(mne != NULL);

  for (size_t i = 0;
       i < sizeof(opcode2mnemonic_tbl)/sizeof(opcode2mnemonic_tbl[0]);
       i++) {
    if (strcmp(opcode2mnemonic_tbl[i].mne, mne) == 0)
      return opcode2mnemonic_tbl[i].code;
  }

  return -1;
}

const char *
scm_asm_opcode2mnemonic(int code)
{
  for (size_t i = 0;
       i < sizeof(opcode2mnemonic_tbl)/sizeof(opcode2mnemonic_tbl[0]);
       i++) {
    if (opcode2mnemonic_tbl[i].code == code)
      return opcode2mnemonic_tbl[i].mne;
  }

  return NULL;
}

ScmObj
scm_asm_mnemonic(int opcode)
{
  const char *p;

  p = scm_asm_opcode2mnemonic(opcode);
  if (p == NULL) {
    scm_capi_error("Assembler: unknown opcode", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_make_symbol_from_cstr(p, SCM_ENC_SRC);
}
