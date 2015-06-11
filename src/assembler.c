#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/earray.h"
#include "scythe/vminst.h"
#include "scythe/assembler.h"


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

ScmTypeInfo SCM_ASSEMBLER_TYPE_INFO = {
  .name                = "assembler",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmAssembler),
  .gc_ini_func         = scm_asm_gc_initialize,
  .gc_fin_func         = scm_asm_gc_finalize,
  .gc_accept_func      = scm_asm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_asm_initialize(ScmObj asmb, ScmObj iseq)
{
  int r;

  SCM_REFSTK_INIT_REG(&asmb, &iseq);

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_null_p(iseq) || scm_fcd_iseq_p(iseq));

  if (scm_obj_null_p(iseq)) {
    iseq = scm_fcd_make_iseq();
    if (scm_obj_null_p(iseq)) return -1;
  }

  SCM_SLOT_SETQ(ScmAssembler, asmb, iseq, iseq);

  r = eary_init(SCM_ASSEMBLER_LABEL_DECL(asmb), sizeof(ScmAsmLabelDecl), 0);
  if (r < 0) return -1;

  r = eary_init(SCM_ASSEMBLER_LABEL_REF(asmb), sizeof(ScmAsmLabelRef), 0);
  if (r < 0) return -1;

  return 0;
}

void
scm_asm_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);

  SCM_ASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_fin(SCM_ASSEMBLER_LABEL_DECL(obj));
  eary_fin(SCM_ASSEMBLER_LABEL_REF(obj));
}

static inline bool
scm_asm_label_id_assigned_p(ScmObj asmb, size_t id)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  return (id < EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb)));
}

static int
scm_asm_register_label_ref(ScmObj asmb, size_t offset, size_t label_id)
{
  ScmAsmLabelRef ref = { .offset = offset, .label_id = label_id };
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  EARY_PUSH(SCM_ASSEMBLER_LABEL_REF(asmb), ScmAsmLabelRef, ref, r);
  if (r < 0) return -1;

  return 0;
}

static int
scm_asm_calc_iof(const ScmAsmLabelRef *ref, const ScmAsmLabelDecl *decl,
                 int *iof)
{
  ptrdiff_t x;

  scm_assert(ref != NULL);
  scm_assert(decl != NULL);
  scm_assert(iof != NULL);

  if (decl->offset < 0) {
    scm_fcd_error("failed to resolve reference to assembler labels: "
                  "specified label has not undeclared", 0);
    return -1;
  }

  x = ((ptrdiff_t)decl->offset
       - (ptrdiff_t)(ref->offset + SCM_OPFMT_INST_SZ_IOF));
  if ((ref->offset > PTRDIFF_MAX - SCM_OPFMT_INST_SZ_IOF)
      || (x < INT_MIN || INT_MAX < x)) {
    scm_fcd_error("failed to resolve reference to assembler labels: "
                  "arithmetic overflow", 0);
    return -1;
  }

  *iof = (int)x;
  return 0;
}

ssize_t
scm_asm_assign_label_id(ScmObj asmb)
{
  ScmAsmLabelDecl d = { .offset = -1 };
  size_t i;
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  i = EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb));
  if (i >= SSIZE_MAX) {
    scm_fcd_error("failed to assign assembler label id: too many lables", 0);
    return -1;
  }

  EARY_PUSH(SCM_ASSEMBLER_LABEL_DECL(asmb), ScmAsmLabelDecl, d, r);
  if (r < 0) return -1;

  return (ssize_t)i;
}

int
scm_asm_register_label_id(ScmObj asmb, size_t id)
{
  const ScmAsmLabelDecl d = { .offset = -1 };
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (id >= SSIZE_MAX) {
    scm_fcd_error("failed to register assembler label id: too big", 0);
    return -1;
  }

  for (size_t i = EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb)); i <= id; i++) {
    EARY_SET(SCM_ASSEMBLER_LABEL_DECL(asmb), ScmAsmLabelDecl, i, d, r);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_asm_push_inst_noopd(ScmObj asmb, scm_opcode_t op)
{
  scm_byte_t inst[SCM_OPFMT_INST_SZ_NOOPD];
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_noopd(inst, op);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), NULL, 0);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_obj(ScmObj asmb, scm_opcode_t op, ScmObj obj)
{
  static const size_t objs[1] = { SCM_INST_OPD_OFFSET_OBJ_1 };
  scm_byte_t inst[SCM_OPFMT_INST_SZ_OBJ];
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_obj(inst, op, obj);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), objs, 1);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_obj_obj(ScmObj asmb,
                          scm_opcode_t op, ScmObj obj1, ScmObj obj2)
{
  static const size_t objs[2] = {
    SCM_INST_OPD_OFFSET_OBJ_OBJ_1, SCM_INST_OPD_OFFSET_OBJ_OBJ_2
  };
  scm_byte_t inst[SCM_OPFMT_INST_SZ_OBJ_OBJ];
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_obj_obj(inst, op, obj1, obj2);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), objs, 2);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si(ScmObj asmb, scm_opcode_t op, int si)
{
  scm_byte_t inst[SCM_OPFMT_INST_SZ_SI];
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_si(inst, op, si);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), NULL, 0);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si_si(ScmObj asmb, scm_opcode_t op, int si1, int si2)
{
  scm_byte_t inst[SCM_OPFMT_INST_SZ_SI_SI];
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_si_si(inst, op, si1, si2);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), NULL, 0);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si_si_obj(ScmObj asmb,
                            scm_opcode_t op, int si1, int si2, ScmObj obj)
{
  static const size_t objs[1] = { SCM_INST_OPD_OFFSET_SI_SI_OBJ_3 };
  scm_byte_t inst[SCM_OPFMT_INST_SZ_SI_SI_OBJ];
  ssize_t r;

  SCM_REFSTK_INIT_REG(&asmb, &obj);

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (op == SCM_OPCODE_CLOSE && scm_fcd_assembler_p(obj))
    obj = scm_asm_iseq(obj);

  op = scm_fcd_internal_opcode(op);
  scm_vminst_set_inst_si_si_obj(inst, op, si1, si2, obj);
  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                             &inst, sizeof(inst), objs, 1);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_iof(ScmObj asmb, scm_opcode_t op, bool label, ...)
{
  va_list arg;
  int ret;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  va_start(arg, label);
  if (label) {
    ret = scm_asm_push_inst_rlid(asmb, op, va_arg(arg, size_t));
  }
  else {
    scm_byte_t inst[SCM_OPFMT_INST_SZ_IOF];
    op = scm_fcd_internal_opcode(op);
    scm_vminst_set_inst_iof(inst, op, va_arg(arg, int));
    ssize_t r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                                       &inst, sizeof(inst), NULL, 0);
    if (r < 0) ret = -1;
    else       ret = 0;
  }
  va_end(arg);

  return ret;
}

int
scm_asm_push_inst_rlid(ScmObj asmb, scm_opcode_t op, size_t id)
{
  const ScmAsmLabelDecl *decl;
  size_t cur;
  int r, iof;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (!scm_asm_label_id_assigned_p(asmb, id)) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified label has not undeclared", 0);
    return -1;
  }

  cur = scm_fcd_iseq_length(SCM_ASSEMBLER_ISEQ(asmb));

  decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), id);
  if (decl->offset < 0) {
    r = scm_asm_register_label_ref(asmb, cur, id);
    if (r < 0) return -1;
    iof = 0;
  }
  else {
    ScmAsmLabelRef ref = { .offset = cur, .label_id = id };
    r = scm_asm_calc_iof(&ref, decl, &iof);
    if (r < 0) return -1;
  }

  r = scm_asm_push_inst_iof(asmb, op, false, (int)iof);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_pinst_label(ScmObj asmb, scm_opcode_t op, size_t id)
{
  ScmAsmLabelDecl *decl;
  size_t cur;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (id >= EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb))) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified label has not assigned", 0);
    return -1;
  }

  decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), id);
  if (decl->offset >= 0) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified lable has already declared", 0);
    return -1;
  }

  cur = scm_fcd_iseq_length(SCM_ASSEMBLER_ISEQ(asmb));
  if (cur > SSIZE_MAX) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "instructin sequence too big", 0);
    return -1;
  }

  decl->offset = (ssize_t)cur;

  return 0;
}

int
scm_asm_resolve_label_ref(ScmObj asmb)
{
  const ScmAsmLabelDecl *decl;
  const ScmAsmLabelRef *ref;
  size_t idx;
  int r, iof;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  EARY_FOR_EACH(SCM_ASSEMBLER_LABEL_REF(asmb), idx, ref) {
    decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), ref->label_id);
    r = scm_asm_calc_iof(ref, decl, &iof);
    if (r < 0) return -1;

    scm_fcd_update_vminst_opd_iof(SCM_ASSEMBLER_ISEQ(asmb),
                                  scm_fcd_iseq_to_ip(SCM_ASSEMBLER_ISEQ(asmb))
                                  + ref->offset,
                                  iof);
  }

  eary_truncate(SCM_ASSEMBLER_LABEL_REF(asmb));
  return 0;
}

void
scm_asm_clear_labels(ScmObj asmb)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  if (EARY_SIZE(SCM_ASSEMBLER_LABEL_REF(asmb)) == 0)
    eary_truncate(SCM_ASSEMBLER_LABEL_DECL(asmb));
}

void
scm_asm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);

  SCM_ASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_init(SCM_ASSEMBLER_LABEL_DECL(obj), 0, 0);
  eary_init(SCM_ASSEMBLER_LABEL_REF(obj), 0, 0);
}

void
scm_asm_gc_finalize(ScmObj obj)
{
  scm_asm_finalize(obj);
}

int
scm_asm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_ASSEMBLER_ISEQ(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/**************************************************************************/
/* Assembler (interface)                                                  */
/**************************************************************************/

bool
scm_fcd_assembler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_ASSEMBLER_TYPE_INFO);
}

ScmObj
scm_fcd_assembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq)
{
  ScmObj asmb = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq,
                      &asmb);

  scm_assert(scm_obj_null_p(iseq) || scm_fcd_iseq_p(iseq));

  asmb = scm_fcd_mem_alloc(&SCM_ASSEMBLER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  if (scm_asm_initialize(asmb, iseq) < 0)
    return SCM_OBJ_NULL;

  return asmb;
}

ScmObj
scm_fcd_make_assembler(ScmObj iseq)
{
  scm_assert(scm_obj_null_p(iseq) || scm_fcd_iseq_p(iseq));
  return scm_fcd_assembler_new(SCM_MEM_HEAP, iseq);
}

ssize_t
scm_fcd_assembler_assign_label_id(ScmObj asmb)
{
  scm_assert(scm_fcd_assembler_p(asmb));
  return scm_asm_assign_label_id(asmb);
}

int
scm_fcd_assembler_register_label_id(ScmObj asmb, size_t id)
{
  scm_assert(scm_fcd_assembler_p(asmb));
  return scm_asm_register_label_id(asmb, id);
}

static inline int
chk_operand_obj(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_fcd_error("failed to push a instruction to ISeq: invalid operand",
                  1, obj);
    return -1;
  }

  return 0;
}

int
scm_fcd_assembler_push_va(ScmObj asmb, scm_opcode_t op, va_list operands)
{
  ScmObj opd_obj1 = SCM_OBJ_INIT, opd_obj2 = SCM_OBJ_INIT;
  size_t opd_label_id;
  int r, opd_si1, opd_si2;
  bool label;

  SCM_REFSTK_INIT_REG(&asmb,
                      &opd_obj1, &opd_obj2);

  scm_assert(scm_fcd_assembler_p(asmb));
  /* scm_assert(op >= 0); */

  if (op < SCM_VMINST_NR_OP) {
    switch (scm_opfmt_table[op]) {
    case SCM_OPFMT_NOOPD:
      return scm_asm_push_inst_noopd(asmb, op);
      break;
    case SCM_OPFMT_OBJ:
      opd_obj1 = va_arg(operands, ScmObj);
      r = chk_operand_obj(opd_obj1);
      if (r < 0) return -1;
      return scm_asm_push_inst_obj(asmb, op, opd_obj1);
      break;
    case SCM_OPFMT_OBJ_OBJ:
      opd_obj1 = va_arg(operands, ScmObj);
      opd_obj2 = va_arg(operands, ScmObj);
      r = chk_operand_obj(opd_obj1);
      if (r < 0) return -1;
      r = chk_operand_obj(opd_obj2);
      if (r < 0) return -1;
      return scm_asm_push_inst_obj_obj(asmb, op, opd_obj1, opd_obj2);
      break;
    case SCM_OPFMT_SI:
      opd_si1 = va_arg(operands, int);
      return scm_asm_push_inst_si(asmb, op, opd_si1);
      break;
    case SCM_OPFMT_SI_SI:
      opd_si1 = va_arg(operands, int);
      opd_si2 = va_arg(operands, int);
      return scm_asm_push_inst_si_si(asmb, op, opd_si1, opd_si2);
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      opd_si1 = va_arg(operands, int);
      opd_si2 = va_arg(operands, int);
      opd_obj1 = va_arg(operands, ScmObj);
      r = chk_operand_obj(opd_obj1);
      if (r < 0) return -1;
      return scm_asm_push_inst_si_si_obj(asmb, op, opd_si1, opd_si2, opd_obj1);
      break;
    case SCM_OPFMT_IOF:
      label = (bool)va_arg(operands, int);
      if (label) {
        opd_label_id = va_arg(operands, size_t);
        return scm_asm_push_inst_iof(asmb, op, label, opd_label_id);
      }
      else {
        opd_si1 = va_arg(operands, int);
        return scm_asm_push_inst_iof(asmb, op, label, opd_si1);
      }
      break;
    default:
      scm_fcd_error("failed to assemble: unknown instruction format", 0);
      return -1;
      break;
    }
  }
  else {
    switch (op) {
    case SCM_ASM_PI_START:
      opd_label_id = va_arg(operands, size_t);
      return scm_asm_push_pinst_label(asmb, op, opd_label_id);
      break;
    default:
      scm_fcd_error("failed to assemble: unknown operator code", 0);
      return -1;
      break;
    }
  }
}

int
scm_fcd_assembler_push(ScmObj asmb, scm_opcode_t op, ...)
{
  va_list operands;
  int r;

  scm_assert(scm_fcd_assembler_p(asmb));
  /* scm_assert(op >= 0); */

  va_start(operands, op);
  r = scm_fcd_assembler_push_va(asmb, op, operands);
  va_end(operands);

  return r;
}

int
scm_fcd_assembler_commit(ScmObj asmb)
{
  int r;

  scm_assert(scm_fcd_assembler_p(asmb));

  r = scm_asm_resolve_label_ref(asmb);
  if (r < 0) return -1;

  scm_asm_clear_labels(asmb);
  return 0;
}

ScmObj
scm_fcd_assembler_iseq(ScmObj asmb)
{
  scm_assert(scm_fcd_assembler_p(asmb));
  return scm_asm_iseq(asmb);
}


/**************************************************************************/
/* Disassembler                                                           */
/**************************************************************************/

ScmTypeInfo SCM_DISASSEMBLER_TYPE_INFO = {
  .name                = "disassembler",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmDisassembler),
  .gc_ini_func         = scm_disasm_gc_initialize,
  .gc_fin_func         = scm_disasm_gc_finalize,
  .gc_accept_func      = scm_disasm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static scm_byte_t *
scm_disasm_ip_proceed(scm_byte_t *ip)
{
  scm_opcode_t op = scm_fcd_external_opcode(SCM_VMINST_GET_OP(ip));
  switch (scm_opfmt_table[op]) {
  case SCM_OPFMT_NOOPD:
    ip += SCM_OPFMT_INST_SZ_NOOPD;
    break;
  case SCM_OPFMT_OBJ:
    ip += SCM_OPFMT_INST_SZ_OBJ;
    break;
  case SCM_OPFMT_OBJ_OBJ:
    ip += SCM_OPFMT_INST_SZ_OBJ_OBJ;
    break;
  case SCM_OPFMT_SI:
    ip += SCM_OPFMT_INST_SZ_SI;
    break;
  case SCM_OPFMT_SI_SI:
    ip += SCM_OPFMT_INST_SZ_SI_SI;
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    ip += SCM_OPFMT_INST_SZ_SI_SI_OBJ;
    break;
  case SCM_OPFMT_IOF:
    ip += SCM_OPFMT_INST_SZ_IOF;
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instruction format", 0);
    return NULL;
    break;
  }

  return ip;
}

static int
scm_disasm_push_label_decl(ScmObj disasm, size_t offset)
{
  ScmAsmLabelDecl d = { .offset = (ssize_t)offset };
  ScmAsmLabelDecl *ary;
  size_t idx;
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(offset <= SSIZE_MAX);

  EARY_PUSH(SCM_DISASSEMBLER_LABEL_DECL(disasm), ScmAsmLabelDecl, d, r);
  if (r < 0) return -1;

  idx = EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm)) - 1;
  ary = EARY_HEAD(SCM_DISASSEMBLER_LABEL_DECL(disasm));
  while (idx > 0 && d.offset < ary[idx - 1].offset)
    idx--;

  if (idx > 0 && d.offset == ary[idx - 1].offset) {
    EARY_POP(SCM_DISASSEMBLER_LABEL_DECL(disasm), ScmAsmLabelDecl, d);
    return 0;
  }

  for (size_t i = EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm)); i > idx; i--)
    ary[i] = ary[i - 1];
  ary[idx] = d;

  return 0;
}

static int
scm_disasm_acc_labels(ScmObj disasm)
{
  scm_byte_t *ip;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  ip = scm_fcd_iseq_to_ip(SCM_DISASSEMBLER_ISEQ(disasm));
  while (scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm), ip)) {
    scm_opcode_t op = scm_fcd_external_opcode(SCM_VMINST_GET_OP(ip));
    if (scm_opfmt_table[op] == SCM_OPFMT_IOF) {
      ssize_t offset;
      int r, iof;

      SCM_VMINST_FETCH_OPD_IOF(ip, iof);
      offset = scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                         ip + iof);
      if (offset < 0) return -1;
      r = scm_disasm_push_label_decl(disasm, (size_t)offset);
      if (r < 0) return -1;
    }
    else {
      ip = scm_disasm_ip_proceed(ip);
    }
  }

  return 0;
}

int
scm_disasm_initialize(ScmObj disasm, ScmObj iseq)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_fcd_iseq_p(iseq));

  r = eary_init(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                sizeof(ScmAsmLabelDecl), 0);
  if (r < 0) return -1;

  SCM_DISASSEMBLER_SET_TOKEN(disasm, scm_fcd_malloc(sizeof(ScmDisasmToken)));
  if (SCM_DISASSEMBLER_TOKEN(disasm) == NULL) return -1;

  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;

  SCM_SLOT_SETQ(ScmDisassembler, disasm, iseq, iseq);
  SCM_DISASSEMBLER_SET_IP(disasm, NULL);
  SCM_DISASSEMBLER_SET_DECL_IDX(disasm, 0);

  return scm_disasm_acc_labels(disasm);
}

void
scm_disasm_finalize(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  eary_fin(SCM_DISASSEMBLER_LABEL_DECL(disasm));
  if (SCM_DISASSEMBLER_TOKEN(disasm) != NULL) {
    scm_fcd_free(SCM_DISASSEMBLER_TOKEN(disasm));
    SCM_DISASSEMBLER_SET_TOKEN(disasm, NULL);
  }
}

static int
label_decl_cmp(const void *x, const void *y)
{
  const ScmAsmLabelDecl *a = x, *b = y;

  if (a->offset < b->offset)
    return -1;
  else if (a->offset == b->offset)
    return 0;
  else
    return 1;
}

static int
scm_disasm_setup_token_inst(ScmObj disasm)
{
  ScmAsmLabelDecl key, *p;
  scm_opcode_t op;
  int iof;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm),
                                        SCM_DISASSEMBLER_IP(disasm)));

  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_INST;

  op = SCM_VMINST_GET_OP(SCM_DISASSEMBLER_IP(disasm));
  op = scm_fcd_external_opcode(op);
  SCM_DISASSEMBLER_TOKEN(disasm)->inst.fmt = scm_opfmt_table[op];

  switch (scm_opfmt_table[op]) {
  case SCM_OPFMT_NOOPD:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.noopd,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_NOOPD);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_NOOPD);
    break;
  case SCM_OPFMT_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj,
                      SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_OBJ);
    break;
  case SCM_OPFMT_OBJ_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj,
                      SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_OBJ_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_OBJ_OBJ);
    break;
  case SCM_OPFMT_SI:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_SI);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI);
    break;
  case SCM_OPFMT_SI_SI:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_SI_SI);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI_SI);
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si_obj,
                      SCM_DISASSEMBLER_IP(disasm),
                      SCM_OPFMT_INST_SZ_SI_SI_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI_SI_OBJ);
    break;
  case SCM_OPFMT_IOF:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.iof,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_IOF);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_IOF);
    iof = SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.iof.opd1;
    key.offset = scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                           SCM_DISASSEMBLER_IP(disasm) + iof);
    if (key.offset < 0) return -1;
    p = eary_bsearch(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                     &key, label_decl_cmp);
    scm_assert(p != NULL);      /* must not happen */
    SCM_DISASSEMBLER_TOKEN(disasm)->label_id =
      (size_t)(p - (ScmAsmLabelDecl *)EARY_HEAD(SCM_DISASSEMBLER_LABEL_DECL(disasm)));
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instruction format", 0);
    return -1;
    break;
  }

  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op = op;

  return 0;
}

static int
scm_disasm_setup_token(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(SCM_DISASSEMBLER_IP(disasm) != NULL);

  if (SCM_DISASSEMBLER_TOKEN(disasm)->type == SCM_DISASM_TK_END)
    return 0;

  if (SCM_DISASSEMBLER_DECL_IDX(disasm)
      < EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm))) {
    ScmAsmLabelDecl *p = eary_idx_to_ptr(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                                         SCM_DISASSEMBLER_DECL_IDX(disasm));
    if (p->offset == scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                               SCM_DISASSEMBLER_IP(disasm))) {
      SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;
      SCM_DISASSEMBLER_TOKEN(disasm)->inst.fmt = SCM_OPFMT_NOOPD;
      SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op = SCM_ASM_PI_LABEL;
      SCM_DISASSEMBLER_TOKEN(disasm)->label_id = SCM_DISASSEMBLER_DECL_IDX(disasm);
      SCM_DISASSEMBLER_INC_DECL_IDX(disasm);
      return 0;
    }
  }

  if (scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm),
                                 SCM_DISASSEMBLER_IP(disasm))) {
    return scm_disasm_setup_token_inst(disasm);
  }
  else {
    SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_END;
    return 0;
  }
}

static inline int
scm_disasm_init_token_if_needed(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  if (SCM_DISASSEMBLER_IP(disasm) != NULL)
    return 0;

  SCM_DISASSEMBLER_SET_IP(disasm,
                          scm_fcd_iseq_to_ip(SCM_DISASSEMBLER_ISEQ(disasm)));
  return scm_disasm_setup_token(disasm);
}

const ScmDisasmToken *
scm_disasm_token(ScmObj disasm)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  r = scm_disasm_init_token_if_needed(disasm);
  if (r < 0) return NULL;

  return SCM_DISASSEMBLER_TOKEN(disasm);
}

int
scm_disasm_next(ScmObj disasm)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  r = scm_disasm_init_token_if_needed(disasm);
  if (r < 0) return -1;

  return scm_disasm_setup_token(disasm);
}

void
scm_disasm_rewind(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  SCM_DISASSEMBLER_SET_IP(disasm, NULL);
  SCM_DISASSEMBLER_SET_DECL_IDX(disasm, 0);
  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;
}

int
scm_disasm_cnv_to_marshalable(ScmObj disasm)
{
  SCM_REFSTK_INIT_REG(&disasm);

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  if (scm_disasm_token(disasm) == NULL)
    return -1;

  if (SCM_DISASSEMBLER_TOKEN(disasm)->type != SCM_DISASM_TK_INST)
    return 0;

  switch (SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op) {
  case SCM_OPCODE_GREF:         /* fall through */
  case SCM_OPCODE_GDEF:         /* fall through */
  case SCM_OPCODE_GSET:
    if (scm_fcd_gloc_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1))
      SCM_WB_SETQ(disasm,
                  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1,
                  scm_fcd_gloc_symbol(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1));
    if (scm_fcd_module_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2))
      SCM_WB_SETQ(disasm,
                  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2,
                  scm_fcd_module_name(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2));
    break;
  case SCM_OPCODE_MODULE:
    if (scm_fcd_module_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj.opd1))
      SCM_WB_SETQ(disasm,
                  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj.opd1,
                  scm_fcd_module_name(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj.opd1));
    break;
  default:
    break;
  }

  return 0;
}

void
scm_disasm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_DISASSEMBLER_TYPE_INFO);

  SCM_DISASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_init(SCM_DISASSEMBLER_LABEL_DECL(obj), 0, 0);
  SCM_DISASSEMBLER_SET_TOKEN(obj, NULL);
}

void
scm_disasm_gc_finalize(ScmObj obj)
{
  scm_disasm_finalize(obj);
}

int
scm_disasm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_DISASSEMBLER_ISEQ(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  if (SCM_DISASSEMBLER_TOKEN(obj) != NULL
      && SCM_DISASSEMBLER_TOKEN(obj)->type == SCM_DISASM_TK_INST) {
    switch (SCM_DISASSEMBLER_TOKEN(obj)->inst.fmt) {
    case SCM_OPFMT_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj.opd1,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    case SCM_OPFMT_OBJ_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj_obj.opd1,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj_obj.opd2,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.si_si_obj.opd3,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    default:
      break;
    }
  }

  return rslt;
}


/**************************************************************************/
/* Disassembler (interface)                                               */
/**************************************************************************/

bool
scm_fcd_disassembler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_DISASSEMBLER_TYPE_INFO);
}

ScmObj
scm_fcd_disassembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq)
{
  ScmObj disasm;

  SCM_REFSTK_INIT_REG(&iseq,
                      &disasm);

  scm_assert(scm_fcd_iseq_p(iseq));

  disasm = scm_fcd_mem_alloc(&SCM_DISASSEMBLER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(disasm)) return SCM_OBJ_NULL;

  if (scm_disasm_initialize(disasm, iseq) < 0)
    return SCM_OBJ_NULL;

  return disasm;
}

ScmObj
scm_fcd_make_disassembler(ScmObj iseq)
{
  return scm_fcd_disassembler_new(SCM_MEM_HEAP, iseq);
}

const ScmDisasmToken *
scm_fcd_disassembler_token(ScmObj disasm)
{
  scm_assert(scm_fcd_disassembler_p(disasm));
  return scm_disasm_token(disasm);
}

int
scm_fcd_disassembler_next(ScmObj disasm)
{
  scm_assert(scm_fcd_disassembler_p(disasm));
  return scm_disasm_next(disasm);
}

void
scm_fcd_disassembler_rewind(ScmObj disasm)
{
  scm_assert(scm_fcd_disassembler_p(disasm));
  return scm_disasm_rewind(disasm);
}

int
scm_fcd_disassembler_cnv_to_marshalable(ScmObj disasm)
{
  scm_assert(scm_fcd_disassembler_p(disasm));
  return scm_disasm_cnv_to_marshalable(disasm);
}


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

static struct {
  scm_opcode_t code;
  const char *mne;
} opcode2mnemonic_tbl[] = {
  { SCM_OPCODE_NOP,         "nop" },
  { SCM_OPCODE_HALT,        "halt" },
  { SCM_OPCODE_INT,         "int" },
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
  { SCM_OPCODE_MODULE,      "module" },
  { SCM_ASM_PI_LABEL,       "label" },
};

static int
scm_asm_sym2opcode(ScmObj op)
{
  SCM_REFSTK_INIT_REG(&op);

  scm_assert(!scm_obj_null_p(op));

  if (scm_fcd_integer_p(op)) {
    scm_sword_t cd;
    int r = scm_fcd_integer_to_sword(op, &cd);
    if (r < 0) return -1;

    if ((0 <= cd && cd < SCM_VMINST_NR_OP)
        || (SCM_ASM_PI_START <= cd && cd < SCM_ASM_PI_START + SCM_ASM_NR_PI)) {
      return (int)cd;
    }
    else if (cd == SCM_ASM_PI_LABEL) {
      return (int)cd;
    }
    else {
      scm_fcd_error("assembler: invalid opcode", 1, op);
      return -1;
    }
  }
  else if (scm_fcd_symbol_p(op)) {
    ssize_t rslt;
    char mne[32];
    char *p = scm_fcd_symbol_to_cstr(op, mne, sizeof(mne));
    if (p == NULL) return -1;

    rslt = scm_asm_mnemonic2opcode(mne);
    if (rslt < 0) {
      scm_fcd_error("assembler: unknown mnemonic", 1, op);
      return -1;
    }

    return (int)rslt;
  }
  else {
    scm_fcd_error("assembler: invalid opcode", 1, op);
    return -1;
  }
}

static int
scm_asm_chk_nr_operand(const ScmObj *inst, size_t n, size_t min, size_t max)
{
  ScmObj lst = SCM_OBJ_INIT;

  scm_assert(n == 0 || inst != NULL);

  if (n == 0 || (n - 1) < min) {
    lst = scm_fcd_list_cv(inst, n);
    if (scm_obj_not_null_p(lst))
      scm_fcd_error("assembler: too few operands", 1, lst);
    return -1;
  }
  else if ((n - 1) > max) {
    lst = scm_fcd_list_cv(inst, n);
    if (scm_obj_not_null_p(lst))
      scm_fcd_error("assembler: too many operands", 1, lst);
    return -1;
  }

  return 0;
}

static int
scm_asm_cnv_operand_to_si(ScmObj operand, ScmObj operator, int *si)
{
  scm_sword_t val;
  int r;

  scm_assert(scm_obj_not_null_p(operand));
  scm_assert(scm_obj_not_null_p(operator));
  scm_assert(si != NULL);

  if (!scm_fcd_integer_p(operand)) {
    scm_fcd_error("assembler: invalid operands: integer requried",
                  2, operator, operand);
    return -1;
  }

  r = scm_fcd_integer_to_sword(operand, &val);
  if (r < 0) return -1;

  if (val < INT_MIN || INT_MAX < val) {
    scm_fcd_error("assembler: invalid operands: out of range",
                  2, operator, operand);
    return -1;
  }

  *si = (int)val;
  return 0;
}

static int
scm_asm_asm_inst_noopd(ScmObj asmb,
                       scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 0, 0);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_noopd(asmb, opcode);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_obj(ScmObj asmb,
                     scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 1, 1);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_obj(asmb, opcode, inst[1]);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_obj_obj(ScmObj asmb,
                         scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb,
                      &arg1, &arg2);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 2, 2);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_obj_obj(asmb, opcode, inst[1], inst[2]);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si(ScmObj asmb,
                    scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt, val;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 1, 1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(inst[1], inst[0], &val);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_si(asmb, opcode, val);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si_si(ScmObj asmb,
                       scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt, val1, val2;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 2, 2);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(inst[1], inst[0], &val1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(inst[2], inst[0], &val2);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_si_si(asmb, opcode, val1, val2);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si_si_obj(ScmObj asmb,
                           scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt, val1, val2;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 3, 3);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(inst[1], inst[0], &val1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(inst[2], inst[0], &val2);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_si_si_obj(asmb, opcode, val1, val2, inst[3]);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_iof_raw(ScmObj asmb,
                         scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 2, 2);
  if (rslt < 0) return -1;

  if (scm_fcd_true_p(inst[1])) {
    size_t id;

    if (!scm_fcd_integer_p(inst[2]) || !scm_fcd_positive_p(inst[2])) {
      scm_fcd_error("assembler: label id: positive integer required",
                    2, inst[0], inst[1]);
      return -1;
    }

    rslt = scm_fcd_integer_to_size_t(inst[2], &id);
    if (rslt < 0) return -1;

    rslt = scm_asm_register_label_id(asmb, id);
    if (rslt < 0) return -1;

    rslt = scm_asm_push_inst_iof(asmb, opcode, true, id);
    if (rslt < 0) return -1;
  }
  else {
    int iof;

    rslt = scm_asm_cnv_operand_to_si(inst[2], inst[0], &iof);
    if (rslt < 0) return -1;

    rslt = scm_asm_push_inst_iof(asmb, opcode, false, iof);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_asm_asm_inst_iof(ScmObj asmb,
                     scm_opcode_t opcode, const ScmObj *inst, size_t n)
{
  ScmObj new_inst[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb);
  SCM_REFSTK_REG_ARY(new_inst, 3);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 1, 2);
  if (rslt < 0) return -1;

  if (scm_fcd_boolean_p(inst[1]))
    return scm_asm_asm_inst_iof_raw(asmb, opcode, inst, n);

  new_inst[0] = inst[0];
  if (scm_fcd_pair_p(inst[1])) {
    new_inst[1] = SCM_TRUE_OBJ;
    new_inst[2] = scm_fcd_list_ref(inst[1], 1);
    return scm_asm_asm_inst_iof_raw(asmb, opcode, new_inst, 3);
  }
  else if (scm_fcd_integer_p(inst[1])) {
    new_inst[1] = SCM_FALSE_OBJ;
    new_inst[2] = inst[1];
    return scm_asm_asm_inst_iof_raw(asmb, opcode, new_inst, 3);
  }
  else {
    scm_fcd_error("assembler: invalid operands", 2, inst[0], inst[1]);
    return -1;
  }

  return 0;
}

static int
scm_asm_asm_inst_label(ScmObj asmb,
                       scm_opcode_t opcode, const ScmObj *inst, size_t n)

{
  size_t id;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(n == 0 || inst != NULL);

  rslt = scm_asm_chk_nr_operand(inst, n, 1, 1);
  if (rslt < 0) return -1;

  if (!scm_fcd_integer_p(inst[1]) || !scm_fcd_positive_p(inst[1])) {
    scm_fcd_error("assembler: label id: positive integer requried",
                  2, inst[0], inst[1]);
    return -1;
  }

  rslt = scm_fcd_integer_to_size_t(inst[1], &id);
  if (rslt < 0) return -1;

  rslt = scm_asm_register_label_id(asmb, id);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_pinst_label(asmb, opcode, id);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_asm_assemble_1inst_cv(ScmObj asmb, const ScmObj *inst, size_t n)
{
  int opcode;

  SCM_REFSTK_INIT_REG(&asmb);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(n == 0 || inst != NULL);

  opcode = scm_asm_sym2opcode(inst[0]);
  if (opcode < 0) return -1;

  if (opcode < SCM_ASM_PI_START) {
    switch (scm_opfmt_table[opcode]) {
    case SCM_OPFMT_NOOPD:
      return scm_asm_asm_inst_noopd(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_OBJ:
      return scm_asm_asm_inst_obj(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_OBJ_OBJ:
      return scm_asm_asm_inst_obj_obj(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_SI:
      return scm_asm_asm_inst_si(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_SI_SI:
      return scm_asm_asm_inst_si_si(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      return scm_asm_asm_inst_si_si_obj(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    case SCM_OPFMT_IOF:
      return scm_asm_asm_inst_iof(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    default:
      scm_assert(false);
      break;
    }
  }
  else {
    switch (opcode) {
    case SCM_ASM_PI_LABEL:
      return scm_asm_asm_inst_label(asmb, (scm_opcode_t)opcode, inst, n);
      break;
    default:
      scm_assert(false);
      break;
    }
  }

  return -1;
}

static ssize_t
scm_asm_asm_inst_list_to_cv(ScmObj lst, ScmObj *cv, size_t n)
{
  ScmObj x = SCM_OBJ_INIT;
  size_t cnt;

  scm_assert(scm_fcd_nil_p(lst) || scm_fcd_pair_p(lst));
  scm_assert(cv != NULL);

  for (x = lst, cnt = 0;
       scm_fcd_pair_p(x) && cnt < n;
       x = scm_fcd_cdr(x), cnt++)
    cv[cnt] = scm_fcd_car(x);

  if (scm_fcd_pair_p(x)) {
    scm_fcd_error("assembler: too many operands", 1, lst);
    return -1;
  }
  else if (cnt == 0) {
    scm_fcd_error("assembler: invalid assembler format", 1, lst);
    return -1;
  }

  scm_assert(cnt <= SSIZE_MAX);
  return (ssize_t)cnt;
}

int
scm_asm_assemble_1inst(ScmObj asmb, ScmObj inst)
{
  ScmObj cv[SCM_ASM_NR_OPD_MAX + 1];  /* + 1 はオペレータ分 */
  ssize_t n;

  SCM_REFSTK_INIT_REG(&asmb, &inst);

  for (int i = 0; i < SCM_ASM_NR_OPD_MAX + 1; i++) cv[i] = SCM_OBJ_NULL;
  SCM_REFSTK_REG_ARY(cv, SCM_ASM_NR_OPD_MAX + 1);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_pair_p(inst) || scm_fcd_nil_p(inst));

  n = scm_asm_asm_inst_list_to_cv(inst, cv, SCM_ASM_NR_OPD_MAX + 1);
  if (n < 0) return -1;

  return scm_asm_assemble_1inst_cv(asmb, cv, (size_t)n);
}

int
scm_asm_assemble(ScmObj asmb, ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&asmb, &lst,
                      &cur);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_pair_p(lst) || scm_fcd_nil_p(lst));

  for (cur = lst; scm_fcd_pair_p(cur); cur = scm_fcd_cdr(cur)) {
    int r = scm_asm_assemble_1inst(asmb, scm_fcd_car(cur));
    if (r < 0) return -1;
  }

  return 0;
}

static ScmObj
scm_asm_disasm_inst_noopd(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_cons(mne, SCM_NIL_OBJ);
}

static ScmObj
scm_asm_disasm_inst_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_list(2, mne, tk->inst.i.obj.opd1);
}

static ScmObj
scm_asm_disasm_inst_obj_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_list(3, mne, tk->inst.i.obj_obj.opd1, tk->inst.i.obj_obj.opd2);
}

static ScmObj
scm_asm_disasm_inst_si(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, num1);
}

static ScmObj
scm_asm_disasm_inst_si_si(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1, &num2);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si_si.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_fcd_make_number_from_sword(tk->inst.i.si_si.opd2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_fcd_list(3, mne, num1, num2);
}

static ScmObj
scm_asm_disasm_inst_si_si_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1, &num2);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si_si_obj.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_fcd_make_number_from_sword(tk->inst.i.si_si_obj.opd2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_fcd_list(4, mne, num1, num2, tk->inst.i.si_si_obj.opd3);
}

static ScmObj
scm_asm_disasm_inst_iof(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj sym = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &sym, &inst);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  sym = scm_fcd_make_symbol_from_cstr("label", SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  inst = scm_fcd_make_number_from_size_t(tk->label_id);
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  inst = scm_fcd_list(2, sym, inst);
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, inst);
}

static ScmObj
scm_asm_disasm_inst(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  switch (tk->inst.fmt) {
  case SCM_OPFMT_NOOPD:
    return scm_asm_disasm_inst_noopd(tk, mne);
    break;
  case SCM_OPFMT_OBJ:
    return scm_asm_disasm_inst_obj(tk, mne);
    break;
  case SCM_OPFMT_OBJ_OBJ:
    return scm_asm_disasm_inst_obj_obj(tk, mne);
    break;
  case SCM_OPFMT_SI:
    return scm_asm_disasm_inst_si(tk, mne);
    break;
  case SCM_OPFMT_SI_SI:
    return scm_asm_disasm_inst_si_si(tk, mne);
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    return scm_asm_disasm_inst_si_si_obj(tk, mne);
    break;
  case SCM_OPFMT_IOF:
    return scm_asm_disasm_inst_iof(tk, mne);
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instructin format", 0);
    break;
  }

  return SCM_OBJ_NULL;
}

static ScmObj
scm_asm_disasm_label(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num = scm_fcd_make_number_from_size_t(tk->label_id);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, num);
}

ScmObj
scm_asm_disassemble(ScmObj disasm)
{
  ScmObj list = SCM_OBJ_INIT, tail = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;
  ScmObj mne = SCM_OBJ_INIT;
  const ScmDisasmToken *tk;
  int r;

  SCM_REFSTK_INIT_REG(&disasm,
                      &list, &tail, &inst);

  scm_assert(scm_fcd_disassembler_p(disasm));

  list = tail = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_NIL_OBJ);
  if (scm_obj_null_p(list)) return SCM_OBJ_NULL;

  while ((tk = scm_disasm_token(disasm)) != NULL
         && tk->type != SCM_DISASM_TK_END) {
    mne = scm_asm_mnemonic(tk->inst.i.op);
    if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

    switch (tk->type) {
    case SCM_DISASM_TK_INST:
      inst = scm_asm_disasm_inst(tk, mne);
      break;
    case SCM_DISASM_TK_LABEL:
      inst = scm_asm_disasm_label(tk, mne);
      break;
    default:
      scm_assert(false);
      return SCM_OBJ_NULL;
      break;
    }

    if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

    inst = scm_fcd_cons(inst, SCM_NIL_OBJ);
    if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

    scm_fcd_set_cdr_i(tail, inst);
    tail = inst;

    r = scm_disasm_next(disasm);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (tk == NULL) return SCM_OBJ_NULL;

  return scm_fcd_cdr(list);
}


#define SCM_ASM_PRINTABLE_MNE_UNDEF      "undef"
#define SCM_ASM_PRINTABLE_MNE_UNINIT     "uninit"
#define SCM_ASM_PRINTABLE_MNE_QQTEMPLATE "qqtemplate"

static ScmObj
scm_asm_unprintable_inst_undef(ScmObj inst)
{
  ScmObj op = SCM_OBJ_INIT;

  op = scm_fcd_make_symbol_from_cstr(scm_asm_opcode2mnemonic(SCM_OPCODE_IMMVAL),
                                     SCM_ENC_SRC);
  if (scm_obj_null_p(op)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, op, SCM_UNDEF_OBJ);
}

static ScmObj
scm_asm_unprintable_inst_uninit(ScmObj inst)
{
  ScmObj op = SCM_OBJ_INIT;

  op = scm_fcd_make_symbol_from_cstr(scm_asm_opcode2mnemonic(SCM_OPCODE_IMMVAL),
                                     SCM_ENC_SRC);
  if (scm_obj_null_p(op)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, op, SCM_UNINIT_OBJ);
}

static ScmObj
scm_asm_unprintable_inst_qqtemplate(ScmObj inst)
{
  ScmObj op = SCM_OBJ_INIT, qq = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&inst,
                      &op, &qq);

  op = scm_fcd_make_symbol_from_cstr(scm_asm_opcode2mnemonic(SCM_OPCODE_IMMVAL),
                                     SCM_ENC_SRC);
  if (scm_obj_null_p(op)) return SCM_OBJ_NULL;

  qq = scm_fcd_list_ref(inst, 1);
  if (scm_obj_null_p(qq)) return SCM_OBJ_NULL;

  if (!scm_fcd_qqtmpl_p(qq)) {
    qq = scm_fcd_compile_qq_template(qq);
    if (scm_obj_null_p(qq)) return SCM_OBJ_NULL;
  }

  return scm_fcd_list(2, op, qq);
}

static ScmObj
scm_asm_unprintable_inst_close(ScmObj inst)
{
  ScmObj new = SCM_OBJ_INIT, body = SCM_OBJ_NULL;
  int r;

  SCM_REFSTK_INIT_REG(&inst,
                      &new, &body);

  body = scm_fcd_list_ref(inst, 3);
  if (scm_obj_null_p(body)) return SCM_OBJ_NULL;

  if (scm_fcd_assembler_p(body)) {
    body = scm_asm_iseq(body);
  }
  else if (!scm_fcd_iseq_p(body)) {
    body = scm_asm_unprintable(body);
    if (scm_obj_null_p(body)) return SCM_OBJ_NULL;

    body = scm_fcd_assemble(body, SCM_OBJ_NULL);
    if (scm_obj_null_p(body)) return SCM_OBJ_NULL;
  }
  else {
    return inst;
  }

  new = scm_fcd_list_copy(inst);
  if (scm_obj_null_p(new)) return SCM_OBJ_NULL;

  r = scm_fcd_list_set_i(new, 3, body);
  if (r < 0) return SCM_OBJ_NULL;

  return new;
}

static ScmObj
scm_asm_printable_inst_immval(ScmObj inst)
{
  ScmObj val = SCM_OBJ_INIT, op = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&inst,
                      &val, &op);

  val = scm_fcd_list_ref(inst, 1);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  if (scm_fcd_undef_object_p(val)) {
    op = scm_fcd_make_symbol_from_cstr(SCM_ASM_PRINTABLE_MNE_UNDEF,
                                       SCM_ENC_SRC);
    if (scm_obj_null_p(op)) return SCM_OBJ_NULL;
    return scm_fcd_list(1, op);
  }
  else if (scm_fcd_landmine_object_p(val)) {
    op = scm_fcd_make_symbol_from_cstr(SCM_ASM_PRINTABLE_MNE_UNINIT,
                                       SCM_ENC_SRC);
    if (scm_obj_null_p(op)) return SCM_OBJ_NULL;
    return scm_fcd_list(1, op);
  }
  else if (scm_fcd_qqtmpl_p(val)) {
    op = scm_fcd_make_symbol_from_cstr(SCM_ASM_PRINTABLE_MNE_QQTEMPLATE,
                                       SCM_ENC_SRC);
    if (scm_obj_null_p(op)) return SCM_OBJ_NULL;

    val = scm_fcd_qqtmpl_template(val);
    if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

    return scm_fcd_list(2, op, val);
  }
  else {
    return inst;
  }
}

static ScmObj
scm_asm_printable_inst_close(ScmObj inst)
{
  ScmObj body = SCM_OBJ_INIT, printable = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&inst,
                      &body, &printable);

  body = scm_fcd_list_ref(inst, 3);
  if (scm_obj_null_p(body)) return SCM_OBJ_NULL;

  if (scm_fcd_iseq_p(body)) {
    body = scm_fcd_disassemble(body);
    if (scm_obj_null_p(body)) return SCM_OBJ_NULL;

    body = scm_asm_printable(body);
    if (scm_obj_null_p(body)) return SCM_OBJ_NULL;
  }

  body = scm_asm_printable(body);
  if (scm_obj_null_p(body)) return SCM_OBJ_NULL;

  printable = scm_fcd_list_copy(inst);
  if (scm_obj_null_p(printable)) return SCM_OBJ_NULL;

  r = scm_fcd_list_set_i(printable, 3, body);
  if (r < 0) return SCM_OBJ_NULL;

  return printable;
}

static ScmObj
scm_asm_printable_inst_global_var(ScmObj inst)
{
  ScmObj module = SCM_OBJ_INIT, var = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&inst,
                      &module, &var);

  var = scm_fcd_list_ref(inst, 1);
  if (scm_obj_null_p(var)) return SCM_OBJ_NULL;

  module = scm_fcd_list_ref(inst, 2);
  if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

  if (scm_fcd_gloc_p(var))
    var = scm_fcd_gloc_symbol(var);

  if (scm_fcd_module_p(module))
    module = scm_fcd_module_name(module);

  return scm_fcd_list(3, scm_fcd_car(inst), var, module);
}

struct scm_asm_cnv {
  const char *op;
  ScmObj (*func)(ScmObj);
};

static ScmObj
scm_asm_printable_inst_module(ScmObj inst)
{
  ScmObj module = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&inst,
                      &module);

  module = scm_fcd_list_ref(inst, 1);
  if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

  if (scm_fcd_module_p(module))
    module = scm_fcd_module_name(module);

  return scm_fcd_list(2, scm_fcd_car(inst), module);
}

static ScmObj
scm_asm_convert_inst(ScmObj inst, const struct scm_asm_cnv *cnv)
{
  ScmObj op = SCM_OBJ_INIT;
  bool cmp;
  int r;

  SCM_REFSTK_INIT_REG(&inst,
                      &op);

  scm_assert(scm_fcd_pair_p(inst));

  op = scm_fcd_car(inst);
  for (size_t i = 0; cnv[i].op != NULL; i++) {
    r = scm_fcd_symbol_eq_cstr(op, cnv[i].op, SCM_ENC_SRC, &cmp);
    if (r < 0) return SCM_OBJ_NULL;

    if (cmp)
      return cnv[i].func(inst);
  }

  return inst;
}

ScmObj
scm_asm_unprintable_inst(ScmObj inst)
{
  static const struct scm_asm_cnv tbl[] = {
    { .op   = SCM_ASM_PRINTABLE_MNE_UNDEF,
      .func = scm_asm_unprintable_inst_undef },
    { .op   = SCM_ASM_PRINTABLE_MNE_UNINIT,
      .func = scm_asm_unprintable_inst_uninit },
    { .op   = SCM_ASM_PRINTABLE_MNE_QQTEMPLATE,
      .func = scm_asm_unprintable_inst_qqtemplate },
    { .op   = "close",
      .func = scm_asm_unprintable_inst_close },
    { .op   = NULL,
      .func = NULL },
  };

  scm_assert(scm_fcd_pair_p(inst));
  return scm_asm_convert_inst(inst, tbl);
}

ScmObj
scm_asm_printable_inst(ScmObj inst)
{
  static const struct scm_asm_cnv tbl[] = {
    { .op = "immval", .func = scm_asm_printable_inst_immval, },
    { .op = "gdef",   .func = scm_asm_printable_inst_global_var },
    { .op = "gref",   .func = scm_asm_printable_inst_global_var },
    { .op = "gset",   .func = scm_asm_printable_inst_global_var },
    { .op = "close",  .func = scm_asm_printable_inst_close },
    { .op = "module", .func = scm_asm_printable_inst_module },
    { .op = NULL,     .func = NULL },
  };

  scm_assert(scm_fcd_pair_p(inst));
  return scm_asm_convert_inst(inst, tbl);
}

ScmObj
scm_asm_unprintable(ScmObj lst)
{
  ScmObj inst = SCM_OBJ_INIT, rest = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &inst);

  if (!scm_fcd_pair_p(lst))
    return lst;

  inst = scm_asm_unprintable_inst(scm_fcd_car(lst));
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  rest = scm_asm_unprintable(scm_fcd_cdr(lst));
  if (scm_obj_null_p(rest)) return SCM_OBJ_NULL;

  return scm_fcd_cons(inst, rest);
}

ScmObj
scm_asm_printable(ScmObj lst)
{
  ScmObj inst = SCM_OBJ_INIT, rest = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &inst);

  if (!scm_fcd_pair_p(lst))
    return lst;

  inst = scm_asm_printable_inst(scm_fcd_car(lst));
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  rest = scm_asm_printable(scm_fcd_cdr(lst));
  if (scm_obj_null_p(rest)) return SCM_OBJ_NULL;

  return scm_fcd_cons(inst, rest);
}

int
scm_asm_mnemonic2opcode(const char *mne)
{
  scm_assert(mne != NULL);

  for (size_t i = 0;
       i < sizeof(opcode2mnemonic_tbl)/sizeof(opcode2mnemonic_tbl[0]);
       i++) {
    if (strcmp(opcode2mnemonic_tbl[i].mne, mne) == 0)
      return (int)opcode2mnemonic_tbl[i].code;
  }

  return -1;
}

const char *
scm_asm_opcode2mnemonic(scm_opcode_t code)
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
scm_asm_mnemonic(scm_opcode_t opcode)
{
  const char *p;

  p = scm_asm_opcode2mnemonic(opcode);
  if (p == NULL) {
    scm_fcd_error("assembler: unknown opcode", 0);
    return SCM_OBJ_NULL;
  }

  return  scm_fcd_make_symbol_from_cstr(p, SCM_ENC_SRC);
}


/**************************************************************************/
/* Assemble/Disassemble (interface)                                       */
/**************************************************************************/

static ScmObj
asm_get_asmb(ScmObj acc)
{
  if (scm_fcd_assembler_p(acc))
    return acc;
  else
    return scm_fcd_make_assembler(acc);
}

static ScmObj
asm_return_result(ScmObj asmb, ScmObj acc)
{
  if (scm_fcd_assembler_p(acc)) {
    return acc;
  }
  else {
    int r = scm_fcd_assembler_commit(asmb);
    if (r < 0) return SCM_OBJ_NULL;

    return scm_asm_iseq(asmb);
  }
}

ScmObj
scm_fcd_assemble_1inst_cv(const ScmObj *inst, size_t n, ScmObj acc)
{
  ScmObj asmb = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&acc,
                      &asmb);

  scm_assert(n == 0 || inst != NULL);
  scm_assert(scm_obj_null_p(acc)
             || scm_fcd_iseq_p(acc) || scm_fcd_assembler_p(acc));

  asmb = asm_get_asmb(acc);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_asm_assemble_1inst_cv(asmb, inst, n);
  if (r < 0) return SCM_OBJ_NULL;

  return asm_return_result(asmb, acc);
}

ScmObj
scm_fcd_assemble_1inst(ScmObj inst, ScmObj acc)
{
  ScmObj asmb = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&inst, &acc,
                      &asmb);

  scm_assert(scm_fcd_pair_p(inst) || scm_fcd_nil_p(inst));
  scm_assert(scm_obj_null_p(acc)
             || scm_fcd_iseq_p(acc) || scm_fcd_assembler_p(acc));

  asmb = asm_get_asmb(acc);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_asm_assemble_1inst(asmb, inst);
  if (r < 0) return SCM_OBJ_NULL;

  return asm_return_result(asmb, acc);
}

ScmObj
scm_fcd_assemble(ScmObj lst, ScmObj acc)
{
  ScmObj asmb = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&lst, &acc,
                      &asmb);

  scm_assert(scm_fcd_pair_p(lst) || scm_fcd_nil_p(lst));
  scm_assert(scm_obj_null_p(acc)
             || scm_fcd_iseq_p(acc) || scm_fcd_assembler_p(acc));

  asmb = asm_get_asmb(acc);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_asm_assemble(asmb, lst);
  if (r < 0) return SCM_OBJ_NULL;

  return asm_return_result(asmb, acc);
}

ScmObj
scm_fcd_disassemble(ScmObj obj)
{
  scm_assert(scm_fcd_iseq_p(obj) || scm_fcd_disassembler_p(obj));

  if (scm_fcd_iseq_p(obj)) {
    obj = scm_fcd_make_disassembler(obj);
    if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;
  }

  return scm_asm_disassemble(obj);
}

ScmObj
scm_fcd_unprintable_assembler_1inst(ScmObj inst)
{
  scm_assert(scm_fcd_pair_p(inst));
  return scm_asm_unprintable_inst(inst);
}

ScmObj
scm_fcd_printable_assembler_1inst(ScmObj inst)
{
  scm_assert(scm_fcd_pair_p(inst));
  return scm_asm_printable_inst(inst);
}

ScmObj
scm_fcd_unprintable_assembler(ScmObj lst)
{
  return scm_asm_unprintable(lst);
}

ScmObj
scm_fcd_printable_assembler(ScmObj lst)
{
  return scm_asm_printable(lst);
}

void
scm_fcd_update_vminst_opd_iof(ScmObj iseq, scm_byte_t *ip, int iof)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(scm_fcd_iseq_to_ip(iseq) <= ip
             && ip < (scm_fcd_iseq_to_ip(iseq)
                      + (ptrdiff_t)scm_fcd_iseq_length(iseq)
                      - SCM_OPFMT_INST_SZ_IOF));

  scm_vminst_update_opd_iof(ip, iof, SCM_VMINST_UPD_FLG_OPD1);
}

int
scm_fcd_update_vminst_opd_obj_obj_1(ScmObj clsr, scm_byte_t *ip, ScmObj obj)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&clsr, &obj,
                      &iseq);

  scm_assert(scm_fcd_closure_p(clsr));
  scm_assert(scm_obj_not_null_p(obj));

  iseq = scm_fcd_closure_to_iseq(clsr);
  if (scm_obj_null_p(iseq)) return -1;

  scm_assert(scm_fcd_iseq_to_ip(iseq) <= ip
             && ip < (scm_fcd_iseq_to_ip(iseq)
                      + (ptrdiff_t)scm_fcd_iseq_length(iseq)
                      - SCM_OPFMT_INST_SZ_OBJ_OBJ));

  SCM_WB_EXP(iseq, scm_vminst_update_opd_obj_obj(ip, obj, SCM_OBJ_NULL,
                                                 SCM_VMINST_UPD_FLG_OPD1));

  return 0;
}
