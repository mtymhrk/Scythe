#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/assembler.h"


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

extern inline bool
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
  scm_assert(op >= 0);

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
    case SCM_ASM_PI_UNDEF:
      return scm_asm_push_pinst_undef(asmb, op);
      break;
    case SCM_ASM_PI_UNINIT:
      return scm_asm_push_pinst_uninit(asmb, op);
      break;
    case SCM_ASM_PI_QQTEMPLATE:
      opd_obj1 = va_arg(operands, ScmObj);
      r = chk_operand_obj(opd_obj1);
      if (r < 0) return -1;
      return scm_asm_push_pinst_qqtemplate(asmb, op, opd_obj1);
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
  scm_assert(op >= 0);

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

extern inline bool
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
