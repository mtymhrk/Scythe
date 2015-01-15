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

int
scm_fcd_disassembler_cnv_to_printable(ScmObj disasm)
{
  scm_assert(scm_fcd_disassembler_p(disasm));
  return scm_disasm_cnv_to_printable(disasm);
}


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

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

  if (scm_fcd_assembler_p(acc)) {
    asmb = acc;
  }
  else {
    asmb = scm_fcd_make_assembler(acc);
    if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;
  }

  r = scm_asm_assemble(asmb, lst);
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_fcd_assembler_p(acc)) {
    return acc;
  }
  else {
    r = scm_fcd_assembler_commit(asmb);
    if (r < 0) return SCM_OBJ_NULL;

    return scm_asm_iseq(asmb);
  }
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
