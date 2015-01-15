#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/assembler.h"

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
