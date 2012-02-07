#include "procedure.h"
#include "object.h"
#include "reference.h"
#include "vm.h"

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmSubrutine),         /* obj_size             */
  scm_subrutine_gc_initialize,  /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  NULL,                         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

void
scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func)
{
  SCM_STACK_PUSH(&subr);

  SCM_OBJ_ASSERT_TYPE(subr, &SCM_SUBRUTINE_TYPE_INFO);
  assert(func != NULL);

  SCM_SUBRUTINE_FUNC(subr) = func;
}

ScmObj
scm_subrutine_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmSubrFunc func)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&subr);

  SCM_OBJ_ASSERT_TYPE(subr, &SCM_SUBRUTINE_TYPE_INFO);
  assert(func != NULL);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_SUBRUTINE_TYPE_INFO, mtype, SCM_REF_MAKE(subr));

  scm_subrutine_initialize(subr, func);

  return subr;
}

void
scm_subrutine_call(ScmObj subr)
{
  ScmObj ret = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&ret);

  SCM_OBJ_ASSERT_TYPE(subr, &SCM_SUBRUTINE_TYPE_INFO);

  SCM_SUBRUTINE_CALL(ret, subr);
  if (SCM_OBJ_IS_NULL(ret))
    ;                           /* TODO: error handling */

  scm_vm_update_current_val_reg(ret);
  scm_vm_return_to_caller(scm_vm_current_vm());
}

void
scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SUBRUTINE_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));

  SCM_SUBRUTINE_FUNC(obj) = NULL;
}
