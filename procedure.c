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

  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(func != NULL);

  SCM_SUBRUTINE(subr)->subr_func = func;
}

ScmObj
scm_subrutine_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmSubrFunc func)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&subr);

  scm_assert(func != NULL);

  subr = scm_mem_alloc(scm_vm_current_mm(), &SCM_SUBRUTINE_TYPE_INFO, mtype);

  scm_subrutine_initialize(subr, func);

  return subr;
}

void
scm_subrutine_call(ScmObj subr)
{
  ScmObj ret = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&ret);

  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  ret = SCM_SUBRUTINE(subr)->subr_func();
  if (scm_obj_null_p(ret))
    ;                           /* TODO: error handling */

  scm_vm_update_current_val_reg(ret);
  scm_vm_return_to_caller(scm_vm_current_vm());
}

void
scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_SUBRUTINE(obj)->subr_func = NULL;
}
