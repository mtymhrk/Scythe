#include "procedure.h"
#include "object.h"
#include "reference.h"
#include "vm.h"
#include "obuffer.h"

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  scm_subrutine_pretty_print,   /* pp_func              */
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
  SCM_OBJ_ASSERT_TYPE(subr, &SCM_SUBRUTINE_TYPE_INFO);

  SCM_SUBRUTINE_CALL(subr);
}

void
scm_subrutine_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  /* TODO: write me */
  return;
}

void
scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SUBRUTINE_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));

  SCM_SUBRUTINE_FUNC(obj) = NULL;
}
