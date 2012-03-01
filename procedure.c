
#include "object.h"
#include "reference.h"
#include "vm.h"
#include "api.h"
#include "procedure.h"

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmSubrutine),         /* obj_size             */
  NULL,                         /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  NULL,                         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

int
scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func)
{
  SCM_STACK_FRAME_PUSH(&subr);

  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(func != NULL);

  SCM_SUBRUTINE(subr)->subr_func = func;

  return 0;
}

ScmObj
scm_subrutine_new(SCM_MEM_TYPE_T mtype, ScmSubrFunc func)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr);

  scm_assert(func != NULL);

  subr = scm_capi_mem_alloc(&SCM_SUBRUTINE_TYPE_INFO, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_subrutine_initialize(subr, func))
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return subr;
}

ScmObj
scm_subrutine_call(ScmObj subr)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->subr_func();
}
