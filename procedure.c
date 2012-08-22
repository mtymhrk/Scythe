#include <stdio.h>

#include "object.h"
#include "api.h"
#include "procedure.h"

/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  .name                = "subrutine",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_subrutine_pretty_print,
  .obj_size            = sizeof(ScmSubrutine),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
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

  subr = scm_capi_mem_alloc(&SCM_SUBRUTINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_subrutine_initialize(subr, func))
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return subr;
}

int
scm_subrutine_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
    char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<subr %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmTypeInfo SCM_CLOSURE_TYPE_INFO = {
  .name                = "closure",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_closure_pretty_print,
  .obj_size            = sizeof(ScmClosure),
  .gc_ini_func         = scm_closure_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_closure_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static ScmObj dummy_free_vars[1] = { SCM_OBJ_NULL };

int
scm_closure_initialize(ScmObj clsr, ScmObj iseq, ScmObj env)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  SCM_SLOT_SETQ(ScmClosure, clsr, iseq, iseq);
  SCM_SLOT_SETQ(ScmClosure, clsr, env, env);

  return 0;
}

ScmObj
scm_closure_new(SCM_MEM_TYPE_T mtype, ScmObj iseq, ScmObj env)
{
  ScmObj clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &env,
                       &clsr);

  scm_assert(scm_capi_iseq_p(iseq));

  clsr = scm_capi_mem_alloc(&SCM_CLOSURE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(clsr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_closure_initialize(clsr, iseq, env);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return clsr;
}

int
scm_closure_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<closure %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_closure_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  SCM_CLOSURE(obj)->iseq = SCM_OBJ_NULL;
  SCM_CLOSURE(obj)->env = SCM_OBJ_NULL;
}

int
scm_closure_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CLOSURE(obj)->iseq, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CLOSURE(obj)->env, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
