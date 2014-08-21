#include "../../src/api.c"

ScmObj
scm_capi_ut_compile(ScmEvaluator *ev, ScmObj exp)
{
  ScmObj compile = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_STACK_FRAME_PUSH(&exp,
                       &compile, &args, &val);

  r = scm_capi_cached_global_var_ref(SCM_CACHED_GV_COMPILE,
                                     SCM_CSETTER_L(compile));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(compile)) {
    scm_capi_error("unbound variable: compile", 0);
    return SCM_OBJ_NULL;
  }

  args = scm_api_make_compiler(SCM_OBJ_NULL);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  args = scm_capi_list(2, exp, args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  val = scm_vm_apply(scm_vm_current_vm(), compile, args);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_capi_vector_ref(val, 0);
}

ScmObj
scm_capi_ut_eval(ScmEvaluator *ev, ScmObj exp)
{
  ScmObj eval = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_STACK_FRAME_PUSH(&exp,
                       &eval, &args, &val);

  r = scm_capi_cached_global_var_ref(SCM_CACHED_GV_EVAL, SCM_CSETTER_L(eval));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(eval)) {
    scm_capi_error("unbound variable: eval", 0);
    return SCM_OBJ_NULL;
  }

  args = scm_api_cons(exp, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  val = scm_vm_apply(scm_vm_current_vm(), eval, args);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_capi_vector_ref(val, 0);
}

void
scm_capi_ut_disposal_unhandled_exc(ScmEvaluator *ev)
{
  scm_vm_disposal_unhandled_exc(ev->vm);
}
