#include <stdio.h>

#include "object.h"
#include "api.h"
#include "procedure.h"

/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  .pp_func             = scm_subrutine_pretty_print,
  .obj_size            = sizeof(ScmSubrutine),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
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

int
scm_subrutine_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
    char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<subr %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(port, cstr, SCM_ENC_ASCII);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmTypeInfo SCM_CLOSURE_TYPE_INFO = {
  .pp_func             = scm_closure_pretty_print,
  .obj_size            = sizeof(ScmClosure),
  .gc_ini_func         = scm_closure_gc_initialize,
  .gc_fin_func         = scm_closure_gc_finalize,
  .gc_accept_func      = scm_closure_gc_accept,
  .gc_accept_func_weak = NULL,
};

static ScmObj dummy_free_vars[1] = { SCM_OBJ_NULL };

int
scm_closure_initialize(ScmObj clsr, ScmObj iseq,
                       size_t nr_free_vars, ScmObj *sp)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  SCM_SLOT_SETQ(ScmClosure, clsr, iseq, iseq);

  if (nr_free_vars == 0) {
    SCM_CLOSURE(clsr)->free_vars = dummy_free_vars;
  }
  else {
    scm_assert(sp != NULL);
    SCM_CLOSURE(clsr)->free_vars
      = scm_capi_malloc(sizeof(ScmObj) * nr_free_vars);
    if (SCM_CLOSURE(clsr)->free_vars == NULL) return -1; /* [ERR]: [through] */

    for (size_t i = 0; i < nr_free_vars; i++) {
      SCM_SLOT_SETQ(ScmClosure, clsr, free_vars[i],
                    SCM_OBJ(sp[- nr_free_vars + 1]));
    }
  }

  return 0;
}

void
scm_closure_finalize(ScmObj clsr)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);

  if (SCM_CLOSURE(clsr)->free_vars != dummy_free_vars)
    SCM_CLOSURE(clsr)->free_vars = scm_capi_free(SCM_CLOSURE(clsr)->free_vars);
}

ScmObj
scm_closure_new(SCM_MEM_TYPE_T mtype, ScmObj iseq,
                size_t nr_free_vars, ScmObj *sp)
{
  ScmObj clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &clsr);

  scm_assert(scm_capi_iseq_p(iseq));

  clsr = scm_capi_mem_alloc(&SCM_CLOSURE_TYPE_INFO, mtype);
  if (scm_obj_null_p(clsr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_closure_initialize(clsr, iseq, nr_free_vars, sp);
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

  rslt = scm_capi_write_cstr(port, cstr, SCM_ENC_ASCII);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_closure_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  SCM_CLOSURE(obj)->iseq = SCM_OBJ_NULL;
  SCM_CLOSURE(obj)->free_vars = NULL;
}

void
scm_closure_gc_finalize(ScmObj obj)
{
  scm_closure_finalize(obj);
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

  if (SCM_CLOSURE(obj)->free_vars == NULL) return rslt;

  for(size_t i = 0; i < SCM_CLOSURE(obj)->nr_free_vars; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_CLOSURE(obj)->free_vars[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
