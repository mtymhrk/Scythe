#include <stdio.h>

#include "object.h"
#include "api.h"
#include "procedure.h"

/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

int
scm_proc_initialize(ScmObj proc, ScmObj name, int arity, unsigned int flags)
{
  scm_assert(scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC));
  scm_assert(scm_obj_null_p(name) || scm_capi_string_p(name));

  SCM_SLOT_SETQ(ScmProcedure, proc, name, name);
  SCM_PROCEDURE(proc)->arity = arity;
  SCM_PROCEDURE(proc)->flags = flags;

  return 0;
}

void
scm_proc_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  SCM_PROCEDURE(obj)->name = SCM_OBJ_NULL;
}

int
scm_proc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PROCEDURE(obj)->name, mem);
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  .name                = "subrutine",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .pp_func             = scm_subrutine_pretty_print,
  .obj_size            = sizeof(ScmSubrutine),
  .gc_ini_func         = scm_subrutine_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_subrutine_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func,
                         ScmObj name, int arity, unsigned int flags,
                         ScmObj module)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&name,
                       &subr, &module);

  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name) || scm_capi_string_p(name));
  scm_assert(scm_obj_null_p(module) || scm_capi_module_p(module));

  rslt = scm_proc_initialize(subr, name, arity, flags);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmSubrutine, subr, module, module);

  SCM_SUBRUTINE(subr)->subr_func = func;

  return 0;
}

ScmObj
scm_subrutine_new(SCM_MEM_TYPE_T mtype,
                  ScmSubrFunc func, ScmObj name, int arity, unsigned int flags,
                  ScmObj module)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&name,
                       &subr);

  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name) || scm_capi_string_p(name));
  scm_assert(scm_obj_null_p(module) || scm_capi_module_p(module));

  subr = scm_capi_mem_alloc(&SCM_SUBRUTINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_subrutine_initialize(subr, func, name, arity, flags, module))
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

void
scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  scm_proc_gc_initialize(obj, mem);

  SCM_SUBRUTINE(obj)->module = SCM_OBJ_NULL;
}

int
scm_subrutine_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = scm_proc_gc_accept(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SUBRUTINE(obj)->module, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmTypeInfo SCM_CLOSURE_TYPE_INFO = {
  .name                = "closure",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .pp_func             = scm_closure_pretty_print,
  .obj_size            = sizeof(ScmClosure),
  .gc_ini_func         = scm_closure_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_closure_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_closure_initialize(ScmObj clsr,
                       ScmObj iseq, ScmObj env, ScmObj name, int arity)
{
  int rslt;

  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_obj_null_p(name) || scm_capi_string_p(name));

  rslt = scm_proc_initialize(clsr, name, arity, 0);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmClosure, clsr, iseq, iseq);
  SCM_SLOT_SETQ(ScmClosure, clsr, env, env);

  return 0;
}

ScmObj
scm_closure_new(SCM_MEM_TYPE_T mtype,
                ScmObj iseq, ScmObj env, ScmObj name, int arity)
{
  ScmObj clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &env, &name,
                       &clsr);

  scm_assert(scm_capi_iseq_p(iseq));

  clsr = scm_capi_mem_alloc(&SCM_CLOSURE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(clsr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_closure_initialize(clsr, iseq, env, name, arity);
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

  scm_proc_gc_initialize(obj, mem);

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

  rslt = scm_proc_gc_accept(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CLOSURE(obj)->iseq, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CLOSURE(obj)->env, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

ScmTypeInfo SCM_CONTINUATION_TYPE_INFO = {
  .name                = "continuation",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .pp_func             = scm_cont_pretty_print,
  .obj_size            = sizeof(ScmContinuation),
  .gc_ini_func         = scm_cont_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_cont_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_cont_initialize(ScmObj cont, ScmObj contcap)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cont, &contcap,
                       &name);

  scm_assert_obj_type(cont, &SCM_CONTINUATION_TYPE_INFO);

  name = scm_capi_make_string_from_cstr("continuation", SCM_ENC_ASCII);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_proc_initialize(cont, name, -1, SCM_PROC_ADJ_UNWISHED);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmContinuation, cont, contcap, contcap);

  return 0;
}

ScmObj
scm_cont_new(SCM_MEM_TYPE_T mtype, ScmObj contcap)
{
  ScmObj cont = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&contcap,
                       &cont);

  cont = scm_capi_mem_alloc(&SCM_CONTINUATION_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cont)) return SCM_OBJ_NULL;

  if (scm_cont_initialize(cont, contcap) < 0)
    return SCM_OBJ_NULL;

  return cont;
}

int
scm_cont_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_CONTINUATION_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<continuation %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_cont_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_CONTINUATION_TYPE_INFO);

  scm_proc_gc_initialize(obj, mem);

  SCM_CONT(obj)->contcap = SCM_OBJ_NULL;
}

int
scm_cont_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_CONTINUATION_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = scm_proc_gc_accept(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONT(obj)->contcap, mem);
}
