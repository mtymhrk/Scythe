#include <stdio.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/procedure.h"

/*******************************************************************/
/*  Proc                                                           */
/*******************************************************************/

int
scm_proc_initialize(ScmObj proc, ScmObj name,
                    int arity, unsigned int flags, ScmObj env)
{
  scm_assert(scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC));
  scm_assert(scm_obj_null_p(name)
             || scm_fcd_string_p(name) || scm_fcd_symbol_p(name));

  SCM_SLOT_SETQ(ScmProcedure, proc, name, name);
  SCM_SLOT_SETQ(ScmProcedure, proc, env, env);
  SCM_PROCEDURE(proc)->arity = arity;
  SCM_PROCEDURE(proc)->flags = flags;

  return 0;
}

void
scm_proc_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  SCM_PROCEDURE(obj)->name = SCM_OBJ_NULL;
  SCM_PROCEDURE(obj)->env = SCM_OBJ_NULL;
}

int
scm_proc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PROCEDURE(obj)->name, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PROCEDURE(obj)->env, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO = {
  .name                = "subrutine",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_subrutine_obj_print,
  .obj_size            = sizeof(ScmSubrutine),
  .gc_ini_func         = scm_subrutine_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_subrutine_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func,
                         ScmObj name, int arity, unsigned int flags, ScmObj env)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&name,
                      &subr, &env);

  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name)
             || scm_fcd_string_p(name) || scm_fcd_symbol_p(name));

  rslt = scm_proc_initialize(subr, name, arity, flags, env);
  if (rslt < 0) return -1;

  SCM_SUBRUTINE(subr)->subr_func = func;

  return 0;
}

int
scm_subrutine_obj_print(ScmObj obj, ScmObj port, int kind,
                        ScmObjPrintHandler handler)
{
  ScmObj name = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &name);

  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  name = scm_proc_name(obj);

  if (scm_obj_null_p(name))
    return scm_obj_default_print_func(obj, port, kind, handler);

  return scm_fcd_pformat_cstr(port,"#<subr ~a>", name, SCM_OBJ_NULL);
}

void
scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  scm_proc_gc_initialize(obj, mem);
}

int
scm_subrutine_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return scm_proc_gc_accept(obj, mem, handler);
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmTypeInfo SCM_CLOSURE_TYPE_INFO = {
  .name                = "closure",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
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
  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(scm_obj_null_p(name)
             || scm_fcd_string_p(name) || scm_fcd_symbol_p(name));

  rslt = scm_proc_initialize(clsr, name, arity, 0, env);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmClosure, clsr, iseq, iseq);

  return 0;
}

void
scm_closure_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  scm_proc_gc_initialize(obj, mem);

  SCM_CLOSURE(obj)->iseq = SCM_OBJ_NULL;
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

  return rslt;
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

int
scm_subr_func_continuation(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj cap = SCM_OBJ_INIT;
  int r;

  scm_assert(scm_fcd_continuation_p(subr));
  scm_assert(argc >= 0);

  cap = scm_proc_env(subr);

  r = scm_fcd_reinstantemnet_continuation(cap, NULL, 0);
  if (r < 0) return -1;

  return scm_fcd_return_val(argv, argc);
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmTypeInfo SCM_PARAMETER_TYPE_INFO = {
  .name                = "parameter",
  .flags               = SCM_TYPE_FLG_PROC | SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmParameter),
  .gc_ini_func         = scm_parameter_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_parameter_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_parameter_initialize(ScmObj prm, ScmObj name, ScmObj conv)
{
  int rslt;

  scm_assert_obj_type(prm, &SCM_PARAMETER_TYPE_INFO);
  scm_assert(scm_obj_null_p(name) || scm_fcd_string_p(name));
  scm_assert(scm_obj_null_p(conv) || scm_fcd_procedure_p(conv));

  rslt = scm_proc_initialize(prm, name, SCM_OBJ_NULL, 0, 0);
  if (rslt < 0) return -1;

  SCM_PARAMETER(prm)->init = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmParameter, prm, conv, conv);

  return 0;
}

void
scm_parameter_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_PARAMETER_TYPE_INFO);

  scm_proc_gc_initialize(obj, mem);

  SCM_PARAMETER(obj)->init = SCM_OBJ_NULL;
  SCM_PARAMETER(obj)->conv = SCM_OBJ_NULL;
}

int
scm_parameter_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_PARAMETER_TYPE_INFO);

  rslt = scm_proc_gc_accept(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return -1;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PARAMETER(obj)->init, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PARAMETER(obj)->conv, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
