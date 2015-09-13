#include <stdio.h>

#include "scythe/object.h"
#include "scythe/vm.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/format.h"
#include "scythe/iseq.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"
#include "scythe/string.h"
#include "scythe/symbol.h"
#include "scythe/procedure.h"


/*******************************************************************/
/*  Proc                                                           */
/*******************************************************************/

ScmObj
scm_procedure_P(ScmObj proc)
{
  return (scm_procedure_p(proc) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

int
scm_proc_initialize(ScmObj proc, ScmObj name,
                    int arity, unsigned int flags, ScmObj env)
{
  scm_assert(scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC));
  scm_assert(scm_obj_null_p(name) || scm_string_p(name) || scm_symbol_p(name));

  SCM_SLOT_SETQ(ScmProcedure, proc, name, name);
  SCM_SLOT_SETQ(ScmProcedure, proc, env, env);
  SCM_PROCEDURE(proc)->arity = arity;
  SCM_PROCEDURE(proc)->flags = flags;

  return 0;
}

void
scm_proc_gc_initialize(ScmObj obj)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  SCM_PROCEDURE(obj)->name = SCM_OBJ_NULL;
  SCM_PROCEDURE(obj)->env = SCM_OBJ_NULL;
}

int
scm_proc_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_PROC));

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PROCEDURE(obj)->name);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PROCEDURE(obj)->env);
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
  scm_assert(scm_obj_null_p(name) || scm_string_p(name) || scm_symbol_p(name));

  rslt = scm_proc_initialize(subr, name, arity, flags, env);
  if (rslt < 0) return -1;

  SCM_SUBRUTINE(subr)->subr_func = func;

  return 0;
}

ScmObj
scm_subrutine_new(scm_mem_type_t mtype,
                  ScmSubrFunc func, ScmObj name, int arity, unsigned int flags,
                  ScmObj env)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &subr, &env);

  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name) || scm_string_p(name) || scm_symbol_p(name));

  subr = scm_alloc_mem(&SCM_SUBRUTINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL;

  if (scm_subrutine_initialize(subr, func, name, arity, flags, env))
    return SCM_OBJ_NULL;

  return subr;
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

  return scm_pformat_cstr(port,"#<subr ~a>", name, SCM_OBJ_NULL);
}

void
scm_subrutine_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  scm_proc_gc_initialize(obj);
}

int
scm_subrutine_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_SUBRUTINE_TYPE_INFO);

  return scm_proc_gc_accept(obj, handler);
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
  scm_assert(scm_iseq_p(iseq));
  scm_assert(scm_obj_null_p(name) || scm_string_p(name) || scm_symbol_p(name));

  rslt = scm_proc_initialize(clsr, name, arity, 0, env);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmClosure, clsr, iseq, iseq);

  return 0;
}

ScmObj
scm_closure_new(scm_mem_type_t mtype,
                ScmObj iseq, ScmObj env, ScmObj name, int arity)
{
  ScmObj clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &env, &name,
                      &clsr);

  scm_assert(scm_iseq_p(iseq));

  clsr = scm_alloc_mem(&SCM_CLOSURE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(clsr)) return SCM_OBJ_NULL;

  rslt = scm_closure_initialize(clsr, iseq, env, name, arity);
  if (rslt < 0) return SCM_OBJ_NULL;

  return clsr;
}

scm_byte_t *
scm_closure_to_ip(ScmObj clsr)
{
  scm_assert(scm_closure_p(clsr));
  return scm_iseq_to_ip(scm_closure_iseq(clsr));
}

void
scm_closure_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  scm_proc_gc_initialize(obj);

  SCM_CLOSURE(obj)->iseq = SCM_OBJ_NULL;
}

int
scm_closure_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_CLOSURE_TYPE_INFO);

  rslt = scm_proc_gc_accept(obj, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CLOSURE(obj)->iseq);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

ScmTypeInfo SCM_DWHCALLERENV_TYPE_INFO = {
  .name                = "dwhcallerenv",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmDWHCallerEnv),
  .gc_ini_func         = scm_dwhcallerenv_gc_initialize,
  .gc_fin_func         = scm_dwhcallerenv_gc_finalize,
  .gc_accept_func      = scm_dwhcallerenv_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_dwhcallerenv_initialize(ScmObj dwhce,
                            ScmObj cont, const ScmObj *val, size_t vc)
{
  scm_assert_obj_type(dwhce, &SCM_DWHCALLERENV_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(cont));
  scm_assert(vc == 0 || val != NULL);

  SCM_SLOT_SETQ(ScmDWHCallerEnv, dwhce, cont, cont);

  if (vc == 0) {
    SCM_DWHCALLERENV(dwhce)->val = NULL;
  }
  else {
    SCM_DWHCALLERENV(dwhce)->val = scm_malloc(sizeof(ScmObj) * vc);
    if (SCM_DWHCALLERENV(dwhce)->val == NULL)
      return -1;
    SCM_DWHCALLERENV(dwhce)->vc = vc;
  }

  for (size_t i = 0; i < vc; i++)
    SCM_SLOT_SETQ(ScmDWHCallerEnv, dwhce, val[i], val[i]);

  return 0;
}

void
scm_dwhcallerenv_finalize(ScmObj dwhce)
{
  scm_assert_obj_type(dwhce, &SCM_DWHCALLERENV_TYPE_INFO);

  if (SCM_DWHCALLERENV(dwhce)->vc == 0)
    return ;

  scm_free(SCM_DWHCALLERENV(dwhce)->val);
  SCM_DWHCALLERENV(dwhce)->val = NULL;
  SCM_DWHCALLERENV(dwhce)->vc = 0;
}

ScmObj
scm_dwhcallerenv_new(scm_mem_type_t mtype,
                     ScmObj cont, const ScmObj *val, size_t vc)
{
  ScmObj dwhce = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&cont,
                      &dwhce);

  scm_assert(scm_obj_not_null_p(cont));
  scm_assert(vc == 0 || val != NULL);

  dwhce = scm_alloc_mem(&SCM_DWHCALLERENV_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(dwhce)) return SCM_OBJ_NULL;

  if (scm_dwhcallerenv_initialize(dwhce, cont, val, vc) < 0)
    return SCM_OBJ_NULL;

  return dwhce;
}

void
scm_dwhcallerenv_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_DWHCALLERENV_TYPE_INFO);

  SCM_DWHCALLERENV(obj)->cont = SCM_OBJ_NULL;
  SCM_DWHCALLERENV(obj)->vc = 0;
}

void
scm_dwhcallerenv_gc_finalize(ScmObj obj)
{
  scm_dwhcallerenv_finalize(obj);
}

int
scm_dwhcallerenv_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                 obj, SCM_DWHCALLERENV(obj)->cont);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < SCM_DWHCALLERENV(obj)->vc; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_DWHCALLERENV(obj)->val[i]);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}

static int
call_dw_handler(ScmObj caller, ScmObj handlers)
{
  scm_assert(scm_procedure_p(caller));
  scm_assert(scm_pair_p(handlers));

  return scm_trampolining(scm_car(handlers), SCM_NIL_OBJ,
                          caller, scm_cdr(handlers));
}

static int
scm_subr_func_dw_handler_calller(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj env = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&subr,
                      &env);

  if (!scm_pair_p(argv[0])) {
    env = scm_proc_env(subr);
    scm_assert_obj_type(env, &SCM_DWHCALLERENV_TYPE_INFO);

    r = scm_reinstantemnet_continuation(scm_dwhcallerenv_cont(env));
    if (r < 0) return -1;

    return scm_return_val(scm_dwhcallerenv_val(env),
                          (int)scm_dwhcallerenv_vc(env));
  }
  else {
    return call_dw_handler(subr, argv[0]);
  }
}

int
scm_subr_func_continuation(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj handlers = SCM_OBJ_INIT, caller = SCM_OBJ_INIT, env = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &handlers, &caller);

  scm_assert(scm_continuation_p(subr));
  scm_assert(argc >= 0);

  handlers = scm_collect_dynamic_wind_handler(scm_proc_env(subr));
  if (scm_obj_null_p(handlers)) return -1;

  if (!scm_pair_p(handlers)) {
    int r = scm_reinstantemnet_continuation(scm_proc_env(subr));
    if (r < 0) return -1;

    return scm_return_val(argv, argc);
  }
  else {
    env = scm_dwhcallerenv_new(SCM_MEM_HEAP,
                               scm_proc_env(subr), argv, (size_t)argc);
    if (scm_obj_null_p(env)) return -1;

    caller = scm_make_subrutine(scm_subr_func_dw_handler_calller,
                                -2, SCM_PROC_ADJ_UNWISHED, env);
    if (scm_obj_null_p(caller)) return -1;

    return call_dw_handler(caller, handlers);
  }
}

ScmObj
scm_continuation_new(scm_mem_type_t mtype, ScmObj contcap)
{
  ScmObj name = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&contcap,
                      &name);

  name = scm_make_symbol_from_cstr("continuation", SCM_ENC_SRC);
  if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

  return scm_subrutine_new(mtype, scm_subr_func_continuation,
                           name, -1, SCM_PROC_ADJ_UNWISHED, contcap);
}

ScmObj
scm_make_continuation(void)
{
  ScmObj cap = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&cap);

  cap = scm_capture_continuation();
  if (scm_obj_null_p(cap)) return SCM_OBJ_NULL;

  return scm_continuation_new(SCM_MEM_HEAP, cap);
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

static int
scm_subr_func_parameter__postproc_init(ScmObj subr,
                                       int argc, const ScmObj *argv)
{
  scm_parameter_set_init_val(argv[0], argv[1]);
  return scm_return_val(argv, 1);
}

static int
scm_subr_func_parameter__postproc_cons(ScmObj subr,
                                       int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_cons(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_return_val(&val, 1);
}

/*
 * 引数無しでの parameter オブジェクトの呼び出し
 *   (<parameter object>)
 *      ;=> parameter オブジェクトを(動的に)束縛している値を返す
 *
 * 引数を 1 つ指定した parameter オブジェクトの呼び出し
 *   (<parameter object> arg1)
 *      ;=> 何もせずただ #<undef> を返す
 *
 * 引数を 2 つ指定し、第 2 引数が #t である場合の parameter オブジェクトの呼び
 * 出し
 *   (<parameter object> arg1 #t)
 *      ;=> (<converter> arg1) の結果を parameter オブジェクの初期値に設定し、
 *          parameter オブジェクトを返す
 *
 * 引数を 2 つ指定し、第 2 引数が #f である場合の parameter オブジェクトの呼び
 * 出し
 *   (<parameter object> arg1 #f)
 *      ;=> (<parameter object> . {(<converter> arg1) の結果}) を返す
 *
 * 引数を 2 つ指定し、第 2 引数が真偽値でない場合の parameter オブジェクトの呼
 * び出し
 *   (<parameter object> arg1 arg2)
 *      ;=> 何もせずただ #<undef> を返す
 *
 */
int
scm_subr_func_parameter(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, conv = SCM_OBJ_INIT, postproc = SCM_OBJ_INIT;
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &conv, &postproc,
                      &arg1, &arg2);

  scm_assert(scm_parameter_p(subr));

  if (scm_nil_p(argv[0])) {
    val = scm_parameter_value(subr);
    if (scm_obj_null_p(val)) return -1;

    return scm_return_val(&val, 1);
  }

  arg1 = scm_car(argv[0]);
  if (scm_nil_p(scm_cdr(argv[0])))
    arg2 = SCM_OBJ_NULL;
  else
    arg2 = scm_list_ref(argv[0], 1);

  if (scm_obj_null_p(arg2)) {
    val = SCM_UNDEF_OBJ;
    return scm_return_val(&val, 1);
  }
  else if (scm_true_object_p(arg2)) {
    conv = scm_parameter_converter(subr);
    if (scm_procedure_p(conv)) {
      arg1 = scm_cons(arg1, SCM_NIL_OBJ);
      if (scm_obj_null_p(arg1)) return -1;

      postproc = scm_make_subrutine(scm_subr_func_parameter__postproc_init,
                                        2, 0, SCM_OBJ_NULL);
      if (scm_obj_null_p(postproc)) return -1;

      return scm_trampolining(conv, arg1, postproc, subr);
    }
    else {
      scm_parameter_set_init_val(subr, arg1);
      return scm_return_val(&subr, 1);
    }
  }
  else if (scm_false_object_p(arg2)){
    conv = scm_parameter_converter(subr);
    if (scm_procedure_p(conv)) {
      arg1 = scm_cons(arg1, SCM_NIL_OBJ);
      if (scm_obj_null_p(arg1)) return -1;

      postproc = scm_make_subrutine(scm_subr_func_parameter__postproc_cons,
                                    2, 0, SCM_OBJ_NULL);
      if (scm_obj_null_p(postproc)) return -1;

      return scm_trampolining(conv, arg1, postproc, subr);
    }
    else {
      val = scm_cons(subr, arg1);
      if (scm_obj_null_p(val)) return -1;
      return scm_return_val(&val, 1);
    }
  }
  else {
    val = SCM_UNDEF_OBJ;
    return scm_return_val(&val, 1);
  }
}

ScmObj
scm_parameter_new(scm_mem_type_t mtype, ScmObj init, ScmObj conv)
{
  ScmObj name = SCM_OBJ_INIT, env = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&init, &conv,
                      &name, &env);

  scm_assert(scm_obj_null_p(conv) || scm_procedure_p(conv));

  name = scm_make_symbol_from_cstr("parameter", SCM_ENC_SRC);
  if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

  env = scm_cons(scm_obj_null_p(init) ? SCM_UNDEF_OBJ : init,
                 scm_obj_null_p(conv) ? SCM_UNDEF_OBJ : conv);
  if (scm_obj_null_p(env)) return SCM_OBJ_NULL;

  return scm_subrutine_new(mtype, scm_subr_func_parameter,
                           name, -1, 0, env);
}

ScmObj
scm_parameter_init_val(ScmObj prm)
{
  scm_assert(scm_parameter_p(prm));
  return scm_car(scm_proc_env(prm));
}

ScmObj
scm_parameter_converter(ScmObj prm)
{
  scm_assert(scm_parameter_p(prm));
  return scm_cdr(scm_proc_env(prm));
}

void
scm_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  scm_assert(scm_parameter_p(prm));
  scm_assert(scm_obj_not_null_p(val));
  scm_set_car(scm_proc_env(prm), val);
}
