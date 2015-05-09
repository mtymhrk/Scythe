#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/procedure.h"
#include "scythe/iseq.h"
#include "scythe/vm.h"


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

extern inline bool
scm_fcd_procedure_p(ScmObj proc)
{
  return scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC);
}

extern inline ScmObj
scm_fcd_procedure_P(ScmObj proc)
{
  return (scm_fcd_procedure_p(proc) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

int
scm_fcd_arity(ScmObj proc)
{
  scm_assert(scm_fcd_procedure_p(proc));
  return scm_proc_arity(proc);
}

bool
scm_fcd_procedure_flg_set_p(ScmObj proc, SCM_PROC_FLG_T flg)
{
  scm_assert(scm_fcd_procedure_p(proc));
  return scm_proc_flg_set_p(proc, flg);
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

extern inline bool
scm_fcd_subrutine_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SUBRUTINE_TYPE_INFO) ? true : false);
}

ScmObj
scm_fcd_subrutine_new(SCM_MEM_TYPE_T mtype,
                      ScmSubrFunc func, ScmObj name, int arity, unsigned int flags,
                      ScmObj env)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &subr, &env);

  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name)
             || scm_fcd_string_p(name) || scm_fcd_symbol_p(name));

  subr = scm_fcd_mem_alloc(&SCM_SUBRUTINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL;

  if (scm_subrutine_initialize(subr, func, name, arity, flags, env))
    return SCM_OBJ_NULL;

  return subr;
}

ScmObj
scm_fcd_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags,
                       ScmObj env)
{
  scm_assert(func != NULL);

  return scm_fcd_subrutine_new(SCM_MEM_HEAP, func,
                               SCM_OBJ_NULL, arity, flags, env);
}

int
scm_fcd_call_subrutine(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_assert(scm_fcd_subrutine_p(subr));
  return scm_subrutine_call(subr, argc, argv);
}

ScmObj
scm_fcd_subrutine_env(ScmObj subr)
{
  scm_assert(scm_fcd_subrutine_p(subr));
  return scm_proc_env(subr);
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

extern inline bool
scm_fcd_closure_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_CLOSURE_TYPE_INFO) ? true : false);
}

ScmObj
scm_fcd_closure_new(SCM_MEM_TYPE_T mtype,
                    ScmObj iseq, ScmObj env, ScmObj name, int arity)
{
  ScmObj clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&iseq, &env, &name,
                      &clsr);

  scm_assert(scm_fcd_iseq_p(iseq));

  clsr = scm_fcd_mem_alloc(&SCM_CLOSURE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(clsr)) return SCM_OBJ_NULL;

  rslt = scm_closure_initialize(clsr, iseq, env, name, arity);
  if (rslt < 0) return SCM_OBJ_NULL;

  return clsr;
}

ScmObj
scm_fcd_make_closure(ScmObj iseq, ScmObj env, int arity)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_fcd_closure_new(SCM_MEM_HEAP, iseq, env, SCM_OBJ_NULL, arity);
}

ScmObj
scm_fcd_closure_to_iseq(ScmObj clsr)
{
  scm_assert(scm_fcd_closure_p(clsr));
  return scm_closure_body(clsr);
}

scm_byte_t *
scm_fcd_closure_to_ip(ScmObj clsr)
{
  ScmObj iseq = SCM_OBJ_INIT;

  scm_assert(scm_fcd_closure_p(clsr));
  iseq = scm_closure_body(clsr);
  return scm_iseq_to_ip(iseq);
}

ScmObj
scm_fcd_closure_env(ScmObj clsr)
{
  scm_assert(scm_fcd_closure_p(clsr));
  return scm_closure_env(clsr);
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

extern inline bool
scm_fcd_parameter_p(ScmObj obj)
{
  return (scm_fcd_subrutine_p(obj)
          && scm_subrutine_func(obj) == scm_subr_func_parameter);
}

ScmObj
scm_fcd_parameter_new(SCM_MEM_TYPE_T mtype, ScmObj init, ScmObj conv)
{
  ScmObj name = SCM_OBJ_INIT, env = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&init, &conv,
                      &name, &env);

  scm_assert(scm_obj_null_p(conv) || scm_fcd_procedure_p(conv));

  name = scm_fcd_make_symbol_from_cstr("parameter", SCM_ENC_SRC);
  if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

  env = scm_fcd_cons(scm_obj_null_p(init) ? SCM_UNDEF_OBJ : init,
                     scm_obj_null_p(conv) ? SCM_UNDEF_OBJ : conv);
  if (scm_obj_null_p(env)) return SCM_OBJ_NULL;

  return scm_fcd_subrutine_new(mtype, scm_subr_func_parameter,
                               name, -1, 0, env);
}

ScmObj
scm_fcd_make_parameter(ScmObj init, ScmObj conv)
{
  scm_assert(scm_obj_null_p(conv) || scm_fcd_procedure_p(conv));
  return scm_fcd_parameter_new(SCM_MEM_HEAP, init, conv);
}

ScmObj
scm_fcd_parameter_init_val(ScmObj prm)
{
  scm_assert(scm_fcd_parameter_p(prm));
  return scm_fcd_car(scm_proc_env(prm));
}

ScmObj
scm_fcd_parameter_converter(ScmObj prm)
{
  scm_assert(scm_fcd_parameter_p(prm));
  return scm_fcd_cdr(scm_proc_env(prm));
}

void
scm_fcd_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  scm_assert(scm_fcd_parameter_p(prm));
  scm_assert(scm_obj_not_null_p(val));
  scm_fcd_set_car_i(scm_proc_env(prm), val);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

bool
scm_fcd_continuation_p(ScmObj obj)
{
  return (scm_fcd_subrutine_p(obj)
          && scm_subrutine_func(obj) == scm_subr_func_continuation);
}

ScmObj
scm_fcd_continuation_new(SCM_MEM_TYPE_T mtype, ScmObj contcap)
{
  ScmObj name = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&contcap,
                      &name);

  name = scm_fcd_make_symbol_from_cstr("continuation", SCM_ENC_SRC);
  if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

  return scm_fcd_subrutine_new(mtype, scm_subr_func_continuation,
                               name, -1, SCM_PROC_ADJ_UNWISHED, contcap);
}

ScmObj
scm_fcd_make_continuation(void)
{
  ScmObj cap = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&cap);

  cap = scm_fcd_capture_continuation();
  if (scm_obj_null_p(cap)) return SCM_OBJ_NULL;

  return scm_fcd_continuation_new(SCM_MEM_HEAP, cap);
}
