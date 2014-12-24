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
                      ScmObj module)
{
  ScmObj subr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &subr, &module);

  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(name) || scm_fcd_string_p(name));
  scm_assert(scm_obj_null_p(module) || scm_fcd_module_p(module));

  subr = scm_fcd_mem_alloc(&SCM_SUBRUTINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(subr)) return SCM_OBJ_NULL;

  if (scm_subrutine_initialize(subr, func, name, arity, flags, module))
    return SCM_OBJ_NULL;

  return subr;
}

ScmObj
scm_fcd_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags,
                       ScmObj module)
{
  scm_assert(func != NULL);
  scm_assert(scm_obj_null_p(module) || scm_fcd_module_p(module));
  return scm_fcd_subrutine_new(SCM_MEM_HEAP, func,
                               SCM_OBJ_NULL, arity, flags, module);
}

int
scm_fcd_call_subrutine(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_assert(scm_fcd_subrutine_p(subr));
  return scm_subrutine_call(subr, argc, argv);
}

ScmObj
scm_fcd_subrutine_module(ScmObj subr)
{
  scm_assert(scm_fcd_subrutine_p(subr));
  return scm_subrutine_module(subr);
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
  return scm_obj_type_p(obj, &SCM_PARAMETER_TYPE_INFO);
}

ScmObj
scm_fcd_parameter_new(SCM_MEM_TYPE_T mtype, ScmObj name, ScmObj conv)
{
  ScmObj prm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&prm, &conv);

  scm_assert(scm_obj_null_p(name) || scm_fcd_string_p(name));
  scm_assert(scm_obj_null_p(conv) || scm_fcd_procedure_p(conv));

  prm = scm_fcd_mem_alloc(&SCM_PARAMETER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(prm)) return SCM_OBJ_NULL;

  if (scm_parameter_initialize(prm, name,conv) < 0)
    return SCM_OBJ_NULL;

  return prm;
}

ScmObj
scm_fcd_make_parameter(ScmObj conv)
{
  scm_assert(scm_obj_null_p(conv) || scm_fcd_procedure_p(conv));
  return scm_fcd_parameter_new(SCM_MEM_HEAP, SCM_OBJ_NULL, conv);
}

ScmObj
scm_fcd_parameter_init_val(ScmObj prm)
{
  scm_assert(scm_fcd_parameter_p(prm));
  return scm_parameter_init_val(prm);
}

ScmObj
scm_fcd_parameter_converter(ScmObj prm)
{
  scm_assert(scm_fcd_parameter_p(prm));
  return scm_parameter_converter(prm);
}

void
scm_fcd_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  scm_assert(scm_fcd_parameter_p(prm));
  scm_assert(scm_obj_not_null_p(val));
  scm_parameter_set_init_val(prm, val);
}

ScmObj
scm_fcd_parameter_value(ScmObj prm)
{
  scm_assert(scm_obj_not_null_p(prm));
  return scm_vm_parameter_value(scm_fcd_current_vm(), prm);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

bool
scm_fcd_continuation_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_CONTINUATION_TYPE_INFO);
}

ScmObj
scm_fcd_cont_new(SCM_MEM_TYPE_T mtype, ScmObj contcap)
{
  ScmObj cont = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&contcap,
                      &cont);

  cont = scm_fcd_mem_alloc(&SCM_CONTINUATION_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cont)) return SCM_OBJ_NULL;

  if (scm_cont_initialize(cont, contcap) < 0)
    return SCM_OBJ_NULL;

  return cont;
}

ScmObj
scm_fcd_capture_cont(void)
{
  ScmObj cap = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&cap);

  cap = scm_vm_capture_cont(scm_fcd_current_vm());
  if (scm_obj_null_p(cap)) return SCM_OBJ_NULL;

  return scm_fcd_cont_new(SCM_MEM_HEAP, cap);
}

ScmObj
scm_fcd_cont_capture_obj(ScmObj cont)
{
  scm_assert(scm_fcd_continuation_p(cont));
  return scm_cont_content(cont);
}

