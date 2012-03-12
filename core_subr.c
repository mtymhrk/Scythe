#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"

/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/


ScmObj
scm_subr_func_cons(void)
{
  if (scm_capi_get_nr_func_arg() != 2)
    ;                           /* TODO: error handling */

  return scm_api_cons(scm_capi_get_func_arg(0), scm_capi_get_func_arg(1));
}

ScmObj
scm_subr_func_car(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair);

  if (scm_capi_get_nr_func_arg() != 1)
    ;                           /* TODO: error handling */


  pair = scm_capi_get_func_arg(0);

  if (!scm_capi_pair_p(pair))
    ;                           /* TODO: err handling  */

  return scm_api_car(pair);
}

ScmObj
scm_subr_func_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair);

  if (scm_capi_get_nr_func_arg() != 1)
    ;                           /* TODO: error handling */


  pair = scm_capi_get_func_arg(0);

  if (!scm_capi_pair_p(pair))
    ;                           /* TODO: err handling  */

  return scm_api_cdr(pair);
}


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

ScmObj
scm_subr_func_eval_asm(void)
{
  ScmObj code = SCM_OBJ_INIT;
  ScmObj args = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&code, &args);

  if (scm_capi_get_nr_func_arg() != 1)
    ;                           /* TODO: error handling */

  code = scm_capi_get_func_arg(0);

  if (scm_capi_pair_p(code)) {
    code = scm_api_assemble(code);
    if (scm_obj_null_p(code)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else if (!scm_capi_iseq_p(code)) {
    ;                           /* TODO: err handling  */
  }

  args = scm_api_nil();

  rslt = scm_capi_trampolining(code, args, NULL);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return SCM_OBJ_NULL;
}


void
scm_core_subr_system_setup(void)
{
  const char *syms[] = { "cons", "car", "cdr" };
  ScmSubrFunc funcs[] = { scm_subr_func_cons, scm_subr_func_car,
                          scm_subr_func_cdr };
  ScmObj sym  = SCM_OBJ_INIT;
  ScmObj subr = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &subr, &rslt);

  for (size_t i = 0; i < sizeof(syms)/sizeof(syms[0]); i++) {
    sym = scm_capi_make_symbol_from_cstr(syms[i], SCM_ENC_ASCII);
    subr = scm_capi_make_subrutine(funcs[i]);
    rslt = scm_api_global_var_define(sym, subr);

    if (scm_obj_null_p(rslt) || scm_obj_null_p(subr) ||scm_obj_null_p(sym))
      return;
  }

}

