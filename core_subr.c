#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"

ScmObj
scm_subr_func_cons(void)
{
  if (scm_api_get_nr_func_arg() != 2)
    ;                           /* TODO: error handling */

  return scm_api_cons(scm_api_get_func_arg(0), scm_api_get_func_arg(1));
}

ScmObj
scm_subr_func_car(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair);

  if (scm_api_get_nr_func_arg() != 1)
    ;                           /* TODO: error handling */


  SCM_SETQ(pair, scm_api_get_func_arg(0));

  if (!scm_api_pair_p(pair))
    ;                           /* TODO: err handling  */

  return scm_api_car(pair);
}

ScmObj
scm_subr_func_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair);

  if (scm_api_get_nr_func_arg() != 1)
    ;                           /* TODO: error handling */


  SCM_SETQ(pair, scm_api_get_func_arg(0));

  if (!scm_api_pair_p(pair))
    ;                           /* TODO: err handling  */

  return scm_api_cdr(pair);
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
    SCM_SETQ(sym, scm_api_make_symbol_ascii(syms[i]));
    SCM_SETQ(subr, scm_api_make_subrutine(funcs[i]));
    SCM_SETQ(rslt, scm_api_global_var_define(sym, subr));

    if (scm_obj_null_p(rslt) || scm_obj_null_p(subr) ||scm_obj_null_p(sym))
      return;
  }

}
