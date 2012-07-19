#include <cutter.h>

#include "api.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_eval__self_eval_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  expected = scm_capi_make_number_from_sword(1);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__refer_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "cons";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  expected = scm_api_global_var_ref(exp);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_eq_P(expected, actual)));
}

void
test_eval__quote_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "'(a b c)";
  const char *ect_str = "(a b c)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

/* void */
/* test_eval__application_1(void) */
/* { */
/*   ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT; */
/*   ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT; */
/*   const char *exp_str = "(cons 'a 'b)"; */
/*   const char *ect_str = "(a . b)"; */

/*   SCM_STACK_FRAME_PUSH(&exp, &port, */
/*                        &actual, &expected); */


/*   port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII); */
/*   exp = scm_api_read(port); */

/*   port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII); */
/*   expected = scm_api_read(port); */

/*   actual = scm_capi_ut_eval(ev, exp); */

/*   cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual))); */
/* } */


