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

void
test_eval__application_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cons 'a 'b)";
  const char *ect_str = "(a . b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__lambda_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_closure_p(actual));
}

void
test_eval__lambda_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2) 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_closure_p(actual));
}

void
test_eval__lambda_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2 . v3) 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_closure_p(actual));
}

void
test_eval__refer_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) x) 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__refer_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (f1 b2) (lambda (b1 b2) b2)) 'a 'b) 'c 'd)";
  const char *ect_str = "d";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__set_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (f1 f2)"
                        "    (lambda (b1 b2)"
                        "      (set! b2 'e) b2))"
                        "  'a 'b)"
                        " 'c 'd)";
  const char *ect_str = "e";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__set_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (f1 b2)"
                        "    (lambda (b1 b2)"
                        "      (set! b2 'e) b2))"
                        "  'a 'b)"
                        " 'c 'd)";
  const char *ect_str = "e";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__refer_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (f1 f2)"
                        "    (lambda (b1 b2)"
                        "      f2))"
                        "  'a 'b)"
                        " 'c 'd)";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}
