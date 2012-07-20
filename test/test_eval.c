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
test_eval__define_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define (func x) x)";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("func", SCM_ENC_ASCII);

  scm_capi_ut_eval(ev, exp);

  actual = scm_api_global_var_ref(sym);

  cut_assert_true(scm_capi_closure_p(actual));
}

void
test_eval__define_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define var 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  expected = scm_capi_make_number_from_sword(1);

  scm_capi_ut_eval(ev, exp);

  actual = scm_api_global_var_ref(sym);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

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
test_eval__refer_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj gvar = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) cons) 1)";
  const char *gvar_str = "cons";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected,
                       &gvar);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(gvar_str, SCM_ENC_ASCII);
  gvar = scm_api_read(port);

  expected = scm_api_global_var_ref(gvar);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_eq_P(expected, actual)));
}

/* void */
/* test_eval__update_global_variable_1(void) */
/* { */
/*   ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT; */
/*   ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT; */
/*   ScmObj val_bef = SCM_OBJ_INIT; */
/*   const char *exp_str = "(set! var 10)"; */

/*   SCM_STACK_FRAME_PUSH(&exp, &port, &sym, */
/*                        &actual, &expected, */
/*                        &val_bef); */


/*   port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII); */
/*   exp = scm_api_read(port); */

/*   sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII); */

/*   val_bef = scm_capi_make_number_from_sword(1); */
/*   expected = scm_capi_make_number_from_sword(10); */

/*   scm_api_global_var_define(sym, val_bef); */

/*   scm_capi_ut_eval(ev, exp); */

/*   actual = scm_api_global_var_ref(sym); */

/*   cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual))); */
/* } */


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
test_eval__application_2(void)
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
test_eval__application_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (x) (lambda (y) (cons x y))) 1) 2)";
  const char *ect_str = "(1 . 2)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

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

void
test_eval__set_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda (f1 f2)"
                        "    (lambda (b1 b2)"
                        "      (set! f2 'e) f2))"
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
test_eval__conditional_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if 'a 'b 'c)";
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

void
test_eval__conditional_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if 'a 'b)";
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

void
test_eval__conditional_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if #f 'b 'c)";
  const char *ect_str = "c";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}
