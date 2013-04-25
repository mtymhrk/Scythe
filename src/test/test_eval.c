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

ScmObj
main_module(void)
{
  ScmObj name = SCM_OBJ_INIT, module = SCM_OBJ_INIT;

  name = scm_capi_make_symbol_from_cstr("main", SCM_ENC_ASCII);

  scm_capi_find_module(name, SCM_CSETTER_L(module));

  return module;
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__define_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;

  const char *exp_str = "(define (func x) x)";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected,
                       &module);

  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("func", SCM_ENC_ASCII);

  scm_capi_ut_eval(ev, exp);

  scm_capi_global_var_ref(module, sym, SCM_CSETTER_L(actual));

  cut_assert_true(scm_capi_closure_p(actual));
}

void
test_eval__define_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  const char *exp_str = "(define var 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected,
                       &module);

  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  expected = scm_capi_make_number_from_sword(1);

  scm_capi_ut_eval(ev, exp);

  scm_capi_global_var_ref(module, sym, SCM_CSETTER_L(actual));

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__refer_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  const char *exp_str = "cons";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected,
                       &module);

  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  scm_capi_global_var_ref(module, exp, SCM_CSETTER_L(expected));

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_eq_P(expected, actual)));
}

void
test_eval__refer_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj gvar = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) cons) 1)";
  const char *gvar_str = "cons";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected,
                       &gvar,
                       &module);


  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(gvar_str, SCM_ENC_ASCII);
  gvar = scm_api_read(port);

  scm_capi_global_var_ref(module, gvar, SCM_CSETTER_L(expected));

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_eq_P(expected, actual)));
}

void
test_eval__update_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj val_bef = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  const char *exp_str = "(set! var 10)";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected,
                       &val_bef,
                       &module);

  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  val_bef = scm_capi_make_number_from_sword(1);
  expected = scm_capi_make_number_from_sword(10);

  scm_capi_define_global_var(module, sym, val_bef, true);

  scm_capi_ut_eval(ev, exp);

  scm_capi_global_var_ref(module,sym, SCM_CSETTER_L(actual));

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__variable_arity_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda x x))";
  const char *ect_str = "()";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__variable_arity_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda x x) 1 2 3)";
  const char *ect_str = "(1 2 3)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__variable_arity_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x . y)  (cons x y)) 1)";
  const char *ect_str = "(1)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__variable_arity_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x . y)  (cons x y)) 1 2 3)";
  const char *ect_str = "(1 2 3)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_1(void)
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

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_2(void)
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((((lambda (x)"
                        "      (lambda ()"
                        "         (lambda (y) (cons x y)))) 1)) 2)";
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((((lambda (x)"
                        "      (lambda (y)"
                        "         (lambda (z) (cons x z)))) 1) 2) 3)";
  const char *ect_str = "(1 . 3)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((((lambda (x)"
                        "      (lambda (y)"
                        "         (let ((a 1))"
                        "            (lambda (z) (cons x z))))) 1) 2) 3)";
  const char *ect_str = "(1 . 3)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__closure_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x)"
                        "    (let ((a (lambda () (set! x 100)))"
                        "          (b (lambda () x)))"
                        "       (a)"
                        "       (b))) 1)";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__tail_call_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () ((lambda () 1))))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__tail_call_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x y) ((lambda () 1))) '10 '100)";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__tail_call_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () ((lambda (x y) y) 10 100)))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__tail_call_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x y) ((lambda (a b c) c) 10 100 1000)) 3 5)";
  const char *ect_str = "1000";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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
test_eval__lambda_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)) 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)(y 2)) (cons x y))";
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)(y 2)) (set! y 100) y)";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1))"
                        "   (let ((a (lambda () (set! x 100)))"
                        "         (b (lambda () x)))"
                        "      (a)"
                        "      (b)))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (let ((x 1)(y 2)) x)))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ())";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__named_let_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let loop ((x 1)(y 2)) x)";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__named_let_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let loop ())";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__named_let_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let loop ((i '(a b c))(o '()))"
                        "   (if (null? i) o"
                        "       (loop (cdr i) (cons (car i) o))))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__named_let_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "    (let loop ((i '(a b c))(o '()))"
                        "       (if (null? i) o"
                        "           (loop (cdr i) (cons (car i) o))))))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__named_let_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "    (let loop ((i '(a b c))(o '()))"
                        "       (if (null? i) o"
                        "           (loop (cdr i) (cons (car i) o))))"
                        "    '(c b a)))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)) 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) (cons x y))";
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) (set! x 100) x)";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)"
                        "      (a (lambda () (set! x 100)))"
                        "      (b (lambda () x)))"
                        "  (a)"
                        "  (b))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (let* ((x 1)(y 2)) x)))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__let_a_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ())";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x 1)(y 2)) x)";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x (lambda () y))(y 100)) (x))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x 1)(y 2)) (set! y 10) y)";
  const char *ect_str = "10";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (letrec ((x 1)(y 2)) x)))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ())";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y 2)) x)";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x (lambda () y))(y 100)) (x))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y 2)) (set! y 10) y)";
  const char *ect_str = "10";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y (cons 'a x))) y)";
  const char *ect_str = "(a . 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (letrec* ((x 1)(y 2)) x)))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__letrec_a_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ())";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__internal_definition_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (define x 1) (define y 2) x))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__internal_definition_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(((lambda () (define x (lambda () y)) (define y 100) x)))";
  const char *ect_str = "100";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__internal_definition_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (define x 1) (define y 2) (set! y 10) y))";
  const char *ect_str = "10";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__internal_definition_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (define x 1) (define y (cons 'a x)) y))";
  const char *ect_str = "(a . 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__internal_definition_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "   (define x 1)"
                        "   (begin"
                        "     (define y 2)"
                        "     x)))";
  const char *ect_str = "1";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__begin_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(begin (cons 'a 'b) (cons 'x 'y))";
  const char *ect_str = "(x . y)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__begin_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  const char *exp_str = "(begin (define gvar 1))";

  SCM_STACK_FRAME_PUSH(&exp, &port, &sym,
                       &actual, &expected,
                       &module);

  module = main_module();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  sym = scm_capi_make_symbol_from_cstr("gvar", SCM_ENC_ASCII);

  expected = scm_capi_make_number_from_sword(1);

  scm_capi_ut_eval(ev, exp);

  scm_capi_global_var_ref(module, sym, SCM_CSETTER_L(actual));

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__begin_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(begin)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_1(void)
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_2(void)
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_3(void)
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

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if #f 'b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (if 'a 'b 'c)))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__if_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (if '#f 'b)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond (else 'a))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a 'b))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a 'b)('c 'd))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a => (lambda (x) (cons x 1))))";
  const char *ect_str = "(a . 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a)('b 'c))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond (#f 'a)('b 'c))";
  const char *ect_str = "c";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_8(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond (#f 'a)(else 'b))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_9(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond (else 'a))))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_10(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond ('a))))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_11(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond ('a 'b))))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_12(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond ('a 'b)('c 'd))))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_13(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond ('a => (lambda (x) (cons x 1))))))";
  const char *ect_str = "(a . 1)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_14(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond ('a)('b 'c))))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_15(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond (#f 'a)('b 'c))))";
  const char *ect_str = "c";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__cond_16(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (cond (#f 'a)(else 'b))))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and)";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and #f)";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and 'a 'b)";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and #f 'b)";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and (null? '()) (null? '()))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and (null? 'a) (null? '()))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_8(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and)))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_9(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and 'a)))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_10(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and #f)))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_11(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and 'a 'b)))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_12(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and #f 'b)))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_13(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and (null? '()) (null? '()))))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__and_14(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (and (null? 'a) (null? '()))))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or)";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or #f)";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or 'a 'b)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or #f 'b)";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or (null? '()) (null? 'a))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or (null? 'a) (null? '()))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_8(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or)))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_9(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or 'a)))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_10(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or #f)))";
  const char *ect_str = "#f";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_11(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or 'a 'b)))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_12(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or #f 'b)))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_13(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or (null? '()) (null? 'a))))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__or_14(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (or (null? 'a) (null? '()))))";
  const char *ect_str = "#t";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when 'a 'b)";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when #f 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when (null? ()) (cons 'a 'b))";
  const char *ect_str = "(a . b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (when 'a)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (when 'a 'b)))";
  const char *ect_str = "b";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (when #f 'a)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__when_8(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (when (null? ()) (cons 'a 'b))))";
  const char *ect_str = "(a . b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless 'a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless #f 'a)";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless 'a 'b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless (null? 'a) (cons 'a 'b))";
  const char *ect_str = "(a . b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (unless 'a)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (unless #f 'a)))";
  const char *ect_str = "a";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (unless 'a 'b)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);


  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__unless_8(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda () (unless (null? 'a) (cons 'a 'b))))";
  const char *ect_str = "(a . b)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x '(a b c) (cdr x))"
                        "     (y '() (cons (car x) y)))"
                        "    ((null? x) y)"
                        "  )";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x '(a b c) (cdr x))"
                        "     (y '()))"
                        "    ((null? x) y)"
                        "  (set! y (cons (car x) y)))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x '(a b c) (cdr x))"
                        "     (y '() (cons (car x) y)))"
                        "    ((null? x))"
                        "  )";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ()"
                        "    (#t)"
                        "  )";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  expected = scm_api_undef();

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "    (do ((x '(a b c) (cdr x))"
                        "         (y  '() (cons (car x) y)))"
                        "        ((null? x) y)"
                        "     )))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "    (do ((x '(a b c) (cdr x))"
                        "         (y '() (cons (car x) y)))"
                        "        ((null? x) (car y))"
                        "     )))";
  const char *ect_str = "c";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_eval__do_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda ()"
                        "    (do ((x '(a b c) (cdr x))"
                        "         (y '()))"
                        "        ((null? x) y)"
                        "      (set! y (cons (car x) y)))))";
  const char *ect_str = "(c b a)";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(ect_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_capi_ut_eval(ev, exp);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}
