#include <cutter.h>

#include "vm.h"
#include "api.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_api_compile__self_eval_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "1";
  const char *asm_str = "((immval 1)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "global_var";
  const char *asm_str = "((gref global_var)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) global_var))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((gref global_var)(return 2)))"
                        "    (return 2)))"
                        " (return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__set_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(set! global_var 'a)";
  const char *asm_str = "((immval a)(gset global_var)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__quote(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "'(a b c)";
  const char *asm_str = "((immval (a b c))(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(func)";
  const char *asm_str = "((gref func)(tcall 0 0)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(func 'a 'b)";
  const char *asm_str = "((immval a)(push)(immval b)(push)(gref func)(tcall 2 0)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () 'a)";
  const char *asm_str = "((asm-close 0 ((immval a)(return 0)))(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2) 'a)";
  const char *asm_str = "((asm-close 0 ((immval a)(return 2)))(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2 . v3) 'a)";
  const char *asm_str = "((asm-close 0 ((immval a)(return 3)))(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) b2))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((sref 1)(return 2)))"
                        "    (return 2)))"
                        " (return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) b2))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((sref 1)(return 2)))"
                        "    (return 2)))"
                        " (return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

/* void */
/* test_scm_api_compile__set_bound_variable_1(void) */
/* { */
/*   ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT; */
/*   ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT; */
/*   const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))"; */
/*   const char *asm_str = "((asm-close 0" */
/*                         "   ((asm-close 0" */
/*                         "      ((box 1)(immval a)(sset 1)(return 2)))" */
/*                         "    (return 2)))" */
/*                         " (return 0))"; */

/*   SCM_STACK_FRAME_PUSH(&exp, &port, */
/*                        &actual, &expected); */

/*   port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII); */
/*   exp = scm_api_read(port); */

/*   port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII); */
/*   expected = scm_api_read(port); */

/*   actual = scm_api_compile(exp); */

/*   scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
/*   scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

/*   cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual))); */
/* } */

void
test_scm_api_compile__refer_free_variable(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) f2))";
  const char *asm_str = "((asm-close 0"
                        "   ((sref 1)"
                        "    (push)"
                        "    (asm-close 1"
                        "      ((cref 0)(return 2)))"
                        "    (return 2)))"
                        " (return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}
