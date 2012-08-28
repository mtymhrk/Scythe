#include <cutter.h>

#include "vm.h"
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
test_scm_api_compile__self_eval_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "1";
  const char *asm_str = "((immval 1))";

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
test_scm_api_compile__define_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define global_var 1)";
  const char *asm_str = "((immval 1)(gdef global_var))";

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
test_scm_api_compile__define_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define (func x) x)";
  const char *asm_str = "((asm-close 0 ((sref 0 0)(return)))(gdef func))";

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
  const char *asm_str = "((gref global_var))";

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
                        "      ((gref global_var)(return)))"
                        "    (return))))";


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
  const char *asm_str = "((immval a)(gset global_var))";

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
  const char *asm_str = "((immval (a b c)))";

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
  const char *asm_str = "((cframe)(gref func)(call 0))";

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
  const char *asm_str = "((frame)"
                        " (immval a)(push)"
                        " (immval b)(push)"
                        " (gref func)"
                        " (call 2))";

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
test_scm_api_compile__application_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) x) 1)";
  const char *asm_str = "((frame)"
                        " (immval 1)(push)"
                        " (asm-close 0"
                        "   ((sref 0 0)(return)))"
                        " (call 1))";

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
test_scm_api_compile__application_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) (lambda (y) (cons x y))) 1)";
  const char *asm_str = "((frame)"
                        " (immval 1)(push)"
                        " (asm-close 0"
                        "   ((asm-close 1"
                        "      ((eframe)"
                        "       (sref 0 1)(push)"
                        "       (sref 0 0)(push)"
                        "       (gref cons)"
                        "       (tcall 2)"
                        "       (return)))" /* 無駄な return 命令*/
                        "    (return)))"
                        " (call 1))";

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
test_scm_api_compile__application_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () ((lambda () 1)))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0 ((immval 1)(return)))"
                        "    (tcall 0)"
                        "    (return))))"; /* 無駄な return 命令*/

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
  const char *asm_str = "((asm-close 0 ((immval a)(return))))";

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
  const char *asm_str = "((asm-close 0 ((immval a)(return))))";

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
  const char *asm_str = "((asm-close 0 ((immval a)(return))))";

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
test_scm_api_compile__lambda_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda v 'a)";
  const char *asm_str = "((asm-close 0 ((immval a)(return))))";

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
test_scm_api_compile__let_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)(y 2)) x)";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (ecommit 2)"
                        " (sref 0 0)"
                        " (epop))";

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
test_scm_api_compile__let_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ())";
  const char *asm_str = "()";

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
test_scm_api_compile__let_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)(y 2)) (lambda () (cons x y)))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (ecommit 2)"
                        " (asm-close 1"
                        "   ((eframe)"
                        "    (sref 0 0)(push)"
                        "    (sref 1 0)(push)"
                        "    (gref cons)"
                        "    (tcall 2)"
                        "    (return)))"
                        " (epop))";

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
test_scm_api_compile__let_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)(y 2)) (set! y 100))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (ecommit 2)"
                        " (box 1 0)"
                        " (immval 100)"
                        " (sset 1 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x 1)(y 2)) x)";
  const char *asm_str = "((emine 2)"
                        " (eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (edemine 2 0)"
                        " (sref 0 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x (lambda () y))(y 100)) x)";
  const char *asm_str = "((emine 2)"
                        " (eframe)"
                        " (asm-close 1 ((sref 1 0)(return)))(push)"
                        " (immval 100)(push)"
                        " (edemine 2 0)"
                        " (sref 0 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x 1)(y 2)) (set! y 10) y)";
  const char *asm_str = "((emine 2)"
                        " (eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (edemine 2 0)"
                        " (immval 10)"
                        " (sset 1 0)"
                        " (sref 1 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_a_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y 2)) x)";
  const char *asm_str = "((emine 2)"
                        " (immval 1)(demine 0 0)"
                        " (immval 2)(demine 1 0)"
                        " (sref 0 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_a_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x (lambda () y))(y 100)) x)";
  const char *asm_str = "((emine 2)"
                        " (asm-close 1 ((sref 1 0)(return)))(demine 0 0)"
                        " (immval 100)(demine 1 0)"
                        " (sref 0 0)"
                        " (epop))";

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
test_scm_api_compile__letrec_a_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y 2)) (set! y 10) y)";
  const char *asm_str = "((emine 2)"
                        " (immval 1)(demine 0 0)"
                        " (immval 2)(demine 1 0)"
                        " (immval 10)"
                        " (sset 1 0)"
                        " (sref 1 0)"
                        " (epop))";

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
                        "      ((sref 1 0)(return)))"
                        "    (return))))";

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
                        "      ((sref 1 0)(return)))"
                        "    (return))))";

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
test_scm_api_compile__refer_bound_variable_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a) b2))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((box 1 0)"
                        "       (immval a)(sset 1 0)"
                        "       (sref 1 0)"
                        "       (return)))"
                        "    (return))))";

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
test_scm_api_compile__set_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((box 1 0)(immval a)(sset 1 0)(return)))"
                        "    (return))))";

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
test_scm_api_compile__set_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a)))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 0"
                        "      ((box 1 0)(immval a)(sset 1 0)(return)))"
                        "    (return))))";

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
test_scm_api_compile__refer_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) f2))";
  const char *asm_str = "((asm-close 0"
                        "   ((asm-close 1"
                        "      ((sref 1 1)(return)))"
                        "    (return))))";

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
test_scm_api_compile__refer_free_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a) f2))";
  const char *asm_str = "((asm-close 0"
                        "   ((box 1 0)"
                        "    (asm-close 1"
                        "      ((immval a)(sset 1 1)"
                        "       (sref 1 1)"
                        "       (return)))"
                        "    (return))))";

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
test_scm_api_compile__set_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a)))";
  const char *asm_str = "((asm-close 0"
                        "   ((box 1 0)"
                        "    (asm-close 1"
                        "      ((immval a)(sset 1 1)(return)))"
                        "    (return))))";

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
test_scm_api_compile__conditional_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if 'a 'b 'c)";
  const char *asm_str = "(  (immval a)"
                        "   (jmpf lbl_if-a_1)"
                        "   (immval b)"
                        "   (jmp lbl_if-j_0)"
                        " (label lbl_if-a_1)"
                        "   (immval c)"
                        " (label lbl_if-j_0)"
                        ")";

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
test_scm_api_compile__conditional_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if 'a 'b)";
  const char *asm_str = "(  (immval a)"
                        "   (jmpf lbl_if-j_2)"
                        "   (immval b)"
                        " (label lbl_if-j_2)"
                        ")";

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

