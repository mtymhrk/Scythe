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
cut_setup(void)
{
  scm_capi_ut_clear_compiler_label_id();
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__define_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define global_var 1)";
  const char *asm_str = "((immval 1)(gdef global_var (main)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__define_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(define (func x) x)";
  const char *asm_str = "((asm-close 0 1"
                        "   ((sref 0 0)(return)))(gdef func (main)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "global_var";
  const char *asm_str = "((gref global_var (main)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_global_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) global_var))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((gref global_var (main))(return)))"
                        "    (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__set_global_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(set! global_var 'a)";
  const char *asm_str = "((immval a)(gset global_var (main)))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(func)";
  const char *asm_str = "((cframe)(gref func (main))(call 0)(arity 1))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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
                        " (gref func (main))"
                        " (call 2)"
                        " (arity 1))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) x) 1)";
  const char *asm_str = "((frame)"
                        " (immval 1)(push)"
                        " (asm-close 0 1"
                        "   ((sref 0 0)(return)))"
                        " (call 1)"
                        " (arity 1))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "((lambda (x) (lambda (y) (cons x y))) 1)";
  const char *asm_str = "((frame)"
                        " (immval 1)(push)"
                        " (asm-close 0 1"
                        "   ((asm-close 1 1"
                        "      ((eframe)"
                        "       (sref 0 1)(push)"
                        "       (sref 0 0)(push)"
                        "       (gref cons (main))"
                        "       (tcall 2)))"
                        "    (return)))"
                        " (call 1)"
                        " (arity 1))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__application_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () ((lambda () 1)))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((asm-close 0 0 ((immval 1)(return)))"
                        "    (tcall 0))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () 'a)";
  const char *asm_str = "((asm-close 0 0 ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2) 'a)";
  const char *asm_str = "((asm-close 0 2 ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (v1 v2 . v3) 'a)";
  const char *asm_str = "((asm-close 0 -3 ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda v 'a)";
  const char *asm_str = "((asm-close 0 -1 ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda ())";
  const char *asm_str = "((asm-close 0 0 ((undef)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__lambda_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cons 'a 'b) (cons 'c 'd))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((frame)"
                        "    (immval a)(push)"
                        "    (immval b)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (eframe)"
                        "    (immval c)(push)"
                        "    (immval d)(push)"
                        "    (gref cons (main))"
                        "    (tcall 2))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ())";
  const char *asm_str = "((undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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
                        " (asm-close 1 0"
                        "   ((eframe)"
                        "    (sref 0 0)(push)"
                        "    (sref 1 0)(push)"
                        "    (gref cons (main))"
                        "    (tcall 2)))"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (let ((x 1)(y 2)) x))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((eframe)"
                        "    (immval 1)(push)"
                        "    (immval 2)(push)"
                        "    (ecommit 2)"
                        "    (sref 0 0)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x 1)) (cons 'a 'b) (cons 'c 'd))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (frame)"
                        " (immval a)(push)"
                        " (immval b)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity -1)"
                        " (frame)"
                        " (immval c)(push)"
                        " (immval d)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity 1)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__named_let_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let loop ((x 1)(y 2)) (loop x y))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (ecommit 2)"
                        " (emine 1)"
                        " (asm-close 1 2"
                        "   ((eframe)"
                        "    (sref 0 0)(push)"
                        "    (sref 1 0)(push)"
                        "    (sref 0 1)"
                        "    (tcall 2)))"
                        " (demine 0 0)"
                        " (frame)"
                        " (sref 0 1)(push)"
                        " (sref 1 1)(push)"
                        " (sref 0 0)"
                        " (call 2)"
                        " (arity 1)"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__named_let_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let loop ())";
  const char *asm_str = "((emine 1)"
                        " (asm-close 0 0 ((undef)(return)))"
                        " (demine 0 0)"
                        " (cframe)"
                        " (sref 0 0)"
                        " (call 0)"
                        " (arity 1)"
                        " (epop))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__named_let_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (let loop ((x 1)(y 2)) (loop 3 4)))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((eframe)"
                        "    (immval 1)(push)"
                        "    (immval 2)(push)"
                        "    (ecommit 2)"
                        "    (emine 1)"
                        "    (asm-close 1 2"
                        "      ((eframe)"
                        "       (immval 3)(push)"
                        "       (immval 4)(push)"
                        "       (sref 0 1)"
                        "       (tcall 2)))"
                        "    (demine 0 0)"
                        "    (eframe)"
                        "    (sref 0 1)(push)"
                        "    (sref 1 1)(push)"
                        "    (sref 0 0)"
                        "    (tcall 2))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) x)";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (eframe)"
                        " (immval 2)(push)"
                        " (ecommit 1)"
                        " (sref 0 1)"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ())";
  const char *asm_str = "((undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) (lambda () (cons x y)))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (eframe)"
                        " (immval 2)(push)"
                        " (ecommit 1)"
                        " (asm-close 2 0"
                        "   ((eframe)"
                        "    (sref 0 1)(push)"
                        "    (sref 0 0)(push)"
                        "    (gref cons (main))"
                        "    (tcall 2)))"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) (set! x 100))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (box 0 0)"
                        " (eframe)"
                        " (immval 2)(push)"
                        " (ecommit 1)"
                        " (immval 100)"
                        " (sset 0 1)"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (let* ((x 1)(y 2)) x))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((eframe)"
                        "    (immval 1)(push)"
                        "    (ecommit 1)"
                        "    (eframe)"
                        "    (immval 2)(push)"
                        "    (ecommit 1)"
                        "    (sref 0 1)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y x)) x)";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (eframe)"
                        " (sref 0 0)(push)"
                        " (ecommit 1)"
                        " (sref 0 1)"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__let_a_7(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let* ((x 1)(y 2)) (cons x y) (cons y x))";
  const char *asm_str = "((eframe)"
                        " (immval 1)(push)"
                        " (ecommit 1)"
                        " (eframe)"
                        " (immval 2)(push)"
                        " (ecommit 1)"
                        " (frame)"
                        " (sref 0 1)(push)"
                        " (sref 0 0)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity -1)"
                        " (frame)"
                        " (sref 0 0)(push)"
                        " (sref 0 1)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity 1)"
                        " (epop)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x (lambda () y))(y 100)) x)";
  const char *asm_str = "((emine 2)"
                        " (eframe)"
                        " (asm-close 1 0 ((sref 1 0)(return)))(push)"
                        " (immval 100)(push)"
                        " (edemine 2 0)"
                        " (sref 0 0)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (letrec ((x 1)(y 2)) x))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((emine 2)"
                        "    (eframe)"
                        "    (immval 1)(push)"
                        "    (immval 2)(push)"
                        "    (edemine 2 0)"
                        "    (sref 0 0)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ())";
  const char *asm_str = "((undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((x 1)(y 2)) (cons x y) (cons y x))";
  const char *asm_str = "((emine 2)"
                        " (eframe)"
                        " (immval 1)(push)"
                        " (immval 2)(push)"
                        " (edemine 2 0)"
                        " (frame)"
                        " (sref 0 0)(push)"
                        " (sref 1 0)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity -1)"
                        " (frame)"
                        " (sref 1 0)(push)"
                        " (sref 0 0)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity 1)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_a_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x (lambda () y))(y 100)) x)";
  const char *asm_str = "((emine 2)"
                        " (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)"
                        " (immval 100)(demine 1 0)"
                        " (sref 0 0)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_a_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (letrec* ((x 1)(y 2)) x))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((emine 2)"
                        "    (immval 1)(demine 0 0)"
                        "    (immval 2)(demine 1 0)"
                        "    (sref 0 0)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_a_5(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ())";
  const char *asm_str = "((undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__letrec_a_6(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec* ((x 1)(y 2)) (cons x y) (cons y x))";
  const char *asm_str = "((emine 2)"
                        " (immval 1)(demine 0 0)"
                        " (immval 2)(demine 1 0)"
                        " (frame)"
                        " (sref 0 0)(push)"
                        " (sref 1 0)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity -1)"
                        " (frame)"
                        " (sref 1 0)(push)"
                        " (sref 0 0)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity 1)"
                        " (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__internal_definition_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (define x 1) (define y 2) x)";
  const char *asm_str = "((asm-close 0 0"
                        "  ((emine 2)"
                        "   (immval 1)(demine 0 0)"
                        "   (immval 2)(demine 1 0)"
                        "   (sref 0 0)"
                        "   (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__internal_definition_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (define x (lambda () y)) (define y 100) x)";
  const char *asm_str = "((asm-close 0 0"
                        "  ((emine 2)"
                        "   (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)"
                        "   (immval 100)(demine 1 0)"
                        "   (sref 0 0)"
                        "   (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__internal_definition_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (define x 1) (define y 2) (set! y 10) y)";
  const char *asm_str = "((asm-close 0 0"
                        "  ((emine 2)"
                        "   (immval 1)(demine 0 0)"
                        "   (immval 2)(demine 1 0)"
                        "   (immval 10)"
                        "   (sset 1 0)"
                        "   (sref 1 0)"
                        "   (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__internal_definition_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda ()"
                        "  (define x 1)"
                        "  (begin"
                        "    (define y 2)"
                        "    x))";
  const char *asm_str = "((asm-close 0 0"
                        "  ((emine 2)"
                        "   (immval 1)(demine 0 0)"
                        "   (immval 2)(demine 1 0)"
                        "   (sref 0 0)"
                        "   (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__begin_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(begin (cons 'a 'b) (cons 'x 'y))";
  const char *asm_str = "((frame)"
                        " (immval a)(push)"
                        " (immval b)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity -1)"
                        " (frame)"
                        " (immval x)(push)"
                        " (immval y)(push)"
                        " (gref cons (main))"
                        " (call 2)"
                        " (arity 1))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__begin_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(begin (define gvar 1))";
  const char *asm_str = "((immval 1)"
                        " (gdef gvar (main)))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__begin_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(begin)";
  const char *asm_str = "((undef))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) b2))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((sref 1 0)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) b2))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((sref 1 0)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_bound_variable_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a) b2))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((box 1 0)"
                        "       (immval a)(sset 1 0)"
                        "       (sref 1 0)"
                        "       (return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__set_bound_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((box 1 0)(immval a)(sset 1 0)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__set_bound_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a)))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 0 2"
                        "      ((box 1 0)(immval a)(sset 1 0)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) f2))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((asm-close 1 2"
                        "      ((sref 1 1)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_free_variable_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a) f2))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((box 1 0)"
                        "    (asm-close 1 2"
                        "      ((immval a)(sset 1 1)"
                        "       (sref 1 1)"
                        "       (return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__set_free_variable_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a)))";
  const char *asm_str = "((asm-close 0 2"
                        "   ((box 1 0)"
                        "    (asm-close 1 2"
                        "      ((immval a)(sset 1 1)(return)))"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__if_1(void)
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

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__if_2(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(if 'a 'b)";
  const char *asm_str = "(  (immval a)"
                        "   (jmpf lbl_if-a_1)"
                        "   (immval b)"
                        "   (jmp lbl_if-j_0)"
                        " (label lbl_if-a_1)"
                        "   (undef)"
                        " (label lbl_if-j_0)"
                        ")";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__if_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (if 'a 'b 'c))";
  const char *asm_str = "((asm-close 0 0"
                        "   (  (immval a)"
                        "      (jmpf lbl_if-a_0)"
                        "      (immval b)"
                        "      (return)"
                        "    (label lbl_if-a_0)"
                        "      (immval c)"
                        "      (return)"
                        "   )"
                        " ))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__if_4(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (if 'a 'b))";
  const char *asm_str = "((asm-close 0 0"
                        "   (  (immval a)"
                        "      (jmpf lbl_if-a_0)"
                        "      (immval b)"
                        "      (return)"
                        "    (label lbl_if-a_0)"
                        "      (undef)"
                        "      (return)"
                        "   )"
                        " ))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond)";
  const char *asm_str = "((undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond (else))";
  const char *asm_str = "((undef)(label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond (else 'a))";
  const char *asm_str = "((label lbl_cond-c_1)"
                        "    (immval a)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a 'b))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (immval b)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a 'b)('c 'd))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-c_2)"
                        "    (immval c)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_2)"
                        "    (immval b)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (immval d)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a => write))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (frame)"
                        "    (push)"
                        "    (gref write (main))"
                        "    (call 1)"
                        "    (arity 1)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a => write)('b => write))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-c_2)"
                        "    (immval b)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_2)"
                        "    (frame)"
                        "    (push)"
                        "    (gref write (main))"
                        "    (call 1)"
                        "    (arity 1)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (frame)"
                        "    (push)"
                        "    (gref write (main))"
                        "    (call 1)"
                        "    (arity 1)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_008(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-j_0)"
                        "    (undef)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_009(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a)('b 'c))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-j_0)"
                        "    (immval b)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (immval c)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_010(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(cond ('a 'b)('c))";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_cond-c_1)"
                        "    (immval c)"
                        "    (jmpt lbl_cond-j_0)"
                        "    (undef)"
                        "    (jmp lbl_cond-j_0)"
                        " (label lbl_cond-c_1)"
                        "    (immval b)"
                        " (label lbl_cond-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_011(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((undef)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_012(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond (else)))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((undef)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_013(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond (else 'a)))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((label lbl_cond-c_0)"
                        "       (immval a)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_014(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond ('a 'b)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_cond-c_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_cond-c_0)"
                        "       (immval b)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_015(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond ('a)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_cond-c_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_cond-c_0)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_016(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond ('a)('b 'c)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_cond-c_1)"
                        "       (immval b)"
                        "       (jmpt lbl_cond-c_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_cond-c_1)"
                        "       (return)"
                        "    (label lbl_cond-c_0)"
                        "       (immval c)"
                        "       (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_017(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond ('a 'b)('c)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_cond-c_1)"
                        "       (immval c)"
                        "       (jmpt lbl_cond-c_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_cond-c_1)"
                        "       (immval b)"
                        "       (return)"
                        "    (label lbl_cond-c_0)"
                        "       (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__cond_018(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (cond ('a => write)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_cond-c_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_cond-c_0)"
                        "       (eframe)"
                        "       (push)"
                        "       (gref write (main))"
                        "       (tcall 1))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and)";
  const char *asm_str = "((immval #t))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and 'a)";
  const char *asm_str = "((immval a))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and 'a 'b)";
  const char *asm_str = "(  (immval a)"
                        "   (jmpf lbl_and-j_0)"
                        "   (immval b)"
                        " (label lbl_and-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(and (null? 'a) (null? 'b))";
  const char *asm_str = "(  (frame)"
                        "   (immval a)"
                        "   (push)"
                        "   (gref null? (main))"
                        "   (call 1)"
                        "   (arity 1)"
                        "   (jmpf lbl_and-j_0)"
                        "   (frame)"
                        "   (immval b)"
                        "   (push)"
                        "   (gref null? (main))"
                        "   (call 1)"
                        "   (arity 1)"
                        " (label lbl_and-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (and))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((immval #t)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (and 'a))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (and 'a 'b))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpf lbl_and-j_0)"
                        "       (immval b)"
                        "       (return)"
                        "    (label lbl_and-j_0)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__and_008(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (and (null? 'a) (null? 'b)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (frame)"
                        "       (immval a)"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (call 1)"
                        "       (arity 1)"
                        "       (jmpf lbl_and-j_0)"
                        "       (eframe)"
                        "       (immval b)"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (tcall 1)"
                        "    (label lbl_and-j_0)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or)";
  const char *asm_str = "((immval #f))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or 'a)";
  const char *asm_str = "((immval a))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or 'a 'b)";
  const char *asm_str = "(  (immval a)"
                        "   (jmpt lbl_or-j_0)"
                        "   (immval b)"
                        " (label lbl_or-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(or (null? 'a) (null? 'b))";
  const char *asm_str = "(  (frame)"
                        "   (immval a)"
                        "   (push)"
                        "   (gref null? (main))"
                        "   (call 1)"
                        "   (arity 1)"
                        "   (jmpt lbl_or-j_0)"
                        "   (frame)"
                        "   (immval b)"
                        "   (push)"
                        "   (gref null? (main))"
                        "   (call 1)"
                        "   (arity 1)"
                        " (label lbl_or-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (or))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((immval #f)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (or 'a))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((immval a)(return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (or 'a 'b))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_or-j_0)"
                        "       (immval b)"
                        "       (return)"
                        "    (label lbl_or-j_0)"
                        "    (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__or_008(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (or (null? 'a) (null? 'b)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (frame)"
                        "       (immval a)"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (call 1)"
                        "       (arity 1)"
                        "       (jmpt lbl_or-j_0)"
                        "       (eframe)"
                        "       (immval b)"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (tcall 1)"
                        "    (label lbl_or-j_0)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when 'a)";
  const char *asm_str = "(   (immval a)"
                        "    (jmpf lbl_when-a_1)"
                        "    (undef)"
                        "    (jmp lbl_when-j_0)"
                        " (label lbl_when-a_1)"
                        "    (undef)"
                        " (label lbl_when-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when 'a 'b)";
  const char *asm_str = "(   (immval a)"
                        "    (jmpf lbl_when-a_1)"
                        "    (immval b)"
                        "    (jmp lbl_when-j_0)"
                        " (label lbl_when-a_1)"
                        "    (undef)"
                        " (label lbl_when-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (when 'a))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpf lbl_when-a_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_when-a_0)"
                        "       (undef)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (when 'a 'b))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpf lbl_when-a_0)"
                        "       (immval b)"
                        "       (return)"
                        "    (label lbl_when-a_0)"
                        "       (undef)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when (null? '()) (cons 'a 'b))";
  const char *asm_str = "(   (frame)"
                        "    (immval ())"
                        "    (push)"
                        "    (gref null? (main))"
                        "    (call 1)"
                        "    (arity 1)"
                        "    (jmpf lbl_when-a_1)"
                        "    (frame)"
                        "    (immval a)"
                        "    (push)"
                        "    (immval b)"
                        "    (push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity 1)"
                        "    (jmp lbl_when-j_0)"
                        " (label lbl_when-a_1)"
                        "    (undef)"
                        " (label lbl_when-j_0))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (when (null? '()) (cons 'a 'b)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (frame)"
                        "       (immval ())"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (call 1)"
                        "       (arity 1)"
                        "       (jmpf lbl_when-a_0)"
                        "       (eframe)"
                        "       (immval a)"
                        "       (push)"
                        "       (immval b)"
                        "       (push)"
                        "       (gref cons (main))"
                        "       (tcall 2)"
                        "    (label lbl_when-a_0)"
                        "       (undef)"
                        "       (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__when_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(when 'z (cons 'a 'b) (cons 'c 'd))";
  const char *asm_str = "(   (immval z)"
                        "    (jmpf lbl_when-a_1)"
                        "    (frame)"
                        "    (immval a)(push)"
                        "    (immval b)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (frame)"
                        "    (immval c)(push)"
                        "    (immval d)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity 1)"
                        "    (jmp lbl_when-j_0)"
                        " (label lbl_when-a_1)"
                        "    (undef)"
                        " (label lbl_when-j_0))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless 'a)";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_unless-a_1)"
                        "    (undef)"
                        "    (jmp lbl_unless-j_0)"
                        " (label lbl_unless-a_1)"
                        "    (undef)"
                        " (label lbl_unless-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless 'a 'b)";
  const char *asm_str = "(   (immval a)"
                        "    (jmpt lbl_unless-a_1)"
                        "    (immval b)"
                        "    (jmp lbl_unless-j_0)"
                        " (label lbl_unless-a_1)"
                        "    (undef)"
                        " (label lbl_unless-j_0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (unless 'a))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_unless-a_0)"
                        "       (undef)"
                        "       (return)"
                        "    (label lbl_unless-a_0)"
                        "       (undef)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (unless 'a 'b))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (immval a)"
                        "       (jmpt lbl_unless-a_0)"
                        "       (immval b)"
                        "       (return)"
                        "    (label lbl_unless-a_0)"
                        "       (undef)"
                        "       (return))))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless (null? '()) (cons 'a 'b))";
  const char *asm_str = "(   (frame)"
                        "    (immval ())"
                        "    (push)"
                        "    (gref null? (main))"
                        "    (call 1)"
                        "    (arity 1)"
                        "    (jmpt lbl_unless-a_1)"
                        "    (frame)"
                        "    (immval a)"
                        "    (push)"
                        "    (immval b)"
                        "    (push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity 1)"
                        "    (jmp lbl_unless-j_0)"
                        " (label lbl_unless-a_1)"
                        "    (undef)"
                        " (label lbl_unless-j_0))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda () (unless (null? '()) (cons 'a 'b)))";
  const char *asm_str = "((asm-close 0 0"
                        "   (   (frame)"
                        "       (immval ())"
                        "       (push)"
                        "       (gref null? (main))"
                        "       (call 1)"
                        "       (arity 1)"
                        "       (jmpt lbl_unless-a_0)"
                        "       (eframe)"
                        "       (immval a)"
                        "       (push)"
                        "       (immval b)"
                        "       (push)"
                        "       (gref cons (main))"
                        "       (tcall 2)"
                        "    (label lbl_unless-a_0)"
                        "       (undef)"
                        "       (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__unless_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(unless 'z (cons 'a 'b) (cons 'c 'd))";
  const char *asm_str = "(   (immval z)"
                        "    (jmpt lbl_unless-a_1)"
                        "    (frame)"
                        "    (immval a)(push)"
                        "    (immval b)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (frame)"
                        "    (immval c)(push)"
                        "    (immval d)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity 1)"
                        "    (jmp lbl_unless-j_0)"
                        " (label lbl_unless-a_1)"
                        "    (undef)"
                        " (label lbl_unless-j_0))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_001(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x 'ix 'sx)"
                        "     (y 'iy 'sy))"
                        "    ('t 'e)"
                        "  'c)";
  const char *asm_str = "(   (eframe)"
                        "    (immval ix)"
                        "    (push)"
                        "    (immval iy)"
                        "    (push)"
                        "    (ecommit 2)"
                        " (label lbl_do-s_1)"
                        "    (immval t)"
                        "    (jmpt lbl_do-e_0)"
                        "    (immval c)"
                        "    (eframe)"
                        "    (immval sx)"
                        "    (push)"
                        "    (immval sy)"
                        "    (push)"
                        "    (erebind 2)"
                        "    (jmp lbl_do-s_1)"
                        " (label lbl_do-e_0)"
                        "    (immval e)"
                        "    (epop))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_002(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x 'ix)"
                        "     (y 'iy 'sy))"
                        "    ('t 'e)"
                        "  'c)";
  const char *asm_str = "(   (eframe)"
                        "    (immval ix)"
                        "    (push)"
                        "    (immval iy)"
                        "    (push)"
                        "    (ecommit 2)"
                        " (label lbl_do-s_1)"
                        "    (immval t)"
                        "    (jmpt lbl_do-e_0)"
                        "    (immval c)"
                        "    (eframe)"
                        "    (sref 0 0)"
                        "    (push)"
                        "    (immval sy)"
                        "    (push)"
                        "    (erebind 2)"
                        "    (jmp lbl_do-s_1)"
                        " (label lbl_do-e_0)"
                        "    (immval e)"
                        "    (epop))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_003(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x 'ix 'sx)"
                        "     (y 'iy 'sy))"
                        "    ('t 'e)"
                        "  (set! y 1))";
  const char *asm_str = "(   (eframe)"
                        "    (immval ix)"
                        "    (push)"
                        "    (immval iy)"
                        "    (push)"
                        "    (ecommit 2)"
                        " (label lbl_do-s_1)"
                        "    (box 1 0)"
                        "    (immval t)"
                        "    (jmpt lbl_do-e_0)"
                        "    (immval 1)"
                        "    (sset 1 0)"
                        "    (eframe)"
                        "    (immval sx)"
                        "    (push)"
                        "    (immval sy)"
                        "    (push)"
                        "    (erebind 2)"
                        "    (jmp lbl_do-s_1)"
                        " (label lbl_do-e_0)"
                        "    (immval e)"
                        "    (epop))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_004(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ()"
                        "    ('t)"
                        "  )";
  const char *asm_str = "((label lbl_do-s_1)"
                        "    (immval t)"
                        "    (jmpt lbl_do-e_0)"
                        "    (jmp lbl_do-s_1)"
                        " (label lbl_do-e_0)"
                        "    (undef))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_005(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda ()"
                        "  (do ((x 'ix 'sx)"
                        "       (y 'iy 'sy))"
                        "      ('t 'e)"
                        "    'c))";
  const char *asm_str = "((asm-close 0 0"
                        "   (  (eframe)"
                        "      (immval ix)"
                        "      (push)"
                        "      (immval iy)"
                        "      (push)"
                        "      (ecommit 2)"
                        "   (label lbl_do-s_1)"
                        "      (immval t)"
                        "      (jmpt lbl_do-e_0)"
                        "      (immval c)"
                        "      (eframe)"
                        "      (immval sx)"
                        "      (push)"
                        "      (immval sy)"
                        "      (push)"
                        "      (erebind 2)"
                        "      (jmp lbl_do-s_1)"
                        "   (label lbl_do-e_0)"
                        "      (immval e)"
                        "      (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_006(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda ()"
                        "  (do ((x 'ix 'sx)"
                        "       (y 'iy 'sy))"
                        "      ('t (cons 'a 'b))"
                        "    'c))";
  const char *asm_str = "((asm-close 0 0"
                        "   (  (eframe)"
                        "      (immval ix)"
                        "      (push)"
                        "      (immval iy)"
                        "      (push)"
                        "      (ecommit 2)"
                        "   (label lbl_do-s_1)"
                        "      (immval t)"
                        "      (jmpt lbl_do-e_0)"
                        "      (immval c)"
                        "      (eframe)"
                        "      (immval sx)"
                        "      (push)"
                        "      (immval sy)"
                        "      (push)"
                        "      (erebind 2)"
                        "      (jmp lbl_do-s_1)"
                        "   (label lbl_do-e_0)"
                        "      (eframe)"
                        "      (immval a)"
                        "      (push)"
                        "      (immval b)"
                        "      (push)"
                        "      (gref cons (main))"
                        "      (tcall 2))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_007(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(lambda ()"
                        "  (do ()"
                        "      ('t)"
                        "    ))";
  const char *asm_str = "((asm-close 0 0"
                        "   ((label lbl_do-s_1)"
                        "       (immval t)"
                        "       (jmpt lbl_do-e_0)"
                        "       (jmp lbl_do-s_1)"
                        "    (label lbl_do-e_0)"
                        "       (undef)"
                        "       (return))))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__do_008(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(do ((x 'ix 'sx)"
                        "     (y 'iy 'sy))"
                        "    ('t (cons 'a 'b) (cons 'c 'd))"
                        "  (cons 'e 'f)"
                        "  (cons 'g 'h))";
  const char *asm_str = "(   (eframe)"
                        "    (immval ix)"
                        "    (push)"
                        "    (immval iy)"
                        "    (push)"
                        "    (ecommit 2)"
                        " (label lbl_do-s_1)"
                        "    (immval t)"
                        "    (jmpt lbl_do-e_0)"
                        "    (frame)"
                        "    (immval e)(push)"
                        "    (immval f)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (frame)"
                        "    (immval g)(push)"
                        "    (immval h)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (eframe)"
                        "    (immval sx)"
                        "    (push)"
                        "    (immval sy)"
                        "    (push)"
                        "    (erebind 2)"
                        "    (jmp lbl_do-s_1)"
                        " (label lbl_do-e_0)"
                        "    (frame)"
                        "    (immval a)(push)"
                        "    (immval b)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity -1)"
                        "    (frame)"
                        "    (immval c)(push)"
                        "    (immval d)(push)"
                        "    (gref cons (main))"
                        "    (call 2)"
                        "    (arity 1)"
                        "    (epop))";


  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp, SCM_OBJ_NULL);

  /* scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  cut_assert_true(scm_capi_true_object_p(scm_api_equal_P(expected, actual)));
}
