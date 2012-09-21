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
test_eval__callcc_1(void)       /* no reinstatement */
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(letrec ((func (lambda (x)"
                        "            (if (null? x) x"
                        "               (call/cc (lambda (cont)"
                        "                           (cons (car x)"
                        "                                 (func (cdr x)))))))))"
                        "   (func '(a b c d e f))"
                        " )";
  const char *ect_str = "(a b c d e f)";

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
test_eval__callcc_2(void)       /* non-local exit */
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((x '(a b c d e f))"
                        "      (ls '()))"
                        "   (call/cc"
                        "      (lambda (break)"
                        "         (let loop ((y x))"
                        "            (when (null? y) (break ls))"
                        "            (set! ls (cons (car y) ls))"
                        "            (loop (cdr y)))))"
                        " )";
  const char *ect_str = "(f e d c b a)";

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
test_eval__callcc_3(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "(let ((c '())"
                        "      (f #t)"
                        "      (r '()))"
                        "   (let* ((x '())"
                        "          (y (cons 'a x))"
                        "          (z (cons 'b x)))"
                        "      (let ((v (call/cc"
                        "                  (lambda (cont)"
                        "                      (set! c cont)"
                        "                      z)))"
                        "             (w 'c))"
                        "         (set! r (cons w v))))"
                        "   (when f (set! f #f) (c '(d e)))"
                        "   r"
                        " )";
  const char *ect_str = "(c d e)";

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
