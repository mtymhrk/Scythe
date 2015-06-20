#include "scythe/api.h"

#include "test.h"

TEST_GROUP(exec_proc);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(exec_proc)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_evaluator_load_core(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(exec_proc)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static ScmObj
eval_cstr(const char *str)
{
  return scm_capi_ut_eval(ev, read_cstr(str));
}

static void
test_eval__comp_val_with_obj(const char *expr, const char *expc)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = read_cstr(expc);
  actual = eval_cstr(expr);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(exec_proc, callcc_1)
{
  test_eval__comp_val_with_obj("(letrec ((func (lambda (x)"
                               "            (if (null? x) x"
                               "               (call/cc (lambda (cont)"
                               "                           (cons (car x)"
                               "                                 (func (cdr x)))))))))"
                               "   (func '(a b c d e f))"
                               " )",
                               "(a b c d e f)");
}

TEST(exec_proc, callcc_2)       /* non-local exit */
{
  test_eval__comp_val_with_obj("(let ((x '(a b c d e f))"
                               "      (ls '()))"
                               "   (call/cc"
                               "      (lambda (break)"
                               "         (let loop ((y x))"
                               "            (when (null? y) (break ls))"
                               "            (set! ls (cons (car y) ls))"
                               "            (loop (cdr y)))))"
                               " )",
                               "(f e d c b a)");
}

TEST(exec_proc, callcc_3)
{
  test_eval__comp_val_with_obj("(let ((c '())"
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
                               " )",
                               "(c d e)");
}

TEST(exec_proc, multiple_return_values_1)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda () (values 1 2 3))"
                               "   (lambda (x y z) (cons x (cons y (cons z '()))))"
                               " )",
                               "(1 2 3)");
}

TEST(exec_proc, multiple_return_values_2)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda () (values))"
                               "   (lambda x x)"
                               " )",
                               "()");
}

TEST(exec_proc, multiple_return_values_3)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda () 1)"
                               "   (lambda (x) x)"
                               " )",
                               "1");
}

TEST(exec_proc, multiple_return_values_4)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda () (values 1 2 3 4))"
                               "   (lambda (x y . z) (cons x (cons y z)))"
                               " )",
                               "(1 2 3 4)");
}

TEST(exec_proc, multiple_return_values_5)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda ()"
                               "      (values 1 2 3 4 5 6 7 8 9 10))"
                               "   (lambda (a b c d e f g h i j)"
                               "      (list a b c d e f g h i j))"
                               " )",
                               "(1 2 3 4 5 6 7 8 9 10)");
}

TEST(exec_proc, multiple_return_values_6)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda ()"
                               "      (values 1 2 3 4 5 6 7 8 9 10 11))"
                               "   (lambda (a b c d e f g h i j k)"
                               "      (list a b c d e f g h i j k))"
                               " )",
                               "(1 2 3 4 5 6 7 8 9 10 11)");
}

TEST(exec_proc, multiple_return_values_7)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda ()"
                               "      (call/cc (lambda (c) (c 1 2 3 4))))"
                               "   (lambda (x y . z) (cons x (cons y z)))"
                               " )",
                               "(1 2 3 4)");
}

TEST(exec_proc, multiple_return_values_8)
{
  test_eval__comp_val_with_obj("(call-with-values"
                               "   (lambda ()"
                               "      (call/cc (lambda (c) (c))))"
                               "   (lambda x x)"
                               " )",
                               "()");
}

TEST(exec_proc, with_exception_handler__non_raise)
{
  test_eval__comp_val_with_obj("(call/cc"
                               "   (lambda (k)"
                               "      (with-exception-handler"
                               "         (lambda (x) (k 100))"
                               "         (lambda () 10))))",
                               "10");
}

TEST(exec_proc, with_exception_handler__raise)
{
  test_eval__comp_val_with_obj("(call/cc"
                               "   (lambda (k)"
                               "      (with-exception-handler"
                               "         (lambda (x) (k 100))"
                               "         (lambda () (raise 'an-error)))))",
                               "100");
}

TEST(exec_proc, with_exception_handler__raise_2)
{
  test_eval__comp_val_with_obj("(let ((var 0))"
                               "   (call/cc"
                               "      (lambda (k)"
                               "         (with-exception-handler"
                               "            (lambda (x) (set! var (+ var 100)) (k))"
                               "            (lambda ()"
                               "               (with-exception-handler"
                               "                  (lambda (x) (set! var x))"
                               "                  (lambda () (raise 10)))))))"
                               "   var)",
                               "110");
}

TEST(exec_proc, with_exception_handler__raise_continuable)
{
  test_eval__comp_val_with_obj("(with-exception-handler"
                               "   (lambda (x) (* x 10))"
                               "   (lambda () (+ (raise-continuable 1)"
                               "                 (raise-continuable 2))))",
                               "30");
}

TEST(exec_proc, with_exception_handler__raise_continuable_2)
{
  test_eval__comp_val_with_obj("(with-exception-handler"
                               "   (lambda (x) (* x 5))"
                               "   (lambda ()"
                               "      (with-exception-handler"
                               "         (lambda (x) (* (raise-continuable 2) x))"
                               "         (lambda () (+ (raise-continuable 10) 1)))))",
                               "101");
}

TEST(exec_proc, with_exception_handler__error)
{
  test_eval__comp_val_with_obj("(call/cc"
                               "   (lambda (k)"
                               "      (with-exception-handler"
                               "         (lambda (x) (k 100))"
                               "         (lambda () (error \"an-error\" 'foo)))))",
                               "100");
}

TEST(exec_proc, parameter__make_parameter)
{
  test_eval__comp_val_with_obj("((make-parameter 100))",
                               "100");
}

TEST(exec_proc, parameter__make_parameter__specify_converter)
{
  test_eval__comp_val_with_obj("((make-parameter 100 (lambda (x) (+ x 1))))",
                               "101");
}

TEST(exec_proc, dynamic_wind__order)
{
  test_eval__comp_val_with_obj("(let ((x ()))"
                               "  (dynamic-wind"
                               "    (lambda () (set! x (cons 'a x)))"
                               "    (lambda () (set! x (cons 'b x)))"
                               "    (lambda () (set! x (cons 'c x))))"
                               "  x)",
                               "(c b a)");
}

TEST(exec_proc, dynamic_wind__return_value)
{
  test_eval__comp_val_with_obj("(dynamic-wind"
                               "  (lambda () 1)"
                               "  (lambda () 2)"
                               "  (lambda () 3))",
                               "2");
}
