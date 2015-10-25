#include "scythe/refstk.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(exec_proc);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(exec_proc)
{
  scy = ut_scythe_setup(true);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(exec_proc)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

static ScmObj
eval_cstr(const char *str)
{
  return ut_eval(ut_read_cstr(str));
}

static void
test_eval__comp_val_with_obj(const char *expr, const char *expc)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = ut_read_cstr(expc);
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

TEST(exec_proc, with_exception_handler__continuation__exit_from_with_exception_handler)
{
  test_eval__comp_val_with_obj("(call/cc"
                               " (lambda (break1)"
                               "   (with-exception-handler"
                               "    (lambda (e) (break1 (cons 'out e)))"
                               "    (lambda ()"
                               "      (call/cc"
                               "       (lambda (break2)"
                               "         (with-exception-handler"
                               "          (lambda (e) (break1 (cons 'inn e)))"
                               "          (lambda ()"
                               "            (break2)))))"
                               "      (raise 100)))))",
                               "(out . 100)");
}

TEST(exec_proc, with_exception_handler__continuation__enter_with_exception_handler)
{
  test_eval__comp_val_with_obj("(call/cc"
                               " (lambda (break)"
                               "   (let ((k #f))"
                               "     (with-exception-handler"
                               "      (lambda (e) (break (cons 'out e)))"
                               "      (lambda ()"
                               "        (with-exception-handler"
                               "         (lambda (e) (break (cons 'inn e)))"
                               "         (lambda ()"
                               "           (set! k (call/cc (lambda (k) k)))"
                               "           (unless k (raise 100))))))"
                               "     (when k (k #f)))))",
                               "(inn . 100)");
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

TEST(exec_proc, dynamic_wind__continuation__exit_from_dw)
{
  test_eval__comp_val_with_obj("(let ((x ()))"
                               "  (call/cc"
                               "    (lambda (k)"
                               "      (dynamic-wind"
                               "        (lambda () (set! x (cons 'B-1 x)))"
                               "        (lambda ()"
                               "          (dynamic-wind"
                               "            (lambda () (set! x (cons 'B-2 x)))"
                               "            (lambda () (k #f))"
                               "            (lambda () (set! x (cons 'A-2 x)))))"
                               "        (lambda () (set! x (cons 'A-1 x))))))"
                               "  x)",
                               "(A-1 A-2 B-2 B-1)");
}

TEST(exec_proc, dynamic_wind__continuation__enter_dw)
{
  test_eval__comp_val_with_obj("(let ((x ()) (k #f))"
                               "  (dynamic-wind"
                               "    (lambda () (set! x (cons 'B-1 x)))"
                               "    (lambda ()"
                               "      (dynamic-wind"
                               "        (lambda () (set! x (cons 'B-2 x)))"
                               "        (lambda ()"
                               "          (set! k (call/cc (lambda (kk) kk))))"
                               "        (lambda () (set! x (cons 'A-2 x)))))"
                               "    (lambda () (set! x (cons 'A-1 x))))"
                               "  (when k (k #f))"
                               "  x)",
                               "(A-1 A-2 B-2 B-1 "
                               " A-1 A-2 B-2 B-1)");
}

TEST(exec_proc, dynamic_wind__continuation__exit_and_enter)
{
  test_eval__comp_val_with_obj("(let ((x ()) (k #f))"
                               "  (dynamic-wind"
                               "    (lambda () (set! x (cons 'B-1 x)))"
                               "    (lambda ()"
                               "      (dynamic-wind"
                               "        (lambda () (set! x (cons 'B-2-1 x)))"
                               "        (lambda ()"
                               "          (set! k (call/cc (lambda (kk) kk))))"
                               "        (lambda () (set! x (cons 'A-2-1 x))))"
                               "      (dynamic-wind"
                               "        (lambda () (set! x (cons 'B-2-2 x)))"
                               "        (lambda () (when k (k #f)))"
                               "        (lambda () (set! x (cons 'A-2-2 x)))))"
                               "    (lambda () (set! x (cons 'A-1 x))))"
                               "  x)",
                               "(A-1 A-2-2 B-2-2 A-2-1 B-2-1"
                               "     A-2-2 B-2-2 A-2-1 B-2-1 B-1)");
}

/* 継続の呼び出しにより実行される After thunk の動的環境 (Dynamic Wind) が
   dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__exit_from_dw_AFTER_while_exiting_from_dw_thunk)
{
  test_eval__comp_val_with_obj("(let ((x ()))"
                               "  (call/cc"
                               "    (lambda (k)"
                               "      (dynamic-wind"
                               "        (lambda () (set! x (cons 'B-1 x)))"
                               "        (lambda ()"
                               "          (dynamic-wind"
                               "            (lambda () (set! x (cons 'B-2 x)))"
                               "            (lambda () (k #f))"
                               "            (lambda () "
                               "              (set! x (cons 'A-2 x))"
                               "              (k #f))))"
                               "        (lambda () (set! x (cons 'A-1 x))))))"
                               "  x)",
                               "(A-1 A-2 B-2 B-1)");
}

/* 継続の呼び出しにより実行される Before thunk の動的環境 (Dynamic Wind) が
   dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__exit_from_dw_BEFORE_while_entering_dw)
{
  test_eval__comp_val_with_obj("(let ((x ()) (in #f) (out #f))"
                               "   (when (call/cc"
                               "          (lambda (k)"
                               "            (dynamic-wind"
                               "              (lambda () (set! x (cons 'B-1 x)))"
                               "              (lambda ()"
                               "                (dynamic-wind"
                               "                  (lambda ()"
                               "                    (set! x (cons 'B-2 x))"
                               "                    (when out (out #f)))"
                               "                  (lambda ()"
                               "                    (set! out k)"
                               "                    (set! in (call/cc (lambda (k) k))))"
                               "                  (lambda () (set! x (cons 'A-2 x)))))"
                               "              (lambda () (set! x (cons 'A-1 x))))))"
                               "     (in #f))"
                               "   x)",
                               "(A-1 B-2 B-1 A-1 A-2 B-2 B-1)");
}

/* 継続の呼び出しにより実行される After thunk の動的環境 (Dynamic Bindings)
   が dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__exit_from_parameterize)
{
  test_eval__comp_val_with_obj("(let ((x ()) (prm (make-parameter 1)))"
                               "  (call/cc"
                               "   (lambda (k)"
                               "     (dynamic-wind"
                               "       (lambda () (set! x (cons (prm) x)))"
                               "       (lambda ()"
                               "         (parameterize ((prm 10))"
                               "           (set! x (cons (prm) x))"
                               "           (k #f)))"
                               "       (lambda () (set! x (cons (prm) x))))))"
                               "  x))",
                               "(1 10 1)");
}

/* 継続の呼び出しにより実行される Before thunk の動的環境 (Dynamic Bindings)
   が dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__enter_parameterize)
{
  test_eval__comp_val_with_obj("(let ((x ()) (k #f) (prm (make-parameter 1)))"
                               "  (parameterize ((prm 10))"
                               "    (dynamic-wind"
                               "      (lambda () (set! x (cons (prm) x)))"
                               "      (lambda ()"
                               "        (set! k (call/cc (lambda (k) k)))"
                               "        (set! x (cons (prm) x)))"
                               "      (lambda () (set! x (cons (prm) x)))))"
                               "  (when k (k #f))"
                               "  x)",
                               "(10 10 10 10 10 10)");
}

/* 継続の呼び出しにより実行される After thunk の動的環境 (Exception Handler)
   が dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__exit_from_with_exception_handler)
{
  test_eval__comp_val_with_obj("(call/cc"
                               " (lambda (break)"
                               "   (with-exception-handler"
                               "    (lambda (e) (break (cons 'out e)))"
                               "    (lambda ()"
                               "      (dynamic-wind"
                               "        (lambda ())"
                               "        (lambda ()"
                               "          (with-exception-handler"
                               "           (lambda (e) (break (cons 'inn e)))"
                               "           (lambda () (break #f))))"
                               "        (lambda ()"
                               "          (raise 10)))))))",
                               "(out . 10)");
}

/* 継続の呼び出しにより実行される Before thunk の動的環境 (Exception
   Handler) が dynamic-wind の呼び出しと同じかを確認する */
TEST(exec_proc, dynamic_wind__continuation__enter_with_exception_handler)
{
  test_eval__comp_val_with_obj("(call/cc"
                               " (lambda (break)"
                               "   (let ((k #f))"
                               "     (with-exception-handler"
                               "      (lambda (e) (break (cons 'exc e)))"
                               "      (lambda ()"
                               "        (dynamic-wind"
                               "          (lambda () (when k (raise 10)))"
                               "          (lambda () (set! k (call/cc (lambda (k) k))))"
                               "          (lambda ()))))"
                               "     (when k (k #f)))))",
                               "(exc . 10)");
}

TEST(exec_proc, map__single_list)
{
  test_eval__comp_val_with_obj("(map car '((a b) (c d) (e f)))",
                               "(a c e)");
}

TEST(exec_proc, map__multiple_lists)
{
  test_eval__comp_val_with_obj("(map * '(2 3 5 7) '(11 13 17))",
                               "(22 39 85)");
}

TEST(exec_proc, for_each__single_list)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (for-each (lambda (x) (set! acc (cons x acc)))"
                               "            '(a b c))"
                               "  acc)",
                               "(c b a)");
}

TEST(exec_proc, for_each__multiple_lists)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (for-each (lambda (x y)"
                               "              (set! acc (cons (* x y) acc)))"
                               "            '(2 3 5 7)"
                               "            '(11 13 17))"
                               "  acc)",
                               "(85 39 22)");
}

TEST(exec_proc, string_map__single_string)
{
  test_eval__comp_val_with_obj("(string-map (lambda (c)"
                               "              (integer->char (+ (char->integer c) 1)))"
                               "            \"abcde\")",
                               "\"bcdef\"");
}

TEST(exec_proc, string_map__multiple_strings)
{
  test_eval__comp_val_with_obj("(string-map (lambda (c k)"
                               "              (integer->char"
                               "                (if (eqv? k #\\+)"
                               "                     (+ (char->integer c) 1)"
                               "                     (- (char->integer c) 1))))"
                               "            \"abcde\""
                               "            \"+-+-+-\")",
                               "\"badcf\"");
}

TEST(exec_proc, string_for_each__single_string)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (string-for-each"
                               "    (lambda (c)"
                               "      (set! acc (cons c acc)))"
                               "    \"abcde\")"
                               "  acc)",
                               "(#\\e #\\d #\\c #\\b #\\a)");
}

TEST(exec_proc, string_for_each__multiple_string)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (string-for-each"
                               "    (lambda (c d)"
                               "      (set! acc (cons (cons c d) acc)))"
                               "    \"abc\""
                               "    \"defg\")"
                               "  acc)",
                               "((#\\c . #\\f) (#\\b . #\\e) (#\\a . #\\d))");
}

TEST(exec_proc, vector_map__single_vector)
{
  test_eval__comp_val_with_obj("(vector-map car #((a b) (c d) (e f)))",
                               "#(a c e)");
}

TEST(exec_proc, vector_map__multiple_vectors)
{
  test_eval__comp_val_with_obj("(vector-map * #(2 3 5 7) #(11 13 17))",
                               "#(22 39 85)");
}

TEST(exec_proc, vector_for_each__single_vector)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (vector-for-each"
                               "     (lambda (x) (set! acc (cons x acc)))"
                               "     #(a b c))"
                               "  acc)",
                               "(c b a)");
}

TEST(exec_proc, vector_for_each__multiple_vectors)
{
  test_eval__comp_val_with_obj("(let ((acc '()))"
                               "  (vector-for-each"
                               "    (lambda (x y)"
                               "      (set! acc (cons (* x y) acc)))"
                               "    '#(2 3 5 7)"
                               "    '#(11 13 17))"
                               "  acc)",
                               "(85 39 22)");
}
