#include "scythe/api.h"

#include "test.h"

TEST_GROUP(exec_compiler);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(exec_compiler)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_evaluator_load_core(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(exec_compiler)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static ScmObj
compile(ScmObj exp)
{
  return scm_capi_ut_compile(ev, exp);
}

static ScmObj
compile_cstr(const char *str)
{
  return compile(read_cstr(str));
}

static void
test_compile(const char *expr, const char *asmbl)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = read_cstr(asmbl);
  actual = compile_cstr(expr);

  /* scm_api_write(expected, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */
  /* scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL); */

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(exec_compiler, self_eval_1)
{
  test_compile("1",
               "((immval 1))");
}

TEST(exec_compiler, define_global_variable_1)
{
  test_compile("(define global_var 1)",
               "((immval 1)(gdef global_var (main)))");
}

TEST(exec_compiler, define_global_variable_2)
{
  test_compile("(define (func x) x)",
               "((close 0 1"
               "   ((sref 0 0)(return)))(gdef func (main)))");
}

TEST(exec_compiler, refer_global_variable_1)
{
  test_compile("global_var",
               "((gref global_var (main)))");
}

TEST(exec_compiler, refer_global_variable_2)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) global_var))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((gref global_var (main))(return)))"
               "    (return))))");
}

TEST(exec_compiler, set_global_variable_1)
{
  test_compile("(set! global_var 'a)",
               "((immval a)(gset global_var (main)))");
}

TEST(exec_compiler, quote)
{
  test_compile("'(a b c)",
               "((immval (a b c)))");
}

TEST(exec_compiler, application_1)
{
  test_compile("(func)",
               "(  (cframe (label 0))"
               "   (gref func (main))"
               "   (call 0)"
               "   (nop)"
               " (label 0))");
}

TEST(exec_compiler, application_2)
{
  test_compile("(func 'a 'b)",
               "(  (cframe (label 0))"
               "   (immval a)(push)"
               "   (immval b)(push)"
               "   (gref func (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0))");
}

TEST(exec_compiler, application_3)
{
  test_compile("((lambda (x) x) 1)",
               "(  (cframe (label 0))"
               "   (immval 1)(push)"
               "   (close 0 1"
               "     ((sref 0 0)(return)))"
               "   (call 1)"
               "   (nop)"
               " (label 0))");
}

TEST(exec_compiler, application_4)
{
  test_compile("((lambda (x) (lambda (y) (cons x y))) 1)",
               "(  (cframe (label 0))"
               "   (immval 1)(push)"
               "   (close 0 1"
               "     ((close 1 1"
               "        ((sref 0 1)(push)"
               "         (sref 0 0)(push)"
               "         (gref cons (main))"
               "         (tcall 2)))"
               "      (return)))"
               "   (call 1)"
               "   (nop)"
               " (label 0))");
}

TEST(exec_compiler, application_5)
{
  test_compile("(lambda () ((lambda () 1)))",
               "((close 0 0"
               "   ((close 0 0 ((immval 1)(return)))"
               "    (tcall 0))))");
}

TEST(exec_compiler, lambda_1)
{
  test_compile("(lambda () 'a)",
               "((close 0 0 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_2)
{
  test_compile("(lambda (v1 v2) 'a)",
               "((close 0 2 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_3)
{
  test_compile("(lambda (v1 v2 . v3) 'a)",
               "((close 0 -3 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_4)
{
  test_compile("(lambda v 'a)",
               "((close 0 -1 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_5)
{
  test_compile("(lambda ())",
               "((close 0 0 ((undef)(return))))");
}

TEST(exec_compiler, lambda_6)
{
  test_compile("(lambda () (cons 'a 'b) (cons 'c 'd))",
               "((close 0 0"
               "   (  (cframe (label 0))"
               "      (immval a)(push)"
               "      (immval b)(push)"
               "      (gref cons (main))"
               "      (call 2)"
               "      (nop)"
               "    (label 0)"
               "      (immval c)(push)"
               "      (immval d)(push)"
               "      (gref cons (main))"
               "      (tcall 2))))");
}

TEST(exec_compiler, let_1)
{
  test_compile("(let ((x 1)(y 2)) x)",
               "((immval 1)(push)"
               " (immval 2)(push)"
               " (eframe 2)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_2)
{
  test_compile("(let ())",
               "((undef))");
}

TEST(exec_compiler, let_3)
{
  test_compile("(let ((x 1)(y 2)) (lambda () (cons x y)))",
               "((immval 1)(push)"
               " (immval 2)(push)"
               " (eframe 2)"
               " (close 1 0"
               "   ((sref 0 0)(push)"
               "    (sref 1 0)(push)"
               "    (gref cons (main))"
               "    (tcall 2)))"
               " (epop))");
}

TEST(exec_compiler, let_4)
{
  test_compile("(let ((x 1)(y 2)) (set! y 100))",
               "((immval 1)(push)"
               " (immval 2)(push)"
               " (eframe 2)"
               " (box 1 0)"
               " (immval 100)"
               " (sset 1 0)"
               " (epop))");
}

TEST(exec_compiler, let_5)
{
  test_compile("(lambda () (let ((x 1)(y 2)) x))",
               "((close 0 0"
               "   ((immval 1)(push)"
               "    (immval 2)(push)"
               "    (eframe 2)"
               "    (sref 0 0)"
               "    (return))))");
}

TEST(exec_compiler, let_6)
{
  test_compile("(let ((x 1)) (cons 'a 'b) (cons 'c 'd))",
               "(  (immval 1)(push)"
               "   (eframe 1)"
               "   (cframe (label 0))"
               "   (immval a)(push)"
               "   (immval b)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (cframe (label 1))"
               "   (immval c)(push)"
               "   (immval d)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (epop))");
}

TEST(exec_compiler, named_let_1)
{
  test_compile("(let loop ((x 1)(y 2)) (loop x y))",
               "(  (immval 1)(push)"
               "   (immval 2)(push)"
               "   (eframe 2)"
               "   (emine 1)"
               "   (close 1 2"
               "     ((sref 0 0)(push)"
               "      (sref 1 0)(push)"
               "      (sref 0 1)"
               "      (tcall 2)))"
               "   (demine 0 0)"
               "   (cframe (label 0))"
               "   (sref 0 1)(push)"
               "   (sref 1 1)(push)"
               "   (sref 0 0)"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (epop)"
               "   (epop))");
}

TEST(exec_compiler, named_let_2)
{
  test_compile("(let loop ())",
               "(  (emine 1)"
               "   (close 0 0 ((undef)(return)))"
               "   (demine 0 0)"
               "   (cframe (label 0))"
               "   (sref 0 0)"
               "   (call 0)"
               "   (nop)"
               " (label 0)"
               "   (epop))");
}

TEST(exec_compiler, named_let_3)
{
  test_compile("(lambda () (let loop ((x 1)(y 2)) (loop 3 4)))",
               "((close 0 0"
               "   ((immval 1)(push)"
               "    (immval 2)(push)"
               "    (eframe 2)"
               "    (emine 1)"
               "    (close 1 2"
               "      ((immval 3)(push)"
               "       (immval 4)(push)"
               "       (sref 0 1)"
               "       (tcall 2)))"
               "    (demine 0 0)"
               "    (sref 0 1)(push)"
               "    (sref 1 1)(push)"
               "    (sref 0 0)"
               "    (tcall 2))))");
}

TEST(exec_compiler, let_a_1)
{
  test_compile("(let* ((x 1)(y 2)) x)",
               "((immval 1)(push)"
               " (eframe 1)"
               " (immval 2)(push)"
               " (eframe 1)"
               " (sref 0 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_2)
{
  test_compile("(let* ())",
               "((undef))");
}

TEST(exec_compiler, let_a_3)
{
  test_compile("(let* ((x 1)(y 2)) (lambda () (cons x y)))",
               "((immval 1)(push)"
               " (eframe 1)"
               " (immval 2)(push)"
               " (eframe 1)"
               " (close 2 0"
               "   ((sref 0 1)(push)"
               "    (sref 0 0)(push)"
               "    (gref cons (main))"
               "    (tcall 2)))"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_4)
{
  test_compile("(let* ((x 1)(y 2)) (set! x 100))",
               "((immval 1)(push)"
               " (eframe 1)"
               " (box 0 0)"
               " (immval 2)(push)"
               " (eframe 1)"
               " (immval 100)"
               " (sset 0 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_5)
{
  test_compile("(lambda () (let* ((x 1)(y 2)) x))",
               "((close 0 0"
               "   ((immval 1)(push)"
               "    (eframe 1)"
               "    (immval 2)(push)"
               "    (eframe 1)"
               "    (sref 0 1)"
               "    (return))))");
}

TEST(exec_compiler, let_a_6)
{
  test_compile("(let* ((x 1)(y x)) x)",
               "((immval 1)(push)"
               " (eframe 1)"
               " (sref 0 0)(push)"
               " (eframe 1)"
               " (sref 0 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_7)
{
  test_compile("(let* ((x 1)(y 2)) (cons x y) (cons y x))",
               "(  (immval 1)(push)"
               "   (eframe 1)"
               "   (immval 2)(push)"
               "   (eframe 1)"
               "   (cframe (label 0))"
               "   (sref 0 1)(push)"
               "   (sref 0 0)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (cframe (label 1))"
               "   (sref 0 0)(push)"
               "   (sref 0 1)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (epop)"
               "   (epop))");
}

TEST(exec_compiler, letrec_1)
{
  test_compile("(letrec ((x 1)(y 2)) x)",
               "((emine 2)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (edemine 2 0)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_2)
{
  test_compile("(letrec ((x (lambda () y))(y 100)) x)",
               "((emine 2)"
               " (close 1 0 ((sref 1 0)(return)))(push)"
               " (immval 100)(push)"
               " (edemine 2 0)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_3)
{
  test_compile("(letrec ((x 1)(y 2)) (set! y 10) y)",
               "((emine 2)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (edemine 2 0)"
               " (immval 10)"
               " (sset 1 0)"
               " (sref 1 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_4)
{
  test_compile("(lambda () (letrec ((x 1)(y 2)) x))",
               "((close 0 0"
               "   ((emine 2)"
               "    (immval 1)(push)"
               "    (immval 2)(push)"
               "    (edemine 2 0)"
               "    (sref 0 0)"
               "    (return))))");
}

TEST(exec_compiler, letrec_5)
{
  test_compile("(letrec ())",
               "((undef))");
}

TEST(exec_compiler, letrec_6)
{
  test_compile("(letrec ((x 1)(y 2)) (cons x y) (cons y x))",
               "(  (emine 2)"
               "   (immval 1)(push)"
               "   (immval 2)(push)"
               "   (edemine 2 0)"
               "   (cframe (label 0))"
               "   (sref 0 0)(push)"
               "   (sref 1 0)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (cframe (label 1))"
               "   (sref 1 0)(push)"
               "   (sref 0 0)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (epop))");
}

TEST(exec_compiler, letrec_a_1)
{
  test_compile("(letrec* ((x 1)(y 2)) x)",
               "((emine 2)"
               " (immval 1)(demine 0 0)"
               " (immval 2)(demine 1 0)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_a_2)
{
  test_compile("(letrec* ((x (lambda () y))(y 100)) x)",
               "((emine 2)"
               " (close 1 0 ((sref 1 0)(return)))(demine 0 0)"
               " (immval 100)(demine 1 0)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_a_3)
{
  test_compile("(letrec* ((x 1)(y 2)) (set! y 10) y)",
               "((emine 2)"
               " (immval 1)(demine 0 0)"
               " (immval 2)(demine 1 0)"
               " (immval 10)"
               " (sset 1 0)"
               " (sref 1 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_a_4)
{
  test_compile("(lambda () (letrec* ((x 1)(y 2)) x))",
               "((close 0 0"
               "   ((emine 2)"
               "    (immval 1)(demine 0 0)"
               "    (immval 2)(demine 1 0)"
               "    (sref 0 0)"
               "    (return))))");
}

TEST(exec_compiler, letrec_a_5)
{
  test_compile("(letrec* ())",
               "((undef))");
}

TEST(exec_compiler, letrec_a_6)
{
  test_compile("(letrec* ((x 1)(y 2)) (cons x y) (cons y x))",
               "(  (emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (cframe (label 0))"
               "   (sref 0 0)(push)"
               "   (sref 1 0)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (cframe (label 1))"
               "   (sref 1 0)(push)"
               "   (sref 0 0)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (epop))");
}

TEST(exec_compiler, internal_definition_1)
{
  test_compile("(lambda () (define x 1) (define y 2) x)",
               "((close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_2)
{
  test_compile("(lambda () (define x (lambda () y)) (define y 100) x)",
               "((close 0 0"
               "  ((emine 2)"
               "   (close 1 0 ((sref 1 0)(return)))(demine 0 0)"
               "   (immval 100)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_3)
{
  test_compile("(lambda () (define x 1) (define y 2) (set! y 10) y)",
               "((close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (immval 10)"
               "   (sset 1 0)"
               "   (sref 1 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_4)
{
  test_compile("(lambda ()"
               "  (define x 1)"
               "  (begin"
               "    (define y 2)"
               "    x))",
               "((close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_5)
{
  test_compile("(lambda ()"
               "  (define x 1)"
               "  (begin"
               "    (define y 2))"
               "  x)",
               "((close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, begin_1)
{
  test_compile("(begin (cons 'a 'b) (cons 'x 'y))",
               "(  (cframe (label 0))"
               "   (immval a)(push)"
               "   (immval b)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (cframe (label 1))"
               "   (immval x)(push)"
               "   (immval y)(push)"
               "   (gref cons (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1))");
}

TEST(exec_compiler, begin_2)
{
  test_compile("(begin (define gvar 1))",
               "((immval 1)"
               " (gdef gvar (main)))");
}

TEST(exec_compiler, begin_3)
{
  test_compile("(begin)",
               "((undef))");
}

TEST(exec_compiler, begin_4)
{
  test_compile("(lambda ()"
               "  (begin (cons 'a 'b)))",
               "((close 0 0"
               "   ((immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (tcall 2))))");
}

TEST(exec_compiler, refer_bound_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) b2))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((sref 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_bound_variable_2)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) b2))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((sref 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_bound_variable_3)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a) b2))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((box 1 0)"
               "       (immval a)(sset 1 0)"
               "       (sref 1 0)"
               "       (return)))"
               "    (return))))");
}

TEST(exec_compiler, set_bound_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((box 1 0)(immval a)(sset 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, set_bound_variable_2)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a)))",
               "((close 0 2"
               "   ((close 0 2"
               "      ((box 1 0)(immval a)(sset 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_free_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) f2))",
               "((close 0 2"
               "   ((close 1 2"
               "      ((sref 1 1)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_free_variable_2)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a) f2))",
               "((close 0 2"
               "   ((box 1 0)"
               "    (close 1 2"
               "      ((immval a)(sset 1 1)"
               "       (sref 1 1)"
               "       (return)))"
               "    (return))))");
}

TEST(exec_compiler, set_free_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a)))",
               "((close 0 2"
               "   ((box 1 0)"
               "    (close 1 2"
               "      ((immval a)(sset 1 1)(return)))"
               "    (return))))");
}

TEST(exec_compiler, if_1)
{
  test_compile("(if 'a 'b 'c)",
               "(  (immval a)"
               "   (jmpf (label 1))"
               "   (immval b)"
               "   (jmp (label 0))"
               " (label 1)"
               "   (immval c)"
               " (label 0)"
               ")");
}

TEST(exec_compiler, if_2)
{
  test_compile("(if 'a 'b)",
               "(  (immval a)"
               "   (jmpf (label 1))"
               "   (immval b)"
               "   (jmp (label 0))"
               " (label 1)"
               "   (undef)"
               " (label 0)"
               ")");
}

TEST(exec_compiler, if_3)
{
  test_compile("(lambda () (if 'a 'b 'c))",
               "((close 0 0"
               "   (  (immval a)"
               "      (jmpf (label 0))"
               "      (immval b)"
               "      (return)"
               "    (label 0)"
               "      (immval c)"
               "      (return)"
               "   )"
               " ))");
}

TEST(exec_compiler, if_4)
{
  test_compile("(lambda () (if 'a 'b))",
               "((close 0 0"
               "   (  (immval a)"
               "      (jmpf (label 0))"
               "      (immval b)"
               "      (return)"
               "    (label 0)"
               "      (undef)"
               "      (return)"
               "   )"
               " ))");
}

TEST(exec_compiler, cond_001)
{
  test_compile("(cond)",
               "((undef)"
               " (label 0))");
}

TEST(exec_compiler, cond_002)
{
  test_compile("(cond (else))",
               "((undef)(label 0))");
}

TEST(exec_compiler, cond_003)
{
  test_compile("(cond (else 'a))",
               "(   (immval a)"
               " (label 0))");
}

TEST(exec_compiler, cond_004)
{
  test_compile("(cond ('a 'b))",
               "(   (immval a)"
               "    (jmpt (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (immval b)"
               " (label 0))");
}

TEST(exec_compiler, cond_005)
{
  test_compile("(cond ('a 'b)('c 'd))",
               "(   (immval a)"
               "    (jmpt (label 1))"
               "    (immval c)"
               "    (jmpt (label 2))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 2)"
               "    (immval d)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (immval b)"
               " (label 0))");
}

TEST(exec_compiler, cond_006)
{
  test_compile("(cond ('a => write))",
               "(   (immval a)"
               "    (jmpt (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (cframe (label 2))"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               " (label 2)"
               " (label 0))");
}

TEST(exec_compiler, cond_007)
{
  test_compile("(cond ('a => write)('b => write))",
               "(   (immval a)"
               "    (jmpt (label 1))"
               "    (immval b)"
               "    (jmpt (label 2))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 2)"
               "    (cframe (label 3))"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               " (label 3)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (cframe (label 4))"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               " (label 4)"
               " (label 0))");
}

TEST(exec_compiler, cond_008)
{
  test_compile("(cond ('a))",
               "(   (immval a)"
               "    (jmpt (label 0))"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, cond_009)
{
  test_compile("(cond ('a)('b 'c))",
               "(   (immval a)"
               "    (jmpt (label 0))"
               "    (immval b)"
               "    (jmpt (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (immval c)"
               " (label 0))");
}

TEST(exec_compiler, cond_010)
{
  test_compile("(cond ('a 'b)('c))",
               "(   (immval a)"
               "    (jmpt (label 1))"
               "    (immval c)"
               "    (jmpt (label 0))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (immval b)"
               " (label 0))");
}

TEST(exec_compiler, cond_011)
{
  test_compile("(lambda () (cond))",
               "((close 0 0"
               "   ((undef)(return))))");
}

TEST(exec_compiler, cond_012)
{
  test_compile("(lambda () (cond (else)))",
               "((close 0 0"
               "   ((undef)(return))))");
}

TEST(exec_compiler, cond_013)
{
  test_compile("(lambda () (cond (else 'a)))",
               "((close 0 0"
               "   ((immval a)"
               "    (return))))");
}

TEST(exec_compiler, cond_014)
{
  test_compile("(lambda () (cond ('a 'b)))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (immval b)"
               "       (return))))");
}

TEST(exec_compiler, cond_015)
{
  test_compile("(lambda () (cond ('a)))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (return))))");
}

TEST(exec_compiler, cond_016)
{
  test_compile("(lambda () (cond ('a)('b 'c)))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (immval b)"
               "       (jmpt (label 1))"
               "       (undef)"
               "       (return)"
               "    (label 1)"
               "       (immval c)"
               "       (return)"
               "    (label 0)"
               "       (return))))");
}

TEST(exec_compiler, cond_017)
{
  test_compile("(lambda () (cond ('a 'b)('c)))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (immval c)"
               "       (jmpt (label 1))"
               "       (undef)"
               "       (return)"
               "    (label 1)"
               "       (return)"
               "    (label 0)"
               "       (immval b)"
               "       (return))))");
}

TEST(exec_compiler, cond_018)
{
  test_compile("(lambda () (cond ('a => write)))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (push)"
               "       (gref write (main))"
               "       (tcall 1))))");
}

TEST(exec_compiler, and_001)
{
  test_compile("(and)",
               "((immval #t))");
}

TEST(exec_compiler, and_002)
{
  test_compile("(and 'a)",
               "((immval a))");
}

TEST(exec_compiler, and_003)
{
  test_compile("(and 'a 'b)",
               "(  (immval a)"
               "   (jmpf (label 0))"
               "   (immval b)"
               " (label 0))");
}

TEST(exec_compiler, and_004)
{
  test_compile("(and (null? 'a) (null? 'b))",
               "(  (cframe (label 1))"
               "   (immval a)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label 1)"
               "   (jmpf (label 0))"
               "   (cframe (label 2))"
               "   (immval b)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label 2)"
               " (label 0))");
}

TEST(exec_compiler, and_005)
{
  test_compile("(lambda () (and))",
               "((close 0 0"
               "   ((immval #t)(return))))");
}

TEST(exec_compiler, and_006)
{
  test_compile("(lambda () (and 'a))",
               "((close 0 0"
               "   ((immval a)(return))))");
}

TEST(exec_compiler, and_007)
{
  test_compile("(lambda () (and 'a 'b))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpf (label 0))"
               "       (immval b)"
               "       (return)"
               "    (label 0)"
               "    (return))))");
}

TEST(exec_compiler, and_008)
{
  test_compile("(lambda () (and (null? 'a) (null? 'b)))",
               "((close 0 0"
               "   (   (cframe (label 1))"
               "       (immval a)"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "    (label 1)"
               "       (jmpf (label 0))"
               "       (immval b)"
               "       (push)"
               "       (gref null? (main))"
               "       (tcall 1)"
               "    (label 0)"
               "       (return))))");
}

TEST(exec_compiler, or_001)
{
  test_compile("(or)",
               "((immval #f))");
}

TEST(exec_compiler, or_002)
{
  test_compile("(or 'a)",
               "((immval a))");
}

TEST(exec_compiler, or_003)
{
  test_compile("(or 'a 'b)",
               "(  (immval a)"
               "   (jmpt (label 0))"
               "   (immval b)"
               " (label 0))");
}

TEST(exec_compiler, or_004)
{
  test_compile("(or (null? 'a) (null? 'b))",
               "(  (cframe (label 1))"
               "   (immval a)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label 1)"
               "   (jmpt (label 0))"
               "   (cframe (label 2))"
               "   (immval b)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label 2)"
               " (label 0))");
}

TEST(exec_compiler, or_005)
{
  test_compile("(lambda () (or))",
               "((close 0 0"
               "   ((immval #f)(return))))");
}

TEST(exec_compiler, or_006)
{
  test_compile("(lambda () (or 'a))",
               "((close 0 0"
               "   ((immval a)(return))))");
}

TEST(exec_compiler, or_007)
{
  test_compile("(lambda () (or 'a 'b))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpt (label 0))"
               "       (immval b)"
               "       (return)"
               "    (label 0)"
               "    (return))))");
}


TEST(exec_compiler, or_008)
{
  test_compile("(lambda () (or (null? 'a) (null? 'b)))",
               "((close 0 0"
               "   (   (cframe (label 1))"
               "       (immval a)"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "    (label 1)"
               "       (jmpt (label 0))"
               "       (immval b)"
               "       (push)"
               "       (gref null? (main))"
               "       (tcall 1)"
               "    (label 0)"
               "       (return))))");
}

TEST(exec_compiler, when_001)
{
  test_compile("(when 'a)",
               "(   (immval a)"
               "    (jmpf (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, when_002)
{
  test_compile("(when 'a 'b)",
               "(   (immval a)"
               "    (jmpf (label 1))"
               "    (immval b)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, when_003)
{
  test_compile("(lambda () (when 'a))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpf (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, when_004)
{

  test_compile("(lambda () (when 'a 'b))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpf (label 0))"
               "       (immval b)"
               "       (return)"
               "    (label 0)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, when_005)
{
  test_compile("(when (null? '()) (cons 'a 'b))",
               "(   (cframe (label 2))"
               "    (immval ())"
               "    (push)"
               "    (gref null? (main))"
               "    (call 1)"
               "    (mrve)"
               " (label 2)"
               "    (jmpf (label 1))"
               "    (cframe (label 3))"
               "    (immval a)"
               "    (push)"
               "    (immval b)"
               "    (push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 3)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, when_006)
{
  test_compile("(lambda () (when (null? '()) (cons 'a 'b)))",
               "((close 0 0"
               "   (   (cframe (label 1))"
               "       (immval ())"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "    (label 1)"
               "       (jmpf (label 0))"
               "       (immval a)"
               "       (push)"
               "       (immval b)"
               "       (push)"
               "       (gref cons (main))"
               "       (tcall 2)"
               "    (label 0)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, when_007)
{
  test_compile("(when 'z (cons 'a 'b) (cons 'c 'd))",
               "(   (immval z)"
               "    (jmpf (label 1))"
               "    (cframe (label 2))"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 2)"
               "    (cframe (label 3))"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 3)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, unless_001)
{
  test_compile("(unless 'a)",
               "(   (immval a)"
               "    (jmpf (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (undef)"
               " (label 0))");
}

TEST(exec_compiler, unless_002)
{
  test_compile("(unless 'a 'b)",
               "(   (immval a)"
               "    (jmpf (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (immval b)"
               " (label 0))");
}

TEST(exec_compiler, unless_003)
{
  test_compile("(lambda () (unless 'a))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpf (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, unless_004)
{
  test_compile("(lambda () (unless 'a 'b))",
               "((close 0 0"
               "   (   (immval a)"
               "       (jmpf (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (immval b)"
               "       (return))))");
}

TEST(exec_compiler, unless_005)
{
  test_compile("(unless (null? '()) (cons 'a 'b))",
               "(   (cframe (label 2))"
               "    (immval ())"
               "    (push)"
               "    (gref null? (main))"
               "    (call 1)"
               "    (mrve)"
               " (label 2)"
               "    (jmpf (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (cframe (label 3))"
               "    (immval a)"
               "    (push)"
               "    (immval b)"
               "    (push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 3)"
               " (label 0))");
}

TEST(exec_compiler, unless_006)
{
  test_compile("(lambda () (unless (null? '()) (cons 'a 'b)))",
               "((close 0 0"
               "   (   (cframe (label 1))"
               "       (immval ())"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "    (label 1)"
               "       (jmpf (label 0))"
               "       (undef)"
               "       (return)"
               "    (label 0)"
               "       (immval a)"
               "       (push)"
               "       (immval b)"
               "       (push)"
               "       (gref cons (main))"
               "       (tcall 2))))");
}

TEST(exec_compiler, unless_007)
{
  test_compile("(unless 'z (cons 'a 'b) (cons 'c 'd))",
               "(   (immval z)"
               "    (jmpf (label 1))"
               "    (undef)"
               "    (jmp (label 0))"
               " (label 1)"
               "    (cframe (label 2))"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 2)"
               "    (cframe (label 3))"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 3)"
               " (label 0))");
}

TEST(exec_compiler, do_001)
{
  test_compile("(do ((x 'ix 'sx)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  'c)",
               "(   (immval ix)"
               "    (push)"
               "    (immval iy)"
               "    (push)"
               "    (eframe 2)"
               " (label 1)"
               "    (immval t)"
               "    (jmpt (label 0))"
               "    (immval c)"
               "    (immval sx)"
               "    (push)"
               "    (immval sy)"
               "    (push)"
               "    (eframe 2)"
               "    (eshift 1)"
               "    (jmp (label 1))"
               " (label 0)"
               "    (immval e)"
               "    (epop))");
}

TEST(exec_compiler, do_002)
{
  test_compile("(do ((x 'ix)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  'c)",
               "(   (immval ix)"
               "    (push)"
               "    (immval iy)"
               "    (push)"
               "    (eframe 2)"
               " (label 1)"
               "    (immval t)"
               "    (jmpt (label 0))"
               "    (immval c)"
               "    (sref 0 0)"
               "    (push)"
               "    (immval sy)"
               "    (push)"
               "    (eframe 2)"
               "    (eshift 1)"
               "    (jmp (label 1))"
               " (label 0)"
               "    (immval e)"
               "    (epop))");
}

TEST(exec_compiler, do_003)
{
  test_compile("(do ((x 'ix 'sx)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  (set! y 1))",
               "(   (immval ix)"
               "    (push)"
               "    (immval iy)"
               "    (push)"
               "    (eframe 2)"
               " (label 1)"
               "    (box 1 0)"
               "    (immval t)"
               "    (jmpt (label 0))"
               "    (immval 1)"
               "    (sset 1 0)"
               "    (immval sx)"
               "    (push)"
               "    (immval sy)"
               "    (push)"
               "    (eframe 2)"
               "    (eshift 1)"
               "    (jmp (label 1))"
               " (label 0)"
               "    (immval e)"
               "    (epop))");
}

TEST(exec_compiler, do_004)
{
  test_compile("(do ()"
               "    ('t)"
               "  )",
               "((label 1)"
               "    (immval t)"
               "    (jmpt (label 0))"
               "    (jmp (label 1))"
               " (label 0)"
               "    (undef))");
}

TEST(exec_compiler, do_005)
{
  test_compile("(lambda ()"
               "  (do ((x 'ix 'sx)"
               "       (y 'iy 'sy))"
               "      ('t 'e)"
               "    'c))",
               "((close 0 0"
               "   (  (immval ix)"
               "      (push)"
               "      (immval iy)"
               "      (push)"
               "      (eframe 2)"
               "   (label 1)"
               "      (immval t)"
               "      (jmpt (label 0))"
               "      (immval c)"
               "      (immval sx)"
               "      (push)"
               "      (immval sy)"
               "      (push)"
               "      (eframe 2)"
               "      (eshift 1)"
               "      (jmp (label 1))"
               "   (label 0)"
               "      (immval e)"
               "      (return))))");
}

TEST(exec_compiler, do_006)
{
  test_compile("(lambda ()"
               "  (do ((x 'ix 'sx)"
               "       (y 'iy 'sy))"
               "      ('t (cons 'a 'b))"
               "    'c))",
               "((close 0 0"
               "   (  (immval ix)"
               "      (push)"
               "      (immval iy)"
               "      (push)"
               "      (eframe 2)"
               "   (label 1)"
               "      (immval t)"
               "      (jmpt (label 0))"
               "      (immval c)"
               "      (immval sx)"
               "      (push)"
               "      (immval sy)"
               "      (push)"
               "      (eframe 2)"
               "      (eshift 1)"
               "      (jmp (label 1))"
               "   (label 0)"
               "      (immval a)"
               "      (push)"
               "      (immval b)"
               "      (push)"
               "      (gref cons (main))"
               "      (tcall 2))))");
}

TEST(exec_compiler, do_007)
{
  test_compile("(lambda ()"
               "  (do ()"
               "      ('t)"
               "    ))",
               "((close 0 0"
               "   ((label 1)"
               "       (immval t)"
               "       (jmpt (label 0))"
               "       (jmp (label 1))"
               "    (label 0)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, do_008)
{
  test_compile("(do ((x 'ix 'sx)"
               "     (y 'iy 'sy))"
               "    ('t (cons 'a 'b) (cons 'c 'd))"
               "  (cons 'e 'f)"
               "  (cons 'g 'h))",
               "(   (immval ix)"
               "    (push)"
               "    (immval iy)"
               "    (push)"
               "    (eframe 2)"
               " (label 1)"
               "    (immval t)"
               "    (jmpt (label 0))"
               "    (cframe (label 2))"
               "    (immval e)(push)"
               "    (immval f)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 2)"
               "    (cframe (label 3))"
               "    (immval g)(push)"
               "    (immval h)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 3)"
               "    (immval sx)"
               "    (push)"
               "    (immval sy)"
               "    (push)"
               "    (eframe 2)"
               "    (eshift 1)"
               "    (jmp (label 1))"
               " (label 0)"
               "    (cframe (label 4))"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 4)"
               "    (cframe (label 5))"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               " (label 5)"
               "    (epop))");
}

TEST(exec_compiler, let_values_1)
{
  test_compile("(let-values (((a) 1)) a)",
               "((immval 1)"
               " (mvpush)"
               " (eframe 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_2)
{
  test_compile("(let-values ((a 1)) a)",
               "((immval 1)"
               " (mrvc -1)"
               " (mvpush)"
               " (eframe 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_3)
{
  test_compile("(let-values (((a b) (values 1 2))) b)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (sref 1 0)"
               "   (epop))");
}

TEST(exec_compiler, let_values_4)
{
  test_compile("(let-values (((a b . c) (values 1 2 3 4))) c)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (immval 3) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 4)"
               "   (nop)"
               " (label 0)"
               "   (mrvc -3)"
               "   (mvpush)"
               "   (eframe 3)"
               "   (sref 2 0)"
               "   (epop))");
}

TEST(exec_compiler, let_values_5)
{
  test_compile("(let-values ((() (values))))",
               "(  (cframe (label 0))"
               "   (gref values (main))"
               "   (call 0)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 0)"
               "   (undef))");
}

TEST(exec_compiler, let_values_6)
{
  test_compile("(let-values (((a b) (values 1 2))"
               "             ((c d) (values 3 4)))"
               "  c)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (cframe (label 1))"
               "   (immval 3) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 4)"
               "   (sref 2 0)"
               "   (epop))");
}

TEST(exec_compiler, let_values_7)
{
  test_compile("(let-values (((a b) (values 1 2))"
               "             (() (values)))"
               "  a)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (cframe (label 1))"
               "   (gref values (main))"
               "   (call 0)"
               "   (nop)"
               " (label 1)"
               "   (mrvc 0)"
               "   (eframe 2)"
               "   (sref 0 0)"
               "   (epop))");
}

TEST(exec_compiler, let_values_8)
{
  test_compile("(let-values (((a b) (values 1 2)))"
               "  (set! a 100))",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (box 0 0)"
               "   (immval 100)"
               "   (sset 0 0)"
               "   (epop))");
}

TEST(exec_compiler, let_values_9)
{
  test_compile("(lambda () (let-values (((a) 1)) (a)))",
               "((close 0 0"
               "   ((immval 1)"
               "    (mvpush)"
               "    (eframe 1)"
               "    (sref 0 0)"
               "    (tcall 0))))");
}

TEST(exec_compiler, let_a_values_1)
{
  test_compile("(let*-values (((a) 1)) a)",
               "((immval 1)"
               " (mvpush)"
               " (eframe 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_2)
{
  test_compile("(let*-values ((a 1)) a)",
               "((immval 1)"
               " (mrvc -1)"
               " (mvpush)"
               " (eframe 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_3)
{
  test_compile("(let*-values (((a b) (values 1 2))) b)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (sref 1 0)"
               "   (epop))");
}

TEST(exec_compiler, let_a_values_4)
{
  test_compile("(let*-values (((a b . c) (values 1 2 3 4))) c)",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (immval 3) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 4)"
               "   (nop)"
               " (label 0)"
               "   (mrvc -3)"
               "   (mvpush)"
               "   (eframe 3)"
               "   (sref 2 0)"
               "   (epop))");
}

TEST(exec_compiler, let_a_values_5)
{
  test_compile("(let*-values ((() (values))))",
               "(  (cframe (label 0))"
               "   (gref values (main))"
               "   (call 0)"
               "   (nop)"
               "   (label 0)"
               "   (mrvc 0)"
               "   (undef))");
}

TEST(exec_compiler, let_a_values_6)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values 3 4)))"
               "  (list a b c d))",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (cframe (label 1))"
               "   (immval 3) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (cframe (label 2))"
               "   (sref 0 1) (push)"
               "   (sref 1 1) (push)"
               "   (sref 0 0) (push)"
               "   (sref 1 0) (push)"
               "   (gref list (main))"
               "   (call 4)"
               "   (nop)"
               " (label 2)"
               "   (epop)"
               "   (epop))");
}

TEST(exec_compiler, let_a_values_7)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values a 4)))"
               "  (list a b c d))",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (cframe (label 1))"
               "   (sref 0 0) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (cframe (label 2))"
               "   (sref 0 1) (push)"
               "   (sref 1 1) (push)"
               "   (sref 0 0) (push)"
               "   (sref 1 0) (push)"
               "   (gref list (main))"
               "   (call 4)"
               "   (nop)"
               " (label 2)"
               "   (epop)"
               "   (epop))");
}

TEST(exec_compiler, let_a_values_8)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values 3 4)))"
               "  (set! b 100))",
               "(  (cframe (label 0))"
               "   (immval 1) (push)"
               "   (immval 2) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 0)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (box 1 0)"
               "   (cframe (label 1))"
               "   (immval 3) (push)"
               "   (immval 4) (push)"
               "   (gref values (main))"
               "   (call 2)"
               "   (nop)"
               " (label 1)"
               "   (mrvc 2)"
               "   (mvpush)"
               "   (eframe 2)"
               "   (immval 100)"
               "   (sset 1 1)"
               "   (epop)"
               "   (epop))");
}

TEST(exec_compiler, let_a_values_9)
{
  test_compile("(lambda () (let*-values (((a) 1)) (a)))",
               "((close 0 0"
               "   ((immval 1)"
               "    (mvpush)"
               "    (eframe 1)"
               "    (sref 0 0)"
               "    (tcall 0))))");
}
