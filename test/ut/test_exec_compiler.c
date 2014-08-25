#include "api.h"

#include "test.h"

TEST_GROUP(exec_compiler);

static ScmEvaluator *ev;

TEST_SETUP(exec_compiler)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_evaluator_load_core(ev);
}

TEST_TEAR_DOWN(exec_compiler)
{
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

  SCM_STACK_FRAME_PUSH(&actual, &expected);

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
               "((asm-close 0 1"
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
               "((asm-close 0 2"
               "   ((asm-close 0 2"
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
               "((cframe)(gref func (main))(call 0)(nop))");
}

TEST(exec_compiler, application_2)
{
  test_compile("(func 'a 'b)",
               "((frame)"
               " (immval a)(push)"
               " (immval b)(push)"
               " (gref func (main))"
               " (call 2)"
               " (nop))");
}

TEST(exec_compiler, application_3)
{
  test_compile("((lambda (x) x) 1)",
               "((frame)"
               " (immval 1)(push)"
               " (asm-close 0 1"
               "   ((sref 0 0)(return)))"
               " (call 1)"
               " (nop))");
}

TEST(exec_compiler, application_4)
{
  test_compile("((lambda (x) (lambda (y) (cons x y))) 1)",
               "((frame)"
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
               " (nop))");
}

TEST(exec_compiler, application_5)
{
  test_compile("(lambda () ((lambda () 1)))",
               "((asm-close 0 0"
               "   ((asm-close 0 0 ((immval 1)(return)))"
               "    (tcall 0))))");
}

TEST(exec_compiler, lambda_1)
{
  test_compile("(lambda () 'a)",
               "((asm-close 0 0 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_2)
{
  test_compile("(lambda (v1 v2) 'a)",
               "((asm-close 0 2 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_3)
{
  test_compile("(lambda (v1 v2 . v3) 'a)",
               "((asm-close 0 -3 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_4)
{
  test_compile("(lambda v 'a)",
               "((asm-close 0 -1 ((immval a)(return))))");
}

TEST(exec_compiler, lambda_5)
{
  test_compile("(lambda ())",
               "((asm-close 0 0 ((undef)(return))))");
}

TEST(exec_compiler, lambda_6)
{
  test_compile("(lambda () (cons 'a 'b) (cons 'c 'd))",
               "((asm-close 0 0"
               "   ((frame)"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (eframe)"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (tcall 2))))");
}

TEST(exec_compiler, let_1)
{
  test_compile("(let ((x 1)(y 2)) x)",
               "((eframe)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (ecommit 2)"
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
               "((eframe)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (ecommit 2)"
               " (asm-close 1 0"
               "   ((eframe)"
               "    (sref 0 0)(push)"
               "    (sref 1 0)(push)"
               "    (gref cons (main))"
               "    (tcall 2)))"
               " (epop))");
}

TEST(exec_compiler, let_4)
{
  test_compile("(let ((x 1)(y 2)) (set! y 100))",
               "((eframe)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (ecommit 2)"
               " (box 1 0)"
               " (immval 100)"
               " (sset 1 0)"
               " (epop))");
}

TEST(exec_compiler, let_5)
{
  test_compile("(lambda () (let ((x 1)(y 2)) x))",
               "((asm-close 0 0"
               "   ((eframe)"
               "    (immval 1)(push)"
               "    (immval 2)(push)"
               "    (ecommit 2)"
               "    (sref 0 0)"
               "    (return))))");
}

TEST(exec_compiler, let_6)
{
  test_compile("(let ((x 1)) (cons 'a 'b) (cons 'c 'd))",
               "((eframe)"
               " (immval 1)(push)"
               " (ecommit 1)"
               " (frame)"
               " (immval a)(push)"
               " (immval b)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (frame)"
               " (immval c)(push)"
               " (immval d)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (epop))");
}

TEST(exec_compiler, named_let_1)
{
  test_compile("(let loop ((x 1)(y 2)) (loop x y))",
               "((eframe)"
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
               " (nop)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, named_let_2)
{
  test_compile("(let loop ())",
               "((emine 1)"
               " (asm-close 0 0 ((undef)(return)))"
               " (demine 0 0)"
               " (cframe)"
               " (sref 0 0)"
               " (call 0)"
               " (nop)"
               " (epop))");
}

TEST(exec_compiler, named_let_3)
{
  test_compile("(lambda () (let loop ((x 1)(y 2)) (loop 3 4)))",
               "((asm-close 0 0"
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
               "    (tcall 2))))");
}

TEST(exec_compiler, let_a_1)
{
  test_compile("(let* ((x 1)(y 2)) x)",
               "((eframe)"
               " (immval 1)(push)"
               " (ecommit 1)"
               " (eframe)"
               " (immval 2)(push)"
               " (ecommit 1)"
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
               "((eframe)"
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
               " (epop))");
}

TEST(exec_compiler, let_a_4)
{
  test_compile("(let* ((x 1)(y 2)) (set! x 100))",
               "((eframe)"
               " (immval 1)(push)"
               " (ecommit 1)"
               " (box 0 0)"
               " (eframe)"
               " (immval 2)(push)"
               " (ecommit 1)"
               " (immval 100)"
               " (sset 0 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_5)
{
  test_compile("(lambda () (let* ((x 1)(y 2)) x))",
               "((asm-close 0 0"
               "   ((eframe)"
               "    (immval 1)(push)"
               "    (ecommit 1)"
               "    (eframe)"
               "    (immval 2)(push)"
               "    (ecommit 1)"
               "    (sref 0 1)"
               "    (return))))");
}

TEST(exec_compiler, let_a_6)
{
  test_compile("(let* ((x 1)(y x)) x)",
               "((eframe)"
               " (immval 1)(push)"
               " (ecommit 1)"
               " (eframe)"
               " (sref 0 0)(push)"
               " (ecommit 1)"
               " (sref 0 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_7)
{
  test_compile("(let* ((x 1)(y 2)) (cons x y) (cons y x))",
               "((eframe)"
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
               " (nop)"
               " (frame)"
               " (sref 0 0)(push)"
               " (sref 0 1)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, letrec_1)
{
  test_compile("(letrec ((x 1)(y 2)) x)",
               "((emine 2)"
               " (eframe)"
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
               " (eframe)"
               " (asm-close 1 0 ((sref 1 0)(return)))(push)"
               " (immval 100)(push)"
               " (edemine 2 0)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, letrec_3)
{
  test_compile("(letrec ((x 1)(y 2)) (set! y 10) y)",
               "((emine 2)"
               " (eframe)"
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
               "((asm-close 0 0"
               "   ((emine 2)"
               "    (eframe)"
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
               "((emine 2)"
               " (eframe)"
               " (immval 1)(push)"
               " (immval 2)(push)"
               " (edemine 2 0)"
               " (frame)"
               " (sref 0 0)(push)"
               " (sref 1 0)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (frame)"
               " (sref 1 0)(push)"
               " (sref 0 0)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (epop))");
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
               " (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)"
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
               "((asm-close 0 0"
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
               "((emine 2)"
               " (immval 1)(demine 0 0)"
               " (immval 2)(demine 1 0)"
               " (frame)"
               " (sref 0 0)(push)"
               " (sref 1 0)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (frame)"
               " (sref 1 0)(push)"
               " (sref 0 0)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (epop))");
}

TEST(exec_compiler, internal_definition_1)
{
  test_compile("(lambda () (define x 1) (define y 2) x)",
               "((asm-close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_2)
{
  test_compile("(lambda () (define x (lambda () y)) (define y 100) x)",
               "((asm-close 0 0"
               "  ((emine 2)"
               "   (asm-close 1 0 ((sref 1 0)(return)))(demine 0 0)"
               "   (immval 100)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, internal_definition_3)
{
  test_compile("(lambda () (define x 1) (define y 2) (set! y 10) y)",
               "((asm-close 0 0"
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
               "((asm-close 0 0"
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
               "((asm-close 0 0"
               "  ((emine 2)"
               "   (immval 1)(demine 0 0)"
               "   (immval 2)(demine 1 0)"
               "   (sref 0 0)"
               "   (return))))");
}

TEST(exec_compiler, begin_1)
{
  test_compile("(begin (cons 'a 'b) (cons 'x 'y))",
               "((frame)"
               " (immval a)(push)"
               " (immval b)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop)"
               " (frame)"
               " (immval x)(push)"
               " (immval y)(push)"
               " (gref cons (main))"
               " (call 2)"
               " (nop))");
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
               "((asm-close 0 0"
               "   ((eframe)"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (tcall 2))))");
}

TEST(exec_compiler, refer_bound_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) b2))",
               "((asm-close 0 2"
               "   ((asm-close 0 2"
               "      ((sref 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_bound_variable_2)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) b2))",
               "((asm-close 0 2"
               "   ((asm-close 0 2"
               "      ((sref 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_bound_variable_3)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a) b2))",
               "((asm-close 0 2"
               "   ((asm-close 0 2"
               "      ((box 1 0)"
               "       (immval a)(sset 1 0)"
               "       (sref 1 0)"
               "       (return)))"
               "    (return))))");
}

TEST(exec_compiler, set_bound_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! b2 'a)))",
               "((asm-close 0 2"
               "   ((asm-close 0 2"
               "      ((box 1 0)(immval a)(sset 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, set_bound_variable_2)
{
  test_compile("(lambda (f1 b2) (lambda (b1 b2) (set! b2 'a)))",
               "((asm-close 0 2"
               "   ((asm-close 0 2"
               "      ((box 1 0)(immval a)(sset 1 0)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_free_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) f2))",
               "((asm-close 0 2"
               "   ((asm-close 1 2"
               "      ((sref 1 1)(return)))"
               "    (return))))");
}

TEST(exec_compiler, refer_free_variable_2)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a) f2))",
               "((asm-close 0 2"
               "   ((box 1 0)"
               "    (asm-close 1 2"
               "      ((immval a)(sset 1 1)"
               "       (sref 1 1)"
               "       (return)))"
               "    (return))))");
}

TEST(exec_compiler, set_free_variable_1)
{
  test_compile("(lambda (f1 f2) (lambda (b1 b2) (set! f2 'a)))",
               "((asm-close 0 2"
               "   ((box 1 0)"
               "    (asm-close 1 2"
               "      ((immval a)(sset 1 1)(return)))"
               "    (return))))");
}


TEST(exec_compiler, if_1)
{
  test_compile("(if 'a 'b 'c)",
               "(  (immval a)"
               "   (jmpf lbl_if-a_1)"
               "   (immval b)"
               "   (jmp lbl_if-j_0)"
               " (label lbl_if-a_1)"
               "   (immval c)"
               " (label lbl_if-j_0)"
               ")");
}


TEST(exec_compiler, if_2)
{
  test_compile("(if 'a 'b)",
               "(  (immval a)"
               "   (jmpf lbl_if-a_1)"
               "   (immval b)"
               "   (jmp lbl_if-j_0)"
               " (label lbl_if-a_1)"
               "   (undef)"
               " (label lbl_if-j_0)"
               ")");
}

TEST(exec_compiler, if_3)
{
  test_compile("(lambda () (if 'a 'b 'c))",
               "((asm-close 0 0"
               "   (  (immval a)"
               "      (jmpf lbl_if-a_0)"
               "      (immval b)"
               "      (return)"
               "    (label lbl_if-a_0)"
               "      (immval c)"
               "      (return)"
               "   )"
               " ))");
}

TEST(exec_compiler, if_4)
{
  test_compile("(lambda () (if 'a 'b))",
               "((asm-close 0 0"
               "   (  (immval a)"
               "      (jmpf lbl_if-a_0)"
               "      (immval b)"
               "      (return)"
               "    (label lbl_if-a_0)"
               "      (undef)"
               "      (return)"
               "   )"
               " ))");
}

TEST(exec_compiler, cond_001)
{
  test_compile("(cond)",
               "((undef)"
               " (label lbl_cond-j_0))");
}


TEST(exec_compiler, cond_002)
{
  test_compile("(cond (else))",
               "((undef)(label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_003)
{
  test_compile("(cond (else 'a))",
               "(   (immval a)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_004)
{
  test_compile("(cond ('a 'b))",
               "(   (immval a)"
               "    (jmpt lbl_cond-c_1)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (immval b)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_005)
{
  test_compile("(cond ('a 'b)('c 'd))",
               "(   (immval a)"
               "    (jmpt lbl_cond-c_1)"
               "    (immval c)"
               "    (jmpt lbl_cond-c_2)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_2)"
               "    (immval d)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (immval b)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_006)
{
  test_compile("(cond ('a => write))",
               "(   (immval a)"
               "    (jmpt lbl_cond-c_1)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (frame)"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_007)
{
  test_compile("(cond ('a => write)('b => write))",
               "(   (immval a)"
               "    (jmpt lbl_cond-c_1)"
               "    (immval b)"
               "    (jmpt lbl_cond-c_2)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_2)"
               "    (frame)"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (frame)"
               "    (push)"
               "    (gref write (main))"
               "    (call 1)"
               "    (nop)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_008)
{
  test_compile("(cond ('a))",
               "(   (immval a)"
               "    (jmpt lbl_cond-j_0)"
               "    (undef)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_009)
{
  test_compile("(cond ('a)('b 'c))",
               "(   (immval a)"
               "    (jmpt lbl_cond-j_0)"
               "    (immval b)"
               "    (jmpt lbl_cond-c_1)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (immval c)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_010)
{
  test_compile("(cond ('a 'b)('c))",
               "(   (immval a)"
               "    (jmpt lbl_cond-c_1)"
               "    (immval c)"
               "    (jmpt lbl_cond-j_0)"
               "    (undef)"
               "    (jmp lbl_cond-j_0)"
               " (label lbl_cond-c_1)"
               "    (immval b)"
               " (label lbl_cond-j_0))");
}

TEST(exec_compiler, cond_011)
{
  test_compile("(lambda () (cond))",
               "((asm-close 0 0"
               "   ((undef)(return))))");
}

TEST(exec_compiler, cond_012)
{
  test_compile("(lambda () (cond (else)))",
               "((asm-close 0 0"
               "   ((undef)(return))))");
}


TEST(exec_compiler, cond_013)
{
  test_compile("(lambda () (cond (else 'a)))",
               "((asm-close 0 0"
               "   ((immval a)"
               "    (return))))");
}

TEST(exec_compiler, cond_014)
{
  test_compile("(lambda () (cond ('a 'b)))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_cond-c_0)"
               "       (undef)"
               "       (return)"
               "    (label lbl_cond-c_0)"
               "       (immval b)"
               "       (return))))");
}

TEST(exec_compiler, cond_015)
{
  test_compile("(lambda () (cond ('a)))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_cond-c_0)"
               "       (undef)"
               "       (return)"
               "    (label lbl_cond-c_0)"
               "       (return))))");
}

TEST(exec_compiler, cond_016)
{
  test_compile("(lambda () (cond ('a)('b 'c)))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_cond-c_0)"
               "       (immval b)"
               "       (jmpt lbl_cond-c_1)"
               "       (undef)"
               "       (return)"
               "    (label lbl_cond-c_1)"
               "       (immval c)"
               "       (return)"
               "    (label lbl_cond-c_0)"
               "       (return))))");
}

TEST(exec_compiler, cond_017)
{
  test_compile("(lambda () (cond ('a 'b)('c)))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_cond-c_0)"
               "       (immval c)"
               "       (jmpt lbl_cond-c_1)"
               "       (undef)"
               "       (return)"
               "    (label lbl_cond-c_1)"
               "       (return)"
               "    (label lbl_cond-c_0)"
               "       (immval b)"
               "       (return))))");
}

TEST(exec_compiler, cond_018)
{
  test_compile("(lambda () (cond ('a => write)))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_cond-c_0)"
               "       (undef)"
               "       (return)"
               "    (label lbl_cond-c_0)"
               "       (eframe)"
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
               "   (jmpf lbl_and-j_0)"
               "   (immval b)"
               " (label lbl_and-j_0))");
}

TEST(exec_compiler, and_004)
{
  test_compile("(and (null? 'a) (null? 'b))",
               "(  (frame)"
               "   (immval a)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               "   (jmpf lbl_and-j_0)"
               "   (frame)"
               "   (immval b)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label lbl_and-j_0))");
}

TEST(exec_compiler, and_005)
{
  test_compile("(lambda () (and))",
               "((asm-close 0 0"
               "   ((immval #t)(return))))");
}

TEST(exec_compiler, and_006)
{
  test_compile("(lambda () (and 'a))",
               "((asm-close 0 0"
               "   ((immval a)(return))))");
}

TEST(exec_compiler, and_007)
{
  test_compile("(lambda () (and 'a 'b))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpf lbl_and-j_0)"
               "       (immval b)"
               "       (return)"
               "    (label lbl_and-j_0)"
               "    (return))))");
}

TEST(exec_compiler, and_008)
{
  test_compile("(lambda () (and (null? 'a) (null? 'b)))",
               "((asm-close 0 0"
               "   (   (frame)"
               "       (immval a)"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "       (jmpf lbl_and-j_0)"
               "       (eframe)"
               "       (immval b)"
               "       (push)"
               "       (gref null? (main))"
               "       (tcall 1)"
               "    (label lbl_and-j_0)"
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
               "   (jmpt lbl_or-j_0)"
               "   (immval b)"
               " (label lbl_or-j_0))");
}

TEST(exec_compiler, or_004)
{
  test_compile("(or (null? 'a) (null? 'b))",
               "(  (frame)"
               "   (immval a)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               "   (jmpt lbl_or-j_0)"
               "   (frame)"
               "   (immval b)"
               "   (push)"
               "   (gref null? (main))"
               "   (call 1)"
               "   (mrve)"
               " (label lbl_or-j_0))");
}

TEST(exec_compiler, or_005)
{
  test_compile("(lambda () (or))",
               "((asm-close 0 0"
               "   ((immval #f)(return))))");
}

TEST(exec_compiler, or_006)
{
  test_compile("(lambda () (or 'a))",
               "((asm-close 0 0"
               "   ((immval a)(return))))");
}

TEST(exec_compiler, or_007)
{
  test_compile("(lambda () (or 'a 'b))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_or-j_0)"
               "       (immval b)"
               "       (return)"
               "    (label lbl_or-j_0)"
               "    (return))))");
}


TEST(exec_compiler, or_008)
{
  test_compile("(lambda () (or (null? 'a) (null? 'b)))",
               "((asm-close 0 0"
               "   (   (frame)"
               "       (immval a)"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "       (jmpt lbl_or-j_0)"
               "       (eframe)"
               "       (immval b)"
               "       (push)"
               "       (gref null? (main))"
               "       (tcall 1)"
               "    (label lbl_or-j_0)"
               "       (return))))");
}

TEST(exec_compiler, when_001)
{
  test_compile("(when 'a)",
               "(   (immval a)"
               "    (jmpf lbl_when-a_1)"
               "    (undef)"
               "    (jmp lbl_when-j_0)"
               " (label lbl_when-a_1)"
               "    (undef)"
               " (label lbl_when-j_0))");
}

TEST(exec_compiler, when_002)
{
  test_compile("(when 'a 'b)",
               "(   (immval a)"
               "    (jmpf lbl_when-a_1)"
               "    (immval b)"
               "    (jmp lbl_when-j_0)"
               " (label lbl_when-a_1)"
               "    (undef)"
               " (label lbl_when-j_0))");
}

TEST(exec_compiler, when_003)
{
  test_compile("(lambda () (when 'a))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpf lbl_when-a_1)"
               "       (undef)"
               "       (return)"
               "    (label lbl_when-a_1)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, when_004)
{
  test_compile("(lambda () (when 'a 'b))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpf lbl_when-a_1)"
               "       (immval b)"
               "       (return)"
               "    (label lbl_when-a_1)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, when_005)
{
  test_compile("(when (null? '()) (cons 'a 'b))",
               "(   (frame)"
               "    (immval ())"
               "    (push)"
               "    (gref null? (main))"
               "    (call 1)"
               "    (mrve)"
               "    (jmpf lbl_when-a_1)"
               "    (frame)"
               "    (immval a)"
               "    (push)"
               "    (immval b)"
               "    (push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (jmp lbl_when-j_0)"
               " (label lbl_when-a_1)"
               "    (undef)"
               " (label lbl_when-j_0))");
}


TEST(exec_compiler, when_006)
{
  test_compile("(lambda () (when (null? '()) (cons 'a 'b)))",
               "((asm-close 0 0"
               "   (   (frame)"
               "       (immval ())"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "       (jmpf lbl_when-a_1)"
               "       (eframe)"
               "       (immval a)"
               "       (push)"
               "       (immval b)"
               "       (push)"
               "       (gref cons (main))"
               "       (tcall 2)"
               "    (label lbl_when-a_1)"
               "       (undef)"
               "       (return))))");
}


TEST(exec_compiler, when_007)
{
  test_compile("(when 'z (cons 'a 'b) (cons 'c 'd))",
               "(   (immval z)"
               "    (jmpf lbl_when-a_1)"
               "    (frame)"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (frame)"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (jmp lbl_when-j_0)"
               " (label lbl_when-a_1)"
               "    (undef)"
               " (label lbl_when-j_0))");
}


TEST(exec_compiler, unless_001)
{
  test_compile("(unless 'a)",
               "(   (immval a)"
               "    (jmpt lbl_unless-a_1)"
               "    (undef)"
               "    (jmp lbl_unless-j_0)"
               " (label lbl_unless-a_1)"
               "    (undef)"
               " (label lbl_unless-j_0))");
}

TEST(exec_compiler, unless_002)
{
  test_compile("(unless 'a 'b)",
               "(   (immval a)"
               "    (jmpt lbl_unless-a_1)"
               "    (immval b)"
               "    (jmp lbl_unless-j_0)"
               " (label lbl_unless-a_1)"
               "    (undef)"
               " (label lbl_unless-j_0))");
}

TEST(exec_compiler, unless_003)
{
  test_compile("(lambda () (unless 'a))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_unless-a_1)"
               "       (undef)"
               "       (return)"
               "    (label lbl_unless-a_1)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, unless_004)
{
  test_compile("(lambda () (unless 'a 'b))",
               "((asm-close 0 0"
               "   (   (immval a)"
               "       (jmpt lbl_unless-a_1)"
               "       (immval b)"
               "       (return)"
               "    (label lbl_unless-a_1)"
               "       (undef)"
               "       (return))))");
}


TEST(exec_compiler, unless_005)
{
  test_compile("(unless (null? '()) (cons 'a 'b))",
               "(   (frame)"
               "    (immval ())"
               "    (push)"
               "    (gref null? (main))"
               "    (call 1)"
               "    (mrve)"
               "    (jmpt lbl_unless-a_1)"
               "    (frame)"
               "    (immval a)"
               "    (push)"
               "    (immval b)"
               "    (push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (jmp lbl_unless-j_0)"
               " (label lbl_unless-a_1)"
               "    (undef)"
               " (label lbl_unless-j_0))");
}

TEST(exec_compiler, unless_006)
{
  test_compile("(lambda () (unless (null? '()) (cons 'a 'b)))",
               "((asm-close 0 0"
               "   (   (frame)"
               "       (immval ())"
               "       (push)"
               "       (gref null? (main))"
               "       (call 1)"
               "       (mrve)"
               "       (jmpt lbl_unless-a_1)"
               "       (eframe)"
               "       (immval a)"
               "       (push)"
               "       (immval b)"
               "       (push)"
               "       (gref cons (main))"
               "       (tcall 2)"
               "    (label lbl_unless-a_1)"
               "       (undef)"
               "       (return))))");
}

TEST(exec_compiler, unless_007)
{
  test_compile("(unless 'z (cons 'a 'b) (cons 'c 'd))",
               "(   (immval z)"
               "    (jmpt lbl_unless-a_1)"
               "    (frame)"
               "    (immval a)(push)"
               "    (immval b)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (frame)"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (jmp lbl_unless-j_0)"
               " (label lbl_unless-a_1)"
               "    (undef)"
               " (label lbl_unless-j_0))");
}

TEST(exec_compiler, do_001)
{
  test_compile("(do ((x 'ix 'sx)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  'c)",
               "(   (eframe)"
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
               "    (epop))");
}

TEST(exec_compiler, do_002)
{
  test_compile("(do ((x 'ix)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  'c)",
               "(   (eframe)"
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
               "    (epop))");
}

TEST(exec_compiler, do_003)
{
  test_compile("(do ((x 'ix 'sx)"
               "     (y 'iy 'sy))"
               "    ('t 'e)"
               "  (set! y 1))",
               "(   (eframe)"
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
               "    (epop))");
}

TEST(exec_compiler, do_004)
{
  test_compile("(do ()"
               "    ('t)"
               "  )",
               "((label lbl_do-s_1)"
               "    (immval t)"
               "    (jmpt lbl_do-e_0)"
               "    (jmp lbl_do-s_1)"
               " (label lbl_do-e_0)"
               "    (undef))");
}

TEST(exec_compiler, do_005)
{
  test_compile("(lambda ()"
               "  (do ((x 'ix 'sx)"
               "       (y 'iy 'sy))"
               "      ('t 'e)"
               "    'c))",
               "((asm-close 0 0"
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
               "      (return))))");
}

TEST(exec_compiler, do_006)
{
  test_compile("(lambda ()"
               "  (do ((x 'ix 'sx)"
               "       (y 'iy 'sy))"
               "      ('t (cons 'a 'b))"
               "    'c))",
               "((asm-close 0 0"
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
               "      (tcall 2))))");
}

TEST(exec_compiler, do_007)
{
  test_compile("(lambda ()"
               "  (do ()"
               "      ('t)"
               "    ))",
               "((asm-close 0 0"
               "   ((label lbl_do-s_1)"
               "       (immval t)"
               "       (jmpt lbl_do-e_0)"
               "       (jmp lbl_do-s_1)"
               "    (label lbl_do-e_0)"
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
               "(   (eframe)"
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
               "    (nop)"
               "    (frame)"
               "    (immval g)(push)"
               "    (immval h)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
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
               "    (nop)"
               "    (frame)"
               "    (immval c)(push)"
               "    (immval d)(push)"
               "    (gref cons (main))"
               "    (call 2)"
               "    (nop)"
               "    (epop))");
}

TEST(exec_compiler, let_values_1)
{
  test_compile("(let-values (((a) 1)) a)",
               "((eframe)"
               " (immval 1)"
               " (mvpush)"
               " (ecommit 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_2)
{
  test_compile("(let-values ((a 1)) a)",
               "((eframe)"
               " (immval 1)"
               " (mrvc -1)"
               " (mvpush)"
               " (ecommit 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_3)
{
  test_compile("(let-values (((a b) (values 1 2))) b)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (sref 1 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_4)
{
  test_compile("(let-values (((a b . c) (values 1 2 3 4))) c)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (immval 3) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 4)"
               " (nop)"
               " (mrvc -3)"
               " (mvpush)"
               " (ecommit 3)"
               " (sref 2 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_5)
{
  test_compile("(let-values ((() (values))))",
               "((cframe)"
               " (gref values (main))"
               " (call 0)"
               " (nop)"
               " (mrvc 0)"
               " (undef))");
}

TEST(exec_compiler, let_values_6)
{
  test_compile("(let-values (((a b) (values 1 2))"
               "             ((c d) (values 3 4)))"
               "  c)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (frame)"
               " (immval 3) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 4)"
               " (sref 2 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_7)
{
  test_compile("(let-values (((a b) (values 1 2))"
               "             (() (values)))"
               "  a)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (cframe)"
               " (gref values (main))"
               " (call 0)"
               " (nop)"
               " (mrvc 0)"
               " (ecommit 2)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_8)
{
  test_compile("(let-values (((a b) (values 1 2)))"
               "  (set! a 100))",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (box 0 0)"
               " (immval 100)"
               " (sset 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_values_9)
{
  test_compile("(lambda () (let-values (((a) 1)) (a)))",
               "((asm-close 0 0"
               "   ((eframe)"
               "    (immval 1)"
               "    (mvpush)"
               "    (ecommit 1)"
               "    (sref 0 0)"
               "    (tcall 0))))");
}

TEST(exec_compiler, let_a_values_1)
{
  test_compile("(let*-values (((a) 1)) a)",
               "((eframe)"
               " (immval 1)"
               " (mvpush)"
               " (ecommit 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_2)
{
  test_compile("(let*-values ((a 1)) a)",
               "((eframe)"
               " (immval 1)"
               " (mrvc -1)"
               " (mvpush)"
               " (ecommit 1)"
               " (sref 0 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_3)
{
  test_compile("(let*-values (((a b) (values 1 2))) b)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (sref 1 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_4)
{
  test_compile("(let*-values (((a b . c) (values 1 2 3 4))) c)",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (immval 3) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 4)"
               " (nop)"
               " (mrvc -3)"
               " (mvpush)"
               " (ecommit 3)"
               " (sref 2 0)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_5)
{
  test_compile("(let*-values ((() (values))))",
               "((cframe)"
               " (gref values (main))"
               " (call 0)"
               " (nop)"
               " (mrvc 0)"
               " (undef))");
}

TEST(exec_compiler, let_a_values_6)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values 3 4)))"
               "  (list a b c d))",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (eframe)"
               " (frame)"
               " (immval 3) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (frame)"
               " (sref 0 1) (push)"
               " (sref 1 1) (push)"
               " (sref 0 0) (push)"
               " (sref 1 0) (push)"
               " (gref list (main))"
               " (call 4)"
               " (nop)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_7)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values a 4)))"
               "  (list a b c d))",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (eframe)"
               " (frame)"
               " (sref 0 0) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (frame)"
               " (sref 0 1) (push)"
               " (sref 1 1) (push)"
               " (sref 0 0) (push)"
               " (sref 1 0) (push)"
               " (gref list (main))"
               " (call 4)"
               " (nop)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_8)
{
  test_compile("(let*-values (((a b) (values 1 2))"
               "              ((c d) (values 3 4)))"
               "  (set! b 100))",
               "((eframe)"
               " (frame)"
               " (immval 1) (push)"
               " (immval 2) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (box 1 0)"
               " (eframe)"
               " (frame)"
               " (immval 3) (push)"
               " (immval 4) (push)"
               " (gref values (main))"
               " (call 2)"
               " (nop)"
               " (mrvc 2)"
               " (mvpush)"
               " (ecommit 2)"
               " (immval 100)"
               " (sset 1 1)"
               " (epop)"
               " (epop))");
}

TEST(exec_compiler, let_a_values_9)
{
  test_compile("(lambda () (let*-values (((a) 1)) (a)))",
               "((asm-close 0 0"
               "   ((eframe)"
               "    (immval 1)"
               "    (mvpush)"
               "    (ecommit 1)"
               "    (sref 0 0)"
               "    (tcall 0))))");
}
