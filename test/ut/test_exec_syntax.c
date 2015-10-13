#include "scythe/refstk.h"
#include "scythe/miscobjects.h"
#include "scythe/module.h"
#include "scythe/procedure.h"
#include "scythe/record.h"
#include "scythe/symbol.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(exec_syntax);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(exec_syntax)
{
  scy = ut_scythe_setup(true);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(exec_syntax)
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
def_global_var(const char *sym, const char *obj)
{
  ScmObj m = SCM_OBJ_INIT, s = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&m, &s, &o);

  m = scm_make_symbol_from_cstr("main", SCM_ENC_SRC);
  s = scm_make_symbol_from_cstr(sym, SCM_ENC_SRC);
  o = ut_read_cstr(obj);

  scm_define_global_var(m, s, o, true);
}

static ScmObj
ref_global_var(const char *sym)
{
  ScmObj m = SCM_OBJ_INIT, s = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&m, &s, &o);

  m = scm_make_symbol_from_cstr("main", SCM_ENC_SRC);
  s = scm_make_symbol_from_cstr(sym, SCM_ENC_SRC);

  scm_refer_global_var(m, s, SCM_CSETTER_L(o));

  return o;
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

static void
test_eval__comp_gv_with_obj(const char *expr, const char *sym, const char *expc)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = ut_read_cstr(expc);
  eval_cstr(expr);
  actual = ref_global_var(sym);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_eval__comp_val_with_gv(const char *expr, const char *sym)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = ref_global_var(sym);
  actual = eval_cstr(expr);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}


static void
test_eval__chk_val_type(const char *expr, bool (*func)(ScmObj))
{
  ScmObj actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual);

  actual = eval_cstr(expr);

  TEST_ASSERT_TRUE(func(actual));
}

static void
test_eval__chk_gv_type(const char *expr, const char *sym,
                       bool (*func)(ScmObj))
{
  ScmObj actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual);

  eval_cstr(expr);
  actual = ref_global_var(sym);

  TEST_ASSERT_TRUE(func(actual));
}

TEST(exec_syntax, self_eval_1)
{
  test_eval__comp_val_with_obj("1",
                               "1");
}

TEST(exec_syntax, define_global_variable_1)
{
  test_eval__chk_gv_type("(define (func x) x)", "func",
                         scm_closure_p);
}

TEST(exec_syntax, define_global_variable_2)
{
  test_eval__comp_gv_with_obj("(define var 1)", "var",
                              "1");
}

TEST(exec_syntax, refer_global_variable_1)
{
  test_eval__comp_val_with_gv("cons",
                              "cons");
}

TEST(exec_syntax, refer_global_variable_2)
{
  test_eval__comp_val_with_gv("((lambda (x) cons) 1)",
                              "cons");
}

TEST(exec_syntax, update_global_variable_1)
{
  def_global_var("var", "1");
  test_eval__comp_gv_with_obj("(set! var 10)", "var",
                              "10");
}

TEST(exec_syntax, quote_1)
{
  test_eval__comp_val_with_obj("'(a b c)",
                               "(a b c)");
}

TEST(exec_syntax, application_1)
{
  test_eval__comp_val_with_obj("(cons 'a 'b)",
                               "(a . b)");
}

TEST(exec_syntax, variable_arity_1)
{
  test_eval__comp_val_with_obj("((lambda x x))",
                               "()");
}

TEST(exec_syntax, variable_arity_2)
{
  test_eval__comp_val_with_obj("((lambda x x) 1 2 3)",
                               "(1 2 3)");
}

TEST(exec_syntax, variable_arity_3)
{
  test_eval__comp_val_with_obj("((lambda (x . y) (cons x y)) 1)",
                               "(1)");
}

TEST(exec_syntax, variable_arity_4)
{
  test_eval__comp_val_with_obj("((lambda (x . y) (cons x y)) 1 2 3)",
                               "(1 2 3)");
}

TEST(exec_syntax, closure_1)
{
  test_eval__comp_val_with_obj("((lambda (x) x) 'a)",
                               "a");
}

TEST(exec_syntax, closure_2)
{
  test_eval__comp_val_with_obj("(((lambda (x) (lambda (y) (cons x y))) 1) 2)",
                               "(1 . 2)");
}

TEST(exec_syntax, closure_3)
{
  test_eval__comp_val_with_obj("((((lambda (x)"
                               "      (lambda ()"
                               "         (lambda (y) (cons x y)))) 1)) 2)",
                               "(1 . 2)");
}

TEST(exec_syntax, closure_4)
{
  test_eval__comp_val_with_obj("((((lambda (x)"
                               "      (lambda (y)"
                               "         (lambda (z) (cons x z)))) 1) 2) 3)",
                               "(1 . 3)");
}

TEST(exec_syntax, closure_5)
{
  test_eval__comp_val_with_obj("((((lambda (x)"
                               "      (lambda (y)"
                               "         (let ((a 1))"
                               "            (lambda (z) (cons x z))))) 1) 2) 3)",
                               "(1 . 3)");
}

TEST(exec_syntax, closure_6)
{
  test_eval__comp_val_with_obj( "((lambda (x)"
                                "    (let ((a (lambda () (set! x 100)))"
                                "          (b (lambda () x)))"
                                "       (a)"
                                "       (b))) 1)",
                                "100");
}

TEST(exec_syntax, tail_call_1)
{
  test_eval__comp_val_with_obj("((lambda () ((lambda () 1))))",
                               "1");
}

TEST(exec_syntax, tail_call_2)
{
  test_eval__comp_val_with_obj("((lambda (x y) ((lambda () 1))) '10 '100)",
                               "1");
}

TEST(exec_syntax, tail_call_3)
{
  test_eval__comp_val_with_obj("((lambda () ((lambda (x y) y) 10 100)))",
                               "100");
}

TEST(exec_syntax, tail_call_4)
{
  test_eval__comp_val_with_obj("((lambda (x y) ((lambda (a b c) c) 10 100 1000)) 3 5)",
                               "1000");
}

TEST(exec_syntax, lambda_1)
{
  test_eval__chk_val_type("(lambda () 'a)",
                          scm_closure_p);
}

TEST(exec_syntax, lambda_2)
{
  test_eval__chk_val_type("(lambda (v1 v2) 'a)",
                          scm_closure_p);
}

TEST(exec_syntax, lambda_3)
{
  test_eval__chk_val_type("(lambda (v1 v2 . v3) 'a)",
                          scm_closure_p);
}

TEST(exec_syntax, lambda_4)
{
  test_eval__chk_val_type("((lambda ()))",
                          scm_undef_object_p);
}

TEST(exec_syntax, let_1)
{
  test_eval__comp_val_with_obj("(let ((x 1)) 'a)",
                               "a");
}

TEST(exec_syntax, let_2)
{
  test_eval__comp_val_with_obj("(let ((x 1)(y 2)) (cons x y))",
                               "(1 . 2)");
}

TEST(exec_syntax, let_3)
{
  test_eval__comp_val_with_obj("(let ((x 1)(y 2)) (set! y 100) y)",
                               "100");
}

TEST(exec_syntax, let_4)
{
  test_eval__comp_val_with_obj("(let ((x 1))"
                               "   (let ((a (lambda () (set! x 100)))"
                               "         (b (lambda () x)))"
                               "      (a)"
                               "      (b)))",
                               "100");
}

TEST(exec_syntax, let_5)
{
  test_eval__comp_val_with_obj("((lambda () (let ((x 1)(y 2)) x)))",
                               "1");
}

TEST(exec_syntax, let_6)
{
  test_eval__chk_val_type("(let ())",
                          scm_undef_object_p);
}

TEST(exec_syntax, named_let_1)
{
  test_eval__comp_val_with_obj("(let loop ((x 1)(y 2)) x)",
                               "1");
}

TEST(exec_syntax, named_let_2)
{
  test_eval__chk_val_type("(let loop ())",
                          scm_undef_object_p);
}

TEST(exec_syntax, named_let_3)
{
  test_eval__comp_val_with_obj("(let loop ((i '(a b c))(o '()))"
                               "   (if (null? i) o"
                               "       (loop (cdr i) (cons (car i) o))))",
                               "(c b a)");
}

TEST(exec_syntax, named_let_4)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (let loop ((i '(a b c))(o '()))"
                               "       (if (null? i) o"
                               "           (loop (cdr i) (cons (car i) o))))))",
                               "(c b a)");
}

TEST(exec_syntax, named_let_5)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (let loop ((i '(a b c))(o '()))"
                               "       (if (null? i) o"
                               "           (loop (cdr i) (cons (car i) o))))"
                               "    '(c b a)))",
                               "(c b a)");
}

TEST(exec_syntax, let_a_1)
{
  test_eval__comp_val_with_obj("(let* ((x 1)) 'a)",
                               "a");
}

TEST(exec_syntax, let_a_2)
{
  test_eval__comp_val_with_obj("(let* ((x 1)(y 2)) (cons x y))",
                               "(1 . 2)");
}

TEST(exec_syntax, let_a_3)
{
  test_eval__comp_val_with_obj("(let* ((x 1)(y 2)) (set! x 100) x)",
                               "100");
}

TEST(exec_syntax, let_a_4)
{
  test_eval__comp_val_with_obj("(let* ((x 1)"
                               "      (a (lambda () (set! x 100)))"
                               "      (b (lambda () x)))"
                               "  (a)"
                               "  (b))",
                               "100");
}

TEST(exec_syntax, let_a_5)
{
  test_eval__comp_val_with_obj("((lambda () (let* ((x 1)(y 2)) x)))",
                               "1");
}

TEST(exec_syntax, let_a_6)
{
  test_eval__chk_val_type("(let* ())",
                          scm_undef_object_p);
}

TEST(exec_syntax, letrec_1)
{
  test_eval__comp_val_with_obj("(letrec ((x 1)(y 2)) x)",
                               "1");
}

TEST(exec_syntax, letrec_2)
{
  test_eval__comp_val_with_obj("(letrec ((x (lambda () y))(y 100)) (x))",
                               "100");
}

TEST(exec_syntax, letrec_3)
{
  test_eval__comp_val_with_obj("(letrec ((x 1)(y 2)) (set! y 10) y)",
                               "10");
}

TEST(exec_syntax, letrec_4)
{
  test_eval__comp_val_with_obj("((lambda () (letrec ((x 1)(y 2)) x)))",
                               "1");
}

TEST(exec_syntax, letrec_5)
{
  test_eval__chk_val_type("(letrec ())",
                          scm_undef_object_p);
}

TEST(exec_syntax, letrec_a_1)
{
  test_eval__comp_val_with_obj("(letrec* ((x 1)(y 2)) x)",
                               "1");
}

TEST(exec_syntax, letrec_a_2)
{
  test_eval__comp_val_with_obj("(letrec* ((x (lambda () y))(y 100)) (x))",
                               "100");
}

TEST(exec_syntax, letrec_a_3)
{
  test_eval__comp_val_with_obj("(letrec* ((x 1)(y 2)) (set! y 10) y)",
                               "10");
}

TEST(exec_syntax, letrec_a_4)
{
  test_eval__comp_val_with_obj("(letrec* ((x 1)(y (cons 'a x))) y)",
                               "(a . 1)");
}

TEST(exec_syntax, letrec_a_5)
{
  test_eval__comp_val_with_obj("((lambda () (letrec* ((x 1)(y 2)) x)))",
                               "1");
}

TEST(exec_syntax, letrec_a_6)
{
  test_eval__chk_val_type("(letrec* ())",
                          scm_undef_object_p);
}

TEST(exec_syntax, internal_definition_1)
{
  test_eval__comp_val_with_obj("((lambda () (define x 1) (define y 2) x))",
                               "1");
}

TEST(exec_syntax, internal_definition_2)
{
  test_eval__comp_val_with_obj("(((lambda () (define x (lambda () y)) (define y 100) x)))",
                               "100");
}

TEST(exec_syntax, internal_definition_3)
{
  test_eval__comp_val_with_obj("((lambda () (define x 1) (define y 2) (set! y 10) y))",
                               "10");
}

TEST(exec_syntax, internal_definition_4)
{
  test_eval__comp_val_with_obj("((lambda () (define x 1) (define y (cons 'a x)) y))",
                               "(a . 1)");
}

TEST(exec_syntax, internal_definition_5)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "   (define x 1)"
                               "   (begin"
                               "     (define y 2)"
                               "     x)))",
                               "1");
}

TEST(exec_syntax, begin_1)
{
  test_eval__comp_val_with_obj("(begin (cons 'a 'b) (cons 'x 'y))",
                               "(x . y)");
}

TEST(exec_syntax, begin_2)
{
  test_eval__comp_val_with_obj("(begin (define gvar 1))",
                               "1");
}

TEST(exec_syntax, begin_3)
{
  test_eval__chk_val_type("(begin)",
                          scm_undef_object_p);
}

TEST(exec_syntax, refe_bound_variable_1)
{
  test_eval__comp_val_with_obj("((lambda (x) x) 'a)",
                               "a");
}

TEST(exec_syntax, refe_bound_variable_2)
{
  test_eval__comp_val_with_obj("(((lambda (f1 b2) (lambda (b1 b2) b2)) 'a 'b) 'c 'd)",
                               "d");
}

TEST(exec_syntax, set_bound_variable_1)
{
  test_eval__comp_val_with_obj("(((lambda (f1 f2)"
                               "    (lambda (b1 b2)"
                               "      (set! b2 'e) b2))"
                               "  'a 'b)"
                               " 'c 'd)",
                               "e");
}

TEST(exec_syntax, set_bound_variable_2)
{
  test_eval__comp_val_with_obj("(((lambda (f1 b2)"
                               "    (lambda (b1 b2)"
                               "      (set! b2 'e) b2))"
                               "  'a 'b)"
                               " 'c 'd)",
                               "e");
}

TEST(exec_syntax, refer_free_variable_1)
{
  test_eval__comp_val_with_obj("(((lambda (f1 f2)"
                               "    (lambda (b1 b2)"
                               "      f2))"
                               "  'a 'b)"
                               " 'c 'd)",
                               "b");
}

TEST(exec_syntax, set_free_variable_1)
{
  test_eval__comp_val_with_obj("(((lambda (f1 f2)"
                               "    (lambda (b1 b2)"
                               "      (set! f2 'e) f2))"
                               "  'a 'b)"
                               " 'c 'd)",
                               "e");
}

TEST(exec_syntax, if_1)
{
  test_eval__comp_val_with_obj("(if 'a 'b 'c)",
                               "b");
}

TEST(exec_syntax, if_2)
{
  test_eval__comp_val_with_obj("(if 'a 'b)",
                               "b");
}

TEST(exec_syntax, if_3)
{
  test_eval__comp_val_with_obj("(if #f 'b 'c)",
                               "c");
}

TEST(exec_syntax, if_4)
{
  test_eval__chk_val_type("(if #f 'b)",
                          scm_undef_object_p);
}

TEST(exec_syntax, if_5)
{
  test_eval__comp_val_with_obj("((lambda () (if 'a 'b 'c)))""(if 'a 'b 'c)",
                               "b");
}

TEST(exec_syntax, if_6)
{
  test_eval__chk_val_type("((lambda () (if '#f 'b)))",
                          scm_undef_object_p);
}

TEST(exec_syntax, cond_1)
{
  test_eval__comp_val_with_obj("(cond (else 'a))",
                               "a");
}

TEST(exec_syntax, cond_2)
{
  test_eval__comp_val_with_obj("(cond ('a))",
                               "a");
}

TEST(exec_syntax, cond_3)
{
  test_eval__comp_val_with_obj("(cond ('a 'b))",
                               "b");
}

TEST(exec_syntax, cond_4)
{
  test_eval__comp_val_with_obj("(cond ('a 'b)('c 'd))",
                               "b");
}

TEST(exec_syntax, cond_5)
{
  test_eval__comp_val_with_obj("(cond ('a => (lambda (x) (cons x 1))))",
                               "(a . 1)");
}

TEST(exec_syntax, cond_6)
{
  test_eval__comp_val_with_obj("(cond ('a)('b 'c))",
                               "a");
}

TEST(exec_syntax, cond_7)
{
  test_eval__comp_val_with_obj("(cond (#f 'a)('b 'c))",
                               "c");
}

TEST(exec_syntax, cond_8)
{
  test_eval__comp_val_with_obj("(cond (#f 'a)(else 'b))",
                               "b");
}

TEST(exec_syntax, cond_9)
{
  test_eval__comp_val_with_obj("((lambda () (cond (else 'a))))",
                               "a");
}

TEST(exec_syntax, cond_10)
{
  test_eval__comp_val_with_obj("((lambda () (cond ('a))))",
                               "a");
}

TEST(exec_syntax, cond_11)
{
  test_eval__comp_val_with_obj("((lambda () (cond ('a 'b))))",
                               "b");
}

TEST(exec_syntax, cond_12)
{
  test_eval__comp_val_with_obj("((lambda () (cond ('a 'b)('c 'd))))",
                               "b");
}

TEST(exec_syntax, cond_13)
{
  test_eval__comp_val_with_obj("((lambda () (cond ('a => (lambda (x) (cons x 1))))))",
                               "(a . 1)");
}

TEST(exec_syntax, cond_14)
{
  test_eval__comp_val_with_obj("((lambda () (cond ('a)('b 'c))))",
                               "a");
}

TEST(exec_syntax, cond_15)
{
  test_eval__comp_val_with_obj("((lambda () (cond (#f 'a)('b 'c))))",
                               "c");
}

TEST(exec_syntax, cond_16)
{
  test_eval__comp_val_with_obj("((lambda () (cond (#f 'a)(else 'b))))",
                               "b");
}

TEST(exec_syntax, and_1)
{
  test_eval__comp_val_with_obj("(and)",
                               "#t");
}

TEST(exec_syntax, and_2)
{
  test_eval__comp_val_with_obj("(and 'a)",
                               "a");
}

TEST(exec_syntax, and_3)
{
  test_eval__comp_val_with_obj("(and #f)",
                               "#f");
}

TEST(exec_syntax, and_4)
{
  test_eval__comp_val_with_obj("(and 'a 'b)",
                               "b");
}

TEST(exec_syntax, and_5)
{
  test_eval__comp_val_with_obj("(and #f 'b)",
                               "#f");
}

TEST(exec_syntax, and_6)
{
  test_eval__comp_val_with_obj("(and (null? '()) (null? '()))",
                               "#t");
}

TEST(exec_syntax, and_7)
{
  test_eval__comp_val_with_obj("(and (null? 'a) (null? '()))",
                               "#f");
}

TEST(exec_syntax, and_8)
{
  test_eval__comp_val_with_obj("((lambda () (and)))",
                               "#t");
}

TEST(exec_syntax, and_9)
{
  test_eval__comp_val_with_obj("((lambda () (and 'a)))",
                               "a");
}

TEST(exec_syntax, and_10)
{
  test_eval__comp_val_with_obj("((lambda () (and #f)))",
                               "#f");
}

TEST(exec_syntax, and_11)
{
  test_eval__comp_val_with_obj("((lambda () (and 'a 'b)))",
                               "b");
}

TEST(exec_syntax, and_12)
{
  test_eval__comp_val_with_obj("((lambda () (and #f 'b)))",
                               "#f");
}

TEST(exec_syntax, and_13)
{
  test_eval__comp_val_with_obj("((lambda () (and (null? '()) (null? '()))))",
                               "#t");
}

TEST(exec_syntax, and_14)
{
  test_eval__comp_val_with_obj("((lambda () (and (null? 'a) (null? '()))))",
                               "#f");
}

TEST(exec_syntax, or_1)
{
  test_eval__comp_val_with_obj("(or)",
                               "#f");
}

TEST(exec_syntax, or_2)
{
  test_eval__comp_val_with_obj("(or 'a)",
                               "a");
}

TEST(exec_syntax, or_3)
{
  test_eval__comp_val_with_obj("(or #f)",
                               "#f");
}

TEST(exec_syntax, or_4)
{
  test_eval__comp_val_with_obj("(or 'a 'b)",
                               "a");
}

TEST(exec_syntax, or_5)
{
  test_eval__comp_val_with_obj("(or #f 'b)",
                               "b");
}

TEST(exec_syntax, or_6)
{
  test_eval__comp_val_with_obj("(or (null? '()) (null? 'a))",
                               "#t");
}

TEST(exec_syntax, or_7)
{
  test_eval__comp_val_with_obj("(or (null? 'a) (null? '()))",
                               "#t");
}

TEST(exec_syntax, or_8)
{
  test_eval__comp_val_with_obj("((lambda () (or)))",
                               "#f");
}

TEST(exec_syntax, or_9)
{
  test_eval__comp_val_with_obj("((lambda () (or 'a)))",
                               "a");
}

TEST(exec_syntax, or_10)
{
  test_eval__comp_val_with_obj("((lambda () (or #f)))",
                               "#f");
}

TEST(exec_syntax, or_11)
{
  test_eval__comp_val_with_obj("((lambda () (or 'a 'b)))",
                               "a");
}

TEST(exec_syntax, or_12)
{
  test_eval__comp_val_with_obj("((lambda () (or #f 'b)))",
                               "b");
}

TEST(exec_syntax, or_13)
{
  test_eval__comp_val_with_obj("((lambda () (or (null? '()) (null? 'a))))",
                               "#t");
}

TEST(exec_syntax, or_14)
{
  test_eval__comp_val_with_obj("((lambda () (or (null? 'a) (null? '()))))",
                               "#t");
}

TEST(exec_syntax, when_1)
{
  test_eval__chk_val_type("(when 'a)",
                          scm_undef_object_p);
}

TEST(exec_syntax, when_2)
{
  test_eval__comp_val_with_obj("(when 'a 'b)",
                               "b");
}


TEST(exec_syntax, when_3)
{
  test_eval__chk_val_type("(when #f 'a)",
                          scm_undef_object_p);
}

TEST(exec_syntax, when_4)
{
  test_eval__comp_val_with_obj("(when (null? ()) (cons 'a 'b))",
                               "(a . b)");
}

TEST(exec_syntax, when_5)
{
  test_eval__chk_val_type("((lambda () (when 'a)))",
                          scm_undef_object_p);
}


TEST(exec_syntax, when_6)
{
  test_eval__comp_val_with_obj("((lambda () (when 'a 'b)))",
                               "b");
}

TEST(exec_syntax, when_7)
{
  test_eval__chk_val_type("((lambda () (when #f 'a)))",
                          scm_undef_object_p);
}

TEST(exec_syntax, when_8)
{
  test_eval__comp_val_with_obj("((lambda () (when (null? ()) (cons 'a 'b))))",
                               "(a . b)");
}

TEST(exec_syntax, unless_1)
{
  test_eval__chk_val_type("(unless 'a)",
                          scm_undef_object_p);
}

TEST(exec_syntax, unless_2)
{
  test_eval__comp_val_with_obj("(unless #f 'a)",
                               "a");
}

TEST(exec_syntax, unless_3)
{
  test_eval__chk_val_type("(unless 'a 'b)",
                          scm_undef_object_p);
}

TEST(exec_syntax, unless_4)
{
  test_eval__comp_val_with_obj("(unless (null? 'a) (cons 'a 'b))",
                               "(a . b)");
}

TEST(exec_syntax, unless_5)
{
  test_eval__chk_val_type("((lambda () (unless 'a)))",
                          scm_undef_object_p);
}

TEST(exec_syntax, unless_6)
{
  test_eval__comp_val_with_obj("((lambda () (unless #f 'a)))",
                               "a");
}

TEST(exec_syntax, unless_7)
{
  test_eval__chk_val_type("((lambda () (unless 'a 'b)))",
                          scm_undef_object_p);
}

TEST(exec_syntax, unless_8)
{
  test_eval__comp_val_with_obj("((lambda () (unless (null? 'a) (cons 'a 'b))))",
                               "(a . b)");
}

TEST(exec_syntax, do_1)
{
  test_eval__comp_val_with_obj("(do ((x '(a b c) (cdr x))"
                               "     (y '() (cons (car x) y)))"
                               "    ((null? x) y)"
                               "  )",
                               "(c b a)");
}

TEST(exec_syntax, do_2)
{
  test_eval__comp_val_with_obj("(do ((x '(a b c) (cdr x))"
                               "     (y '()))"
                               "    ((null? x) y)"
                               "  (set! y (cons (car x) y)))",
                               "(c b a)");
}

TEST(exec_syntax, do_3)
{
  test_eval__chk_val_type("(do ((x '(a b c) (cdr x))"
                          "     (y '() (cons (car x) y)))"
                          "    ((null? x))"
                          "  )",
                          scm_undef_object_p);
}

TEST(exec_syntax, do_4)
{
  test_eval__chk_val_type("(do ()"
                          "    (#t)"
                          "  )",
                          scm_undef_object_p);
}

TEST(exec_syntax, do_5)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (do ((x '(a b c) (cdr x))"
                               "         (y  '() (cons (car x) y)))"
                               "        ((null? x) y)"
                               "     )))",
                               "(c b a)");
}

TEST(exec_syntax, do_6)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (do ((x '(a b c) (cdr x))"
                               "         (y '() (cons (car x) y)))"
                               "        ((null? x) (car y))"
                               "     )))",
                               "c");
}

TEST(exec_syntax, do_7)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (do ((x '(a b c) (cdr x))"
                               "         (y '()))"
                               "        ((null? x) y)"
                               "      (set! y (cons (car x) y)))))",
                               "(c b a)");
}

TEST(exec_syntax, let_values_1)
{
  test_eval__comp_val_with_obj("(let-values (((a) 1)) a)",
                               "1");
}

TEST(exec_syntax, let_values_2)
{
  test_eval__comp_val_with_obj("(let-values ((a 1)) a)",
                               "(1)");
}

TEST(exec_syntax, let_values_3)
{
  test_eval__comp_val_with_obj("(let-values (((a b) (values 1 2)))"
                               "   (list a b))",
                               "(1 2)");
}

TEST(exec_syntax, let_values_4)
{
  test_eval__comp_val_with_obj("(let-values (((a b . c) (values 1 2 3 4)))"
                               "   c)",
                               "(3 4)");
}

TEST(exec_syntax, let_values_5)
{
  test_eval__chk_val_type("(let-values ((() (values))))",
                          scm_undef_object_p);
}

TEST(exec_syntax, let_values_6)
{
  test_eval__comp_val_with_obj("(let-values (((a b) (values 1 2))"
                               "             ((c d) (values 3 4)))"
                               "  (list b c))",
                               "(2 3)");
}

TEST(exec_syntax, let_values_7)
{
  test_eval__comp_val_with_obj("(let-values (((a b) (values 1 2))"
                               "             (() (values)))"
                               "  (list a b))",
                               "(1 2)");
}

TEST(exec_syntax, let_values_8)
{
  test_eval__comp_val_with_obj("(let-values (((a b) (values 1 2)))"
                               "  (set! a 100)"
                               "  a)",
                               "100");
}

TEST(exec_syntax, let_values_9)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "   (let-values (((a) 1))"
                               "     (list a))))",
                               "(1)");
}

TEST(exec_syntax, let_a_values_1)
{
  test_eval__comp_val_with_obj("(let*-values (((a) 1)) a)",
                               "1");
}

TEST(exec_syntax, let_a_values_2)
{
  test_eval__comp_val_with_obj("(let*-values ((a 1)) a)",
                               "(1)");
}

TEST(exec_syntax, let_a_values_3)
{
  test_eval__comp_val_with_obj("(let*-values (((a b) (values 1 2)))"
                               "  (list a b))",
                               "(1 2)");
}

TEST(exec_syntax, let_a_values_4)
{
  test_eval__comp_val_with_obj("(let*-values (((a b . c) (values 1 2 3 4)))"
                               "  c)",
                               "(3 4)");
}

TEST(exec_syntax, let_a_values_5)
{
  test_eval__chk_val_type("(let*-values ((() (values))))",
                          scm_undef_object_p);
}

TEST(exec_syntax, let_a_values_6)
{
  test_eval__comp_val_with_obj("(let*-values (((a b) (values 1 2))"
                               "              ((c d) (values 3 4)))"
                               "  (list a b c d))",
                               "(1 2 3 4)");
}

TEST(exec_syntax, let_a_values_7)
{
  test_eval__comp_val_with_obj("(let*-values (((a b) (values 1 2))"
                               "              ((c d) (values a 4)))"
                               "  (list a b c d))",
                               "(1 2 1 4)");
}

TEST(exec_syntax, let_a_values_8)
{
  test_eval__comp_val_with_obj("(let*-values (((a b) (values 1 2))"
                               "              ((c d) (values 3 4)))"
                               "  (set! b 100)"
                               "  (list a b c d))",
                               "(1 100 3 4)");
}

TEST(exec_syntax, let_a_values_9)
{
  test_eval__comp_val_with_obj("((lambda ()"
                               "    (let*-values (((a) 1))"
                               "       (list a))))",
                               "(1)");
}

TEST(exec_syntax, parameterize__empty)
{
  test_eval__comp_val_with_obj("(parameterize () 1)",
                               "1");
}

TEST(exec_syntax, parameterize__no_converter)
{
  test_eval__comp_val_with_obj("(let ((prm (make-parameter 1)))"
                               "  (parameterize ((prm 2)) (prm)))",
                               "2");
}

TEST(exec_syntax, parameterize__specify_converter)
{
  test_eval__comp_val_with_obj("(let ((prm (make-parameter 1"
                               "                          (lambda (x)"
                               "                            (+ x 1)))))"
                               "  (parameterize ((prm (prm))) (prm)))",
                               "3");
}

TEST(exec_syntax, parameterize__nested)
{
  test_eval__comp_val_with_obj("(let ((prm (make-parameter 1)))"
                               "  (parameterize ((prm 10))"
                               "    (parameterize ((prm 100))"
                               "      (prm))))",
                               "100");
}

TEST(exec_syntax, parameterize__continuation__exit_from_parameterize)
{
  test_eval__comp_val_with_obj("(let ((prm (make-parameter 1)))"
                               "  (parameterize ((prm 10))"
                               "    (call/cc"
                               "     (lambda (k)"
                               "       (parameterize ((prm 100))"
                               "         (k #f))))"
                               "    (prm)))",
                               "10");
}

TEST(exec_syntax, parameterize__continuation__enter_parameterize)
{
  test_eval__comp_val_with_obj("(let ((prm (make-parameter 1)) (k #f))"
                               "  (let ((x (parameterize ((prm 10))"
                               "             (parameterize ((prm 100))"
                               "               (set! k (call/cc (lambda (k) k)))"
                               "               (prm)))))"
                               "    (when k (k #f))"
                               "    x))",
                               "100");
}

TEST(exec_syntax, define_global_syntax)
{
  eval_cstr("(define-syntax foo"
            "  (er-macro-transformer"
            "     (lambda (f r c) '(cons 'a 'b))))");

  test_eval__comp_val_with_obj("(foo)",
                               "(a . b)");
}

TEST(exec_syntax, define_global_syntax__dont_shadow_variable_references__macro)
{
  eval_cstr("(define-syntax foo"
            "  (er-macro-transformer"
            "    (lambda (f r c) `(,(r 'cons) 'a 'b))))");

  test_eval__comp_val_with_obj("(let ((cons list)) (foo))",
                               "(a . b)");
}

TEST(exec_syntax, define_global_syntax__dont_shadow_keyword_references__macro)
{
  eval_cstr("(define-syntax foo"
            "  (er-macro-transformer"
            "    (lambda (f r c) `(,(r 'define) x 123))))");

  test_eval__comp_val_with_obj("(let ((define list))"
                               "  (foo)"
                               "  x)",
                               "123");
}

TEST(exec_syntax, define_global_syntax__dont_shadow_variable_references__user_form)
{
  eval_cstr("(define-syntax swap!"
            "  (er-macro-transformer"
            "    (lambda (f r c)"
            "       (let ((a (list-ref f 1))"
            "             (b (list-ref f 2)))"
            "          `(,(r 'let) ((,(r 'tmp) ,a))"
            "              (,(r 'set!) ,a ,b)"
            "              (,(r 'set!) ,b ,(r 'tmp)))))))");

  test_eval__comp_val_with_obj("(let ((tmp 1) (x 2))"
                               "  (swap! tmp x)"
                               "  (cons tmp x))",
                               "(2 . 1)");
}

TEST(exec_syntax, let_syntax)
{
  test_eval__comp_val_with_obj("(let ((foo cons))"
                               "  (let-syntax ((bar (er-macro-transformer"
                               "                      (lambda (f r c)"
                               "                         `(,(r 'foo) 1 2)))))"
                               "    (bar)))",
                               "(1 . 2)");
}

TEST(exec_syntax, let_syntax__dont_shadow_variable_references__macro)
{
  test_eval__comp_val_with_obj("(let ((foo cons))"
                               "  (let-syntax ((bar (er-macro-transformer"
                               "                      (lambda (f r c)"
                               "                         `(,(r 'foo) 1 2)))))"
                               "    (let ((foo list))"
                               "      (bar))))",
                               "(1 . 2)");
}

TEST(exec_syntax, letrec_syntax)
{
  test_eval__comp_val_with_obj("(let-syntax ((foo (er-macro-transformer"
                               "                    (lambda (f r c)"
                               "                      `(,(r 'cons) 1 2)))))"
                               "  (letrec-syntax ((foo (er-macro-transformer"
                               "                         (lambda (f r c)"
                               "                            `(,(r 'list) 1 2))))"
                               "                  (bar (er-macro-transformer"
                               "                         (lambda (f r c)"
                               "                            `(,(r 'foo))))))"
                               "    (bar)))",
                               "(1 2)");
}

TEST(exec_syntax, let_syntax__dont_shadow_keyword_references__macro)
{
  test_eval__comp_val_with_obj("(let-syntax ((foo (er-macro-transformer"
                               "                    (lambda (f r c)"
                               "                      `(,(r 'cons) 1 2)))))"
                               "  (let-syntax ((bar (er-macro-transformer"
                               "                      (lambda (f r c)"
                               "                         `(,(r 'foo))))))"
                               "    (let-syntax ((foo (er-macro-transformer"
                               "                        (lambda (f r c)"
                               "                           `(,(r 'list) 1 2)))))"
                               "      (bar))))",
                               "(1 . 2)");
}

TEST(exec_syntax, define_record_type__type_name_is_bound_to_record_type_obj)
{
  test_eval__chk_val_type("(begin"
                          "  (define-record-type <pare>"
                          "     (kons x y)"
                          "     pare?"
                          "     (x kar set-kar!)"
                          "     (y kdr))"
                          "  <pare>)",
                          scm_recordtype_p);
}

TEST(exec_syntax, define_record_type__constructor_and_predicate)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (pare? (kons 1 2)))",
                               "#t");
}

TEST(exec_syntax, define_record_type__predicate__return_false)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (pare? (cons 1 2)))",
                               "#f");
}

TEST(exec_syntax, define_record_type__accessor_1)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (kar (kons 1 2)))",
                               "1");
}

TEST(exec_syntax, define_record_type__accessor_2)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (kdr (kons 1 2)))",
                               "2");
}

TEST(exec_syntax, define_record_type__constuctor_arguments_1)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons y x)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (kar (kons 1 2)))",
                               "2");
}

TEST(exec_syntax, define_record_type__constuctor_arguments_2)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (kar (kons 1)))",
                               "1");
}

TEST(exec_syntax, define_record_type__constuctor_arguments_3)
{
  test_eval__chk_val_type("(begin"
                          "  (define-record-type <pare>"
                          "     (kons x)"
                          "     pare?"
                          "     (x kar set-kar!)"
                          "     (y kdr))"
                          "  (kdr (kons 1)))",
                          scm_undef_p);
}

TEST(exec_syntax, define_record_type__modifier)
{
  test_eval__comp_val_with_obj("(begin"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (let ((k (kons 1 2)))"
                               "     (set-kar! k 3)"
                               "     (kar k)))",
                               "3");
}

TEST(exec_syntax, define_record_type__internal_definition_1)
{
  test_eval__chk_val_type("(let ()"
                          "  (define-record-type <pare>"
                          "     (kons x y)"
                          "     pare?"
                          "     (x kar set-kar!)"
                          "     (y kdr))"
                          "  <pare>)",
                          scm_recordtype_p);
}

TEST(exec_syntax, define_record_type__internal_definition_2)
{
  test_eval__comp_val_with_obj("(let ()"
                               "  (define-record-type <pare>"
                               "     (kons x y)"
                               "     pare?"
                               "     (x kar set-kar!)"
                               "     (y kdr))"
                               "  (pare? (kons 1 2)))",
                               "#t");
}
