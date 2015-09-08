
#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_compiler);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_compiler)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_compiler)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

static void
test_quasiquote_internal(ScmObj expected, ScmObj template, ScmObj values)
{
  ScmObj qq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &template, &values,
                      &qq);

  qq = scm_fcd_compile_qq_template(template);
  TEST_ASSERT(scm_fcd_qqtmpl_p(qq));
  TEST_ASSERT_SCM_EQUAL(expected, scm_fcd_substitute_qq_template(qq, values));
}

static void
test_quasiquote(const char *expected, const char *template, const char *values)
{
  ScmObj e = SCM_OBJ_INIT, t = SCM_OBJ_INIT, v = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&e, &t, &v);

  e = ut_read_cstr(expected);
  t = ut_read_cstr(template);
  v = ut_read_cstr(values);

  test_quasiquote_internal(e, t, v);
}

static void
test_quasiquote_compile_error(const char *template)
{
  ScmObj t = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&t);

  t = ut_read_cstr(template);

  TEST_ASSERT_SCM_NULL(scm_fcd_compile_qq_template(t));
}

static void
test_quasiquote_substitute_error_internal(ScmObj template, ScmObj values)
{
  ScmObj qq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&template, &values,
                      &qq);

  qq = scm_fcd_compile_qq_template(template);
  TEST_ASSERT(scm_fcd_qqtmpl_p(qq));
  TEST_ASSERT_SCM_NULL(scm_fcd_substitute_qq_template(qq, values));
}

static void
test_quasiquote_substitute_error(const char *template, const char *values)
{
  ScmObj t = SCM_OBJ_INIT, v = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&t, &v);

  t = ut_read_cstr(template);
  v = ut_read_cstr(values);

  test_quasiquote_substitute_error_internal(t, v);
}

static void
test_quasiquote_unquoted_expr(const char *expected, const char *template)
{
  ScmObj qq = SCM_OBJ_INIT, e = SCM_OBJ_INIT, t = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&qq, &e, &t);

  e = ut_read_cstr(expected);
  t = ut_read_cstr(template);

  qq = scm_fcd_compile_qq_template(t);
  TEST_ASSERT(scm_fcd_qqtmpl_p(qq));

  TEST_ASSERT_EQUAL_INT(scm_fcd_vector_length(e),
                        scm_fcd_qqtmpl_nr_unquoted_expr(qq));

  n = scm_fcd_vector_length(e);
  for (size_t i = 0; i < n; i++) {
    TEST_ASSERT_SCM_EQUAL(scm_fcd_vector_ref(e, i),
                          scm_fcd_qqtmpl_unquoted_expr(qq, i));
  }
}

static void
test_quasiquote_original_template(const char *template)
{
  ScmObj qq = SCM_OBJ_INIT, t = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&qq, &t);

  t = ut_read_cstr(template);

  qq = scm_fcd_compile_qq_template(t);
  TEST_ASSERT(scm_fcd_qqtmpl_p(qq));

  TEST_ASSERT_SCM_EQUAL(t, scm_fcd_qqtmpl_template(qq));
}

TEST(fcd_compiler, quasiquote__literal)
{
  test_quasiquote("a", "a", "()");
}

TEST(fcd_compiler, quasiquote__literal__list)
{
  test_quasiquote("(a b c)", "(a b c)", "()");
}

TEST(fcd_compiler, quasiquote__literal__improper_list)
{
  test_quasiquote("(a b . c)", "(a b . c)", "()");
}

TEST(fcd_compiler, quasiquote__literal__vector)
{
  test_quasiquote("#(a b c)", "#(a b c)", "()");
}

TEST(fcd_compiler, quasiquote__unquote)
{
  test_quasiquote("a", ",expr", "(a)");
}

TEST(fcd_compiler, quasiquote__unquote__list)
{
  test_quasiquote("(a b c)", "(a ,expr c)", "(b)");
}

TEST(fcd_compiler, quasiquote__unquote__list_2)
{
  test_quasiquote("(a b c)", "(a ,expr ,expr)", "(b c)");
}

TEST(fcd_compiler, quasiquote__unquote__improper_list)
{
  test_quasiquote("(a b . c)", "(a b . ,expr)", "(c)");
}

TEST(fcd_compiler, quasiquote__unquote__vector)
{
  test_quasiquote("#(a b c)", "#(a ,expr c)", "(b)");
}

TEST(fcd_compiler, quasiquote__unquote__vector_2)
{
  test_quasiquote("#(a b c)", "#(a ,expr ,expr)", "(b c)");
}

TEST(fcd_compiler, quasiquote__unquote_splicing__list)
{
  test_quasiquote("(a b c d)", "(a ,@expr d)", "((b c))");
}

TEST(fcd_compiler, quasiquote__unquote_splicing__vector)
{
  test_quasiquote("#(a b c d)", "#(a ,@expr d)", "((b c))");
}

TEST(fcd_compiler, quasiquote__nest)
{
  test_quasiquote("(a `(,b ,c d) e)",
                  "(a `(,b ,,expr d) e)",
                  "(c)");
}

TEST(fcd_compiler, quasiquote__invalid_unquote_splicing__ERROR)
{
  test_quasiquote_compile_error(",@expr");
}

TEST(fcd_compiler, quasiquote__invalid_unquote_splicing_list__ERROR)
{
  test_quasiquote_compile_error("(a b . ,@expr)");
}

TEST(fcd_compiler, quasiquote__too_few_values__ERROR)
{
  test_quasiquote_substitute_error("(a ,expr ,expr)", "(b)");
}

TEST(fcd_compiler, quasiquote_original_template)
{
  test_quasiquote_original_template("(a ,expr c)");
}

TEST(fcd_compiler, quasiquote_unquoted_expr__empty)
{
  test_quasiquote_unquoted_expr("#()", "(a b c)");
}

TEST(fcd_compiler, quasiquote_unquoted_expr__unquote)
{
  test_quasiquote_unquoted_expr("#(b (+ 1 2))", "(a ,b ,(+ 1 2) c)");
}

TEST(fcd_compiler, quasiquote_unquoted_expr__unquote_splicing)
{
  test_quasiquote_unquoted_expr("#('(b) (list 1 2))",
                                "(a ,@'(b) ,@(list 1 2) c)");
}

static void
test_qqtmpl_eq__equal__internal(ScmObj qq1, ScmObj qq2)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_qqtmpl_eq(qq1, qq2, &actual));
  TEST_ASSERT_TRUE(actual);
}

static void
test_qqtmpl_eq__not_equal__internal(ScmObj qq1, ScmObj qq2)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_qqtmpl_eq(qq1, qq2, &actual));
  TEST_ASSERT_FALSE(actual);
}

static void
test_qqtmpl_eq__internal(const char *tmpl1, const char *tmpl2,
                         void (*test)(ScmObj, ScmObj))
{
  ScmObj t1 = SCM_OBJ_INIT, t2 = SCM_OBJ_INIT;
  ScmObj q1 = SCM_OBJ_INIT, q2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&t1, &t2, &q1, &q2);

  t1 = ut_read_cstr(tmpl1);
  t2 = ut_read_cstr(tmpl2);
  q1 = scm_fcd_compile_qq_template(t1);
  q2 = scm_fcd_compile_qq_template(t2);

  test(q1, q2);
}

static void
test_qqtmpl_eq__equal(const char *tmpl)
{
  test_qqtmpl_eq__internal(tmpl, tmpl,
                           test_qqtmpl_eq__equal__internal);
}

static void
test_qqtmpl_eq__not_equal(const char *tmpl1, const char *tmpl2)
{
  test_qqtmpl_eq__internal(tmpl1, tmpl2,
                           test_qqtmpl_eq__not_equal__internal);
}

TEST(fcd_compiler, qqtmpl_eq__equal__literal)
{
  test_qqtmpl_eq__equal("(a b c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__literal)
{
  test_qqtmpl_eq__not_equal("(a b c)", "(a b 1)");
}

TEST(fcd_compiler, qqtmpl_eq__equal__unquote)
{
  test_qqtmpl_eq__equal(",expr");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote)
{
  test_qqtmpl_eq__not_equal(",expr", ",1");
}

TEST(fcd_compiler, qqtmpl_eq__equal__unquote__list)
{
  test_qqtmpl_eq__equal("(a ,expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote__list)
{
  test_qqtmpl_eq__not_equal("(a ,expr c)", "(a b ,expr)");
}

TEST(fcd_compiler, qqtmpl_eq__equal__unquote__vector)
{
  test_qqtmpl_eq__equal("#(a ,expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote__vector)
{
  test_qqtmpl_eq__not_equal("#(a ,expr c)", "#(a b ,expr)");
}

TEST(fcd_compiler, qqtmpl_eq__equal__unquote_splicing__list)
{
  test_qqtmpl_eq__equal("(a ,@expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote_splicing__list)
{
  test_qqtmpl_eq__not_equal("(a ,@expr c)", "(a b ,@expr)");
}

TEST(fcd_compiler, qqtmpl_eq__equal__unquote_splicing__vector)
{
  test_qqtmpl_eq__equal("#(a ,@expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote_splicing__vector)
{
  test_qqtmpl_eq__not_equal("#(a ,@expr c)", "#(a b ,@expr)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__literal_unquote)
{
  test_qqtmpl_eq__not_equal("(a (x y z) ,expr)", "(a ,expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__literal_unquote_splicing)
{
  test_qqtmpl_eq__not_equal("(a (x y z) ,@expr)", "(a ,@expr c)");
}

TEST(fcd_compiler, qqtmpl_eq__not_equal__unquote_unquote_splicing)
{
  test_qqtmpl_eq__not_equal("(a ,expr c)", "(a ,@expr c)");
}
