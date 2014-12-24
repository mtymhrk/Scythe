#include "fcd_symbol.c"

#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_symbol);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_symbol)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_symbol)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_symbol, fcd_symbol_p__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  sym = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_fcd_symbol_p(sym));
}

TEST(fcd_symbol, fcd_symbol_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_symbol_p(SCM_TRUE_OBJ));
}

TEST(fcd_symbol, fcd_symbol_P__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  sym = read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_fcd_symbol_P(sym));
}

TEST(fcd_symbol, fcd_symbol_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_symbol_P(SCM_TRUE_OBJ));
}

TEST(fcd_symbol, fcd_symbol_eq_p__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_fcd_symbol_eq_p(sym1, sym2));
}

TEST(fcd_symbol, fcd_symbol_eq_p__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_FALSE(scm_fcd_symbol_eq_p(sym1, sym2));
}

TEST(fcd_symbol, fcd_symbol_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(aaa aaa aaa)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_symbol_eq_P_lst(lst));
}

TEST(fcd_symbol, fcd_symbol_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(aaa aaa zzz)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_symbol_eq_P_lst(lst));
}

TEST(fcd_symbol, fcd_symbol_eq_P_lst__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = SCM_NIL_OBJ;

  TEST_ASSERT_SCM_TRUE(scm_fcd_symbol_eq_P_lst(lst));
}

TEST(fcd_symbol, fcd_symbol_eq_P_lst__list_has_item_is_not_symbol__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(aaa \"aaa\" aaa)");

  TEST_ASSERT_SCM_NULL(scm_fcd_symbol_eq_P_lst(lst));
}

TEST(fcd_symbol, fcd_symbol_eq_P__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_fcd_symbol_eq_P(sym1, sym2));
}

TEST(fcd_symbol, fcd_symbol_eq_P__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_fcd_symbol_eq_P(sym1, sym2));
}

TEST(fcd_symbol, fcd_symbol_to_string)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &actual, &expected);

  sym = read_cstr("aaa");
  expected = read_cstr("\"aaa\"");

  actual = scm_fcd_symbol_to_string(sym);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));

  /* TODO: actual が immutable かチェック */
}

TEST(fcd_symbol, fcd_string_to_symbol)
{
  ScmObj str = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = read_cstr("\"aaa\"");
  expected = read_cstr("aaa");

  actual = scm_fcd_string_to_symbol(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_eq_P(expected, actual));
}
