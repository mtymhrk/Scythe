#include "symbol.c"

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/string.h"
#include "scythe/symbol.h"

#include "test.h"

TEST_GROUP(symbol);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(symbol)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(symbol)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(symbol, symbol_p__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  sym = ut_read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_symbol_p(sym));
}

TEST(symbol, symbol_p__return_false)
{
  TEST_ASSERT_FALSE(scm_symbol_p(SCM_TRUE_OBJ));
}

TEST(symbol, symbol_P__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  sym = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_symbol_P(sym));
}

TEST(symbol, symbol_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_symbol_P(SCM_TRUE_OBJ));
}

TEST(symbol, symbol_eq_p__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_symbol_eq_p(sym1, sym2));
}

TEST(symbol, symbol_eq_p__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_FALSE(scm_symbol_eq_p(sym1, sym2));
}

TEST(symbol, symbol_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa aaa aaa)");

  TEST_ASSERT_SCM_TRUE(scm_symbol_eq_P_lst(lst));
}

TEST(symbol, symbol_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa aaa zzz)");

  TEST_ASSERT_SCM_FALSE(scm_symbol_eq_P_lst(lst));
}

TEST(symbol, symbol_eq_P_lst__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = SCM_NIL_OBJ;

  TEST_ASSERT_SCM_TRUE(scm_symbol_eq_P_lst(lst));
}

TEST(symbol, symbol_eq_P_lst__list_has_item_is_not_symbol__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa \"aaa\" aaa)");

  TEST_ASSERT_SCM_NULL(scm_symbol_eq_P_lst(lst));
}

TEST(symbol, symbol_eq_P__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_symbol_eq_P(sym1, sym2));
}

TEST(symbol, symbol_eq_P__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_symbol_eq_P(sym1, sym2));
}

TEST(symbol, symbol_to_string)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &actual, &expected);

  sym = ut_read_cstr("aaa");
  expected = ut_read_cstr("\"aaa\"");

  actual = scm_symbol_to_string(sym);

  TEST_ASSERT_SCM_TRUE(scm_string_eq_P(expected, actual));

  /* TODO: actual が immutable かチェック */
}

TEST(symbol, string_to_symbol)
{
  ScmObj str = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = ut_read_cstr("\"aaa\"");
  expected = ut_read_cstr("aaa");

  actual = scm_string_to_symbol(str);

  TEST_ASSERT_SCM_EQ(expected, actual);
}
