#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/string.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_symbols);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_symbols)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_symbols)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_symbols, api_symbol_P__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  sym = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_api_symbol_P(sym));
}

TEST(api_symbols, api_symbol_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_symbol_P(SCM_TRUE_OBJ));
}

TEST(api_symbols, api_symbol_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa aaa aaa)");

  TEST_ASSERT_SCM_TRUE(scm_api_symbol_eq_P_lst(lst));
}

TEST(api_symbols, api_symbol_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa aaa zzz)");

  TEST_ASSERT_SCM_FALSE(scm_api_symbol_eq_P_lst(lst));
}

TEST(api_symbols, api_symbol_eq_P_lst__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = SCM_NIL_OBJ;

  TEST_ASSERT_SCM_TRUE(scm_api_symbol_eq_P_lst(lst));
}

TEST(api_symbols, api_symbol_eq_P_lst__list_has_item_is_not_symbol__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(aaa \"aaa\" aaa)");

  TEST_ASSERT_SCM_NULL(scm_api_symbol_eq_P_lst(lst));
}

TEST(api_symbols, api_symbol_to_string)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &actual, &expected);

  sym = ut_read_cstr("aaa");
  expected = ut_read_cstr("\"aaa\"");

  actual = scm_api_symbol_to_string(sym);

  TEST_ASSERT_SCM_TRUE(scm_string_eq_P(expected, actual));

  /* TODO: actual が immutable かチェック */
}

TEST(api_symbols, api_symbol_to_string__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_symbol_to_string(SCM_EOF_OBJ));
}

TEST(api_symbols, api_string_to_symbol)
{
  ScmObj str = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = ut_read_cstr("\"aaa\"");
  expected = ut_read_cstr("aaa");

  actual = scm_api_string_to_symbol(str);

  TEST_ASSERT_SCM_TRUE(scm_api_eq_P(expected, actual));
}

TEST(api_symbols, api_string_to_symbol__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_to_symbol(SCM_EOF_OBJ));
}
