#include "test.h"

#include "object.h"
#include "api.h"

TEST_GROUP(symbols);

static ScmEvaluator *ev;

TEST_SETUP(symbols)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(symbols)
{
  scm_capi_evaluator_end(ev);
}

TEST(symbols, capi_symbol_p__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  sym = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_capi_symbol_p(sym));
}

TEST(symbols, capi_symbol_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_symbol_p(SCM_TRUE_OBJ));
}

TEST(symbols, api_symbol_P__return_true)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  sym = read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_api_symbol_P(sym));
}

TEST(symbols, api_symbol_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_symbol_P(SCM_TRUE_OBJ));
}

TEST(symbols, api_symbol_P__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_symbol_P(SCM_OBJ_NULL));
}

TEST(symbols, capi_symbol_eq__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_symbol_eq(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(symbols, capi_symbol_eq__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_symbol_eq(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(symbols, capi_symbol_eq__return_ERROR)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = SCM_FALSE_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_symbol_eq(sym1, sym2, &actual));
}

TEST(symbols, capi_symbol_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(aaa aaa aaa)");

  TEST_ASSERT_SCM_TRUE(scm_capi_symbol_eq_P_lst(lst));
}

TEST(symbols, capi_symbol_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(aaa aaa zzz)");

  TEST_ASSERT_SCM_FALSE(scm_capi_symbol_eq_P_lst(lst));
}

TEST(symbols, capi_symbol_eq_P_lst__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = SCM_NIL_OBJ;

  TEST_ASSERT_SCM_TRUE(scm_capi_symbol_eq_P_lst(lst));
}

TEST(symbols, capi_symbol_eq_P_lst__list_has_item_is_not_symbol__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(aaa \"aaa\" aaa)");

  TEST_ASSERT_SCM_NULL(scm_capi_symbol_eq_P_lst(lst));
}

TEST(symbols, api_symbol_eq_P__equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_api_symbol_eq_P(sym1, sym2));
}

TEST(symbols, api_symbol_eq_P__not_equal)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_api_symbol_eq_P(sym1, sym2));
}

TEST(symbols, api_symbol_eq_P__return_ERROR)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_symbol_eq_P(sym1, sym2));
}

TEST(symbols, api_symbol_to_string)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &actual, &expected);

  sym = read_cstr("aaa");
  expected = read_cstr("\"aaa\"");

  actual = scm_api_symbol_to_string(sym);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));

  /* TODO: actual が immutable かチェック */
}

TEST(symbols, api_symbol_to_string__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_symbol_to_string(SCM_EOF_OBJ));
}

TEST(symbols, api_string_to_symbol)
{
  ScmObj str = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &actual, &expected);

  str = read_cstr("\"aaa\"");
  expected = read_cstr("aaa");

  actual = scm_api_string_to_symbol(str);

  TEST_ASSERT_SCM_TRUE(scm_api_eq_P(expected, actual));
}

TEST(symbols, api_string_to_symbol__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_to_symbol(SCM_EOF_OBJ));
}
