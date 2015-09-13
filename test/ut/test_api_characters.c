#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/number.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_characters);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_characters)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_characters)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_characters, api_char_P__return_true)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_P(chr));
}

TEST(api_characters, api_char_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_char_P(SCM_TRUE_OBJ));
}

TEST(api_characters, api_char_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\a #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_eq_P_lst(lst));
}

TEST(api_characters, capi_har_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\a #\\z)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_eq_P_lst(lst));
}

TEST(api_characters, api_char_eq_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_char_eq_P_lst(SCM_NIL_OBJ));
}

TEST(api_characters, api_char_eq_P__list_has_item_is_not_character__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a a #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_eq_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\c #\\a)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P_lst(SCM_NIL_OBJ));
}

TEST(api_characters, api_char_lt_P_lst__list_has_item_is_not_character__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a b #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P_lst(SCM_NIL_OBJ));
}

TEST(api_characters, api_char_gt_P_lst__list_has_item_is_not_character__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c b #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\c #\\a)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P_lst(SCM_NIL_OBJ));
}

TEST(api_characters, api_char_le_P_lst__list_has_item_is_not_character__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a b #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P_lst(SCM_NIL_OBJ));
}

TEST(api_characters, api_char_ge_P_lst__list_has_item_is_not_character__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c b #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_to_integer)
{
  ScmObj chr = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr, &actual, &expected);

  chr = ut_read_cstr("#\\a");
  expected = ut_read_cstr("97");

  actual = scm_api_char_to_integer(chr);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(api_characters, api_char_to_integer__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_char_to_integer(SCM_EOF_OBJ));
}

TEST(api_characters, capi_integer_to_char)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = ut_read_cstr("97");
  expected = ut_read_cstr("#\\a");

  actual = scm_capi_integer_to_char(num, NULL);

  TEST_ASSERT_SCM_TRUE(scm_char_eq_P(expected, actual));
}

TEST(api_characters, capi_integer_to_char__not_unicode_scalar__return_false)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = ut_read_cstr("55296");  /* 55295 = 0xd800  */

  TEST_ASSERT_SCM_FALSE(scm_capi_integer_to_char(num, NULL));
}

TEST(api_characters, capi_integer_to_char__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_integer_to_char(SCM_EOF_OBJ, NULL));
}
