#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_characters);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(api_characters)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_characters)
{
  scm_capi_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(api_characters, capi_char_p__return_true)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = read_cstr("#\\a");

  TEST_ASSERT_TRUE(scm_capi_char_p(chr));
}

TEST(api_characters, capi_char_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_char_p(SCM_TRUE_OBJ));
}

TEST(api_characters, api_char_P__return_true)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_P(chr));
}

TEST(api_characters, api_char_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_char_P(SCM_TRUE_OBJ));
}

TEST(api_characters, capi_char_eq__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool cmp;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_eq(c1, c2, &cmp));
  TEST_ASSERT_TRUE(cmp);
}

TEST(api_characters, capi_char_eq__not_equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool cmp;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_eq(c1, c2, &cmp));
  TEST_ASSERT_FALSE(cmp);
}

TEST(api_characters, capi_char_eq__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool cmp;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_FALSE_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_char_eq(c1, c2, &cmp));
}

TEST(api_characters, api_char_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\a #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_eq_P_lst(lst));
}

TEST(api_characters, capi_har_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\a #\\z)");

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

  lst = read_cstr("(#\\a a #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_eq_P_lst(lst));
}

TEST(api_characters, api_char_eq_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_eq_P(c1, c2));
}

TEST(api_characters, api_char_eq_P__not_equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_char_eq_P(c1, c2));
}

TEST(api_characters, api_char_eq_P__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_char_eq_P(c1, c2));
}

TEST(api_characters, capi_char_lt__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_lt__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_lt__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_lt__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\c");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_lt(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_lt__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_char_lt(c1, c2, &actual));
}

TEST(api_characters, api_char_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\b #\\c #\\a)");

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

  lst = read_cstr("(#\\a b #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_char_lt_P_lst(lst));
}

TEST(api_characters, api_char_lt_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P(c1, c2));
}

TEST(api_characters, api_char_lt_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_api_char_lt_P(c1, c2));
}

TEST(api_characters, api_char_lt_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_api_char_lt_P(c1, c2));
}

TEST(api_characters, api_char_lt_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\c");

  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_api_char_lt_P(c1, c3));
}

TEST(api_characters, api_char_lt_P__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_char_lt_P(c1, c2));
}

TEST(api_characters, capi_char_gt__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_gt__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_gt__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_gt__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\c");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_gt(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_gt__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_char_lt(c1, c2, &actual));
}

TEST(api_characters, api_char_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\c #\\b #\\a)");

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

  lst = read_cstr("(#\\c b #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_gt_P_lst(lst));
}

TEST(api_characters, api_char_gt_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P(c1, c2));
}

TEST(api_characters, api_char_gt_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P(c1, c2));
}

TEST(api_characters, api_char_gt_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_api_char_gt_P(c1, c2));
}

TEST(api_characters, api_char_gt_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\c");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_api_char_gt_P(c1, c3));
}

TEST(api_characters, api_char_gt_P__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_char_gt_P(c1, c2));
}

TEST(api_characters, capi_char_le__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_le__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_le__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_le__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\c");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_le(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_le__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_char_le(c1, c2, &actual));
}

TEST(api_characters, api_char_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\b #\\c #\\a)");

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

  lst = read_cstr("(#\\a b #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_char_le_P_lst(lst));
}

TEST(api_characters, api_char_le_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P(c1, c2));
}

TEST(api_characters, api_char_le_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_api_char_le_P(c1, c2));
}

TEST(api_characters, api_char_le_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P(c1, c2));
}

TEST(api_characters, api_char_le_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\c");

  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_api_char_le_P(c1, c3));
}

TEST(api_characters, api_char_le_P__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_char_le_P(c1, c2));
}

TEST(api_characters, capi_char_ge__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(api_characters, capi_char_ge__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_ge__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_ge__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\c");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_char_ge(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(api_characters, capi_char_ge__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_char_le(c1, c2, &actual));
}

TEST(api_characters, api_char_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\c #\\b #\\a)");

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

  lst = read_cstr("(#\\c b #\\a)");

  TEST_ASSERT_SCM_NULL(scm_api_char_ge_P_lst(lst));
}

TEST(api_characters, api_char_ge_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_char_ge_P(c1, c2));
}

TEST(api_characters, api_char_ge_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\b");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P(c1, c2));
}

TEST(api_characters, api_char_ge_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P(c1, c2));
}

TEST(api_characters, api_char_ge_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = read_cstr("#\\c");
  c2 = read_cstr("#\\b");
  c3 = read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_api_char_ge_P(c1, c3));
}

TEST(api_characters, api_char_ge_P__return_ERROR)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = read_cstr("#\\a");
  c2 = SCM_EOF_OBJ;

  TEST_ASSERT_SCM_NULL(scm_api_char_ge_P(c1, c2));
}

TEST(api_characters, api_char_to_integer)
{
  ScmObj chr = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr, &actual, &expected);

  chr = read_cstr("#\\a");
  expected = read_cstr("97");

  actual = scm_api_char_to_integer(chr);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(api_characters, api_char_to_integer__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_char_to_integer(SCM_EOF_OBJ));
}

TEST(api_characters, capi_integer_to_char)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = read_cstr("97");
  expected = read_cstr("#\\a");

  actual = scm_capi_integer_to_char(num, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_char_eq_P(expected, actual));
}

TEST(api_characters, capi_integer_to_char__not_unicode_scalar__return_false)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = read_cstr("55296");  /* 55295 = 0xd800  */

  TEST_ASSERT_SCM_FALSE(scm_capi_integer_to_char(num, NULL));
}

TEST(api_characters, capi_integer_to_char__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_integer_to_char(SCM_EOF_OBJ, NULL));
}
