
#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_characters);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_characters)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_characters)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(fcd_characters, fcd_char_p__return_true)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = ut_read_cstr("#\\a");

  TEST_ASSERT_TRUE(scm_fcd_char_p(chr));
}

TEST(fcd_characters, fcd_char_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_char_p(SCM_TRUE_OBJ));
}

TEST(fcd_characters, fcd_char_P__return_true)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_P(chr));
}

TEST(fcd_characters, fcd_char_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_char_P(SCM_TRUE_OBJ));
}

TEST(fcd_characters, fcd_char_eq__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool cmp;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_eq(c1, c2, &cmp));
  TEST_ASSERT_TRUE(cmp);
}

TEST(fcd_characters, fcd_char_eq__not_equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool cmp;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_eq(c1, c2, &cmp));
  TEST_ASSERT_FALSE(cmp);
}

TEST(fcd_characters, fcd_char_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\a #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P_lst(lst));
}

TEST(fcd_characters, fcd_har_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\a #\\z)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_eq_P_lst(lst));
}

TEST(fcd_characters, fcd_char_eq_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_characters, fcd_char_eq_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P(c1, c2));
}

TEST(fcd_characters, fcd_char_eq_P__not_equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_eq_P(c1, c2));
}

TEST(fcd_characters, fcd_char_lt__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_lt__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_lt__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_lt__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\c");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_lt(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_lt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\c #\\a)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_lt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_lt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_characters, fcd_char_lt_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_lt_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_lt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_lt_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_lt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_lt_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\c");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_lt_P(c1, c3));
}

TEST(fcd_characters, fcd_char_gt__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_gt__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_gt__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_gt__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\c");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_gt(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_gt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_gt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P_lst(lst));
}

TEST(fcd_characters, fcd_char_gt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_characters, fcd_char_gt_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_gt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_gt_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_gt_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_gt_P(c1, c2));
}

TEST(fcd_characters, fcd_char_gt_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\c");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_gt_P(c1, c3));
}

TEST(fcd_characters, fcd_char_le__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_le__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_le__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_le__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\c");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_le(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P_lst(lst));
}

TEST(fcd_characters, fcd_char_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P_lst(lst));
}

TEST(fcd_characters, fcd_char_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\c #\\a)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_le_P_lst(lst));
}

TEST(fcd_characters, fcd_char_le_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_characters, fcd_char_le_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P(c1, c2));
}

TEST(fcd_characters, fcd_char_le_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_le_P(c1, c2));
}

TEST(fcd_characters, fcd_char_le_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P(c1, c2));
}

TEST(fcd_characters, fcd_char_le_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\c");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_le_P(c1, c3));
}

TEST(fcd_characters, fcd_char_ge__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c1, c2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_characters, fcd_char_ge__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_ge__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_ge__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\c");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c1, c2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c2, c3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_char_ge(c1, c3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_characters, fcd_char_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\b #\\a #\\c)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_ge_P_lst(lst));
}

TEST(fcd_characters, fcd_char_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\b)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P_lst(lst));
}

TEST(fcd_characters, fcd_char_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\c #\\b #\\a)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P_lst(lst));
}

TEST(fcd_characters, fcd_char_ge_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_characters, fcd_char_ge_P__less)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_char_ge_P(c1, c2));
}

TEST(fcd_characters, fcd_char_ge_P__greater)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\b");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P(c1, c2));
}

TEST(fcd_characters, fcd_char_ge_P__equal)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2);

  c1 = ut_read_cstr("#\\a");
  c2 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P(c1, c2));
}

TEST(fcd_characters, fcd_char_ge_P__transitive)
{
  ScmObj c1 = SCM_OBJ_INIT, c2 = SCM_OBJ_INIT, c3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&c1, &c2, &c3);

  c1 = ut_read_cstr("#\\c");
  c2 = ut_read_cstr("#\\b");
  c3 = ut_read_cstr("#\\a");

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P(c1, c2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P(c2, c3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_char_ge_P(c1, c3));
}

TEST(fcd_characters, fcd_char_to_integer)
{
  ScmObj chr = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr, &actual, &expected);

  chr = ut_read_cstr("#\\a");
  expected = ut_read_cstr("97");

  actual = scm_fcd_char_to_integer(chr);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_characters, fcd_integer_to_char)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = ut_read_cstr("97");
  expected = ut_read_cstr("#\\a");

  actual = scm_fcd_integer_to_char(num, NULL);

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P(expected, actual));
}

TEST(fcd_characters, fcd_integer_to_char__not_unicode_scalar__return_false)
{
  ScmObj num = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &actual, &expected);

  num = ut_read_cstr("55296");  /* 55295 = 0xd800  */

  TEST_ASSERT_SCM_FALSE(scm_fcd_integer_to_char(num, NULL));
}
