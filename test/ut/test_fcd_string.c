#include "fcd_string.c"

#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(fcd_string);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_string)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_string)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_string, fcd_string_p__return_true)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_TRUE(scm_fcd_string_p(str));
}

TEST(fcd_string, fcd_string_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_string_p(SCM_TRUE_OBJ));
}

TEST(fcd_string, fcd_string_P__return_true)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_P(str));
}

TEST(fcd_string, fcd_string_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_string_P(SCM_TRUE_OBJ));
}

TEST(fcd_string, fcd_string_lst)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(#\\a #\\b #\\c)");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_fcd_string_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_lst__not_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_lst(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_lst__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(#\\a #\\b . #\\c)");
  expected = scm_fcd_make_string_from_cstr("ab", SCM_ENC_UTF8);

  actual = scm_fcd_string_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_lst__list_has_a_object_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a () #\\c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_lst(lst));
}

TEST(fcd_string, fcd_string_cv)
{
  ScmObj chr[] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(chr, sizeof(chr)/sizeof(chr[0]));

  chr[0] = read_cstr("#\\a");
  chr[1] = read_cstr("#\\b");
  chr[2] = read_cstr("#\\c");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_fcd_string_cv(chr, sizeof(chr)/sizeof(chr[0]));

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_cv__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_cv(NULL, 0);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_cv__ary_has_item_is_not_character__return_ERROR)
{
  ScmObj chr[] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(chr, sizeof(chr)/sizeof(chr[0]));

  chr[0] = read_cstr("#\\a");
  chr[1] = SCM_FALSE_OBJ;
  chr[2] = read_cstr("#\\c");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_cv(chr, sizeof(chr)/sizeof(chr[0])));
}

TEST(fcd_string, fcd_string)
{
  ScmObj chr[] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(chr, sizeof(chr)/sizeof(chr[0]));

  chr[0] = read_cstr("#\\a");
  chr[1] = read_cstr("#\\b");
  chr[2] = read_cstr("#\\c");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_fcd_string(3, chr[0], chr[1], chr[2]);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string(0);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string__not_character)
{
  ScmObj chr[] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(chr, sizeof(chr)/sizeof(chr[0]));

  chr[0] = read_cstr("#\\a");
  chr[1] = SCM_FALSE_OBJ;
  chr[2] = read_cstr("#\\c");

  TEST_ASSERT_SCM_NULL(scm_fcd_string(3, chr[0], chr[1], chr[2]));
}

TEST(fcd_string, fcd_string_length)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(3, scm_fcd_string_length(str));
}

TEST(fcd_string, fcd_string_length__multi_byte)
{
  uint32_t ucs4[] = { 'a', 'b', 'c' };
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_bin(ucs4, sizeof(ucs4), SCM_ENC_UCS4);

  TEST_ASSERT_EQUAL_INT(3, scm_fcd_string_length(str));
}

TEST(fcd_string, fcd_string_bytesize)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(3, scm_fcd_string_bytesize(str));
}

TEST(fcd_string, fcd_string_bytesize__multi_byte)
{
  uint32_t ucs4[] = { 'a', 'b', 'c' };
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_bin(ucs4, sizeof(ucs4), SCM_ENC_UCS4);

  TEST_ASSERT_EQUAL_INT(12, scm_fcd_string_bytesize(str));
}

TEST(fcd_string, fcd_string_ref)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str,
                      &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  expected = read_cstr("#\\b");

  actual = scm_fcd_string_ref(str, 1);

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_set_i)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &chr);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("azc", SCM_ENC_UTF8);
  chr = read_cstr("#\\z");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_set_i(str, 1, chr));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(fcd_string, fcd_string_eq__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_eq(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_eq__not_euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_eq(s1, s2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"abc\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P_lst(lst));
}

TEST(fcd_string, fcd_string_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"abc\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_eq_P_lst(lst));
}

TEST(fcd_string, fcd_string_eq_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_string, fcd_string_eq_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" abc \"def\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_eq_P_lst(lst));
}

TEST(fcd_string, fcd_string_eq_P__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(s1, s2));
}

TEST(fcd_string, fcd_string_eq_P__not_euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_eq_P(s1, s2));
}

TEST(fcd_string, fcd_string_lt__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_lt__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s2, s1, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_lt__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s1, s2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_lt__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s2, s3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_lt(s1, s3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"def\" \"ghi\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P_lst(lst));
}

TEST(fcd_string, fcd_string_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"def\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_lt_P_lst(lst));
}

TEST(fcd_string, fcd_string_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"def\" \"ghi\" \"abc\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_lt_P_lst(lst));
}

TEST(fcd_string, fcd_string_lt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_string, fcd_string_lt_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_lt_P_lst(lst));
}

TEST(fcd_string, fcd_string_lt_P__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P(s1, s2));
}

TEST(fcd_string, fcd_string_lt_P__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_lt_P(s2, s1));
}

TEST(fcd_string, fcd_string_lt_P__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_lt_P(s1, s2));
}

TEST(fcd_string, fcd_string_lt_P__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P(s1, s2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P(s2, s3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_lt_P(s1, s3));
}

TEST(fcd_string, fcd_string_gt__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s1, s2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_gt__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s2, s1, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_gt__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s1, s2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_gt__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s3, s2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s2, s1, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_gt(s3, s1, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"def\" \"abc\" \"ghi\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_gt_P_lst(lst));
}

TEST(fcd_string, fcd_string_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" \"def\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_gt_P_lst(lst));
}

TEST(fcd_string, fcd_string_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P_lst(lst));
}

TEST(fcd_string, fcd_string_gt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_string, fcd_string_gt_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" def \"abc\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_gt_P_lst(lst));
}

TEST(fcd_string, fcd_string_gt_P__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_gt_P(s1, s2));
}

TEST(fcd_string, fcd_string_gt_P__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P(s2, s1));
}

TEST(fcd_string, fcd_string_gt_P__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_gt_P(s1, s2));
}

TEST(fcd_string, fcd_string_gt_P__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P(s3, s2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P(s2, s1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_gt_P(s3, s1));
}

TEST(fcd_string, fcd_string_le__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_le__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s2, s1, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_le__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_le__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s2, s3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_le(s1, s3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"def\" \"ghi\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P_lst(lst));
}

TEST(fcd_string, fcd_string_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" \"def\" \"def\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P_lst(lst));
}

TEST(fcd_string, fcd_string_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_le_P_lst(lst));
}

TEST(fcd_string, fcd_string_le_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_string, fcd_string_le_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_le_P_lst(lst));
}

TEST(fcd_string, fcd_string_le_P__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P(s1, s2));
}

TEST(fcd_string, fcd_string_le_P__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_le_P(s2, s1));
}

TEST(fcd_string, fcd_string_le_P__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P(s1, s2));
}

TEST(fcd_string, fcd_string_le_P__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P(s1, s2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P(s2, s3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_le_P(s1, s3));
}

TEST(fcd_string, fcd_string_ge__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s1, s2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_string, fcd_string_ge__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s2, s1, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_ge__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;
  bool actual = true;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s1, s2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_ge__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;
  bool actual = false;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s3, s2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s2, s1, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_ge(s3, s1, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_string, fcd_string_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"def\" \"abc\" \"ghi\")");

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_ge_P_lst(lst));
}

TEST(fcd_string, fcd_string_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" \"def\" \"def\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P_lst(lst));
}

TEST(fcd_string, fcd_string_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P_lst(lst));
}

TEST(fcd_string, fcd_string_ge_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P_lst(SCM_NIL_OBJ));
}

TEST(fcd_string, fcd_string_ge_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = read_cstr("(\"ghi\" def \"abc\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_ge_P_lst(lst));
}

TEST(fcd_string, fcd_string_ge_P__less)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_FALSE(scm_fcd_string_ge_P(s1, s2));
}

TEST(fcd_string, fcd_string_ge_P__greater)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P(s2, s1));
}

TEST(fcd_string, fcd_string_ge_P__euqal)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P(s1, s2));
}

TEST(fcd_string, fcd_string_ge_P__transitive)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P(s3, s2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P(s2, s1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_ge_P(s3, s1));
}

TEST(fcd_string, fcd_upcase)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcDEF!?", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("ABCDEF!?", SCM_ENC_UTF8);

  actual = scm_fcd_string_upcase(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_downcase)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcDEF!?", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("abcdef!?", SCM_ENC_UTF8);

  actual = scm_fcd_string_downcase(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_substring)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdefg", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("cde", SCM_ENC_UTF8);

  actual = scm_fcd_substring(str, 2, 5);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append_lst)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(\"abc\" \"def\" \"ghi\")");
  expected = scm_fcd_make_string_from_cstr("abcdefghi", SCM_ENC_UTF8);

  actual = scm_fcd_string_append_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append_lst__list_has_item_is_not_string)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_fcd_string_append_lst(lst));
}

TEST(fcd_string, fcd_string_append_cv)
{
  ScmObj str[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(str, sizeof(str)/sizeof(str[0]));

  str[0] = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  str[1] = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  str[2] = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  expected = scm_fcd_make_string_from_cstr("abcdefghi", SCM_ENC_UTF8);

  actual = scm_fcd_string_append_cv(str, sizeof(str)/sizeof(str[0]));

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append_cv__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_append_cv(NULL, 0);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append_cv__return_ERROR)
{
  ScmObj str[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(str, sizeof(str)/sizeof(str[0]));

  str[0] = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  str[1] = SCM_EOF_OBJ;
  str[2] = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_fcd_string_append_cv(str, sizeof(str)/sizeof(str[0])));
}

TEST(fcd_string, fcd_string_append)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3, &actual, &expected);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = scm_fcd_make_string_from_cstr("def", SCM_ENC_UTF8);
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  expected = scm_fcd_make_string_from_cstr("abcdefghi", SCM_ENC_UTF8);

  actual = scm_fcd_string_append(3, s1, s2, s3);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_append(0);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_append__return_ERROR)
{
  ScmObj s1 = SCM_OBJ_INIT, s2 = SCM_OBJ_INIT, s3 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s1, &s2, &s3);

  s1 = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  s2 = SCM_EOF_OBJ;
  s3 = scm_fcd_make_string_from_cstr("ghi", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_fcd_string_append(3, s1, s2, s3));
}

TEST(fcd_string, fcd_string_to_list__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = read_cstr("(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_fcd_string_to_list(str, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_string, fcd_string_to_list__specify_start)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = read_cstr("(#\\b #\\c #\\d #\\e)");

  actual = scm_fcd_string_to_list(str, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_string, fcd_string_to_list__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = read_cstr("(#\\b #\\c #\\d)");

  actual = scm_fcd_string_to_list(str, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_string, fcd_string_to_list__same_index__return_empty_list)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = SCM_NIL_OBJ;

  actual = scm_fcd_string_to_list(str, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_string, fcd_list_to_string)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(#\\a #\\b #\\c)");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_fcd_list_to_string(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_list_to_string__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_list_to_string(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_list_to_string__not_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_list_to_string(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_list_to_string__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(#\\a #\\b . #\\c)");
  expected = scm_fcd_make_string_from_cstr("ab", SCM_ENC_UTF8);

  actual = scm_fcd_list_to_string(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_list_to_string__list_has_a_object_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(#\\a () #\\c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_list_to_string(lst));
}

TEST(fcd_string, fcd_string_copy__unspecify_stat_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);

  actual = scm_fcd_string_copy(str, -1, -1);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_copy__specify_stat)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("bcdef", SCM_ENC_UTF8);

  actual = scm_fcd_string_copy(str, 1, -1);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_copy__specify_stat_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("bcde", SCM_ENC_UTF8);

  actual = scm_fcd_string_copy(str, 1, 5);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_copy__same_index__return_empty_string)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_fcd_string_copy(str, 1, 1);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_string, fcd_string_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("1abcd", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 1, from, -1, -1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("1cde5", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 1, from, 2, -1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("1cd45", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 1, from, 2, 4));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 1, from, 2, 2));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("13445", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 1, to, 2, 4));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("12235", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_copy_i(to, 2, to, 1, 3));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}


TEST(fcd_string, fcd_string_copy_i__too_many_characters_to_be_copied__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  /* error if (- (string-length to) at) is less than (- end start) */
  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_string_copy_i(to, 2, from, 1, 5));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(fcd_string, fcd_string_fill_i__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = read_cstr("#\\z");
  expected = scm_fcd_make_string_from_cstr("zzzzz", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_fill_i(str, chr, -1, -1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(fcd_string, fcd_string_fill_i__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = read_cstr("#\\z");
  expected = scm_fcd_make_string_from_cstr("azzzz", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_fill_i(str, chr, 1, -1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(fcd_string, fcd_string_fill_i__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = read_cstr("#\\z");
  expected = scm_fcd_make_string_from_cstr("azzze", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_fill_i(str, chr, 1, 4));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(fcd_string, fcd_string_fill_i__same_idx)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = read_cstr("#\\z");
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_string_fill_i(str, chr, 1, 1));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}
