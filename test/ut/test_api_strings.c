#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_strings);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_strings)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_strings)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_strings, api_string_P__return_true)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_TRUE(scm_api_string_P(str));
}

TEST(api_strings, api_string_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_string_P(SCM_TRUE_OBJ));
}

TEST(api_strings, api_string_lst)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_api_string_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_string_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_lst__not_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_string_lst(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_lst__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(#\\a #\\b . #\\c)");
  expected = scm_fcd_make_string_from_cstr("ab", SCM_ENC_UTF8);

  actual = scm_api_string_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_lst__list_has_a_object_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a () #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_string_lst(lst));
}

TEST(api_strings, api_string_length)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  expected = ut_read_cstr("3");

  actual = scm_api_string_length(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_strings, api_string_length__multi_byte)
{
  uint32_t ucs4[] = { 'a', 'b', 'c' };
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_bin(ucs4, sizeof(ucs4), SCM_ENC_UCS4);
  expected = ut_read_cstr("3");

  actual = scm_api_string_length(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_strings, api_string_length__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_length(SCM_FALSE_OBJ));
}

TEST(api_strings, api_string_bytesize)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  expected = ut_read_cstr("3");

  actual = scm_api_string_bytesize(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_strings, api_string_bytesize__multi_byte)
{
  uint32_t ucs4[] = { 'a', 'b', 'c' };
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_bin(ucs4, sizeof(ucs4), SCM_ENC_UCS4);
  expected = ut_read_cstr("12");

  actual = scm_api_string_bytesize(str);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_strings, api_string_bytesize__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_bytesize(SCM_FALSE_OBJ));
}

TEST(api_strings, api_string_ref)
{
  ScmObj str = SCM_OBJ_INIT, pos = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &pos,
                      &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  pos = ut_read_cstr("1");
  expected = ut_read_cstr("#\\b");

  actual = scm_api_string_ref(str, pos);

  TEST_ASSERT_SCM_TRUE(scm_fcd_char_eq_P(expected, actual));
}

TEST(api_strings, api_string_ref__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, pos = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &pos);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  pos = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_string_ref(str, pos));
}

TEST(api_strings, api_string_ref__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_ref(SCM_EOF_OBJ, SCM_FALSE_OBJ));
}

TEST(api_strings, api_string_set_i)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj chr = SCM_OBJ_INIT, pos = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &chr, &pos);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("azc", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  pos = ut_read_cstr("1");

  TEST_ASSERT_SCM_UNDEF(scm_api_string_set_i(str, pos, chr));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_set_i__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, pos = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &pos);

  str = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  pos = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_string_set_i(str, pos, chr));
}

TEST(api_strings, api_string_set_i__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_set_i(SCM_TRUE_OBJ, SCM_EOF_OBJ, SCM_FALSE_OBJ));
}

TEST(api_strings, api_string_eq_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"abc\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P_lst(lst));
}

TEST(api_strings, api_string_eq_P_lst__not_equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"abc\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_eq_P_lst(lst));
}

TEST(api_strings, api_string_eq_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P_lst(SCM_NIL_OBJ));
}

TEST(api_strings, api_string_eq_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" abc \"def\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_eq_P_lst(lst));
}

TEST(api_strings, api_string_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"def\" \"ghi\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_lt_P_lst(lst));
}

TEST(api_strings, api_string_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"def\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_lt_P_lst(lst));
}

TEST(api_strings, api_string_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"def\" \"ghi\" \"abc\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_lt_P_lst(lst));
}

TEST(api_strings, api_string_lt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_string_lt_P_lst(SCM_NIL_OBJ));
}

TEST(api_strings, api_string_lt_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_lt_P_lst(lst));
}

TEST(api_strings, api_string_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"def\" \"abc\" \"ghi\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_gt_P_lst(lst));
}

TEST(api_strings, api_string_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" \"def\" \"def\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_gt_P_lst(lst));
}

TEST(api_strings, api_string_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_gt_P_lst(lst));
}

TEST(api_strings, api_string_gt_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_string_gt_P_lst(SCM_NIL_OBJ));
}

TEST(api_strings, api_string_gt_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" def \"abc\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_gt_P_lst(lst));
}

TEST(api_strings, api_string_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"def\" \"ghi\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_le_P_lst(lst));
}

TEST(api_strings, api_string_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" \"def\" \"def\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_le_P_lst(lst));
}

TEST(api_strings, api_string_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_le_P_lst(lst));
}

TEST(api_strings, api_string_le_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_string_le_P_lst(SCM_NIL_OBJ));
}

TEST(api_strings, api_string_le_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_le_P_lst(lst));
}

TEST(api_strings, api_string_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"def\" \"abc\" \"ghi\")");

  TEST_ASSERT_SCM_FALSE(scm_api_string_ge_P_lst(lst));
}

TEST(api_strings, api_string_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" \"def\" \"def\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_ge_P_lst(lst));
}

TEST(api_strings, api_string_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" \"def\" \"abc\")");

  TEST_ASSERT_SCM_TRUE(scm_api_string_ge_P_lst(lst));
}

TEST(api_strings, api_string_ge_P_lst__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_string_ge_P_lst(SCM_NIL_OBJ));
}

TEST(api_strings, api_string_ge_P_lst__list_has_item_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  lst = ut_read_cstr("(\"ghi\" def \"abc\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_ge_P_lst(lst));
}

TEST(api_strings, api_substring)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdefg", SCM_ENC_UTF8);
  start = ut_read_cstr("2");
  end = ut_read_cstr("5");
  expected = scm_fcd_make_string_from_cstr("cde", SCM_ENC_UTF8);

  actual = scm_api_substring(str, start, end);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_substring__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = scm_fcd_make_string_from_cstr("abcdefg", SCM_ENC_UTF8);
  start = ut_read_cstr("2");
  end = ut_read_cstr("10");

  TEST_ASSERT_SCM_NULL(scm_api_substring(str, start, end));
}

TEST(api_strings, api_substring__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_substring(SCM_TRUE_OBJ, SCM_EOF_OBJ, SCM_FALSE_OBJ));
}

TEST(api_strings, api_string_append_lst)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(\"abc\" \"def\" \"ghi\")");
  expected = scm_fcd_make_string_from_cstr("abcdefghi", SCM_ENC_UTF8);

  actual = scm_api_string_append_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_string_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_append_lst__list_has_item_is_not_string)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(\"abc\" def \"ghi\")");

  TEST_ASSERT_SCM_NULL(scm_api_string_append_lst(lst));
}

TEST(api_strings, api_string_to_list__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = ut_read_cstr("(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_list(str, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_strings, api_string_to_list__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  expected = ut_read_cstr("(#\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_list(str, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_strings, api_string_to_list__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("(#\\b #\\c #\\d)");

  actual = scm_api_string_to_list(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_strings, api_string_to_list__same_index__return_empty_list)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = SCM_NIL_OBJ;

  actual = scm_api_string_to_list(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_strings, api_string_to_list__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_list(str, start, end));
}

TEST(api_strings, api_string_to_list__start_greater_than_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  start = ut_read_cstr("2");
  end = ut_read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_list(str, start, end));
}

TEST(api_strings, api_string_to_list__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_to_list(SCM_TRUE_OBJ, SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(api_strings, api_list_to_string)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(#\\a #\\b #\\c)");
  expected = scm_fcd_make_string_from_cstr("abc", SCM_ENC_UTF8);

  actual = scm_api_list_to_string(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_list_to_string__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_list_to_string(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_list_to_string__not_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_list_to_string(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_list_to_string__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(#\\a #\\b . #\\c)");
  expected = scm_fcd_make_string_from_cstr("ab", SCM_ENC_UTF8);

  actual = scm_api_list_to_string(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_list_to_string__list_has_a_object_is_not_string__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(#\\a () #\\c)");

  TEST_ASSERT_SCM_NULL(scm_api_list_to_string(lst));
}

TEST(api_strings, api_string_copy__unspecify_stat_end)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);

  actual = scm_api_string_copy(str, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_copy__specify_stat)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("bcdef", SCM_ENC_UTF8);

  actual = scm_api_string_copy(str, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_copy__specify_stat_end)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("5");
  expected = scm_fcd_make_string_from_cstr("bcde", SCM_ENC_UTF8);

  actual = scm_api_string_copy(str, start, end);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_copy__same_index__return_empty_string)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &actual, &expected);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("", SCM_ENC_UTF8);

  actual = scm_api_string_copy(str, start, end);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_strings, api_string_copy__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  start = ut_read_cstr("1");
  end = ut_read_cstr("7");

  TEST_ASSERT_SCM_NULL(scm_api_string_copy(str, start, end));
}

TEST(api_strings, api_string_copy__start_greater_then_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = scm_fcd_make_string_from_cstr("abcdef", SCM_ENC_UTF8);
  start = ut_read_cstr("2");
  end = ut_read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_string_copy(str, start, end));
}

TEST(api_strings, api_string_copy__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_string_copy(SCM_TRUE_OBJ, SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(api_strings, api_string_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, at = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("1abcd", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &at, &start, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  expected = scm_fcd_make_string_from_cstr("1cde5", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, from, start, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("4");
  expected = scm_fcd_make_string_from_cstr("1cd45", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, from, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("2");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, from, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("4");
  expected = scm_fcd_make_string_from_cstr("13445", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, to, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  at = ut_read_cstr("2");
  start = ut_read_cstr("1");
  end = ut_read_cstr("3");
  expected = scm_fcd_make_string_from_cstr("12235", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_copy_i(to, at, to, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__start_greater_than_end__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, from, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__too_many_characters_to_be_copied__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("2");
  start = ut_read_cstr("1");
  end = ut_read_cstr("5");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  /* error if (- (string-length to) at) is less than (- end start) */
  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, from, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__at_out_of_range__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, at = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at,  &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("6");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__start_out_of_range__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("5");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, from, start, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__end_out_of_range__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &at, &start, &end, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("1");
  start = ut_read_cstr("4");
  end = ut_read_cstr("6");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, from, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__return_ERROR_1)
{
  ScmObj from = SCM_OBJ_INIT, at = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&from, &at, &expected);

  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  at = ut_read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(SCM_FALSE_OBJ, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(api_strings, api_string_copy_i__return_ERROR_2)
{
  ScmObj to = SCM_OBJ_INIT, at = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &at, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  at = ut_read_cstr("0");
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);


  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, at, SCM_TRUE_OBJ, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_copy_i__return_ERROR_3)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);
  from = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("12345", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_copy_i(to, SCM_TRUE_OBJ, from, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, to));
}

TEST(api_strings, api_string_fill_i__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  expected = scm_fcd_make_string_from_cstr("zzzzz", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_fill_i(str, chr, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("azzzz", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_fill_i(str, chr, start, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &end, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = scm_fcd_make_string_from_cstr("azzze", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_fill_i(str, chr, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__same_idx)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &end, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_UNDEF(scm_api_string_fill_i(str, chr, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__start_greater_than_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &end, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("2");
  end = ut_read_cstr("1");
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_fill_i(str, chr, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__start_out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("5");
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_fill_i(str, chr, start, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__end_out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &chr, &start, &end, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  chr = ut_read_cstr("#\\z");
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_fill_i(str, chr, start, end));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__not_character__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected);

  str = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);
  expected = scm_fcd_make_string_from_cstr("abcde", SCM_ENC_UTF8);

  TEST_ASSERT_SCM_NULL(scm_api_string_fill_i(str, SCM_TRUE_OBJ, SCM_OBJ_NULL, SCM_OBJ_NULL));
  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, str));
}

TEST(api_strings, api_string_fill_i__return_ERROR)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  chr = ut_read_cstr("#\\z");

  TEST_ASSERT_SCM_NULL(scm_api_string_fill_i(SCM_FALSE_OBJ, chr, SCM_OBJ_NULL, SCM_OBJ_NULL));
}
