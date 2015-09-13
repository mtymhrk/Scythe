#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/number.h"
#include "scythe/vector.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_vectors);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_vectors)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_vectors)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_vectors, api_vector_P__return_true)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(a b c)");

  TEST_ASSERT_SCM_TRUE(scm_api_vector_P(vec));
}

TEST(api_vectors, api_vector_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_vector_P(SCM_EOF_OBJ));
}

TEST(api_vectors, api_make_vector__dont_specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, len = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &len, &elm);

  len = ut_read_cstr("3");
  vec = scm_api_make_vector(len, SCM_OBJ_NULL);

  TEST_ASSERT_TRUE(scm_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_vector_ref(vec, i);
    TEST_ASSERT_SCM_UNDEF(elm);
  }
}

TEST(api_vectors, api_make_vector__specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, len = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;
  ScmObj elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &len, &fill, &elm);

  len = ut_read_cstr("3");
  fill = ut_read_cstr("abc");
  vec = scm_api_make_vector(len, fill);

  TEST_ASSERT_TRUE(scm_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_vector_ref(vec, i);
    TEST_ASSERT_SCM_EQ(fill, elm);
  }
}

TEST(api_vectors, api_make_vector__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_make_vector(SCM_FALSE_OBJ, SCM_OBJ_NULL));
}

TEST(api_vectors, api_vector_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b c)");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_api_vector_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_length)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c)");
  expected = ut_read_cstr("3");

  actual = scm_api_vector_length(vec);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(api_vectors, vector_length__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_vector_length(SCM_TRUE_OBJ));
}

TEST(api_vectors, api_vector_ref)
{
  ScmObj vec = SCM_OBJ_INIT, num = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &num, &expected, &actual);

  vec = ut_read_cstr("#(a b c)");
  num = ut_read_cstr("1");
  expected = ut_read_cstr("b");

  actual = scm_api_vector_ref(vec, num);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_ref__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &num);

  vec = ut_read_cstr("#(a b c)");
  num = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_vector_ref(vec, num));
}

TEST(api_vectors, api_vector_ref__return_ERROR)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = ut_read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_vector_ref(SCM_FALSE_OBJ, num));
}

TEST(api_vectors, api_vector_set_i)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num, &expected);

  vec = ut_read_cstr("#(a b c)");
  elm = ut_read_cstr("z");
  num = ut_read_cstr("1");
  expected = ut_read_cstr("#(a z c)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_set_i(vec, num, elm));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_set_i__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num);

  vec = ut_read_cstr("#(a b c)");
  elm = ut_read_cstr("z");
  num = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(vec, num, elm));
}

TEST(api_vectors, api_vector_set_i__set_SCM_OBJ_NULL__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num);

  vec = ut_read_cstr("#(a b c)");
  elm = SCM_OBJ_NULL;
  num = ut_read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(vec, num, elm));
}

TEST(api_vectors, api_vector_set_i__return_ERROR)
{
  ScmObj elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&elm, &num);

  elm = ut_read_cstr("z");
  num = ut_read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(SCM_TRUE_OBJ, num, elm));
}

TEST(api_vectors, api_vector_to_list__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("(a b c d e)");

  actual = scm_api_vector_to_list(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  expected = ut_read_cstr("(b c d e)");

  actual = scm_api_vector_to_list(vec, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("(b c d)");

  actual = scm_api_vector_to_list(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__same_idx__return_empty_list)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("()");

  actual = scm_api_vector_to_list(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_list(vec, start, end));
}

TEST(api_vectors, api_vector_to_list__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_list(vec, start, end));
}

TEST(api_vectors, api_list_to_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b c)");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("#()");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__not_list__return_empty_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  expected = ut_read_cstr("#()");

  actual = scm_api_list_to_vector(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b . c)");
  expected = ut_read_cstr("#(a b)");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = ut_read_cstr("\"abcde\"");

  actual = scm_api_vector_to_string(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = ut_read_cstr("1");
  expected = ut_read_cstr("\"bcde\"");

  actual = scm_api_vector_to_string(vec, start, SCM_OBJ_INIT);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("\"bcd\"");

  actual = scm_api_vector_to_string(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__same_idx__return_empty_string)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("\"\"");

  actual = scm_api_vector_to_string(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, start, end));
}

TEST(api_vectors, api_vector_to_string__vector_has_item_is_not_char__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(#\\a #\\b c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(api_vectors, api_vector_to_string__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);;

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, start, end));
}

TEST(api_vectors, api_string_to_vector__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  expected = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_vector(str, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  start = ut_read_cstr("1");
  expected = ut_read_cstr("#(#\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_vector(str, start, SCM_OBJ_INIT);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("#(#\\b #\\c #\\d)");

  actual = scm_api_string_to_vector(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__same_idx__return_empty_vector)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("#()");

  actual = scm_api_string_to_vector(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__start_greater_then_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = ut_read_cstr("\"abcde\"");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_vector(str, start, end));
}

TEST(api_vectors, api_string_to_vector__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);;

  str = ut_read_cstr("\"abcde\"");
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_vector(str, start, end));
}

TEST(api_vectors, api_vector_copy__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(a b c d e)");

  actual = scm_api_vector_copy(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  expected = ut_read_cstr("#(b c d e)");

  actual = scm_api_vector_copy(vec, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("#(b c d)");

  actual = scm_api_vector_copy(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__same_idx__return_empty_vector)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("#()");

  actual = scm_api_vector_copy(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy(vec, start, end));
}

TEST(api_vectors, api_vector_copy__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  start = ut_read_cstr("1");
  end = ut_read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy(vec, start, end));
}

TEST(api_vectors, api_vector_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  expected = ut_read_cstr("#(1 a b c d)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  expected = ut_read_cstr("#(1 c d e 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("#(1 c d 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("2");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("2");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("#(1 3 4 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, to, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  at = ut_read_cstr("2");
  start = ut_read_cstr("1");
  end = ut_read_cstr("3");
  expected = ut_read_cstr("#(1 2 2 3 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, to, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__start_greater_then_end__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("3");
  end = ut_read_cstr("2");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__too_many_objects_to_be_copied___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("2");
  start = ut_read_cstr("1");
  end = ut_read_cstr("5");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__at_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("5");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__start_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("5");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__end_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  at = ut_read_cstr("1");
  start = ut_read_cstr("3");
  end = ut_read_cstr("6");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_append_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(#(a b c) #(d e f) #(g h i))");
  expected = ut_read_cstr("#(a b c d e f g h i)");

  actual = scm_api_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_append_lst__empty_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("#()");

  actual = scm_api_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_append_lst__arg_is_not_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("a");
  expected = ut_read_cstr("#()");

  actual = scm_api_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_append_lst__list_has_object_is_not_vector__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(#(a b c) def #(g h i))");

  TEST_ASSERT_SCM_NULL(scm_api_vector_append_lst(lst));
}

TEST(api_vectors, api_vector_fill_i__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  expected = ut_read_cstr("#(z z z z z)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = ut_read_cstr("1");
  expected = ut_read_cstr("#(a z z z z)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = ut_read_cstr("1");
  end = ut_read_cstr("4");
  expected = ut_read_cstr("#(a z z z e)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__same_idx)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = ut_read_cstr("1");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__start_greater_than_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = ut_read_cstr("2");
  end = ut_read_cstr("1");
  expected = ut_read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__start_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = ut_read_cstr("5");
  end = SCM_OBJ_NULL;
  expected = ut_read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__end_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  start = SCM_OBJ_NULL;
  end = ut_read_cstr("6");
  expected = ut_read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = SCM_OBJ_NULL;
  expected = ut_read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}
