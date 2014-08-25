#include "object.h"
#include "api.h"

#include "test.h"

TEST_GROUP(api_vectors);

static ScmEvaluator *ev;

TEST_SETUP(api_vectors)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(api_vectors)
{
  scm_capi_evaluator_end(ev);
}

TEST(api_vectors, capi_vector_p__return_true)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c)");

  TEST_ASSERT_TRUE(scm_capi_vector_p(vec));
}

TEST(api_vectors, capi_vector_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_vector_p(SCM_EOF_OBJ));
}

TEST(api_vectors, capi_vector_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_vector_p(SCM_OBJ_NULL));
}

TEST(api_vectors, api_vector_P__return_true)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c)");

  TEST_ASSERT_SCM_TRUE(scm_api_vector_P(vec));
}

TEST(api_vectors, api_vector_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_vector_P(SCM_EOF_OBJ));
}

TEST(api_vectors, capi_make_vector__dont_specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  vec = scm_capi_make_vector(3, SCM_OBJ_NULL);

  TEST_ASSERT_TRUE(scm_capi_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_capi_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_capi_vector_ref(vec, i);
    TEST_ASSERT_SCM_UNDEF(elm);
  }
}

TEST(api_vectors, capi_make_vector__specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &elm);

  fill = read_cstr("abc");
  vec = scm_capi_make_vector(3, fill);

  TEST_ASSERT_TRUE(scm_capi_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_capi_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_capi_vector_ref(vec, i);
    TEST_ASSERT_SCM_EQ(fill, elm);
  }
}

TEST(api_vectors, api_make_vector__dont_specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, len = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &len, &elm);

  len = read_cstr("3");
  vec = scm_api_make_vector(len, SCM_OBJ_NULL);

  TEST_ASSERT_TRUE(scm_capi_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_capi_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_capi_vector_ref(vec, i);
    TEST_ASSERT_SCM_UNDEF(elm);
  }
}

TEST(api_vectors, api_make_vector__specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, len = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;
  ScmObj elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &len, &fill, &elm);

  len = read_cstr("3");
  fill = read_cstr("abc");
  vec = scm_api_make_vector(len, fill);

  TEST_ASSERT_TRUE(scm_capi_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_capi_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_capi_vector_ref(vec, i);
    TEST_ASSERT_SCM_EQ(fill, elm);
  }
}

TEST(api_vectors, api_make_vector__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_make_vector(SCM_FALSE_OBJ, SCM_OBJ_NULL));
}

TEST(api_vectors, capi_vector_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(a b c)");
  expected = read_cstr("#(a b c)");

  actual = scm_capi_vector_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_cv)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = read_cstr("a");
  elm[1] = read_cstr("b");
  elm[2] = read_cstr("c");
  expected = read_cstr("#(a b c)");

  actual = scm_capi_vector_cv(elm, sizeof(elm)/sizeof(elm[0]));

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_cv__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = read_cstr("#()");

  actual = scm_capi_vector_cv(NULL, 0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_cv__return_ERROR)
{
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = read_cstr("a");
  elm[1] = SCM_OBJ_NULL;
  elm[2] = read_cstr("c");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_cv(elm, sizeof(elm)/sizeof(elm[0])));
}

TEST(api_vectors, capi_vector)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = read_cstr("a");
  elm[1] = read_cstr("b");
  elm[2] = read_cstr("c");
  expected = read_cstr("#(a b c)");

  actual = scm_capi_vector(3, elm[0], elm[1], elm[2]);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = read_cstr("#()");

  actual = scm_capi_vector(0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector__return_ERROR)
{
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = read_cstr("a");
  elm[1] = SCM_OBJ_NULL;;
  elm[2] = read_cstr("c");

  TEST_ASSERT_SCM_NULL(scm_capi_vector(3, elm[0], elm[1], elm[2]));
}

TEST(api_vectors, capi_vector_length)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c)");

  TEST_ASSERT_EQUAL_INT(3, scm_capi_vector_length(vec));
}

TEST(api_vectors, capi_vector_length__return_ERROR)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_length(SCM_TRUE_OBJ));
}

TEST(api_vectors, api_vector_length)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c)");
  expected = read_cstr("3");

  actual = scm_api_vector_length(vec);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(api_vectors, vector_length__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_vector_length(SCM_TRUE_OBJ));
}

TEST(api_vectors, capi_vector_ref)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c)");
  expected = read_cstr("b");

  actual = scm_capi_vector_ref(vec, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_ref__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_ref(vec, 3));
}

TEST(api_vectors, capi_vector_ref__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_vector_ref(SCM_FALSE_OBJ, 0));
}

TEST(api_vectors, api_vector_ref)
{
  ScmObj vec = SCM_OBJ_INIT, num = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &num, &expected, &actual);

  vec = read_cstr("#(a b c)");
  num = read_cstr("1");
  expected = read_cstr("b");

  actual = scm_api_vector_ref(vec, num);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_ref__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &num);

  vec = read_cstr("#(a b c)");
  num = read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_vector_ref(vec, num));
}

TEST(api_vectors, api_vector_ref__return_ERROR)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_vector_ref(SCM_FALSE_OBJ, num));
}

TEST(api_vectors, capi_vector_set_i)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &expected);

  vec = read_cstr("#(a b c)");
  elm = read_cstr("z");
  expected = read_cstr("#(a z c)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_set_i(vec, 1, elm));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_set_i__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  vec = read_cstr("#(a b c)");
  elm = read_cstr("z");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_set_i(vec, 3, elm));
}

TEST(api_vectors, capi_vector_set_i__set_SCM_OBJ_NULL__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  vec = read_cstr("#(a b c)");
  elm = SCM_OBJ_NULL;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_set_i(vec, 1, elm));
}

TEST(api_vectors, capi_vector_set_i__return_ERROR)
{
  ScmObj elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&elm);

  elm = read_cstr("z");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_set_i(SCM_FALSE_OBJ, 0, elm));
}

TEST(api_vectors, api_vector_set_i)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num, &expected);

  vec = read_cstr("#(a b c)");
  elm = read_cstr("z");
  num = read_cstr("1");
  expected = read_cstr("#(a z c)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_set_i(vec, num, elm));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_set_i__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num);

  vec = read_cstr("#(a b c)");
  elm = read_cstr("z");
  num = read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(vec, num, elm));
}

TEST(api_vectors, api_vector_set_i__set_SCM_OBJ_NULL__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &num);

  vec = read_cstr("#(a b c)");
  elm = SCM_OBJ_NULL;
  num = read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(vec, num, elm));
}

TEST(api_vectors, api_vector_set_i__return_ERROR)
{
  ScmObj elm = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&elm, &num);

  elm = read_cstr("z");
  num = read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_vector_set_i(SCM_TRUE_OBJ, num, elm));
}

TEST(api_vectors, capi_vector_to_list__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("(a b c d e)");

  actual = scm_capi_vector_to_list(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_list__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("(b c d e)");

  actual = scm_capi_vector_to_list(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_list__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("(b c d)");

  actual = scm_capi_vector_to_list(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_list__same_idx__return_empty_list)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("()");

  actual = scm_capi_vector_to_list(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_list__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_to_list(vec, 3, 2));
}

TEST(api_vectors, capi_vector_to_list__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_to_list(vec, 1, 6));
}

TEST(api_vectors, api_vector_to_list__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("(a b c d e)");

  actual = scm_api_vector_to_list(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  expected = read_cstr("(b c d e)");

  actual = scm_api_vector_to_list(vec, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("4");
  expected = read_cstr("(b c d)");

  actual = scm_api_vector_to_list(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__same_idx__return_empty_list)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("1");
  expected = read_cstr("()");

  actual = scm_api_vector_to_list(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_list__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("3");
  end = read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_list(vec, start, end));
}

TEST(api_vectors, api_vector_to_list__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_list(vec, start, end));
}

TEST(api_vectors, api_list_to_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(a b c)");
  expected = read_cstr("#(a b c)");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("#()");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__not_list__return_empty_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  expected = read_cstr("#()");

  actual = scm_api_list_to_vector(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_list_to_vector__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(a b . c)");
  expected = read_cstr("#(a b)");

  actual = scm_api_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_string__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = read_cstr("\"abcde\"");

  actual = scm_capi_vector_to_string(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_string__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = read_cstr("\"bcde\"");

  actual = scm_capi_vector_to_string(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_string__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = read_cstr("\"bcd\"");

  actual = scm_capi_vector_to_string(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_string__same_idx__return_empty_string)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = read_cstr("\"\"");

  actual = scm_capi_vector_to_string(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_to_string__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_to_string(vec, 3, 2));
}

TEST(api_vectors, capi_vector_to_string__vector_has_item_is_not_char__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(#\\a #\\b c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_to_string(vec, -1, -1));
}

TEST(api_vectors, capi_vector_to_string__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_to_string(vec, 1, 6));
}

TEST(api_vectors, api_vector_to_string__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = read_cstr("\"abcde\"");

  actual = scm_api_vector_to_string(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = read_cstr("1");
  expected = read_cstr("\"bcde\"");

  actual = scm_api_vector_to_string(vec, start, SCM_OBJ_INIT);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = read_cstr("1");
  end = read_cstr("4");
  expected = read_cstr("\"bcd\"");

  actual = scm_api_vector_to_string(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__same_idx__return_empty_string)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = read_cstr("1");
  end = read_cstr("1");
  expected = read_cstr("\"\"");

  actual = scm_api_vector_to_string(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_to_string__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = read_cstr("3");
  end = read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, start, end));
}

TEST(api_vectors, api_vector_to_string__vector_has_item_is_not_char__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(#\\a #\\b c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(api_vectors, api_vector_to_string__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);;

  vec = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  start = read_cstr("1");
  end = read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_to_string(vec, start, end));
}

TEST(api_vectors, capi_string_to_vector__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = read_cstr("\"abcde\"");
  expected = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_capi_string_to_vector(str, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_string_to_vector__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = read_cstr("\"abcde\"");
  expected = read_cstr("#(#\\b #\\c #\\d #\\e)");

  actual = scm_capi_string_to_vector(str, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_string_to_vector__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = read_cstr("\"abcde\"");
  expected = read_cstr("#(#\\b #\\c #\\d)");

  actual = scm_capi_string_to_vector(str, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_string_to_vector__same_idx__return_empty_vector)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = read_cstr("\"abcde\"");
  expected = read_cstr("#()");

  actual = scm_capi_string_to_vector(str, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_string_to_vector__start_greater_then_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = read_cstr("\"abcde\"");

  TEST_ASSERT_SCM_NULL(scm_capi_string_to_vector(str, 3, 2));
}

TEST(api_vectors, capi_string_to_vector__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);

  str = read_cstr("\"abcde\"");

  TEST_ASSERT_SCM_NULL(scm_capi_string_to_vector(str, 1, 6));
}

TEST(api_vectors, api_string_to_vector__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = read_cstr("\"abcde\"");
  expected = read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_vector(str, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &expected, &actual);

  str = read_cstr("\"abcde\"");
  start = read_cstr("1");
  expected = read_cstr("#(#\\b #\\c #\\d #\\e)");

  actual = scm_api_string_to_vector(str, start, SCM_OBJ_INIT);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &expected, &actual);

  str = read_cstr("\"abcde\"");
  start = read_cstr("1");
  end = read_cstr("4");
  expected = read_cstr("#(#\\b #\\c #\\d)");

  actual = scm_api_string_to_vector(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__same_idx__return_empty_vector)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end, &expected, &actual);

  str = read_cstr("\"abcde\"");
  start = read_cstr("1");
  end = read_cstr("1");
  expected = read_cstr("#()");

  actual = scm_api_string_to_vector(str, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_string_to_vector__start_greater_then_end__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  str = read_cstr("\"abcde\"");
  start = read_cstr("3");
  end = read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_vector(str, start, end));
}

TEST(api_vectors, api_string_to_vector__out_of_range__return_ERROR)
{
  ScmObj str = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &start, &end);;

  str = read_cstr("\"abcde\"");
  start = read_cstr("1");
  end = read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_string_to_vector(str, start, end));
}

TEST(api_vectors, capi_vector_copy__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("#(a b c d e)");

  actual = scm_capi_vector_copy(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_copy__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("#(b c d e)");

  actual = scm_capi_vector_copy(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_copy__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("#(b c d)");

  actual = scm_capi_vector_copy(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_copy__same_idx__return_empty_vector)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("#()");

  actual = scm_capi_vector_copy(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_copy__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_copy(vec, 3, 2));
}

TEST(api_vectors, capi_vector_copy__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_copy(vec, 1, 6));
}

TEST(api_vectors, api_vector_copy__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  expected = read_cstr("#(a b c d e)");

  actual = scm_api_vector_copy(vec, SCM_OBJ_NULL, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  expected = read_cstr("#(b c d e)");

  actual = scm_api_vector_copy(vec, start, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("4");
  expected = read_cstr("#(b c d)");

  actual = scm_api_vector_copy(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__same_idx__return_empty_vector)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end, &expected, &actual);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("1");
  expected = read_cstr("#()");

  actual = scm_api_vector_copy(vec, start, end);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, api_vector_copy__start_greater_then_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("3");
  end = read_cstr("2");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy(vec, start, end));
}

TEST(api_vectors, api_vector_copy__out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &start, &end);

  vec = read_cstr("#(a b c d e)");
  start = read_cstr("1");
  end = read_cstr("6");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy(vec, start, end));
}

TEST(api_vectors, capi_vector_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 a b c d)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 1, from, -1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 c d e 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 1, from, 2, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 c d 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 1, from, 2, 4));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 1, from, 2, 2));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  expected = read_cstr("#(1 3 4 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 1, to, 2, 4));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  expected = read_cstr("#(1 2 2 3 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_copy_i(to, 2, to, 1, 3));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__start_greater_then_end__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_copy_i(to, 1, from, 3, 2));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__too_many_objects_to_be_copied___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_copy_i(to, 2, from, 1, 5));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__at_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_copy_i(to, 5, from, -1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__start_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_copy_i(to, 1, from, 5, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_copy_i__end_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_copy_i(to, 1, from, 3, 6));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}





TEST(api_vectors, api_vector_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  expected = read_cstr("#(1 a b c d)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("2");
  expected = read_cstr("#(1 c d e 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("2");
  end = read_cstr("4");
  expected = read_cstr("#(1 c d 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("2");
  end = read_cstr("2");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  at = read_cstr("1");
  start = read_cstr("2");
  end = read_cstr("4");
  expected = read_cstr("#(1 3 4 4 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, to, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  at = read_cstr("2");
  start = read_cstr("1");
  end = read_cstr("3");
  expected = read_cstr("#(1 2 2 3 5)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_copy_i(to, at, to, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__start_greater_then_end__return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("3");
  end = read_cstr("2");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__too_many_objects_to_be_copied___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("2");
  start = read_cstr("1");
  end = read_cstr("5");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__at_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("5");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__start_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("5");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, api_vector_copy_i__end_out_of_range___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj at = SCM_OBJ_INIT, start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected, &at, &start, &end);

  to = read_cstr("#(1 2 3 4 5)");
  from = read_cstr("#(a b c d e)");
  at = read_cstr("1");
  start = read_cstr("3");
  end = read_cstr("6");
  expected = read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_copy_i(to, at, from, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(api_vectors, capi_vector_append_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(#(a b c) #(d e f) #(g h i))");
  expected = read_cstr("#(a b c d e f g h i)");

  actual = scm_capi_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append_lst__empty_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("#()");

  actual = scm_capi_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append_lst__arg_is_not_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("a");
  expected = read_cstr("#()");

  actual = scm_capi_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append_lst__list_has_object_is_not_vector__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(#(a b c) def #(g h i))");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_append_lst(lst));
}

TEST(api_vectors, capi_vector_append_cv)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  ScmObj vec[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT};

  SCM_REFSTK_INIT_REG(&expected, &actual);
  SCM_REFSTK_REG_ARY(vec, sizeof(vec)/sizeof(vec[0]));

  vec[0] = read_cstr("#(a b c)");
  vec[1] = read_cstr("#(d e f)");
  vec[2] = read_cstr("#(g h i)");
  expected = read_cstr("#(a b c d e f g h i)");

  actual = scm_capi_vector_append_cv(vec, sizeof(vec)/sizeof(vec[0]));

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append_cv__empty)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &actual);

  expected = read_cstr("#()");

  actual = scm_capi_vector_append_cv(NULL, 0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append_cv__return_ERROR)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  ScmObj vec[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT};

  SCM_REFSTK_INIT_REG(&expected, &actual);
  SCM_REFSTK_REG_ARY(vec, sizeof(vec)/sizeof(vec[0]));

  vec[0] = read_cstr("#(a b c)");
  vec[1] = SCM_OBJ_NULL;
  vec[2] = read_cstr("#(g h i)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_append_cv(vec, sizeof(vec)/sizeof(vec[0])));
}

TEST(api_vectors, capi_vector_append)
{
  ScmObj v1 = SCM_OBJ_INIT, v2 = SCM_OBJ_INIT, v3 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&v1, &v2, &v3, &expected, &actual);

  v1 = read_cstr("#(a b c)");
  v2 = read_cstr("#(d e f)");
  v3 = read_cstr("#(g h i)");
  expected = read_cstr("#(a b c d e f g h i)");

  actual = scm_capi_vector_append(3, v1, v2, v3);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append__empty)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &actual);

  expected = read_cstr("#()");

  actual = scm_capi_vector_append(0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_vectors, capi_vector_append__return_ERROR)
{
  ScmObj v1 = SCM_OBJ_INIT, v2 = SCM_OBJ_INIT, v3 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&v1, &v2, &v3, &expected, &actual);

  v1 = read_cstr("#(a b c)");
  v2 = SCM_OBJ_NULL;
  v3 = read_cstr("#(g h i)");
  expected = read_cstr("#(a b c d e f g h i)");

  TEST_ASSERT_SCM_NULL(scm_capi_vector_append(3, v1, v2, v3));
}

TEST(api_vectors, capi_vector_fill_i__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(z z z z z)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_fill_i(vec, fill, -1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a z z z z)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_fill_i(vec, fill, 1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a z z z e)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_fill_i(vec, fill, 1, 4));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__same_idx)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_vector_fill_i(vec, fill, 1, 1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__start_greater_than_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_fill_i(vec, fill, 2, 1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__start_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_fill_i(vec, fill, 5, -1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__end_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_fill_i(vec, fill, -1, 6));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, capi_vector_fill_i__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = SCM_OBJ_NULL;
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_vector_fill_i(vec, fill, -1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  expected = read_cstr("#(z z z z z)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = read_cstr("1");
  expected = read_cstr("#(a z z z z)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = read_cstr("1");
  end = read_cstr("4");
  expected = read_cstr("#(a z z z e)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__same_idx)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = read_cstr("1");
  end = read_cstr("1");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_UNDEF(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__start_greater_than_end__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = read_cstr("2");
  end = read_cstr("1");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__start_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = read_cstr("5");
  end = SCM_OBJ_NULL;
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__end_out_of_range__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj start = SCM_OBJ_INIT, end = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected, &start, &end);

  vec = read_cstr("#(a b c d e)");
  fill = read_cstr("z");
  start = SCM_OBJ_NULL;
  end = read_cstr("6");
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, start, end));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(api_vectors, api_vector_fill_i__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = read_cstr("#(a b c d e)");
  fill = SCM_OBJ_NULL;
  expected = read_cstr("#(a b c d e)");

  TEST_ASSERT_SCM_NULL(scm_api_vector_fill_i(vec, fill, SCM_OBJ_NULL, SCM_OBJ_NULL));

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}
