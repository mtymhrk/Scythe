
#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_vector);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_vector)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_vector)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(fcd_vector, fcd_vector_p__return_true)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(a b c)");

  TEST_ASSERT_TRUE(scm_fcd_vector_p(vec));
}

TEST(fcd_vector, fcd_vector_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_vector_p(SCM_EOF_OBJ));
}

TEST(fcd_vector, fcd_vector_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_vector_p(SCM_OBJ_NULL));
}

TEST(fcd_vector, fcd_vector_P__return_true)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(a b c)");

  TEST_ASSERT_SCM_TRUE(scm_fcd_vector_P(vec));
}

TEST(fcd_vector, fcd_vector_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_vector_P(SCM_EOF_OBJ));
}

TEST(fcd_vector, fcd_make_vector__dont_specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  vec = scm_fcd_make_vector(3, SCM_OBJ_NULL);

  TEST_ASSERT_TRUE(scm_fcd_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_fcd_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_fcd_vector_ref(vec, i);
    TEST_ASSERT_SCM_UNDEF(elm);
  }
}

TEST(fcd_vector, fcd_make_vector__specify_fill)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &elm);

  fill = ut_read_cstr("abc");
  vec = scm_fcd_make_vector(3, fill);

  TEST_ASSERT_TRUE(scm_fcd_vector_p(vec));
  TEST_ASSERT_EQUAL_INT(3, scm_fcd_vector_length(vec));

  for (size_t i = 0; i < 3; i++) {
    elm = scm_fcd_vector_ref(vec, i);
    TEST_ASSERT_SCM_EQ(fill, elm);
  }
}

TEST(fcd_vector, fcd_vector_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b c)");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_fcd_vector_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_cv)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = ut_read_cstr("a");
  elm[1] = ut_read_cstr("b");
  elm[2] = ut_read_cstr("c");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_fcd_vector_cv(elm, sizeof(elm)/sizeof(elm[0]));

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_cv__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_cv(NULL, 0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_cv__return_ERROR)
{
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = ut_read_cstr("a");
  elm[1] = SCM_OBJ_NULL;
  elm[2] = ut_read_cstr("c");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector_cv(elm, sizeof(elm)/sizeof(elm[0])));
}

TEST(fcd_vector, fcd_vector)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = ut_read_cstr("a");
  elm[1] = ut_read_cstr("b");
  elm[2] = ut_read_cstr("c");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_fcd_vector(3, elm[0], elm[1], elm[2]);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector__empty)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector(0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector__return_ERROR)
{
  ScmObj elm[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(elm, sizeof(elm)/sizeof(elm[0]));

  elm[0] = ut_read_cstr("a");
  elm[1] = SCM_OBJ_NULL;;
  elm[2] = ut_read_cstr("c");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector(3, elm[0], elm[1], elm[2]));
}

TEST(fcd_vector, fcd_vector_length)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(a b c)");

  TEST_ASSERT_EQUAL_INT(3, scm_fcd_vector_length(vec));
}

TEST(fcd_vector, fcd_vector_ref)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c)");
  expected = ut_read_cstr("b");

  actual = scm_fcd_vector_ref(vec, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_set_i)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &elm, &expected);

  vec = ut_read_cstr("#(a b c)");
  elm = ut_read_cstr("z");
  expected = ut_read_cstr("#(a z c)");

  scm_fcd_vector_set_i(vec, 1, elm);

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(fcd_vector, fcd_vector_to_list__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("(a b c d e)");

  actual = scm_fcd_vector_to_list(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_list__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("(b c d e)");

  actual = scm_fcd_vector_to_list(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_list__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("(b c d)");

  actual = scm_fcd_vector_to_list(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_list__same_idx__return_empty_list)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("()");

  actual = scm_fcd_vector_to_list(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_list_to_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b c)");
  expected = ut_read_cstr("#(a b c)");

  actual = scm_fcd_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_list_to_vector__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("#()");

  actual = scm_fcd_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_list_to_vector__not_list__return_empty_vector)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  expected = ut_read_cstr("#()");

  actual = scm_fcd_list_to_vector(SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_list_to_vector__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(a b . c)");
  expected = ut_read_cstr("#(a b)");

  actual = scm_fcd_list_to_vector(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_string__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = ut_read_cstr("\"abcde\"");

  actual = scm_fcd_vector_to_string(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_string__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = ut_read_cstr("\"bcde\"");

  actual = scm_fcd_vector_to_string(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_string__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = ut_read_cstr("\"bcd\"");

  actual = scm_fcd_vector_to_string(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_string__same_idx__return_empty_string)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");
  expected = ut_read_cstr("\"\"");

  actual = scm_fcd_vector_to_string(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_to_string__vector_has_item_is_not_char__return_ERROR)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  vec = ut_read_cstr("#(#\\a #\\b c #\\d #\\e)");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector_to_string(vec, -1, -1));
}

TEST(fcd_vector, fcd_string_to_vector__unspecify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  expected = ut_read_cstr("#(#\\a #\\b #\\c #\\d #\\e)");

  actual = scm_fcd_string_to_vector(str, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_string_to_vector__specify_start)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  expected = ut_read_cstr("#(#\\b #\\c #\\d #\\e)");

  actual = scm_fcd_string_to_vector(str, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_string_to_vector__specify_start_end)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  expected = ut_read_cstr("#(#\\b #\\c #\\d)");

  actual = scm_fcd_string_to_vector(str, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_string_to_vector__same_idx__return_empty_vector)
{
  ScmObj str = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &expected, &actual);

  str = ut_read_cstr("\"abcde\"");
  expected = ut_read_cstr("#()");

  actual = scm_fcd_string_to_vector(str, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_copy__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(a b c d e)");

  actual = scm_fcd_vector_copy(vec, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_copy__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(b c d e)");

  actual = scm_fcd_vector_copy(vec, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_copy__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(b c d)");

  actual = scm_fcd_vector_copy(vec, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_copy__same_idx__return_empty_vector)
{
  ScmObj vec = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &expected, &actual);

  vec = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_copy(vec, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_copy_i__unspecify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(1 a b c d)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 1, from, -1, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_copy_i__specify_start)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(1 c d e 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 1, from, 2, -1));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_copy_i__specify_start_end)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(1 c d 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 1, from, 2, 4));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_copy_i__same_idx)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 1, from, 2, 2));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_copy_i__overlap_1)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  expected = ut_read_cstr("#(1 3 4 4 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 1, to, 2, 4));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_copy_i__overlap_2)
{
  ScmObj to = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  expected = ut_read_cstr("#(1 2 2 3 5)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_vector_copy_i(to, 2, to, 1, 3));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}


TEST(fcd_vector, fcd_vector_copy_i__too_many_objects_to_be_copied___return_ERROR)
{
  ScmObj to = SCM_OBJ_INIT, from = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&to, &from, &expected);

  to = ut_read_cstr("#(1 2 3 4 5)");
  from = ut_read_cstr("#(a b c d e)");
  expected = ut_read_cstr("#(1 2 3 4 5)");

  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_vector_copy_i(to, 2, from, 1, 5));

  TEST_ASSERT_SCM_EQUAL(expected, to);
}

TEST(fcd_vector, fcd_vector_append_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(#(a b c) #(d e f) #(g h i))");
  expected = ut_read_cstr("#(a b c d e f g h i)");

  actual = scm_fcd_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append_lst__empty_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append_lst__arg_is_not_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("a");
  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_append_lst(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append_lst__list_has_object_is_not_vector__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(#(a b c) def #(g h i))");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector_append_lst(lst));
}

TEST(fcd_vector, fcd_vector_append_cv)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  ScmObj vec[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT};

  SCM_REFSTK_INIT_REG(&expected, &actual);
  SCM_REFSTK_REG_ARY(vec, sizeof(vec)/sizeof(vec[0]));

  vec[0] = ut_read_cstr("#(a b c)");
  vec[1] = ut_read_cstr("#(d e f)");
  vec[2] = ut_read_cstr("#(g h i)");
  expected = ut_read_cstr("#(a b c d e f g h i)");

  actual = scm_fcd_vector_append_cv(vec, sizeof(vec)/sizeof(vec[0]));

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append_cv__empty)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &actual);

  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_append_cv(NULL, 0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append_cv__return_ERROR)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  ScmObj vec[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT};

  SCM_REFSTK_INIT_REG(&expected, &actual);
  SCM_REFSTK_REG_ARY(vec, sizeof(vec)/sizeof(vec[0]));

  vec[0] = ut_read_cstr("#(a b c)");
  vec[1] = SCM_OBJ_NULL;
  vec[2] = ut_read_cstr("#(g h i)");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector_append_cv(vec, sizeof(vec)/sizeof(vec[0])));
}

TEST(fcd_vector, fcd_vector_append)
{
  ScmObj v1 = SCM_OBJ_INIT, v2 = SCM_OBJ_INIT, v3 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&v1, &v2, &v3, &expected, &actual);

  v1 = ut_read_cstr("#(a b c)");
  v2 = ut_read_cstr("#(d e f)");
  v3 = ut_read_cstr("#(g h i)");
  expected = ut_read_cstr("#(a b c d e f g h i)");

  actual = scm_fcd_vector_append(3, v1, v2, v3);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append__empty)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &actual);

  expected = ut_read_cstr("#()");

  actual = scm_fcd_vector_append(0);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_vector, fcd_vector_append__return_ERROR)
{
  ScmObj v1 = SCM_OBJ_INIT, v2 = SCM_OBJ_INIT, v3 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&v1, &v2, &v3, &expected, &actual);

  v1 = ut_read_cstr("#(a b c)");
  v2 = SCM_OBJ_NULL;
  v3 = ut_read_cstr("#(g h i)");
  expected = ut_read_cstr("#(a b c d e f g h i)");

  TEST_ASSERT_SCM_NULL(scm_fcd_vector_append(3, v1, v2, v3));
}

TEST(fcd_vector, fcd_vector_fill_i__unspecify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  expected = ut_read_cstr("#(z z z z z)");

  scm_fcd_vector_fill_i(vec, fill, -1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(fcd_vector, fcd_vector_fill_i__specify_start)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  expected = ut_read_cstr("#(a z z z z)");

  scm_fcd_vector_fill_i(vec, fill, 1, -1);

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(fcd_vector, fcd_vector_fill_i__specify_start_end)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  expected = ut_read_cstr("#(a z z z e)");

  scm_fcd_vector_fill_i(vec, fill, 1, 4);

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}

TEST(fcd_vector, fcd_vector_fill_i__same_idx)
{
  ScmObj vec = SCM_OBJ_INIT, fill = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec, &fill, &expected);

  vec = ut_read_cstr("#(a b c d e)");
  fill = ut_read_cstr("z");
  expected = ut_read_cstr("#(a b c d e)");

  scm_fcd_vector_fill_i(vec, fill, 1, 1);

  TEST_ASSERT_SCM_EQUAL(expected, vec);
}
