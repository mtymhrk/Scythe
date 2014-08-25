#include "object.h"
#include "api.h"

#include "test.h"

TEST_GROUP(api_pair_and_lists);

static ScmEvaluator *ev;

TEST_SETUP(api_pair_and_lists)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(api_pair_and_lists)
{
  scm_capi_evaluator_end(ev);
}

TEST(api_pair_and_lists, capi_pair_p__return_true)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_api_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_TRUE(scm_capi_pair_p(pair));
}

TEST(api_pair_and_lists, capi_pair_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_pair_p(SCM_FALSE_OBJ));
}

TEST(api_pair_and_lists, capi_pair_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_pair_p(SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, api_pair_P__return_true_obj)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_api_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_api_pair_P(pair));
}

TEST(api_pair_and_lists, api_pair_P__return_false_obj)
{
  TEST_ASSERT_SCM_FALSE(scm_api_pair_P(SCM_FALSE_OBJ));
}

TEST(api_pair_and_lists, api_cons__return_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_TRUE(scm_capi_pair_p(pair));

  car = scm_api_car(pair);
  cdr = scm_api_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(api_pair_and_lists, api_cons__return_ERROR)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_NULL(pair);
}

TEST(api_pair_and_lists, capi_car__return_car_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_api_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(api_pair_and_lists, capi_car__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_car(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_cdr__return_cdr_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_api_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(api_pair_and_lists, capi_cdr__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_cdr(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_set_car_i)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_set_car_i(pair, SCM_EOF_OBJ));

  car = scm_api_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, car);
}

TEST(api_pair_and_lists, capi_set_car_i__return_ERROR_1)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_set_car_i(SCM_OBJ_NULL, SCM_EOF_OBJ));
}

TEST(api_pair_and_lists, capi_set_car_i__return_ERROR_2)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_set_car_i(pair, SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, api_set_car_i)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_TRUE(scm_obj_not_null_p(scm_api_set_car_i(pair, SCM_EOF_OBJ)));

  car = scm_api_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, car);
}

TEST(api_pair_and_lists, api_set_car_i__return_ERROR_1)
{
  TEST_ASSERT_SCM_NULL(scm_api_set_car_i(SCM_OBJ_NULL, SCM_EOF_OBJ));
}

TEST(api_pair_and_lists, api_set_car_i__return_ERROR_2)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_NULL(scm_api_set_car_i(pair, SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, capi_set_cdr_i)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_set_cdr_i(pair, SCM_EOF_OBJ));

  cdr = scm_api_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, cdr);
}

TEST(api_pair_and_lists, capi_set_cdr_i__return_ERROR_1)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_set_cdr_i(SCM_OBJ_NULL, SCM_EOF_OBJ));
}

TEST(api_pair_and_lists, capi_set_cdr_i__return_ERROR_2)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_set_cdr_i(pair, SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, api_set_cdr_i)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_TRUE(scm_obj_not_null_p(scm_api_set_cdr_i(pair, SCM_EOF_OBJ)));

  cdr = scm_api_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, cdr);
}

TEST(api_pair_and_lists, api_set_cdr_i__return_ERROR_1)
{
  TEST_ASSERT_SCM_NULL(scm_api_set_cdr_i(SCM_OBJ_NULL, SCM_EOF_OBJ));
}

TEST(api_pair_and_lists, api_set_cdr_i__return_ERROR_2)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_NULL(scm_api_set_cdr_i(pair, SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, capi_cxr__a)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_capi_cxr(pair, "a");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(api_pair_and_lists, capi_cxr__d)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_capi_cxr(pair, "d");

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(api_pair_and_lists, capi_cxr__ad)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  pair = scm_api_cons(SCM_EOF_OBJ, pair);
  act = scm_capi_cxr(pair, "ad");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, act);
}

TEST(api_pair_and_lists, capi_cxr__passing_unknown_direcitve_return_ERROR)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_NULL(scm_capi_cxr(pair, "c"));
}

TEST(api_pair_and_lists, capi_cxr__passing_a_object_is_not_pair_return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_cxr(SCM_OBJ_NULL, "a"));
}

TEST(api_pair_and_lists, api_list_P__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_api_list_P(SCM_NIL_OBJ));
}

TEST(api_pair_and_lists, api_list_P__proper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_api_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  lst = scm_api_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_TRUE(scm_api_list_P(lst));
}

TEST(api_pair_and_lists, api_list_P__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_api_cons(SCM_TRUE_OBJ, SCM_EOF_OBJ);
  lst = scm_api_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_FALSE(scm_api_list_P(lst));
}

TEST(api_pair_and_lists, api_list_P__not_pair)
{
  TEST_ASSERT_SCM_FALSE(scm_api_list_P(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_P__circularly_linked_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_api_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  scm_capi_set_cdr_i(lst, lst);

  TEST_ASSERT_SCM_FALSE(scm_api_list_P(lst));
}

TEST(api_pair_and_lists, capi_make_list__specifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(#t #t #t)");

  lst = scm_capi_make_list(3, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_make_list__unspecifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = SCM_NIL_OBJ;
  for (int i = 0; i < 3; i++)
    expected = scm_api_cons(SCM_UNDEF_OBJ, expected);

  lst = scm_capi_make_list(3, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, api_make_list__specifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n, &expected);

  expected = read_cstr("(#t #t #t)");

  n = read_cstr("3");

  lst = scm_api_make_list(n, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, api_make_list__unspecifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n, &expected);

  expected = SCM_NIL_OBJ;
  for (int i = 0; i < 3; i++)
    expected = scm_api_cons(SCM_UNDEF_OBJ, expected);

  n = read_cstr("3");

  lst = scm_capi_make_list(3, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_list_cv)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_FALSE_OBJ };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(objs)/sizeof(objs[0]); i++)
    SCM_REFSTK_REG(&objs[i]);

  expected = read_cstr("(#t #f)");

  lst = scm_capi_list_cv(objs, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_list_cv__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_capi_list_cv(NULL, 0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(api_pair_and_lists, capi_list_cv__return_ERROR)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_OBJ_NULL };

  SCM_REFSTK_INIT;

  for (size_t i = 0; i < sizeof(objs)/sizeof(objs[0]); i++)
    SCM_REFSTK_REG(&objs[i]);

  TEST_ASSERT_SCM_NULL(scm_capi_list_cv(objs, 2));
}

TEST(api_pair_and_lists, capi_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(#t #f)");

  lst = scm_capi_list(2, SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_list__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_capi_list(0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(api_pair_and_lists, capi_list__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_list(2, SCM_TRUE_OBJ, SCM_OBJ_NULL));
}

TEST(api_pair_and_lists, capi_length)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a (b c) d)");

  TEST_ASSERT_EQUAL_INT(3, scm_capi_length(lst));
}

TEST(api_pair_and_lists, capi_length__empty_list)
{
  TEST_ASSERT_EQUAL_INT(0, scm_capi_length(SCM_NIL_OBJ));
}

TEST(api_pair_and_lists, capi_length__not_piar__return_ERROR)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_length(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_length__improper_list__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b . c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_length(lst));
}

TEST(api_pair_and_lists, api_length)
{
  ScmObj lst = SCM_OBJ_INIT, len = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &len, &expected);

  expected = scm_capi_make_number_from_sword(3);

  lst = read_cstr("(a (b c) d)");

  len = scm_api_length(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, len));
}

TEST(api_pair_and_lists, api_length__empty_list)
{
  ScmObj len = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&len, &expected);

  expected = scm_capi_make_number_from_sword(0);

  len = scm_api_length(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, len));
}

TEST(api_pair_and_lists, api_length__not_piar__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_length(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_length__improper_list__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b . c)");

  TEST_ASSERT_SCM_NULL(scm_api_length(lst));
}

TEST(api_pair_and_lists, capi_append_lst)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = read_cstr("((a b) (c d) (e f))");
  expected = read_cstr("(a b c d e f)");

  actual = scm_capi_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_append_lst__list_has_item_is_empty_list)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = read_cstr("((a b) () (c d))");
  expected = read_cstr("(a b c d)");

  actual = scm_capi_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = SCM_NIL_OBJ;

  actual = scm_capi_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_append_lst__list_has_item_is_not_list__return_ERROR)
{
  ScmObj lists = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists);

  lists = read_cstr("((a b) foo (c d))");

  TEST_ASSERT_SCM_NULL(scm_capi_append_lst(lists));
}

TEST(api_pair_and_lists, capi_append_cv)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  expected = read_cstr("(a b c d)");

  lists[0] = read_cstr("(a b)");
  lists[1] = read_cstr("(c d)");

  lst = scm_capi_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_append_cv__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  expected = read_cstr("a");

  lists[0] = read_cstr("()");
  lists[1] = read_cstr("a");

  lst = scm_capi_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_append_cv__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_capi_append_cv(NULL, 0));
}

TEST(api_pair_and_lists, capi_append_cv__passing_not_list__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  lists[0] = read_cstr("foo");
  lists[1] = read_cstr("a");

  TEST_ASSERT_SCM_NULL(scm_capi_append_cv(lists, 2));
}

TEST(api_pair_and_lists, capi_append_cv__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  lists[0] = read_cstr("a");
  lists[1] = read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_capi_append_cv(lists, 2));
}

TEST(api_pair_and_lists, capi_append)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  expected = read_cstr("(a b c d)");

  lists[0] = read_cstr("(a b)");
  lists[1] = read_cstr("(c d)");

  lst = scm_capi_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_append__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  expected = read_cstr("a");

  lists[0] = read_cstr("()");
  lists[1] = read_cstr("a");

  lst = scm_capi_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_append__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_capi_append(0));
}

TEST(api_pair_and_lists, capi_append__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  for (size_t i = 0; i < sizeof(lists)/sizeof(lists[0]); i++)
    SCM_REFSTK_REG(&lists[i]);

  lists[0] = read_cstr("a");
  lists[1] = read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_capi_append(2, lists[0], lists[1]));
}

TEST(api_pair_and_lists, capi_reverse)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = read_cstr("(c b a)");
  lst = read_cstr("(a b c)");

  actual = scm_api_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_reverse__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = read_cstr("(b a)");
  lst = read_cstr("(a b . c)");

  actual = scm_api_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_reverse__not_pair)
{
  TEST_ASSERT_SCM_NIL(scm_api_reverse(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_list_tail)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(a b c)");
  expected = scm_api_cdr(lst);

  actual = scm_capi_list_tail(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_list_tail__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_capi_list_tail(lst, 4));
}

TEST(api_pair_and_lists, capi_list_tail__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_capi_list_tail(SCM_TRUE_OBJ, 1));
}

TEST(api_pair_and_lists, api_list_tail)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n,
                       &actual, &expected);


  lst = read_cstr("(a b c)");
  expected = scm_api_cdr(lst);
  n = read_cstr("1");

  actual = scm_api_list_tail(lst, n);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("4");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(lst, n));
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(SCM_TRUE_OBJ, n));
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(lst, n));
}

TEST(api_pair_and_lists, capi_list_ref)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(a (b) c)");
  expected = scm_capi_cxr(lst, "ad");

  actual = scm_capi_list_ref(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_list_ref__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_capi_list_ref(lst, 3));
}

TEST(api_pair_and_lists, capi_list_ref__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_capi_list_ref(SCM_TRUE_OBJ, 0));
}

TEST(api_pair_and_lists, api_list_ref)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n,
                       &actual, &expected);

  lst = read_cstr("(a (b) c)");
  expected = scm_capi_cxr(lst, "ad");
  n = read_cstr("1");

  actual = scm_api_list_ref(lst, n);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(lst, n));
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(SCM_TRUE_OBJ, n));
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(lst, n));
}

TEST(api_pair_and_lists, capi_list_set_i)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(a #t c)");
  lst = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_list_set_i(lst, 1, SCM_TRUE_OBJ));
  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_list_set_i__return_ERROR_1)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_list_set_i(lst, 3, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_list_set_i__return_ERROR_2)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_list_set_i(SCM_TRUE_OBJ, 0, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_set_i)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n, &expected);

  expected = read_cstr("(a #t c)");
  lst = read_cstr("(a b c)");
  n = read_cstr("1");

  TEST_ASSERT_TRUE(scm_obj_not_null_p( scm_api_list_set_i(lst, n, SCM_TRUE_OBJ)));
  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_1)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(lst, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(SCM_NIL_OBJ, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = read_cstr("(a b c)");
  n = read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(lst, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, capi_memq__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                       &actual, &expected);

  lst = read_cstr("(a b c)");
  o = read_cstr("b");
  expected = scm_capi_list_tail(lst, 1);

  actual = scm_capi_memq(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_memq__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_memq(o, lst));
}

TEST(api_pair_and_lists, capi_memq__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_memq(o, lst));
}

TEST(api_pair_and_lists, capi_memv__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                       &actual, &expected);

  lst = read_cstr("(a #\\b c)");
  o = read_cstr("#\\b");
  expected = scm_capi_list_tail(lst, 1);

  actual = scm_capi_memv(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_memv__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_memv(o, lst));
}

TEST(api_pair_and_lists, capi_memv__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_capi_memv(o, lst));
}

TEST(api_pair_and_lists, capi_member__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                       &actual, &expected);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");
  expected = scm_capi_list_tail(lst, 1);

  actual = scm_capi_member(o, lst, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_member__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_capi_member(o, lst, NULL));
}

TEST(api_pair_and_lists, capi_member__specify_compare__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                       &actual, &expected);

  lst = read_cstr("(a b c)");
  o = read_cstr("b");
  expected = scm_capi_list_tail(lst, 1);

  actual = scm_capi_member(o, lst, scm_api_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_member__specify_compare__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_member(o, lst, scm_api_eq_P));
}

TEST(api_pair_and_lists, capi_member__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_member(o, lst, NULL));
}

TEST(api_pair_and_lists, capi_assq__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                       &actual, &expected);

  alist = read_cstr("((a 1) (b 2) (c 2))");
  k = read_cstr("b");
  expected = scm_capi_cxr(alist, "ad");

  actual = scm_capi_assq(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_assq__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) (#\\b 2) (c 2))");
  k = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_capi_assq(k, alist));
}

TEST(api_pair_and_lists, capi_assq__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("b");

  TEST_ASSERT_SCM_FALSE(scm_capi_assq(k, alist));
}

TEST(api_pair_and_lists, capi_assv__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                       &actual, &expected);

  alist = read_cstr("((a 1) (#\\b 2) (c 2))");
  k = read_cstr("#\\b");
  expected = scm_capi_cxr(alist, "ad");

  actual = scm_capi_assv(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_assv__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_assv(k, alist));
}

TEST(api_pair_and_lists, capi_assv__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_capi_assv(k, alist));
}

TEST(api_pair_and_lists, capi_assoc__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                       &actual, &expected);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");
  expected = scm_capi_cxr(alist, "ad");

  actual = scm_capi_assoc(k, alist, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_assoc__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_capi_assoc(k, alist, NULL));
}

TEST(api_pair_and_lists, capi_assoc__specify_compare__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                       &actual, &expected);

  alist = read_cstr("((a 1) (b 2) (c 2))");
  k = read_cstr("b");
  expected = scm_capi_cxr(alist, "ad");

  actual = scm_capi_assoc(k, alist, scm_api_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, capi_assoc__specify_compare__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_assoc(k, alist, scm_api_eq_P));
}

TEST(api_pair_and_lists, capi_assoc__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_capi_assoc(k, alist, NULL));
}

TEST(api_pair_and_lists, api_list_copy)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = read_cstr("(a b c)");

  replica = scm_api_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(api_pair_and_lists, api_list_copy__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = SCM_NIL_OBJ;

  replica = scm_api_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(api_pair_and_lists, api_list_copy__not_list__return_obj)
{
  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, scm_api_list_copy(SCM_EOF_OBJ));
}
