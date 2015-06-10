
#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_pair);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_pair)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_pair)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_pair, fcd_pair_p__return_true)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_fcd_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_TRUE(scm_fcd_pair_p(pair));
}

TEST(fcd_pair, fcd_pair_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_pair_p(SCM_FALSE_OBJ));
}

TEST(fcd_pair, fcd_pair_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_pair_p(SCM_OBJ_NULL));
}

TEST(fcd_pair, fcd_pair_P__return_true_obj)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_fcd_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_pair_P(pair));
}

TEST(fcd_pair, fcd_pair_P__return_false_obj)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_pair_P(SCM_FALSE_OBJ));
}

TEST(fcd_pair, fcd_cons__return_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_TRUE(scm_fcd_pair_p(pair));

  car = scm_fcd_car(pair);
  cdr = scm_fcd_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(fcd_pair, fcd_car__return_car_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_fcd_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(fcd_pair, fcd_cdr__return_cdr_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_fcd_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(fcd_pair, fcd_set_car_i)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  scm_fcd_set_car_i(pair, SCM_EOF_OBJ);
  car = scm_fcd_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, car);
}

TEST(fcd_pair, fcd_set_cdr_i)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  scm_fcd_set_cdr_i(pair, SCM_EOF_OBJ);
  cdr = scm_fcd_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, cdr);
}

TEST(fcd_pair, fcd_cxr__a)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_fcd_cxr(pair, "a");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(fcd_pair, fcd_cxr__d)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_fcd_cxr(pair, "d");

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(fcd_pair, fcd_cxr__ad)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  pair = scm_fcd_cons(SCM_EOF_OBJ, pair);
  act = scm_fcd_cxr(pair, "ad");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, act);
}

TEST(fcd_pair, fcd_cxr__passing_unknown_direcitve_return_ERROR)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_NULL(scm_fcd_cxr(pair, "c"));
}

TEST(fcd_pair, fcd_cxr__passing_a_object_is_not_pair_return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_fcd_cxr(SCM_OBJ_NULL, "a"));
}

TEST(fcd_pair, fcd_list_P__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_list_P(SCM_NIL_OBJ));
}

TEST(fcd_pair, fcd_list_P__proper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_fcd_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  lst = scm_fcd_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_list_P(lst));
}

TEST(fcd_pair, fcd_list_P__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_fcd_cons(SCM_TRUE_OBJ, SCM_EOF_OBJ);
  lst = scm_fcd_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_FALSE(scm_fcd_list_P(lst));
}

TEST(fcd_pair, fcd_list_P__not_pair)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_list_P(SCM_TRUE_OBJ));
}

TEST(fcd_pair, fcd_list_P__circularly_linked_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_fcd_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  scm_fcd_set_cdr_i(lst, lst);

  TEST_ASSERT_SCM_FALSE(scm_fcd_list_P(lst));
}

TEST(fcd_pair, fcd_make_list__specifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(#t #t #t)");

  lst = scm_fcd_make_list(3, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_make_list__unspecifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = SCM_NIL_OBJ;
  for (int i = 0; i < 3; i++)
    expected = scm_fcd_cons(SCM_UNDEF_OBJ, expected);

  lst = scm_fcd_make_list(3, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_list_cv)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_FALSE_OBJ };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(objs, 2);

  expected = read_cstr("(#t #f)");

  lst = scm_fcd_list_cv(objs, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_list_cv__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_fcd_list_cv(NULL, 0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(fcd_pair, fcd_list_cv__return_ERROR)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_OBJ_NULL };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(objs, 2);

  TEST_ASSERT_SCM_NULL(scm_fcd_list_cv(objs, 2));
}

TEST(fcd_pair, fcd_list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(#t #f)");

  lst = scm_fcd_list(2, SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_list__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_fcd_list(0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(fcd_pair, fcd_list__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_fcd_list(2, SCM_TRUE_OBJ, SCM_OBJ_NULL));
}

TEST(fcd_pair, fcd_length)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a (b c) d)");

  TEST_ASSERT_EQUAL_INT(3, scm_fcd_length(lst));
}

TEST(fcd_pair, fcd_length__empty_list)
{
  TEST_ASSERT_EQUAL_INT(0, scm_fcd_length(SCM_NIL_OBJ));
}

TEST(fcd_pair, fcd_length__improper_list__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b . c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_length(lst));
}

TEST(fcd_pair, fcd_append_lst)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = read_cstr("((a b) (c d) (e f))");
  expected = read_cstr("(a b c d e f)");

  actual = scm_fcd_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_pair, fcd_append_lst__list_has_item_is_empty_list)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = read_cstr("((a b) () (c d))");
  expected = read_cstr("(a b c d)");

  actual = scm_fcd_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_pair, fcd_append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = SCM_NIL_OBJ;

  actual = scm_fcd_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_pair, fcd_append_lst__list_has_item_is_not_list__return_ERROR)
{
  ScmObj lists = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists);

  lists = read_cstr("((a b) foo (c d))");

  TEST_ASSERT_SCM_NULL(scm_fcd_append_lst(lists));
}

TEST(fcd_pair, fcd_append_cv)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = read_cstr("(a b c d)");

  lists[0] = read_cstr("(a b)");
  lists[1] = read_cstr("(c d)");

  lst = scm_fcd_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_append_cv__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = read_cstr("a");

  lists[0] = read_cstr("()");
  lists[1] = read_cstr("a");

  lst = scm_fcd_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_append_cv__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_fcd_append_cv(NULL, 0));
}

TEST(fcd_pair, fcd_append_cv__passing_not_list__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = read_cstr("foo");
  lists[1] = read_cstr("a");

  TEST_ASSERT_SCM_NULL(scm_fcd_append_cv(lists, 2));
}

TEST(fcd_pair, fcd_append_cv__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = read_cstr("a");
  lists[1] = read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_append_cv(lists, 2));
}

TEST(fcd_pair, fcd_append)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = read_cstr("(a b c d)");

  lists[0] = read_cstr("(a b)");
  lists[1] = read_cstr("(c d)");

  lst = scm_fcd_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_append__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = read_cstr("a");

  lists[0] = read_cstr("()");
  lists[1] = read_cstr("a");

  lst = scm_fcd_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_append__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_fcd_append(0));
}

TEST(fcd_pair, fcd_append__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = read_cstr("a");
  lists[1] = read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_append(2, lists[0], lists[1]));
}

TEST(fcd_pair, fcd_reverse)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = read_cstr("(c b a)");
  lst = read_cstr("(a b c)");

  actual = scm_fcd_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_pair, fcd_reverse__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = read_cstr("(b a)");
  lst = read_cstr("(a b . c)");

  actual = scm_fcd_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(fcd_pair, fcd_reverse__not_pair)
{
  TEST_ASSERT_SCM_NIL(scm_fcd_reverse(SCM_TRUE_OBJ));
}

TEST(fcd_pair, fcd_list_tail)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(a b c)");
  expected = scm_fcd_cdr(lst);

  actual = scm_fcd_list_tail(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_list_tail__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_list_tail(lst, 4));
}

TEST(fcd_pair, fcd_list_tail__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_fcd_list_tail(SCM_TRUE_OBJ, 1));
}

TEST(fcd_pair, fcd_list_ref)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = read_cstr("(a (b) c)");
  expected = scm_fcd_cxr(lst, "ad");

  actual = scm_fcd_list_ref(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_list_ref__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_fcd_list_ref(lst, 3));
}

TEST(fcd_pair, fcd_list_ref__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_fcd_list_ref(SCM_TRUE_OBJ, 0));
}

TEST(fcd_pair, fcd_list_set_i)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = read_cstr("(a #t c)");
  lst = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_list_set_i(lst, 1, SCM_TRUE_OBJ));
  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(fcd_pair, fcd_list_set_i__return_ERROR_1)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_list_set_i(lst, 3, SCM_TRUE_OBJ));
}

TEST(fcd_pair, fcd_list_set_i__return_ERROR_2)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_list_set_i(SCM_TRUE_OBJ, 0, SCM_TRUE_OBJ));
}

TEST(fcd_pair, fcd_memq__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = read_cstr("(a b c)");
  o = read_cstr("b");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_fcd_memq(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_memq__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_memq(o, lst));
}

TEST(fcd_pair, fcd_memq__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_memq(o, lst));
}

TEST(fcd_pair, fcd_memv__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = read_cstr("(a #\\b c)");
  o = read_cstr("#\\b");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_fcd_memv(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_memv__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_memv(o, lst));
}

TEST(fcd_pair, fcd_memv__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_memv(o, lst));
}

TEST(fcd_pair, fcd_member__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_fcd_member(o, lst, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_member__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_fcd_member(o, lst, NULL));
}

TEST(fcd_pair, fcd_member__specify_compare__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = read_cstr("(a b c)");
  o = read_cstr("b");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_fcd_member(o, lst, scm_fcd_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_member__specify_compare__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = read_cstr("(a (b) c)");
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_member(o, lst, scm_fcd_eq_P));
}

TEST(fcd_pair, fcd_member__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_member(o, lst, NULL));
}

TEST(fcd_pair, fcd_assq__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = read_cstr("((a 1) (b 2) (c 2))");
  k = read_cstr("b");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_fcd_assq(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_assq__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) (#\\b 2) (c 2))");
  k = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assq(k, alist));
}

TEST(fcd_pair, fcd_assq__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assq(k, alist));
}

TEST(fcd_pair, fcd_assv__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = read_cstr("((a 1) (#\\b 2) (c 2))");
  k = read_cstr("#\\b");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_fcd_assv(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_assv__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assv(k, alist));
}

TEST(fcd_pair, fcd_assv__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assv(k, alist));
}

TEST(fcd_pair, fcd_assoc__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_fcd_assoc(k, alist, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_assoc__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assoc(k, alist, NULL));
}

TEST(fcd_pair, fcd_assoc__specify_compare__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = read_cstr("((a 1) (b 2) (c 2))");
  k = read_cstr("b");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_fcd_assoc(k, alist, scm_fcd_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(fcd_pair, fcd_assoc__specify_compare__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = read_cstr("((a 1) ((b) 2) (c 2))");
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assoc(k, alist, scm_fcd_eq_P));
}

TEST(fcd_pair, fcd_assoc__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_fcd_assoc(k, alist, NULL));
}

TEST(fcd_pair, fcd_list_copy)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = read_cstr("(a b c)");

  replica = scm_fcd_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(fcd_pair, fcd_list_copy__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = SCM_NIL_OBJ;

  replica = scm_fcd_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(fcd_pair, fcd_list_copy__not_list__return_obj)
{
  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, scm_fcd_list_copy(SCM_EOF_OBJ));
}
