#include "pair.c"

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/pair.h"

#include "test.h"

TEST_GROUP(pair);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(pair)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(pair)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(pair, pair_p__return_true)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_TRUE(scm_pair_p(pair));
}

TEST(pair, pair_p__return_false)
{
  TEST_ASSERT_FALSE(scm_pair_p(SCM_FALSE_OBJ));
}

TEST(pair, pair_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_pair_p(SCM_OBJ_NULL));
}

TEST(pair, pair_P__return_true_obj)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair);

  pair = scm_cons(SCM_FALSE_OBJ, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_pair_P(pair));
}

TEST(pair, pair_P__return_false_obj)
{
  TEST_ASSERT_SCM_FALSE(scm_pair_P(SCM_FALSE_OBJ));
}

TEST(pair, cons__return_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_TRUE(scm_pair_p(pair));

  car = scm_car(pair);
  cdr = scm_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(pair, car__return_car_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(pair, cdr__return_cdr_of_pair)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(pair, set_car_i)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  scm_set_car(pair, SCM_EOF_OBJ);
  car = scm_car(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, car);
}

TEST(pair, set_cdr_i)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  scm_set_cdr(pair, SCM_EOF_OBJ);
  cdr = scm_cdr(pair);

  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, cdr);
}

TEST(pair, cxr__a)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  car = scm_cxr(pair, "a");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, car);
}

TEST(pair, cxr__d)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &cdr);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  cdr = scm_cxr(pair, "d");

  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, cdr);
}

TEST(pair, cxr__ad)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  pair = scm_cons(SCM_EOF_OBJ, pair);
  act = scm_cxr(pair, "ad");

  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, act);
}

TEST(pair, cxr__passing_unknown_direcitve_return_ERROR)
{
  ScmObj pair = SCM_OBJ_INIT, act = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &act);

  pair = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_NULL(scm_cxr(pair, "c"));
}

TEST(pair, cxr__passing_a_object_is_not_pair_return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_cxr(SCM_OBJ_NULL, "a"));
}

TEST(pair, list_P__empty_list)
{
  TEST_ASSERT_SCM_TRUE(scm_list_P(SCM_NIL_OBJ));
}

TEST(pair, list_P__proper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  lst = scm_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_TRUE(scm_list_P(lst));
}

TEST(pair, list_P__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_cons(SCM_TRUE_OBJ, SCM_EOF_OBJ);
  lst = scm_cons(SCM_FALSE_OBJ, lst);

  TEST_ASSERT_SCM_FALSE(scm_list_P(lst));
}

TEST(pair, list_P__not_pair)
{
  TEST_ASSERT_SCM_FALSE(scm_list_P(SCM_TRUE_OBJ));
}

TEST(pair, list_P__circularly_linked_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_cons(SCM_TRUE_OBJ, SCM_NIL_OBJ);
  scm_set_cdr(lst, lst);

  TEST_ASSERT_SCM_FALSE(scm_list_P(lst));
}

TEST(pair, make_list__specifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = ut_read_cstr("(#t #t #t)");

  lst = scm_make_list(3, SCM_TRUE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, make_list__unspecifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = SCM_NIL_OBJ;
  for (int i = 0; i < 3; i++)
    expected = scm_cons(SCM_UNDEF_OBJ, expected);

  lst = scm_make_list(3, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, list_cv)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_FALSE_OBJ };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(objs, 2);

  expected = ut_read_cstr("(#t #f)");

  lst = scm_list_cv(objs, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, list_cv__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_list_cv(NULL, 0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(pair, list_cv__return_ERROR)
{
  ScmObj objs[2] = { SCM_TRUE_OBJ, SCM_OBJ_NULL };

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(objs, 2);

  TEST_ASSERT_SCM_NULL(scm_list_cv(objs, 2));
}

TEST(pair, list)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = ut_read_cstr("(#t #f)");

  lst = scm_list(2, SCM_TRUE_OBJ, SCM_FALSE_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, list__return_empty_list)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = scm_list(0);

  TEST_ASSERT_SCM_NIL(lst);
}

TEST(pair, list__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_list(2, SCM_TRUE_OBJ, SCM_OBJ_NULL));
}

TEST(pair, length)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a (b c) d)");

  TEST_ASSERT_EQUAL_INT(3, scm_length(lst));
}

TEST(pair, length__empty_list)
{
  TEST_ASSERT_EQUAL_INT(0, scm_length(SCM_NIL_OBJ));
}

TEST(pair, length__improper_list__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a b . c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_length(lst));
}

TEST(pair, append_lst)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = ut_read_cstr("((a b) (c d) (e f))");
  expected = ut_read_cstr("(a b c d e f)");

  actual = scm_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(pair, append_lst__list_has_item_is_empty_list)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = ut_read_cstr("((a b) () (c d))");
  expected = ut_read_cstr("(a b c d)");

  actual = scm_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(pair, append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = SCM_NIL_OBJ;

  actual = scm_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(pair, append_lst__list_has_item_is_not_list__return_ERROR)
{
  ScmObj lists = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists);

  lists = ut_read_cstr("((a b) foo (c d))");

  TEST_ASSERT_SCM_NULL(scm_append_lst(lists));
}

TEST(pair, append_cv)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = ut_read_cstr("(a b c d)");

  lists[0] = ut_read_cstr("(a b)");
  lists[1] = ut_read_cstr("(c d)");

  lst = scm_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, append_cv__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = ut_read_cstr("a");

  lists[0] = ut_read_cstr("()");
  lists[1] = ut_read_cstr("a");

  lst = scm_append_cv(lists, 2);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, append_cv__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_append_cv(NULL, 0));
}

TEST(pair, append_cv__passing_not_list__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = ut_read_cstr("foo");
  lists[1] = ut_read_cstr("a");

  TEST_ASSERT_SCM_NULL(scm_append_cv(lists, 2));
}

TEST(pair, append_cv__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = ut_read_cstr("a");
  lists[1] = ut_read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_append_cv(lists, 2));
}

TEST(pair, append)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = ut_read_cstr("(a b c d)");

  lists[0] = ut_read_cstr("(a b)");
  lists[1] = ut_read_cstr("(c d)");

  lst = scm_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, append__passing_empty_list)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);
  SCM_REFSTK_REG_ARY(lists, 2);

  expected = ut_read_cstr("a");

  lists[0] = ut_read_cstr("()");
  lists[1] = ut_read_cstr("a");

  lst = scm_append(2, lists[0], lists[1]);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, append__no_arg)
{
  TEST_ASSERT_SCM_NIL(scm_append(0));
}

TEST(pair, append__return_ERROR)
{
  ScmObj lists[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);
  SCM_REFSTK_REG_ARY(lists, 2);

  lists[0] = ut_read_cstr("a");
  lists[1] = ut_read_cstr("(b c)");

  TEST_ASSERT_SCM_NULL(scm_append(2, lists[0], lists[1]));
}

TEST(pair, reverse)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = ut_read_cstr("(c b a)");
  lst = ut_read_cstr("(a b c)");

  actual = scm_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(pair, reverse__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = ut_read_cstr("(b a)");
  lst = ut_read_cstr("(a b . c)");

  actual = scm_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(pair, reverse__not_pair)
{
  TEST_ASSERT_SCM_NIL(scm_reverse(SCM_TRUE_OBJ));
}

TEST(pair, list_tail)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(a b c)");
  expected = scm_cdr(lst);

  actual = scm_list_tail(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, list_tail__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_list_tail(lst, 4));
}

TEST(pair, list_tail__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_list_tail(SCM_TRUE_OBJ, 1));
}

TEST(pair, list_ref)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  lst = ut_read_cstr("(a (b) c)");
  expected = scm_cxr(lst, "ad");

  actual = scm_list_ref(lst, 1);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, list_ref__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a b c)");

  TEST_ASSERT_SCM_NULL(scm_list_ref(lst, 3));
}

TEST(pair, list_ref__return_ERROR_2)
{
  TEST_ASSERT_SCM_NULL(scm_list_ref(SCM_TRUE_OBJ, 0));
}

TEST(pair, list_set_i)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected);

  expected = ut_read_cstr("(a #t c)");
  lst = ut_read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_list_set(lst, 1, SCM_TRUE_OBJ));
  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(pair, list_set_i__return_ERROR_1)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_list_set(lst, 3, SCM_TRUE_OBJ));
}

TEST(pair, list_set_i__return_ERROR_2)
{
  TEST_ASSERT_EQUAL_INT(-1, scm_list_set(SCM_TRUE_OBJ, 0, SCM_TRUE_OBJ));
}

TEST(pair, memq__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a b c)");
  o = ut_read_cstr("b");
  expected = scm_list_tail(lst, 1);

  actual = scm_memq(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, memq__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_memq(o, lst));
}

TEST(pair, memq__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_memq(o, lst));
}

TEST(pair, memv__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a #\\b c)");
  o = ut_read_cstr("#\\b");
  expected = scm_list_tail(lst, 1);

  actual = scm_memv(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, memv__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_memv(o, lst));
}

TEST(pair, memv__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_memv(o, lst));
}

TEST(pair, member__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");
  expected = scm_list_tail(lst, 1);

  actual = scm_member(o, lst, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, member__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_member(o, lst, NULL));
}

TEST(pair, member__specify_compare__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a b c)");
  o = ut_read_cstr("b");
  expected = scm_list_tail(lst, 1);

  actual = scm_member(o, lst, scm_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, member__specify_compare__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_member(o, lst, scm_eq_P));
}

TEST(pair, member__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_member(o, lst, NULL));
}

TEST(pair, assq__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) (b 2) (c 2))");
  k = ut_read_cstr("b");
  expected = scm_cxr(alist, "ad");

  actual = scm_assq(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, assq__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) (#\\b 2) (c 2))");
  k = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_assq(k, alist));
}

TEST(pair, assq__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = ut_read_cstr("b");

  TEST_ASSERT_SCM_FALSE(scm_assq(k, alist));
}

TEST(pair, assv__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) (#\\b 2) (c 2))");
  k = ut_read_cstr("#\\b");
  expected = scm_cxr(alist, "ad");

  actual = scm_assv(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, assv__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) ((b) 2) (c 2))");
  k = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_assv(k, alist));
}

TEST(pair, assv__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_assv(k, alist));
}

TEST(pair, assoc__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) ((b) 2) (c 2))");
  k = ut_read_cstr("(b)");
  expected = scm_cxr(alist, "ad");

  actual = scm_assoc(k, alist, NULL);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, assoc__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) ((b) 2) (c 2))");
  k = ut_read_cstr("z");

  TEST_ASSERT_SCM_FALSE(scm_assoc(k, alist, NULL));
}

TEST(pair, assoc__specify_compare__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) (b 2) (c 2))");
  k = ut_read_cstr("b");
  expected = scm_cxr(alist, "ad");

  actual = scm_assoc(k, alist, scm_eq_P);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(pair, assoc__specify_compare__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) ((b) 2) (c 2))");
  k = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_assoc(k, alist, scm_eq_P));
}

TEST(pair, assoc__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_assoc(k, alist, NULL));
}

TEST(pair, list_copy)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = ut_read_cstr("(a b c)");

  replica = scm_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(pair, list_copy__empty_list)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = SCM_NIL_OBJ;

  replica = scm_list_copy(lst);

  TEST_ASSERT_SCM_EQUAL(lst,  replica);
}

TEST(pair, list_copy__not_list__return_obj)
{
  TEST_ASSERT_SCM_EQ(SCM_EOF_OBJ, scm_list_copy(SCM_EOF_OBJ));
}
