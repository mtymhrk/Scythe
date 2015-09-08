#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_pair_and_lists);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_pair_and_lists)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_pair_and_lists)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
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

  TEST_ASSERT_SCM_TRUE(scm_api_pair_P(pair));

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
  scm_api_set_cdr_i(lst, lst);

  TEST_ASSERT_SCM_FALSE(scm_api_list_P(lst));
}

TEST(api_pair_and_lists, api_make_list__specifying_fill)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n, &expected);

  expected = ut_read_cstr("(#t #t #t)");

  n = ut_read_cstr("3");

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

  n = ut_read_cstr("3");

  lst = scm_api_make_list(n, SCM_OBJ_NULL);

  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, capi_length)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a (b c) d)");

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

  lst = ut_read_cstr("(a b . c)");

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_length(lst));
}

TEST(api_pair_and_lists, api_length)
{
  ScmObj lst = SCM_OBJ_INIT, len = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &len, &expected);

  expected = scm_fcd_make_number_from_sword(3);

  lst = ut_read_cstr("(a (b c) d)");

  len = scm_api_length(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, len));
}

TEST(api_pair_and_lists, api_length__empty_list)
{
  ScmObj len = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&len, &expected);

  expected = scm_fcd_make_number_from_sword(0);

  len = scm_api_length(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, len));
}

TEST(api_pair_and_lists, api_length__not_piar__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_length(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_length__improper_list__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(a b . c)");

  TEST_ASSERT_SCM_NULL(scm_api_length(lst));
}

TEST(api_pair_and_lists, api_append_lst)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = ut_read_cstr("((a b) (c d) (e f))");
  expected = ut_read_cstr("(a b c d e f)");

  actual = scm_api_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, api_append_lst__list_has_item_is_empty_list)
{
  ScmObj lists = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists, &actual, &expected);

  lists = ut_read_cstr("((a b) () (c d))");
  expected = ut_read_cstr("(a b c d)");

  actual = scm_api_append_lst(lists);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, api_append_lst__empty_list)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = SCM_NIL_OBJ;

  actual = scm_api_append_lst(SCM_NIL_OBJ);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, api_append_lst__list_has_item_is_not_list__return_ERROR)
{
  ScmObj lists = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lists);

  lists = ut_read_cstr("((a b) foo (c d))");

  TEST_ASSERT_SCM_NULL(scm_api_append_lst(lists));
}

TEST(api_pair_and_lists, capi_reverse)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = ut_read_cstr("(c b a)");
  lst = ut_read_cstr("(a b c)");

  actual = scm_api_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_reverse__improper_list)
{
  ScmObj lst = SCM_OBJ_INIT, actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &actual, &expected);

  expected = ut_read_cstr("(b a)");
  lst = ut_read_cstr("(a b . c)");

  actual = scm_api_reverse(lst);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_pair_and_lists, capi_reverse__not_pair)
{
  TEST_ASSERT_SCM_NIL(scm_api_reverse(SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_tail)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n,
                      &actual, &expected);


  lst = ut_read_cstr("(a b c)");
  expected = scm_api_cdr(lst);
  n = ut_read_cstr("1");

  actual = scm_api_list_tail(lst, n);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("4");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(lst, n));
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = ut_read_cstr("1");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(SCM_TRUE_OBJ, n));
}

TEST(api_pair_and_lists, api_list_tail__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_tail(lst, n));
}

TEST(api_pair_and_lists, api_list_ref)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n,
                      &actual, &expected);

  lst = ut_read_cstr("(a (b) c)");
  expected = scm_fcd_cxr(lst, "ad");
  n = ut_read_cstr("1");

  actual = scm_api_list_ref(lst, n);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(lst, n));
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = ut_read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(SCM_TRUE_OBJ, n));
}

TEST(api_pair_and_lists, api_list_ref__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_ref(lst, n));
}

TEST(api_pair_and_lists, api_list_set_i)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n, &expected);

  expected = ut_read_cstr("(a #t c)");
  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("1");

  TEST_ASSERT_TRUE(scm_obj_not_null_p( scm_api_list_set_i(lst, n, SCM_TRUE_OBJ)));
  TEST_ASSERT_SCM_EQUAL(expected, lst);
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_1)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("3");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(lst, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_2)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&n);

  n = ut_read_cstr("0");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(SCM_NIL_OBJ, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_list_set_i__return_ERROR_3)
{
  ScmObj lst = SCM_OBJ_INIT, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &n);

  lst = ut_read_cstr("(a b c)");
  n = ut_read_cstr("z");

  TEST_ASSERT_SCM_NULL(scm_api_list_set_i(lst, n, SCM_TRUE_OBJ));
}

TEST(api_pair_and_lists, api_memq__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a b c)");
  o = ut_read_cstr("b");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_api_memq(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_memq__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_api_memq(o, lst));
}

TEST(api_pair_and_lists, api_memq__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_api_memq(o, lst));
}

TEST(api_pair_and_lists, api_memv__matched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o,
                      &actual, &expected);

  lst = ut_read_cstr("(a #\\b c)");
  o = ut_read_cstr("#\\b");
  expected = scm_fcd_list_tail(lst, 1);

  actual = scm_api_memv(o, lst);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_memv__unmatched)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = ut_read_cstr("(a (b) c)");
  o = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_api_memv(o, lst));
}

TEST(api_pair_and_lists, api_memv__not_list)
{
  ScmObj lst = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &o);

  lst = SCM_TRUE_OBJ;
  o = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_memv(o, lst));
}

TEST(api_pair_and_lists, api_assq__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) (b 2) (c 2))");
  k = ut_read_cstr("b");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_api_assq(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_assq__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) (#\\b 2) (c 2))");
  k = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_assq(k, alist));
}

TEST(api_pair_and_lists, api_assq__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = ut_read_cstr("b");

  TEST_ASSERT_SCM_FALSE(scm_api_assq(k, alist));
}

TEST(api_pair_and_lists, api_assv__matched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k,
                      &actual, &expected);

  alist = ut_read_cstr("((a 1) (#\\b 2) (c 2))");
  k = ut_read_cstr("#\\b");
  expected = scm_fcd_cxr(alist, "ad");

  actual = scm_api_assv(k, alist);

  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_pair_and_lists, api_assv__unmatched)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = ut_read_cstr("((a 1) ((b) 2) (c 2))");
  k = ut_read_cstr("(b)");

  TEST_ASSERT_SCM_FALSE(scm_api_assv(k, alist));
}

TEST(api_pair_and_lists, api_assv__not_list)
{
  ScmObj alist = SCM_OBJ_INIT, k = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&alist, &k);

  alist = SCM_TRUE_OBJ;
  k = ut_read_cstr("#\\b");

  TEST_ASSERT_SCM_FALSE(scm_api_assv(k, alist));
}

TEST(api_pair_and_lists, api_list_copy)
{
  ScmObj lst = SCM_OBJ_INIT, replica = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &replica);

  lst = ut_read_cstr("(a b c)");

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
