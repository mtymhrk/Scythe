#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/fixnum.h"

#include "test.h"

TEST_GROUP(api_fixnum);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(api_fixnum)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_fixnum)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static void
check_list_elements(ScmObj lst, bool (*check)(ScmObj elm))
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &l);

  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l))
    TEST_ASSERT_TRUE(check(scm_fcd_car(l)));
}

TEST(api_fixnum, api_number_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_number_P(fn));
}

TEST(api_fixnum, api_complex_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_complex_P(fn));
}

TEST(api_fixnum, api_real_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_real_P(fn));
}

TEST(api_fixnum, api_rational_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_rational_P(fn));
}

TEST(api_fixnum, api_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_integer_P(fn));
}

TEST(api_fixnum, api_exact_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_P(fn));
}

TEST(api_fixnum, api_inexact_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_inexact_P(fn));
}

TEST(api_fixnum, api_exact_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_integer_P(fn));
}

TEST(api_fixnum, api_finite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_finite_P(fn));
}

TEST(api_fixnum, api_infinite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_infinite_P(fn));
}

TEST(api_fixnum, api_nan_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_nan_P(fn));
}

TEST(api_fixnum, api_num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 123 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P_lst(lst));
}

TEST(api_fixnum, api_num_eq_P_lst__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 123 124)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_eq_P_lst(lst));
}

TEST(api_fixnum, api_num_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_lt_P_lst(lst));
}

TEST(api_fixnum, api_num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_lt_P_lst(lst));
}

TEST(api_fixnum, api_num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_lt_P_lst(lst));
}

TEST(api_fixnum, api_num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_gt_P_lst(lst));
}

TEST(api_fixnum, api_num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_gt_P_lst(lst));
}

TEST(api_fixnum, api_num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_gt_P_lst(lst));
}

TEST(api_fixnum, api_num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_le_P_lst(lst));
}

TEST(api_fixnum, api_num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_le_P_lst(lst));
}

TEST(api_fixnum, api_num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_le_P_lst(lst));
}

TEST(api_fixnum, api_num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_api_num_ge_P_lst(lst));
}

TEST(api_fixnum, api_num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_ge_P_lst(lst));
}

TEST(api_fixnum, api_num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_api_num_ge_P_lst(lst));
}

TEST(api_fixnum, api_zero_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_zero_P(fn));
}

TEST(api_fixnum, api_zero_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_zero_P(fn));
}

TEST(api_fixnum, api_positive_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_positive_P(fn));
}

TEST(api_fixnum, api_positive_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_positive_P(fn));
}

TEST(api_fixnum, api_negative_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_negative_P(fn));
}

TEST(api_fixnum, api_negative_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_negative_P(fn));
}

TEST(api_fixnum, api_odd_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_odd_P(fn));
}

TEST(api_fixnum, api_odd_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_odd_P(fn));
}

TEST(api_fixnum, api_even_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_even_P(fn));
}

TEST(api_fixnum, api_even_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_even_P(fn));
}

TEST(api_fixnum, api_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fcd_fixnum_p);
  expected = read_cstr("46");

  actual = scm_api_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fcd_fixnum_p);
  expected = read_cstr("-51");

  actual = scm_api_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_plus_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("0");

  actual = scm_api_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_plus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(123)");
  expected = read_cstr("123");

  actual = scm_api_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(123 456 789)");
  expected = read_cstr("1368");

  actual = scm_api_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_mul_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("1");

  actual = scm_api_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_mul_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(123)");
  expected = read_cstr("123");

  actual = scm_api_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(123 456 789)");
  expected = read_cstr("44253432");

  actual = scm_api_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_minus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(1)");
  expected = read_cstr("-1");

  actual = scm_api_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, api_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = read_cstr("(987 654 321)");
  expected = read_cstr("12");

  actual = scm_api_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(api_fixnum, capi_floor_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_floor_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_floor_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_floor_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_floor_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_floor_div(fn1, fn2,
                                               SCM_CSETTER_L(quo),
                                               SCM_CSETTER_L(rem)));
}

TEST(api_fixnum, capi_truncate_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_truncate_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_truncate_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_truncate_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(api_fixnum, capi_truncate_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_truncate_div(fn1, fn2,
                                                  SCM_CSETTER_L(quo),
                                                  SCM_CSETTER_L(rem)));
}
