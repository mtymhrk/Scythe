#include "test.h"

#include "object.h"
#include "api.h"
#include "fixnum.h"

TEST_GROUP(fixnum);

static ScmEvaluator *ev;

TEST_SETUP(fixnum)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(fixnum)
{
  scm_capi_evaluator_end(ev);
}

static void
check_list_elements(ScmObj lst, bool (*check)(ScmObj elm))
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &l);

  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l))
    TEST_ASSERT_TRUE(check(scm_api_car(l)));
}

TEST(fixnum, capi_fixnum_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");

  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));
}

TEST(fixnum, capi_fixnum_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_fixnum_p(SCM_TRUE_OBJ));
}

TEST(fixnum, capi_fixnum_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_fixnum_p(SCM_OBJ_NULL));
}

TEST(fixnum, api_fixnum_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");

  TEST_ASSERT_SCM_TRUE(scm_api_fixnum_P(fn));
}

TEST(fixnum, api_fixnum_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_fixnum_P(SCM_TRUE_OBJ));
}

TEST(fixnum, api_fixnum_P__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_fixnum_P(SCM_OBJ_NULL));
}

TEST(fixnum, capi_number_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_number_p(fn));
}

TEST(fixnum, api_number_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_number_P(fn));
}

TEST(fixnum, capi_complex_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_complex_p(fn));
}

TEST(fixnum, api_complex_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_complex_P(fn));
}

TEST(fixnum, capi_real_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_real_p(fn));
}

TEST(fixnum, api_real_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_real_P(fn));
}

TEST(fixnum, capi_rational_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_rational_p(fn));
}

TEST(fixnum, api_rational_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_rational_P(fn));
}

TEST(fixnum, capi_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_integer_p(fn));
}

TEST(fixnum, api_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_integer_P(fn));
}

TEST(fixnum, capi_exact_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_exact_p(fn));
}

TEST(fixnum, api_exact_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_P(fn));
}

TEST(fixnum, capi_inexact_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_inexact_p(fn));
}

TEST(fixnum, api_inexact_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_inexact_P(fn));
}

TEST(fixnum, capi_exact_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_exact_integer_p(fn));
}

TEST(fixnum, api_exact_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_integer_P(fn));
}

TEST(fixnum, capi_finite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_finite_p(fn));
}

TEST(fixnum, api_finite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_finite_P(fn));
}

TEST(fixnum, capi_infinite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_infinite_p(fn));
}

TEST(fixnum, api_infinite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_infinite_P(fn));
}

TEST(fixnum, capi_nan_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_nan_p(fn));
}

TEST(fixnum, api_nan_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_nan_P(fn));
}

TEST(fixnum, capi_num_eq__return_true)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_eq__return_false)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_eq__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &fn3);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, api_num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(lst);

  lst = read_cstr("(123 123 123)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_eq_P_lst(lst));
}

TEST(fixnum, api_num_eq_P__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(123 123 124)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_eq_P_lst(lst));
}

TEST(fixnum, capi_num_lt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_lt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_lt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_lt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &fn3);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(0);
  fn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, api_num_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(123 231 312)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_lt_P_lst(lst));
}

TEST(fixnum, api_num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(123 231 231)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_lt_P_lst(lst));
}

TEST(fixnum, api_num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(231 312 123)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_lt_P_lst(lst));
}

TEST(fixnum, capi_num_gt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_gt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_gt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_gt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &fn3);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(0);
  fn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, api_num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(231 123 312)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_gt_P_lst(lst));
}

TEST(fixnum, api_num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(312 231 231)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_gt_P_lst(lst));
}

TEST(fixnum, api_num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(312 231 123)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_gt_P_lst(lst));
}




TEST(fixnum, capi_num_le__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_le__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_le__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_le__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &fn3);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(0);
  fn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, api_num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(123 231 312)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_le_P_lst(lst));
}

TEST(fixnum, api_num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(123 231 231)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_le_P_lst(lst));
}

TEST(fixnum, api_num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(231 312 123)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_le_P_lst(lst));
}

TEST(fixnum, capi_num_ge__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, capi_num_ge__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_ge__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, capi_num_ge__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &fn3);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(0);
  fn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, api_num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(231 123 312)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_ge_P_lst(lst));
}

TEST(fixnum, api_num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(312 231 231)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_ge_P_lst(lst));
}

TEST(fixnum, api_num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(312 231 123)");
  check_list_elements(lst, scm_capi_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_ge_P_lst(lst));
}

TEST(fixnum, capi_zero_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_zero_p(fn));
}

TEST(fixnum, capi_zero_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_zero_p(fn));
}

TEST(fixnum, api_zero_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_zero_P(fn));
}

TEST(fixnum, api_zero_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_zero_P(fn));
}

TEST(fixnum, capi_positive_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_positive_p(fn));
}

TEST(fixnum, capi_positive_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_positive_p(fn));
}

TEST(fixnum, api_positive_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_positive_P(fn));
}

TEST(fixnum, api_positive_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_positive_P(fn));
}

TEST(fixnum, capi_negative_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_negative_p(fn));
}

TEST(fixnum, capi_negative_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_negative_p(fn));
}

TEST(fixnum, api_negative_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_negative_P(fn));
}

TEST(fixnum, api_negative_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_negative_P(fn));
}

TEST(fixnum, capi_odd_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_odd_p(fn));
}

TEST(fixnum, capi_odd_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_odd_p(fn));
}

TEST(fixnum, api_odd_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_odd_P(fn));
}

TEST(fixnum, api_odd_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_odd_P(fn));
}

TEST(fixnum, capi_even_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_capi_even_p(fn));
}

TEST(fixnum, capi_even_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_capi_even_p(fn));
}

TEST(fixnum, api_even_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_api_even_P(fn));
}

TEST(fixnum, api_even_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn);

  fn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_api_even_P(fn));
}

TEST(fixnum, api_max__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = read_cstr("1");
  fn2 = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn1, actual));
}

TEST(fixnum, api_max__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX - 1);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn2, actual));
}

TEST(fixnum, api_max__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn1, actual));
}

TEST(fixnum, capi_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_capi_fixnum_p);
  expected = read_cstr("46");

  actual = scm_capi_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_min__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = read_cstr("-1");
  fn2 = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn1, actual));
}

TEST(fixnum, api_min__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN + 1);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn2, actual));
}

TEST(fixnum, api_min__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  actual = scm_api_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn1, actual));
}

TEST(fixnum, capi_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_capi_fixnum_p);
  expected = read_cstr("-51");

  actual = scm_capi_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("123");
  fn2 = read_cstr("765");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("888");

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("-123");
  fn2 = read_cstr("765");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("642");

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("123");
  fn2 = read_cstr("-765");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("-642");

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + SCM_FIXNUM_MIN);

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_plus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_api_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_plus_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("0");

  actual = scm_capi_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(123 456 789)");
  expected = read_cstr("1368");

  actual = scm_capi_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("51");
  fn2 = read_cstr("25");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("1275");

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("-51");
  fn2 = read_cstr("25");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("-1275");

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("-21267647932558653961849226946058125312");

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  v1 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);
  v2 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);

  fn1 = scm_capi_make_number_from_sword(v1);
  fn2 = scm_capi_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  v1 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));
  v2 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));

  fn1 = scm_capi_make_number_from_sword(v1);
  fn2 = scm_capi_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) + 2;
  v2 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;

  fn1 = scm_capi_make_number_from_sword(v1);
  fn2 = scm_capi_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(v1 * v2);

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul__7)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;
  v2 = SCM_SWORD_MAX / v1 + 1;

  fn1 = scm_capi_make_number_from_sword(v1);
  fn2 = scm_capi_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("9223372039002259453");

  actual = scm_api_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_mul_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("()");
  expected = read_cstr("1");

  actual = scm_capi_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(123 456 789)");
  expected = read_cstr("44253432");

  actual = scm_capi_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("765");
  fn2 = read_cstr("123");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("642");

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("-123");
  fn2 = read_cstr("765");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("-888");

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = read_cstr("123");
  fn2 = read_cstr("-765");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = read_cstr("888");

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX - SCM_FIXNUM_MIN);

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_minus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &expected, &actual);

  fn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));
  expected = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_api_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_minus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(1)");
  expected = read_cstr("-1");

  actual = scm_capi_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(987 654 321)");
  expected = read_cstr("12");

  actual = scm_capi_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_abs__positive)
{
  ScmObj fn = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn, &actual);

  fn = read_cstr("123");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));

  actual = scm_api_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(fn, actual));
}

TEST(fixnum, api_abs__negative_1)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn, &expected, &actual);

  fn = read_cstr("-123");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));
  expected = read_cstr("123");

  actual = scm_api_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, api_abs__negative_2)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn, &expected, &actual);

  fn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn));
  expected = scm_capi_make_number_from_sword(-SCM_FIXNUM_MIN);

  actual = scm_api_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(fixnum, capi_floor_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_floor_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_floor_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_floor_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_floor_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_floor_div(fn1, fn2,
                                               SCM_CSETTER_L(quo),
                                               SCM_CSETTER_L(rem)));
}

TEST(fixnum, capi_truncate_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_truncate_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_truncate_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_truncate_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  fn1 = read_cstr("-5");
  fn2 = read_cstr("-2");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(fixnum, capi_truncate_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fn1, &fn2, &quo, &rem);

  fn1 = read_cstr("5");
  fn2 = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_capi_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_truncate_div(fn1, fn2,
                                                  SCM_CSETTER_L(quo),
                                                  SCM_CSETTER_L(rem)));
}
