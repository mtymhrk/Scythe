#include "test.h"

#include "object.h"
#include "api.h"
#include "fixnum.h"
#include "bignum.h"

TEST_GROUP(bignum);

static ScmEvaluator *ev;

TEST_SETUP(bignum)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(bignum)
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

TEST(bignum, capi_bignum_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));
}

TEST(bignum, capi_bignum_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_bignum_p(SCM_EOF_OBJ));
}

TEST(bignum, capi_bignum_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_bignum_p(SCM_OBJ_NULL));
}

TEST(bignum, api_bignum_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  TEST_ASSERT_SCM_TRUE(scm_api_bignum_P(bn));
}

TEST(bignum, api_bignum_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_bignum_P(SCM_EOF_OBJ));
}

TEST(bignum, scm_api_bignum_P__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_bignum_P(SCM_OBJ_NULL));
}

TEST(bignum, capi_number_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_number_p(bn));
}

TEST(bignum, api_number_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_number_P(bn));
}

TEST(bignum, capi_complex_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_complex_p(bn));
}

TEST(bignum, api_complex_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_complex_P(bn));
}

TEST(bignum, capi_real_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_real_p(bn));
}

TEST(bignum, api_real_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_real_P(bn));
}

TEST(bignum, capi_rational_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_rational_p(bn));
}

TEST(bignum, api_rational_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_rational_P(bn));
}

TEST(bignum, capi_integer_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_integer_p(bn));
}

TEST(bignum, api_integer_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_integer_P(bn));
}

TEST(bignum, capi_exact_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_exact_p(bn));
}

TEST(bignum, api_exact_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_P(bn));
}

TEST(bignum, capi_inexact_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_inexact_p(bn));
}

TEST(bignum, api_inexact_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_inexact_P(bn));
}

TEST(bignum, capi_exact_integer_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_exact_integer_p(bn));
}

TEST(bignum, api_exact_integer_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_exact_integer_P(bn));
}

TEST(bignum, capi_finite_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_finite_p(bn));
}

TEST(bignum, api_finite_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_finite_P(bn));
}

TEST(bignum, capi_infinite_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_infinite_p(bn));
}

TEST(bignum, api_infinite_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_infinite_P(bn));
}

TEST(bignum, capi_nan_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_nan_p(bn));
}

TEST(bignum, api_nan_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_nan_P(bn));
}

TEST(bignum, capi_num_eq__return_true)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_eq__return_false)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_eq__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_eq(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, api_num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_eq_P_lst(lst));
}

TEST(bignum, api_num_eq_P__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 4611686018427387904 4611686018427387905)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_eq_P_lst(lst));
}

TEST(bignum, capi_num_lt__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_lt__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_lt__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_lt__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_capi_make_number_from_sword(SCM_SWORD_MAX);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_lt(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, api_num_lt_P_lst__ltss)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(-4611686018427387905 4611686018427387904 9223372036854775807)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_lt_P_lst(lst));
}

TEST(bignum, api_num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(-4611686018427387905 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_lt_P_lst(lst));
}

TEST(bignum, api_num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 9223372036854775807 -4611686018427387905)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_lt_P_lst(lst));
}

TEST(bignum, capi_num_gt__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_gt__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_gt__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_gt__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_sword(SCM_SWORD_MAX);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_gt(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, api_num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 -4611686018427387905 9223372036854775807)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_gt_P_lst(lst));
}

TEST(bignum, api_num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(9223372036854775807 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_gt_P_lst(lst));
}

TEST(bignum, api_num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(9223372036854775807 4611686018427387904 -4611686018427387905)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_gt_P_lst(lst));
}

TEST(bignum, capi_num_le__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_le__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_le__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_le__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_capi_make_number_from_sword(SCM_SWORD_MAX);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_le(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, api_num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(-4611686018427387905 4611686018427387904 9223372036854775807)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_le_P_lst(lst));
}

TEST(bignum, api_num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(-4611686018427387905 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_le_P_lst(lst));
}

TEST(bignum, api_num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 9223372036854775807 -4611686018427387905)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_le_P_lst(lst));
}

TEST(bignum, capi_num_ge__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, capi_num_ge__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_ge__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, capi_num_ge__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_sword(SCM_SWORD_MAX);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_capi_num_ge(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, api_num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(4611686018427387904 -4611686018427387905 9223372036854775807)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_capi_num_ge_P_lst(lst));
}

TEST(bignum, api_num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(9223372036854775807 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_ge_P_lst(lst));
}

TEST(bignum, api_num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  lst = read_cstr("(9223372036854775807 4611686018427387904 -4611686018427387905)");
  check_list_elements(lst, scm_capi_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_capi_num_ge_P_lst(lst));
}

IGNORE_TEST(bignum, capi_zero_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_zero_p(bn));
}

IGNORE_TEST(bignum, capi_zero_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = read_cstr("1");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_zero_p(bn));
}

IGNORE_TEST(bignum, api_zero_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_zero_P(bn));
}

IGNORE_TEST(bignum, api_zero_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = read_cstr("-1");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_zero_P(bn));
}

TEST(bignum, capi_positive_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_positive_p(bn));
}

TEST(bignum, capi_positive_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_positive_p(bn));
}

TEST(bignum, api_positive_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_positive_P(bn));
}

TEST(bignum, api_positive_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_positive_P(bn));
}

TEST(bignum, capi_negative_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_negative_p(bn));
}

TEST(bignum, capi_negative_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_negative_p(bn));
}

TEST(bignum, api_negative_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_negative_P(bn));
}

TEST(bignum, api_negative_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_negative_P(bn));
}

TEST(bignum, capi_odd_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_odd_p(bn));
}

TEST(bignum, capi_odd_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_odd_p(bn));
}

TEST(bignum, api_odd_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_odd_P(bn));
}

TEST(bignum, api_odd_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_odd_P(bn));
}




TEST(bignum, capi_even_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_capi_even_p(bn));
}

TEST(bignum, capi_even_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_capi_even_p(bn));
}

TEST(bignum, api_even_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_api_even_P(bn));
}

TEST(bignum, api_even_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn);

  bn = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_api_even_P(bn));
}

TEST(bignum, api_max__first)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn1, actual));
}

TEST(bignum, api_max__second)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn2, actual));
}

TEST(bignum, api_max__same)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn1, actual));
}

TEST(bignum, capi_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(4611686018427387904 -4611686018427387905 4611686018427388160 4611686022722355200 -4611686022722355200)");
  check_list_elements(lst, scm_capi_bignum_p);
  expected = read_cstr("4611686022722355200");

  actual = scm_capi_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_min__first)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn1, actual));
}

TEST(bignum, api_min__second)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn2, actual));
}

TEST(bignum, api_min__same)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  actual = scm_api_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn1, actual));
}

TEST(bignum, capi_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(4611686018427387904 -4611686018427387905 4611686018427388160 4611686022722355200 -4611686022722355200)");
  check_list_elements(lst, scm_capi_bignum_p);
  expected = read_cstr("-4611686022722355200");

  actual = scm_capi_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_plus__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("9223372036854775809");

  actual = scm_api_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_plus__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-1");

  actual = scm_api_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_plus__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-1");

  actual = scm_api_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_plus__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-9223372036854775811");

  actual = scm_api_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, capi_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_capi_bignum_p);
  expected = read_cstr("18446744078004518912");

  actual = scm_capi_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_mul__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("21267647932558653971072598982912901120");

  actual = scm_api_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_mul__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-21267647932558653971072598982912901120");

  actual = scm_api_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_mul__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);;
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-21267647932558653980295971019767676930");

  actual = scm_api_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_mul__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);;
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("21267647932558653980295971019767676930");

  actual = scm_api_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, capi_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_capi_bignum_p);
  expected = read_cstr("196159429413521478536231284250700179592733985096492646400");

  actual = scm_capi_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_minus__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-1");

  actual = scm_api_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_minus__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("-9223372036854775810");

  actual = scm_api_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_minus__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("9223372036854775810");

  actual = scm_api_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_minus__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &expected, &actual);

  bn1 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_capi_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));
  expected = read_cstr("1");

  actual = scm_api_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, capi_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &expected, &actual);

  lst = read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_capi_bignum_p);
  expected = read_cstr("-9223372041149743104");

  actual = scm_capi_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, api_abs__positive)
{
  ScmObj bn = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn, &actual);

  bn = read_cstr("4611686018427387904");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  actual = scm_api_abs(bn);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(bn, actual));
}

TEST(bignum, api_abs__negative)
{
  ScmObj bn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn, &expected, &actual);

  bn = read_cstr("-4611686018427387905");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));
  expected = read_cstr("4611686018427387905");

  actual = scm_api_abs(bn);

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected, actual));
}

TEST(bignum, capi_floor_div__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("23058430092136939520");
  bn2 = read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(bn1, bn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_floor_div__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("-23058430092136939520");
  bn2 = read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(bn1, bn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_floor_div__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("23058430092136939520");
  bn2 = read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("-3");
  expected_rem = read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(bn1, bn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_floor_div__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("-23058430092136939520");
  bn2 = read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_floor_div(bn1, bn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_floor_div__division_by_zero)
{
  ScmObj bn = SCM_OBJ_INIT, fn = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn, &bn, &quo, &rem);

  bn = read_cstr("23058430092136939520");
  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_floor_div(bn, fn,
                                               SCM_CSETTER_L(quo),
                                               SCM_CSETTER_L(rem)));
}

TEST(bignum, capi_truncate_div__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("23058430092136939520");
  bn2 = read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(bn1, bn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_truncate_div__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("-23058430092136939520");
  bn2 = read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(bn1, bn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_truncate_div__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("23058430092136939520");
  bn2 = read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("-2");
  expected_rem = read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(bn1, bn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_truncate_div__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem,
                       &expected_quo, &expected_rem);

  bn1 = read_cstr("-23058430092136939520");
  bn2 = read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn2));

  expected_quo = read_cstr("2");
  expected_rem = read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_truncate_div(bn1, bn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_api_num_eq_P(expected_rem, rem));
}

TEST(bignum, capi_truncate_div__division_by_zero)
{
  ScmObj bn = SCM_OBJ_INIT, fn = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn, &bn, &quo, &rem);

  bn = read_cstr("23058430092136939520");
  fn = read_cstr("0");
  TEST_ASSERT_TRUE(scm_capi_bignum_p(bn));

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_truncate_div(bn, fn,
                                                  SCM_CSETTER_L(quo),
                                                  SCM_CSETTER_L(rem)));
}
