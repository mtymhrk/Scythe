#include "bignum.c"

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/pair.h"
#include "scythe/fixnum.h"
#include "scythe/number.h"
#include "scythe/bignum.h"

#include "test.h"

TEST_GROUP(bignum);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(bignum)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(bignum)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

static void
check_list_elements(ScmObj lst, bool (*check)(ScmObj elm))
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &l);

  for (l = lst; scm_pair_p(l); l = scm_cdr(l))
    TEST_ASSERT_TRUE(check(scm_car(l)));
}

TEST(bignum, bignum_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  TEST_ASSERT_TRUE(scm_bignum_p(bn));
}

TEST(bignum, bignum_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_bignum_p(SCM_EOF_OBJ));
}

TEST(bignum, bignum_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_bignum_p(SCM_OBJ_NULL));
}

TEST(bignum, bignum_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  TEST_ASSERT_SCM_TRUE(scm_bignum_P(bn));
}

TEST(bignum, bignum_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_bignum_P(SCM_EOF_OBJ));
}

TEST(bignum, number_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_number_p(bn));
}

TEST(bignum, number_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_number_P(bn));
}

TEST(bignum, num_complex_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_complex_p(bn));
}

TEST(bignum, num_complex_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_complex_P(bn));
}

TEST(bignum, num_real_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_real_p(bn));
}

TEST(bignum, num_real_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_real_P(bn));
}

TEST(bignum, num_rational_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_rational_p(bn));
}

TEST(bignum, num_rational_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_rational_P(bn));
}

TEST(bignum, num_integer_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_integer_p(bn));
}

TEST(bignum, num_integer_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_integer_P(bn));
}

TEST(bignum, num_exact_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_exact_p(bn));
}

TEST(bignum, num_exact_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_exact_P(bn));
}

TEST(bignum, num_inexact_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_inexact_p(bn));
}

TEST(bignum, num_inexact_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_inexact_P(bn));
}

TEST(bignum, num_exact_integer_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_exact_integer_p(bn));
}

TEST(bignum, num_exact_integer_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_exact_integer_P(bn));
}

TEST(bignum, num_finite_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_finite_p(bn));
}

TEST(bignum, num_finite_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_finite_P(bn));
}

TEST(bignum, num_infinite_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_infinite_p(bn));
}

TEST(bignum, num_infinite_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_infinite_P(bn));
}

TEST(bignum, num_nan_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_nan_p(bn));
}

TEST(bignum, num_nan_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_nan_P(bn));
}

TEST(bignum, num_eq__return_true)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_eq__return_false)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_eq__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &bn3);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P_lst(lst));
}

TEST(bignum, num_eq_P__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 4611686018427387904 4611686018427387905)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_eq_P_lst(lst));
}

TEST(bignum, num_lt__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_lt__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_lt__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_lt__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &bn3);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_make_number_from_sword(SCM_SWORD_MAX);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_lt_P_lst__ltss)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(-4611686018427387905 4611686018427387904 9223372036854775807)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_lt_P_lst(lst));
}

TEST(bignum, num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(-4611686018427387905 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_lt_P_lst(lst));
}

TEST(bignum, num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 9223372036854775807 -4611686018427387905)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_lt_P_lst(lst));
}

TEST(bignum, num_gt__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_gt__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_gt__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_gt__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &bn3);

  bn1 = scm_make_number_from_sword(SCM_SWORD_MAX);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 -4611686018427387905 9223372036854775807)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_gt_P_lst(lst));
}

TEST(bignum, num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(9223372036854775807 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_gt_P_lst(lst));
}

TEST(bignum, num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(9223372036854775807 4611686018427387904 -4611686018427387905)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_gt_P_lst(lst));
}

TEST(bignum, num_le__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_le__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_le__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_le__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &bn3);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_make_number_from_sword(SCM_SWORD_MAX);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(-4611686018427387905 4611686018427387904 9223372036854775807)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_le_P_lst(lst));
}

TEST(bignum, num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(-4611686018427387905 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_le_P_lst(lst));
}

TEST(bignum, num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 9223372036854775807 -4611686018427387905)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_le_P_lst(lst));
}

TEST(bignum, num_ge__less)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(bignum, num_ge__equal)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_ge__greater)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_ge__transitive)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, bn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &bn3);

  bn1 = scm_make_number_from_sword(SCM_SWORD_MAX);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn3 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  TEST_ASSERT_TRUE(scm_bignum_p(bn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn1, bn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn2, bn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(bn1, bn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(bignum, num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(4611686018427387904 -4611686018427387905 9223372036854775807)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_ge_P_lst(lst));
}

TEST(bignum, num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(9223372036854775807 4611686018427387904 4611686018427387904)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_ge_P_lst(lst));
}

TEST(bignum, num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(9223372036854775807 4611686018427387904 -4611686018427387905)");
  check_list_elements(lst, scm_bignum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_ge_P_lst(lst));
}

IGNORE_TEST(bignum, num_zero_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_zero_p(bn));
}

IGNORE_TEST(bignum, num_zero_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_zero_p(bn));
}

IGNORE_TEST(bignum, num_zero_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_zero_P(bn));
}

IGNORE_TEST(bignum, num_zero_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_zero_P(bn));
}

TEST(bignum, num_positive_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_positive_p(bn));
}

TEST(bignum, num_positive_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_positive_p(bn));
}

TEST(bignum, num_positive_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_positive_P(bn));
}

TEST(bignum, num_positive_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_positive_P(bn));
}

TEST(bignum, num_negative_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_negative_p(bn));
}

TEST(bignum, num_negative_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_negative_p(bn));
}

TEST(bignum, num_negative_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_negative_P(bn));
}

TEST(bignum, num_negative_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_negative_P(bn));
}

TEST(bignum, num_odd_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_odd_p(bn));
}

TEST(bignum, num_odd_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_odd_p(bn));
}

TEST(bignum, num_odd_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_odd_P(bn));
}

TEST(bignum, num_odd_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_odd_P(bn));
}




TEST(bignum, num_even_p__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_TRUE(scm_num_even_p(bn));
}

TEST(bignum, num_even_p__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_FALSE(scm_num_even_p(bn));
}

TEST(bignum, num_even_P__return_true)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_TRUE(scm_num_even_P(bn));
}

TEST(bignum, num_even_P__return_false)
{
  ScmObj bn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn);

  bn = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_SCM_FALSE(scm_num_even_P(bn));
}

TEST(bignum, num_max__first)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn1, actual));
}

TEST(bignum, num_max__second)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn2, actual));
}

TEST(bignum, num_max__same)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_max(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn1, actual));
}

TEST(bignum, num_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(4611686018427387904 -4611686018427387905 4611686018427388160 4611686022722355200 -4611686022722355200)");
  check_list_elements(lst, scm_bignum_p);
  expected = ut_read_cstr("4611686022722355200");

  actual = scm_num_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_min__first)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn1, actual));
}

TEST(bignum, num_min__second)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn2, actual));
}

TEST(bignum, num_min__same)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  actual = scm_num_min(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn1, actual));
}

TEST(bignum, num_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(4611686018427387904 -4611686018427387905 4611686018427388160 4611686022722355200 -4611686022722355200)");
  check_list_elements(lst, scm_bignum_p);
  expected = ut_read_cstr("-4611686022722355200");

  actual = scm_num_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_plus__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("9223372036854775809");

  actual = scm_num_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_plus__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-1");

  actual = scm_num_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_plus__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-1");

  actual = scm_num_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_plus__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-9223372036854775811");

  actual = scm_num_plus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_bignum_p);
  expected = ut_read_cstr("18446744078004518912");

  actual = scm_num_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_mul__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("21267647932558653971072598982912901120");

  actual = scm_num_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_mul__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-21267647932558653971072598982912901120");

  actual = scm_num_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_mul__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);;
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-21267647932558653980295971019767676930");

  actual = scm_num_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_mul__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);;
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("21267647932558653980295971019767676930");

  actual = scm_num_mul(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_bignum_p);
  expected = ut_read_cstr("196159429413521478536231284250700179592733985096492646400");

  actual = scm_num_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_minus__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-1");

  actual = scm_num_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_minus__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("-9223372036854775810");

  actual = scm_num_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_minus__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("9223372036854775810");

  actual = scm_num_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_minus__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &expected, &actual);

  bn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);
  bn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN - 2);
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));
  expected = ut_read_cstr("1");

  actual = scm_num_minus(bn1, bn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(4611686018427387904 4611686022722355200 9223372036854775808)");
  check_list_elements(lst, scm_bignum_p);
  expected = ut_read_cstr("-9223372041149743104");

  actual = scm_num_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_abs__positive)
{
  ScmObj bn = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn, &actual);

  bn = ut_read_cstr("4611686018427387904");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  actual = scm_num_abs(bn);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(bn, actual));
}

TEST(bignum, num_abs__negative)
{
  ScmObj bn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn, &expected, &actual);

  bn = ut_read_cstr("-4611686018427387905");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));
  expected = ut_read_cstr("4611686018427387905");

  actual = scm_num_abs(bn);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(bignum, num_floor_div__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("23058430092136939520");
  bn2 = ut_read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(bn1, bn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_floor_div__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("-23058430092136939520");
  bn2 = ut_read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(bn1, bn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_floor_div__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("23058430092136939520");
  bn2 = ut_read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(bn1, bn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_floor_div__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("-23058430092136939520");
  bn2 = ut_read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(bn1, bn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_floor_div__division_by_zero)
{
  ScmObj bn = SCM_OBJ_INIT, fn = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn, &bn, &quo, &rem);

  bn = ut_read_cstr("23058430092136939520");
  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_EQUAL_INT(-1, scm_num_floor_div(bn, fn,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));
}

TEST(bignum, num_truncate_div__1)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("23058430092136939520");
  bn2 = ut_read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(bn1, bn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_truncate_div__2)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("-23058430092136939520");
  bn2 = ut_read_cstr("9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(bn1, bn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_truncate_div__3)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("23058430092136939520");
  bn2 = ut_read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(bn1, bn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_truncate_div__4)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn1, &bn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  bn1 = ut_read_cstr("-23058430092136939520");
  bn2 = ut_read_cstr("-9223372036854775808");
  TEST_ASSERT_TRUE(scm_bignum_p(bn1));
  TEST_ASSERT_TRUE(scm_bignum_p(bn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-4611686018427387904");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(bn1, bn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(bignum, num_truncate_div__division_by_zero)
{
  ScmObj bn = SCM_OBJ_INIT, fn = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&bn, &bn, &quo, &rem);

  bn = ut_read_cstr("23058430092136939520");
  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_bignum_p(bn));

  TEST_ASSERT_EQUAL_INT(-1, scm_num_truncate_div(bn, fn,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));
}
