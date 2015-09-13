#include "fixnum.c"

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/pair.h"
#include "scythe/number.h"
#include "scythe/fixnum.h"

#include "test.h"

TEST_GROUP(fixnum);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fixnum)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fixnum)
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

TEST(fixnum, fixnum_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");

  TEST_ASSERT_TRUE(scm_fixnum_p(fn));
}

TEST(fixnum, fixnum_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fixnum_p(SCM_TRUE_OBJ));
}

TEST(fixnum, fixnum_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fixnum_p(SCM_OBJ_NULL));
}

TEST(fixnum, fixnum_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");

  TEST_ASSERT_SCM_TRUE(scm_fixnum_P(fn));
}

TEST(fixnum, fixnum_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fixnum_P(SCM_TRUE_OBJ));
}

TEST(fixnum, number_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_number_p(fn));
}

TEST(fixnum, number_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_number_P(fn));
}

TEST(fixnum, num_complex_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_complex_p(fn));
}

TEST(fixnum, num_complex_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_complex_P(fn));
}

TEST(fixnum, num_real_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_real_p(fn));
}

TEST(fixnum, num_real_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_real_P(fn));
}

TEST(fixnum, num_rational_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_rational_p(fn));
}

TEST(fixnum, num_rational_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_rational_P(fn));
}

TEST(fixnum, num_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_integer_p(fn));
}

TEST(fixnum, num_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_integer_P(fn));
}

TEST(fixnum, num_exact_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_exact_p(fn));
}

TEST(fixnum, num_exact_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_exact_P(fn));
}

TEST(fixnum, num_inexact_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_inexact_p(fn));
}

TEST(fixnum, num_inexact_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_inexact_P(fn));
}

TEST(fixnum, num_exact_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_exact_integer_p(fn));
}

TEST(fixnum, num_exact_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_exact_integer_P(fn));
}

TEST(fixnum, num_finite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_finite_p(fn));
}

TEST(fixnum, num_finite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_finite_P(fn));
}

TEST(fixnum, num_infinite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_infinite_p(fn));
}

TEST(fixnum, num_infinite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_infinite_P(fn));
}

TEST(fixnum, num_nan_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_nan_p(fn));
}

TEST(fixnum, num_nan_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_nan_P(fn));
}

TEST(fixnum, num_eq__return_true)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_eq__return_false)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_eq__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn3 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_eq(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 123 123)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P_lst(lst));
}

TEST(fixnum, num_eq_P_lst__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 123 124)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_eq_P_lst(lst));
}

TEST(fixnum, num_lt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_lt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_lt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_lt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(0);
  fn3 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_lt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_lt_P_lst(lst));
}

TEST(fixnum, num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_lt_P_lst(lst));
}

TEST(fixnum, num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_lt_P_lst(lst));
}

TEST(fixnum, num_gt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_gt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_gt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_gt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(0);
  fn3 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_gt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_gt_P_lst(lst));
}

TEST(fixnum, num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_gt_P_lst(lst));
}

TEST(fixnum, num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_gt_P_lst(lst));
}




TEST(fixnum, num_le__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_le__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_le__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_le__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(0);
  fn3 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_le(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_le_P_lst(lst));
}

TEST(fixnum, num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_le_P_lst(lst));
}

TEST(fixnum, num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_le_P_lst(lst));
}

TEST(fixnum, num_ge__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fixnum, num_ge__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_ge__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_ge__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(0);
  fn3 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_num_ge(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fixnum, num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_num_ge_P_lst(lst));
}

TEST(fixnum, num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_ge_P_lst(lst));
}

TEST(fixnum, num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_num_ge_P_lst(lst));
}

TEST(fixnum, num_zero_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_zero_p(fn));
}

TEST(fixnum, num_zero_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_zero_p(fn));
}

TEST(fixnum, num_zero_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_zero_P(fn));
}

TEST(fixnum, num_zero_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_zero_P(fn));
}

TEST(fixnum, num_positive_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_positive_p(fn));
}

TEST(fixnum, num_positive_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_positive_p(fn));
}

TEST(fixnum, num_positive_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_positive_P(fn));
}

TEST(fixnum, num_positive_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_positive_P(fn));
}

TEST(fixnum, num_negative_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_negative_p(fn));
}

TEST(fixnum, num_negative_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_negative_p(fn));
}

TEST(fixnum, num_negative_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_negative_P(fn));
}

TEST(fixnum, num_negative_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_negative_P(fn));
}

TEST(fixnum, num_odd_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_odd_p(fn));
}

TEST(fixnum, num_odd_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_odd_p(fn));
}

TEST(fixnum, num_odd_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_odd_P(fn));
}

TEST(fixnum, num_odd_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_odd_P(fn));
}

TEST(fixnum, num_even_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_num_even_p(fn));
}

TEST(fixnum, num_even_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_num_even_p(fn));
}

TEST(fixnum, num_even_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_num_even_P(fn));
}

TEST(fixnum, num_even_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_num_even_P(fn));
}

TEST(fixnum, num_max__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = ut_read_cstr("1");
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn1, actual));
}

TEST(fixnum, num_max__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX - 1);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn2, actual));
}

TEST(fixnum, num_max__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn1, actual));
}

TEST(fixnum, num_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fixnum_p);
  expected = ut_read_cstr("46");

  actual = scm_num_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_min__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = ut_read_cstr("-1");
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn1, actual));
}

TEST(fixnum, num_min__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN + 1);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn2, actual));
}

TEST(fixnum, num_min__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  actual = scm_num_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn1, actual));
}

TEST(fixnum, num_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fixnum_p);
  expected = ut_read_cstr("-51");

  actual = scm_num_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("888");

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("642");

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("-765");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("-642");

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX + SCM_FIXNUM_MIN);

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_num_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("0");

  actual = scm_num_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123)");
  expected = ut_read_cstr("123");

  actual = scm_num_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123 456 789)");
  expected = ut_read_cstr("1368");

  actual = scm_num_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("51");
  fn2 = ut_read_cstr("25");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("1275");

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-51");
  fn2 = ut_read_cstr("25");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("-1275");

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("-21267647932558653961849226946058125312");

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);
  v2 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);

  fn1 = scm_make_number_from_sword(v1);
  fn2 = scm_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));
  v2 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));

  fn1 = scm_make_number_from_sword(v1);
  fn2 = scm_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) + 2;
  v2 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;

  fn1 = scm_make_number_from_sword(v1);
  fn2 = scm_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(v1 * v2);

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul__7)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;
  v2 = SCM_SWORD_MAX / v1 + 1;

  fn1 = scm_make_number_from_sword(v1);
  fn2 = scm_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("9223372039002259453");

  actual = scm_num_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("1");

  actual = scm_num_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123)");
  expected = ut_read_cstr("123");

  actual = scm_num_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123 456 789)");
  expected = ut_read_cstr("44253432");

  actual = scm_num_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("765");
  fn2 = ut_read_cstr("123");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("642");

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("-888");

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("-765");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = ut_read_cstr("888");

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX - SCM_FIXNUM_MIN);

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));
  expected = scm_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_num_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(1)");
  expected = ut_read_cstr("-1");

  actual = scm_num_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(987 654 321)");
  expected = ut_read_cstr("12");

  actual = scm_num_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_abs__positive)
{
  ScmObj fn = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &actual);

  fn = ut_read_cstr("123");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));

  actual = scm_num_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(fn, actual));
}

TEST(fixnum, num_abs__negative_1)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &expected, &actual);

  fn = ut_read_cstr("-123");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));
  expected = ut_read_cstr("123");

  actual = scm_num_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_abs__negative_2)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &expected, &actual);

  fn = scm_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fixnum_p(fn));
  expected = scm_make_number_from_sword(-SCM_FIXNUM_MIN);

  actual = scm_num_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected, actual));
}

TEST(fixnum, num_floor_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_floor_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_floor_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_floor_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_floor_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_num_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));
}

TEST(fixnum, num_truncate_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_truncate_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_truncate_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_truncate_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_num_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_num_eq_P(expected_rem, rem));
}

TEST(fixnum, num_truncate_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_num_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));
}
