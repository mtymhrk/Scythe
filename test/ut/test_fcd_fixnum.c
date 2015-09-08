#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/fixnum.h"

#include "test.h"

TEST_GROUP(fcd_fixnum);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_fixnum)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_fixnum)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
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

TEST(fcd_fixnum, fcd_fixnum_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");

  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));
}

TEST(fcd_fixnum, fcd_fixnum_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_fixnum_p(SCM_TRUE_OBJ));
}

TEST(fcd_fixnum, fcd_fixnum_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_fixnum_p(SCM_OBJ_NULL));
}

TEST(fcd_fixnum, fcd_fixnum_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");

  TEST_ASSERT_SCM_TRUE(scm_fcd_fixnum_P(fn));
}

TEST(fcd_fixnum, fcd_fixnum_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_fixnum_P(SCM_TRUE_OBJ));
}

TEST(fcd_fixnum, fcd_number_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_number_p(fn));
}

TEST(fcd_fixnum, fcd_number_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_number_P(fn));
}

TEST(fcd_fixnum, fcd_complex_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_complex_p(fn));
}

TEST(fcd_fixnum, fcd_complex_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_complex_P(fn));
}

TEST(fcd_fixnum, fcd_real_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_real_p(fn));
}

TEST(fcd_fixnum, fcd_real_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_real_P(fn));
}

TEST(fcd_fixnum, fcd_rational_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_rational_p(fn));
}

TEST(fcd_fixnum, fcd_rational_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_rational_P(fn));
}

TEST(fcd_fixnum, fcd_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_integer_p(fn));
}

TEST(fcd_fixnum, fcd_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_integer_P(fn));
}

TEST(fcd_fixnum, fcd_exact_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_exact_p(fn));
}

TEST(fcd_fixnum, fcd_exact_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_exact_P(fn));
}

TEST(fcd_fixnum, fcd_inexact_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_inexact_p(fn));
}

TEST(fcd_fixnum, fcd_inexact_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_inexact_P(fn));
}

TEST(fcd_fixnum, fcd_exact_integer_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_exact_integer_p(fn));
}

TEST(fcd_fixnum, fcd_exact_integer_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_exact_integer_P(fn));
}

TEST(fcd_fixnum, fcd_finite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_finite_p(fn));
}

TEST(fcd_fixnum, fcd_finite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_finite_P(fn));
}

TEST(fcd_fixnum, fcd_infinite_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_infinite_p(fn));
}

TEST(fcd_fixnum, fcd_infinite_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_infinite_P(fn));
}

TEST(fcd_fixnum, fcd_nan_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_nan_p(fn));
}

TEST(fcd_fixnum, fcd_nan_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_nan_P(fn));
}

TEST(fcd_fixnum, fcd_num_eq__return_true)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_eq__return_false)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_eq__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn3 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_eq(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_eq(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_eq(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_eq_P_lst__return_true)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 123 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_eq_P_lst__return_false)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 123 124)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_eq_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_lt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_lt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_lt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_lt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(0);
  fn3 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_lt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_lt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_lt_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_lt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_lt_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_lt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_lt_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_gt__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_gt__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_gt__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_gt__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(0);
  fn3 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_gt(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_gt_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_gt_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_gt_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_gt_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_gt_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_gt_P_lst(lst));
}




TEST(fcd_fixnum, fcd_num_le__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_le__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_le__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_le__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(0);
  fn3 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_le(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_le_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_le_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_le_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(123 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_le_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_le_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 312 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_le_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_ge__less)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(fcd_fixnum, fcd_num_ge__equal)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_ge__greater)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_ge__transitive)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, fn3 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &fn3);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(0);
  fn3 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn3));

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn1, fn2, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn2, fn3, &actual));
  TEST_ASSERT_TRUE(actual);

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_num_ge(fn1, fn3, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(fcd_fixnum, fcd_num_ge_P_lst__less)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(231 123 312)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_FALSE(scm_fcd_num_ge_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_ge_P_lst__equal)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 231)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_ge_P_lst(lst));
}

TEST(fcd_fixnum, fcd_num_ge_P_lst__greater)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  lst = ut_read_cstr("(312 231 123)");
  check_list_elements(lst, scm_fcd_fixnum_p);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_ge_P_lst(lst));
}

TEST(fcd_fixnum, fcd_zero_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_zero_p(fn));
}

TEST(fcd_fixnum, fcd_zero_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_zero_p(fn));
}

TEST(fcd_fixnum, fcd_zero_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_zero_P(fn));
}

TEST(fcd_fixnum, fcd_zero_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_zero_P(fn));
}

TEST(fcd_fixnum, fcd_positive_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_positive_p(fn));
}

TEST(fcd_fixnum, fcd_positive_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_positive_p(fn));
}

TEST(fcd_fixnum, fcd_positive_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_positive_P(fn));
}

TEST(fcd_fixnum, fcd_positive_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_positive_P(fn));
}

TEST(fcd_fixnum, fcd_negative_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_negative_p(fn));
}

TEST(fcd_fixnum, fcd_negative_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_negative_p(fn));
}

TEST(fcd_fixnum, fcd_negative_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_negative_P(fn));
}

TEST(fcd_fixnum, fcd_negative_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_negative_P(fn));
}

TEST(fcd_fixnum, fcd_odd_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_odd_p(fn));
}

TEST(fcd_fixnum, fcd_odd_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_odd_p(fn));
}

TEST(fcd_fixnum, fcd_odd_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_odd_P(fn));
}

TEST(fcd_fixnum, fcd_odd_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_odd_P(fn));
}

TEST(fcd_fixnum, fcd_even_p__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_TRUE(scm_fcd_even_p(fn));
}

TEST(fcd_fixnum, fcd_even_p__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_FALSE(scm_fcd_even_p(fn));
}

TEST(fcd_fixnum, fcd_even_P__return_true)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_TRUE(scm_fcd_even_P(fn));
}

TEST(fcd_fixnum, fcd_even_P__return_false)
{
  ScmObj fn = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn);

  fn = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  TEST_ASSERT_SCM_FALSE(scm_fcd_even_P(fn));
}

TEST(fcd_fixnum, fcd_max__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = ut_read_cstr("1");
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn1, actual));
}

TEST(fcd_fixnum, fcd_max__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX - 1);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn2, actual));
}

TEST(fcd_fixnum, fcd_max__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_max(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn1, actual));
}

TEST(fcd_fixnum, fcd_max_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fcd_fixnum_p);
  expected = ut_read_cstr("46");

  actual = scm_fcd_max_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_min__first)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = ut_read_cstr("-1");
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn1, actual));
}

TEST(fcd_fixnum, fcd_min__second)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN + 1);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn2, actual));
}

TEST(fcd_fixnum, fcd_min__same)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  actual = scm_fcd_min(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn1, actual));
}

TEST(fcd_fixnum, fcd_min_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(-51 23 0 46 -21)");
  check_list_elements(lst, scm_fcd_fixnum_p);
  expected = ut_read_cstr("-51");

  actual = scm_fcd_min_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("888");

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("642");

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("-765");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("-642");

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX + SCM_FIXNUM_MIN);

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_fcd_plus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("0");

  actual = scm_fcd_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123)");
  expected = ut_read_cstr("123");

  actual = scm_fcd_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_plus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123 456 789)");
  expected = ut_read_cstr("1368");

  actual = scm_fcd_plus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("51");
  fn2 = ut_read_cstr("25");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("1275");

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-51");
  fn2 = ut_read_cstr("25");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("-1275");

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("-21267647932558653961849226946058125312");

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);
  v2 = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);

  fn1 = scm_fcd_make_number_from_sword(v1);
  fn2 = scm_fcd_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));
  v2 = -((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2));

  fn1 = scm_fcd_make_number_from_sword(v1);
  fn2 = scm_fcd_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) + 2;
  v2 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;

  fn1 = scm_fcd_make_number_from_sword(v1);
  fn2 = scm_fcd_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(v1 * v2);

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul__7)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;
  scm_sword_t v1, v2;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  v1 = ((scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2)) - 1;
  v2 = SCM_SWORD_MAX / v1 + 1;

  fn1 = scm_fcd_make_number_from_sword(v1);
  fn2 = scm_fcd_make_number_from_sword(v2);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("9223372039002259453");

  actual = scm_fcd_mul(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul_lst__arg_0)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("()");
  expected = ut_read_cstr("1");

  actual = scm_fcd_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123)");
  expected = ut_read_cstr("123");

  actual = scm_fcd_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_mul_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(123 456 789)");
  expected = ut_read_cstr("44253432");

  actual = scm_fcd_mul_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("765");
  fn2 = ut_read_cstr("123");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("642");

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("-123");
  fn2 = ut_read_cstr("765");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("-888");

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = ut_read_cstr("123");
  fn2 = ut_read_cstr("-765");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = ut_read_cstr("888");

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX - SCM_FIXNUM_MIN);

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__5)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX);
  fn2 = ut_read_cstr("-1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MAX + 1);

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus__6)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &expected, &actual);

  fn1 = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  fn2 = ut_read_cstr("1");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));
  expected = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN - 1);

  actual = scm_fcd_minus(fn1, fn2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus_lst__arg_1)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(1)");
  expected = ut_read_cstr("-1");

  actual = scm_fcd_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_minus_lst)
{
  ScmObj lst = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &expected, &actual);

  lst = ut_read_cstr("(987 654 321)");
  expected = ut_read_cstr("12");

  actual = scm_fcd_minus_lst(lst);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_abs__positive)
{
  ScmObj fn = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &actual);

  fn = ut_read_cstr("123");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));

  actual = scm_fcd_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(fn, actual));
}

TEST(fcd_fixnum, fcd_abs__negative_1)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &expected, &actual);

  fn = ut_read_cstr("-123");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));
  expected = ut_read_cstr("123");

  actual = scm_fcd_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_abs__negative_2)
{
  ScmObj fn = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn, &expected, &actual);

  fn = scm_fcd_make_number_from_sword(SCM_FIXNUM_MIN);
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn));
  expected = scm_fcd_make_number_from_sword(-SCM_FIXNUM_MIN);

  actual = scm_fcd_abs(fn);

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected, actual));
}

TEST(fcd_fixnum, fcd_floor_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_floor_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_floor_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-3");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_floor_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_floor_div(fn1, fn2,
                                             SCM_CSETTER_L(quo),
                                             SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_floor_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_floor_div(fn1, fn2,
                                              SCM_CSETTER_L(quo),
                                              SCM_CSETTER_L(rem)));
}

TEST(fcd_fixnum, fcd_truncate_div__1)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_truncate_div__2)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_truncate_div__3)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("-2");
  expected_rem = ut_read_cstr("1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_truncate_div__4)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem,
                      &expected_quo, &expected_rem);

  fn1 = ut_read_cstr("-5");
  fn2 = ut_read_cstr("-2");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  expected_quo = ut_read_cstr("2");
  expected_rem = ut_read_cstr("-1");

  TEST_ASSERT_EQUAL_INT(0, scm_fcd_truncate_div(fn1, fn2,
                                                SCM_CSETTER_L(quo),
                                                SCM_CSETTER_L(rem)));

  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_quo, quo));
  TEST_ASSERT_SCM_TRUE(scm_fcd_num_eq_P(expected_rem, rem));
}

TEST(fcd_fixnum, fcd_truncate_div__division_by_zero)
{
  ScmObj fn1 = SCM_OBJ_INIT, fn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fn1, &fn2, &quo, &rem);

  fn1 = ut_read_cstr("5");
  fn2 = ut_read_cstr("0");
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn1));
  TEST_ASSERT_TRUE(scm_fcd_fixnum_p(fn2));

  TEST_ASSERT_EQUAL_INT(-1, scm_fcd_truncate_div(fn1, fn2,
                                                 SCM_CSETTER_L(quo),
                                                 SCM_CSETTER_L(rem)));
}

