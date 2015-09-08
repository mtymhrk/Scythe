#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_booleans);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_booleans)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_booleans)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(fcd_booleans, fcd_boolean_p__return_true_1)
{
  TEST_ASSERT_TRUE(scm_fcd_boolean_p(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_boolean_p__return_true_2)
{
  TEST_ASSERT_TRUE(scm_fcd_boolean_p(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_boolean_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_boolean_p(SCM_UNDEF_OBJ));
}

TEST(fcd_booleans, fcd_boolean_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_boolean_p(SCM_OBJ_NULL));
}

TEST(fcd_booleans, fcd_boolean_P__return_true_1)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_boolean_P(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_boolean_P__return_true_2)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_boolean_P(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_boolean_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_boolean_P(SCM_UNDEF_OBJ));
}

TEST(fcd_booleans, fcd_true_object_p__return_true)
{
  TEST_ASSERT_SCM_TRUE(SCM_TRUE_OBJ);
}

TEST(fcd_booleans, fcd_true_object_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_true_object_p(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_true_object_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_true_object_p(SCM_OBJ_NULL));
}

TEST(fcd_booleans, fcd_false_object_p__return_true)
{
  TEST_ASSERT_SCM_FALSE(SCM_FALSE_OBJ);
}

TEST(fcd_booleans, fcd_false_object_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_false_object_p(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_false_object_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_false_object_p(SCM_OBJ_NULL));
}

TEST(fcd_booleans, fcd_true_p__return_true_1)
{
  TEST_ASSERT_TRUE(scm_fcd_true_p(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_true_p__return_true_2)
{
  TEST_ASSERT_TRUE(scm_fcd_true_p(SCM_NIL_OBJ));
}

TEST(fcd_booleans, fcd_true_p__return_true_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = ut_read_cstr("1");

  TEST_ASSERT_TRUE(scm_fcd_true_p(num));
}

TEST(fcd_booleans, fcd_true_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_true_p(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_false_p__return_true)
{
  TEST_ASSERT_TRUE(scm_fcd_false_p(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_false_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_fcd_false_p(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_false_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_fcd_false_p(SCM_NIL_OBJ));
}

TEST(fcd_booleans, fcd_false_p__return_false_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = ut_read_cstr("1");

  TEST_ASSERT_FALSE(scm_fcd_false_p(num));
}

TEST(fcd_booleans, fcd_not__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_not(SCM_FALSE_OBJ));
}

TEST(fcd_booleans, fcd_not__return_false_1)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_not(SCM_TRUE_OBJ));
}

TEST(fcd_booleans, fcd_not__return_false_2)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_not(SCM_NIL_OBJ));
}

TEST(fcd_booleans, fcd_not__return_false_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = ut_read_cstr("1");

  TEST_ASSERT_SCM_FALSE(scm_fcd_not(num));
}


