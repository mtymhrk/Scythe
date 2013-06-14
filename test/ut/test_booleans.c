#include "test.h"

#include "object.h"
#include "api.h"

TEST_GROUP(booleans);

static ScmEvaluator *ev;

TEST_SETUP(booleans)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(booleans)
{
  scm_capi_evaluator_end(ev);
}

TEST(booleans, capi_boolean_p__return_true_1)
{
  TEST_ASSERT_TRUE(scm_capi_boolean_p(SCM_TRUE_OBJ));
}

TEST(booleans, capi_boolean_p__return_true_2)
{
  TEST_ASSERT_TRUE(scm_capi_boolean_p(SCM_FALSE_OBJ));
}

TEST(booleans, capi_boolean_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_boolean_p(SCM_UNDEF_OBJ));
}

TEST(booleans, capi_boolean_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_boolean_p(SCM_OBJ_NULL));
}

TEST(booleans, api_boolean_P__return_true_1)
{
  TEST_ASSERT_SCM_TRUE(scm_api_boolean_P(SCM_TRUE_OBJ));
}

TEST(booleans, api_boolean_P__return_true_2)
{
  TEST_ASSERT_SCM_TRUE(scm_api_boolean_P(SCM_FALSE_OBJ));
}

TEST(booleans, api_boolean_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_boolean_P(SCM_UNDEF_OBJ));
}

TEST(booleans, api_boolean_P__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_boolean_P(SCM_OBJ_NULL));
}

TEST(booleans, capi_true_object_p__return_true)
{
  TEST_ASSERT_SCM_TRUE(SCM_TRUE_OBJ);
}

TEST(booleans, capi_true_object_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_true_object_p(SCM_FALSE_OBJ));
}

TEST(booleans, capi_true_object_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_true_object_p(SCM_OBJ_NULL));
}

TEST(booleans, capi_false_object_p__return_true)
{
  TEST_ASSERT_SCM_FALSE(SCM_FALSE_OBJ);
}

TEST(booleans, capi_false_object_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_false_object_p(SCM_TRUE_OBJ));
}

TEST(booleans, capi_false_object_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_false_object_p(SCM_OBJ_NULL));
}

TEST(booleans, capi_true_p__return_true_1)
{
  TEST_ASSERT_TRUE(scm_capi_true_p(SCM_TRUE_OBJ));
}

TEST(booleans, capi_true_p__return_true_2)
{
  TEST_ASSERT_TRUE(scm_capi_true_p(SCM_NIL_OBJ));
}

TEST(booleans, capi_true_p__return_true_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num);

  num = read_cstr("1");

  TEST_ASSERT_TRUE(scm_capi_true_p(num));
}

TEST(booleans, capi_true_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_true_p(SCM_FALSE_OBJ));
}

TEST(booleans, capi_false_p__return_true)
{
  TEST_ASSERT_TRUE(scm_capi_false_p(SCM_FALSE_OBJ));
}

TEST(booleans, capi_false_p__return_false_1)
{
  TEST_ASSERT_FALSE(scm_capi_false_p(SCM_TRUE_OBJ));
}

TEST(booleans, capi_false_p__return_false_2)
{
  TEST_ASSERT_FALSE(scm_capi_false_p(SCM_NIL_OBJ));
}

TEST(booleans, capi_false_p__return_false_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num);

  num = read_cstr("1");

  TEST_ASSERT_FALSE(scm_capi_false_p(num));
}

TEST(booleans, api_not__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_api_not(SCM_FALSE_OBJ));
}

TEST(booleans, api_not__return_false_1)
{
  TEST_ASSERT_SCM_FALSE(scm_api_not(SCM_TRUE_OBJ));
}

TEST(booleans, api_not__return_false_2)
{
  TEST_ASSERT_SCM_FALSE(scm_api_not(SCM_NIL_OBJ));
}

TEST(booleans, api_not__return_false_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num);

  num = read_cstr("1");

  TEST_ASSERT_SCM_FALSE(scm_api_not(num));
}

TEST(booleans, api_not__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_api_not(SCM_OBJ_NULL));
}
