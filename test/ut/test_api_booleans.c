#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_booleans);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(api_booleans)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_booleans)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(api_booleans, api_boolean_P__return_true_1)
{
  TEST_ASSERT_SCM_TRUE(scm_api_boolean_P(SCM_TRUE_OBJ));
}

TEST(api_booleans, api_boolean_P__return_true_2)
{
  TEST_ASSERT_SCM_TRUE(scm_api_boolean_P(SCM_FALSE_OBJ));
}

TEST(api_booleans, api_boolean_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_boolean_P(SCM_UNDEF_OBJ));
}

TEST(api_booleans, api_not__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_api_not(SCM_FALSE_OBJ));
}

TEST(api_booleans, api_not__return_false_1)
{
  TEST_ASSERT_SCM_FALSE(scm_api_not(SCM_TRUE_OBJ));
}

TEST(api_booleans, api_not__return_false_2)
{
  TEST_ASSERT_SCM_FALSE(scm_api_not(SCM_NIL_OBJ));
}

TEST(api_booleans, api_not__return_false_3)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num);

  num = read_cstr("1");

  TEST_ASSERT_SCM_FALSE(scm_api_not(num));
}
