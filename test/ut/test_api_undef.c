#include "api.h"

#include "test.h"

TEST_GROUP(api_undef);

static ScmEvaluator *ev;

TEST_SETUP(api_undef)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(api_undef)
{
  scm_capi_evaluator_end(ev);
}

TEST(api_undef, capi_undef_object_p__return_true)
{
  TEST_ASSERT_TRUE(scm_capi_undef_object_p(SCM_UNDEF_OBJ));
}

TEST(api_undef, capi_undef_object_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_undef_object_p(SCM_EOF_OBJ));
}
