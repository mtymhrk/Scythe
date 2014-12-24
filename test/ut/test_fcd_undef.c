#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_undef);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_undef)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_undef)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_undef, fcd_undef_object_p__return_true)
{
  TEST_ASSERT_TRUE(scm_fcd_undef_object_p(SCM_UNDEF_OBJ));
}

TEST(fcd_undef, fcd_undef_object_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_undef_object_p(SCM_EOF_OBJ));
}
