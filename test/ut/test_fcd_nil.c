#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_nil);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_nil)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_nil)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_nil, fcd_nil_p__return_true)
{
  TEST_ASSERT_TRUE(scm_fcd_nil_p(SCM_NIL_OBJ));
}

TEST(fcd_nil, fcd_nil_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_nil_p(SCM_EOF_OBJ));
}

TEST(fcd_nil, fcd_nil_P__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_fcd_nil_P(SCM_NIL_OBJ));
}

TEST(fcd_nil, fcd_nil_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_fcd_nil_P(SCM_EOF_OBJ));
}
