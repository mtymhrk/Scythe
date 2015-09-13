#include "scythe/refstk.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_nil);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_nil)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_nil)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_nil, api_nil_P__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_api_nil_P(SCM_NIL_OBJ));
}

TEST(api_nil, api_nil_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_nil_P(SCM_EOF_OBJ));
}
