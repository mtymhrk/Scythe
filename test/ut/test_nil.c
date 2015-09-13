#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/miscobjects.h"

#include "test.h"

TEST_GROUP(nil);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(nil)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(nil)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(nil, nil_p__return_true)
{
  TEST_ASSERT_TRUE(scm_nil_p(SCM_NIL_OBJ));
}

TEST(nil, nil_p__return_false)
{
  TEST_ASSERT_FALSE(scm_nil_p(SCM_EOF_OBJ));
}

TEST(nil, nil_P__return_true)
{
  TEST_ASSERT_SCM_TRUE(scm_nil_P(SCM_NIL_OBJ));
}

TEST(nil, nil_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_nil_P(SCM_EOF_OBJ));
}
