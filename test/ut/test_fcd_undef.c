#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_undef);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_undef)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_undef)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(fcd_undef, fcd_undef_object_p__return_true)
{
  TEST_ASSERT_TRUE(scm_fcd_undef_object_p(SCM_UNDEF_OBJ));
}

TEST(fcd_undef, fcd_undef_object_p__return_false)
{
  TEST_ASSERT_FALSE(scm_fcd_undef_object_p(SCM_EOF_OBJ));
}
