#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/miscobjects.h"

#include "test.h"

TEST_GROUP(undef);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(undef)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(undef)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(undef, undef_object_p__return_true)
{
  TEST_ASSERT_TRUE(scm_undef_object_p(SCM_UNDEF_OBJ));
}

TEST(undef, undef_object_p__return_false)
{
  TEST_ASSERT_FALSE(scm_undef_object_p(SCM_EOF_OBJ));
}
