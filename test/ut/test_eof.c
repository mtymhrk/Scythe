
#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/miscobjects.h"

#include "test.h"

TEST_GROUP(eof);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(eof)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(eof)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(eof, eof_object_p__return_TRUE)
{
  TEST_ASSERT_TRUE(scm_eof_object_p(SCM_EOF_OBJ));
}

TEST(eof, eof_object_p__return_FALSE)
{
  TEST_ASSERT_FALSE(scm_eof_object_p(SCM_NIL_OBJ));
}

IGNORE_TEST(eof, eof_object_P)
{
}
