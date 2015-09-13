#include "exception.c"

#include "scythe/object.h"
#include "scythe/vm.h"
#include "scythe/refstk.h"
#include "scythe/exception.h"

#include "test.h"

TEST_GROUP(exception);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

static void
check_exception(ScmObj exc, const char *msg, const char *irris)
{
  ScmObj msg_str = SCM_OBJ_INIT, ir_lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&exc,
                      &msg_str, &ir_lst);


  msg_str = scm_make_string_from_cstr((msg == NULL) ? "" : msg, SCM_ENC_UTF8);

  ir_lst = ut_read_cstr(irris);

  TEST_ASSERT_SCM_EQUAL(msg_str, scm_api_error_object_message(exc));
  TEST_ASSERT_SCM_EQUAL(ir_lst, scm_api_error_object_irritants(exc));
}

TEST_SETUP(exception)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(exception)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(exception, raise)
{
  TEST_ASSERT_FALSE(scm_raised_p());
  TEST_ASSERT_EQUAL_INT(0, scm_raise(SCM_NIL_OBJ));
  TEST_ASSERT_TRUE(scm_raised_p());
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, scm_raised_obj());
}

TEST(exception, discard_raised_obj)
{
  scm_raise(SCM_NIL_OBJ);
  scm_discard_raised_obj();
  TEST_ASSERT_FALSE(scm_raised_p());
}

TEST(exception, error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_error("bar", 2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_raised_p());

  exc = scm_raised_obj();

  TEST_ASSERT_TRUE(scm_error_object_p(exc));
  TEST_ASSERT_SCM_FALSE(scm_read_error_P(exc));
  TEST_ASSERT_SCM_FALSE(scm_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}

TEST(exception, read_error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_read_error("bar", 2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_raised_p());

  exc = scm_raised_obj();

  TEST_ASSERT_TRUE(scm_error_object_p(exc));
  TEST_ASSERT_SCM_TRUE(scm_read_error_P(exc));
  TEST_ASSERT_SCM_FALSE(scm_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}

TEST(exception, file_error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_file_error("bar", 2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_raised_p());

  exc = scm_raised_obj();

  TEST_ASSERT_TRUE(scm_error_object_p(exc));
  TEST_ASSERT_SCM_FALSE(scm_read_error_P(exc));
  TEST_ASSERT_SCM_TRUE(scm_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}

