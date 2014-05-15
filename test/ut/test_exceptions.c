#include "test.h"

#include "object.h"
#include "api.h"

TEST_GROUP(exceptions);

static ScmEvaluator *ev;

static void
check_exception(ScmObj exc, const char *msg, const char *irris)
{
  ScmObj msg_str = SCM_OBJ_INIT, ir_lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exc,
                       &msg_str, &ir_lst);


  msg_str = scm_capi_make_string_from_cstr((msg == NULL) ? "" : msg,
                                           SCM_ENC_UTF8);

  ir_lst = read_cstr(irris);

  TEST_ASSERT_SCM_EQUAL(msg_str, scm_api_error_object_message(exc));
  TEST_ASSERT_SCM_EQUAL(ir_lst, scm_api_error_object_irritants(exc));
}

TEST_SETUP(exceptions)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(exceptions)
{
  scm_capi_evaluator_end(ev);
}

TEST(exceptions, capi_raise)
{
  TEST_ASSERT_FALSE(scm_capi_raised_p());
  TEST_ASSERT_EQUAL_INT(0, scm_capi_raise(SCM_NIL_OBJ));
  TEST_ASSERT_TRUE(scm_capi_raised_p());
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, scm_capi_raised_obj());
}

TEST(exceptions, capi_discard_raised_obj)
{
  scm_capi_raise(SCM_NIL_OBJ);
  scm_capi_discard_raised_obj();
  TEST_ASSERT_FALSE(scm_capi_raised_p());
}

TEST(exceptions, capi_error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_capi_error("bar", 2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_capi_raised_p());

  exc = scm_capi_raised_obj();

  TEST_ASSERT_TRUE(scm_capi_error_object_p(exc));
  TEST_ASSERT_SCM_FALSE(scm_api_read_error_P(exc));
  TEST_ASSERT_SCM_FALSE(scm_api_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}

TEST(exceptions, capi_read_error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_capi_read_error("bar",
                                            2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_capi_raised_p());

  exc = scm_capi_raised_obj();

  TEST_ASSERT_TRUE(scm_capi_error_object_p(exc));
  TEST_ASSERT_SCM_TRUE(scm_api_read_error_P(exc));
  TEST_ASSERT_SCM_FALSE(scm_api_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}

TEST(exceptions, capi_file_error)
{
  ScmObj exc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exc);

  TEST_ASSERT_EQUAL_INT(0,
                        scm_capi_file_error("bar",
                                            2, SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  TEST_ASSERT_TRUE(scm_capi_raised_p());

  exc = scm_capi_raised_obj();

  TEST_ASSERT_TRUE(scm_capi_error_object_p(exc));
  TEST_ASSERT_SCM_FALSE(scm_api_read_error_P(exc));
  TEST_ASSERT_SCM_TRUE(scm_api_file_error_P(exc));

  check_exception(exc, "bar", "(#t #f)");
}
