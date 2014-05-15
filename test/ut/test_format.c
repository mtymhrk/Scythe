#include "test.h"

#include "object.h"
#include "api.h"

TEST_GROUP(format);

static ScmEvaluator *ev;

TEST_SETUP(format)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(format)
{
  scm_capi_evaluator_end(ev);
}

TEST(format, capi_format_lst)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(format, capi_format_lst__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\" \"baz\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(format, capi_format_lst__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&actual, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\")");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(format, capi_format_cv)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_STACK_FRAME_PUSH(&actual, &expected, &format);
  SCM_STACK_PUSH_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cv(format, arg, 2);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(format, capi_format_cv__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT;
  ScmObj arg[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_STACK_FRAME_PUSH(&actual, &expected, &format);
  SCM_STACK_PUSH_ARY(arg, 3);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = arg[2] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cv(format, arg, 3);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(format, capi_format_cv__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT;
  ScmObj arg[1] = { SCM_OBJ_INIT };

  SCM_STACK_FRAME_PUSH(&actual, &format);
  SCM_STACK_PUSH_ARY(arg, 1);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = read_cstr("\"foo\"");

  actual = scm_capi_format_cv(format, arg, 1);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(format, api_format)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_STACK_FRAME_PUSH(&actual, &expected, &format);
  SCM_STACK_PUSH_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_api_format(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(format, capi_format_cstr)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  const char *format = "[~a] [~s] [~~] [~%]";

  SCM_STACK_FRAME_PUSH(&actual, &expected);
  SCM_STACK_PUSH_ARY(arg, 2);

  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cstr(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}
