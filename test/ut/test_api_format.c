#include "object.h"
#include "api.h"

#include "test.h"

TEST_GROUP(api_format);

static ScmEvaluator *ev;

TEST_SETUP(api_format)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(api_format)
{
  scm_capi_evaluator_end(ev);
}

TEST(api_format, capi_format_lst)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(api_format, capi_format_lst__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\" \"baz\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(api_format, capi_format_lst__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\")");

  actual = scm_capi_format_lst(format, arg);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(api_format, capi_format_cv)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cv(format, arg, 2);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(api_format, capi_format_cv__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT;
  ScmObj arg[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 3);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = arg[2] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cv(format, arg, 3);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(api_format, capi_format_cv__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT;
  ScmObj arg[1] = { SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &format);
  SCM_REFSTK_REG_ARY(arg, 1);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = read_cstr("\"foo\"");

  actual = scm_capi_format_cv(format, arg, 1);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(api_format, api_format)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_api_format(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}

TEST(api_format, capi_format_cstr)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  const char *format = "[~a] [~s] [~~] [~%]";

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(arg, 2);

  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_capi_format_cstr(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_string_eq_P(expected, actual));
}
