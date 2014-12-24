#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_format);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(api_format)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_format)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(api_format, api_format_lst)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_format, api_format_lst__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\" \"baz\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(api_format, api_format_lst__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\")");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_NULL(actual);
}
