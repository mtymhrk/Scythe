#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/string.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_format);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_format)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_format)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_format, api_format_lst)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = ut_read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = ut_read_cstr("(\"foo\" \"bar\")");
  expected = ut_read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_string_eq_P(expected, actual));
}

TEST(api_format, api_format_lst__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = ut_read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = ut_read_cstr("(\"foo\" \"bar\" \"baz\")");
  expected = ut_read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_string_eq_P(expected, actual));
}

TEST(api_format, api_format_lst__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &format, &arg);

  format = ut_read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = ut_read_cstr("(\"foo\")");

  actual = scm_api_format_lst(format, arg);

  TEST_ASSERT_SCM_NULL(actual);
}
