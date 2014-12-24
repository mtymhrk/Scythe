#include "fcd_format.c"

#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_format);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_format)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_format)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(fcd_format, fcd_format_lst)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_fcd_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_format, fcd_format_lst__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\" \"bar\" \"baz\")");
  expected = read_cstr("\"[foo] [\\\"bar\\\"] [~] [\\n]\"");

  actual = scm_fcd_format_lst(format, arg);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_format, fcd_format_lst__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &format, &arg);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg = read_cstr("(\"foo\")");

  actual = scm_fcd_format_lst(format, arg);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(fcd_format, fcd_format_cv)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_fcd_format_cv(format, arg, 2);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_format, fcd_format_cv__too_many_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT;
  ScmObj arg[3] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 3);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = arg[2] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_fcd_format_cv(format, arg, 3);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_format, fcd_format_cv__error_too_few_arguments)
{
  ScmObj actual = SCM_OBJ_INIT, format = SCM_OBJ_INIT;
  ScmObj arg[1] = { SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &format);
  SCM_REFSTK_REG_ARY(arg, 1);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = read_cstr("\"foo\"");

  actual = scm_fcd_format_cv(format, arg, 1);

  TEST_ASSERT_SCM_NULL(actual);
}

TEST(fcd_format, fcd_format)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj format = SCM_OBJ_INIT, arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };

  SCM_REFSTK_INIT_REG(&actual, &expected, &format);
  SCM_REFSTK_REG_ARY(arg, 2);

  format = read_cstr("\"[~a] [~s] [~~] [~%]\"");
  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_fcd_format(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

TEST(fcd_format, fcd_format_cstr)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  ScmObj arg[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  const char *format = "[~a] [~s] [~~] [~%]";

  SCM_REFSTK_INIT_REG(&actual, &expected);
  SCM_REFSTK_REG_ARY(arg, 2);

  arg[0] = arg[1] = read_cstr("\"foo\"");
  expected = read_cstr("\"[foo] [\\\"foo\\\"] [~] [\\n]\"");

  actual = scm_fcd_format_cstr(format, arg[0], arg[1], SCM_OBJ_NULL);

  TEST_ASSERT_SCM_TRUE(scm_fcd_string_eq_P(expected, actual));
}

