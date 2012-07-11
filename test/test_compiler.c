#include <cutter.h>

#include "vm.h"
#include "api.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_api_compile__self_eval_1(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "1";
  const char *asm_str = "((immval 1)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__refer_global_variable(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "global_var";
  const char *asm_str = "((gref global_var)(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);

  scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}

void
test_scm_api_compile__quote(void)
{
  ScmObj exp = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  const char *exp_str = "'(a b c)";
  const char *asm_str = "((immval (a b c))(return 0))";

  SCM_STACK_FRAME_PUSH(&exp, &port,
                       &actual, &expected);

  port = scm_capi_open_input_string_from_cstr(exp_str, SCM_ENC_ASCII);
  exp = scm_api_read(port);

  port = scm_capi_open_input_string_from_cstr(asm_str, SCM_ENC_ASCII);
  expected = scm_api_read(port);

  actual = scm_api_compile(exp);
  scm_api_write(exp, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL);
  scm_api_write(actual, SCM_OBJ_NULL); scm_api_newline(SCM_OBJ_NULL);

  cut_assert_true(scm_capi_true_p(scm_api_equal_P(expected, actual)));
}
