#include <cutter.h>
#include <stdbool.h>

#include "object.h"
#include "vm.h"
#include "api.h"
#include "miscobjects.h"

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
test_scm_capi_true_p_1(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = scm_api_bool_true();

  cut_assert_true(scm_capi_true_p(bl));
}

void
test_scm_capi_true_p_2(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = scm_api_bool_false();

  cut_assert_false(scm_capi_true_p(bl));
}

void
test_scm_capi_true_p_3(void)
{
  cut_assert_false(scm_capi_true_p(SCM_OBJ_NULL));
}

void
test_scm_capi_false_p_1(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = scm_api_bool_true();

  cut_assert_false(scm_capi_false_p(bl));
}

void
test_scm_capi_false_p_2(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = scm_api_bool_false();

  cut_assert_true(scm_capi_false_p(bl));
}

void
test_scm_capi_false_p_3(void)
{
  cut_assert_false(scm_capi_false_p(SCM_OBJ_NULL));
}









