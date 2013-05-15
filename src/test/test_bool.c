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
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_teardown(void)
{
  scm_capi_unraise();
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}


void
test_scm_capi_true_object_p_1(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = SCM_TRUE_OBJ;

  cut_assert_true(scm_capi_true_object_p(bl));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_capi_true_object_p_2(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = SCM_FALSE_OBJ;

  cut_assert_false(scm_capi_true_object_p(bl));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_capi_true_object_p_3(void)
{
  cut_assert_false(scm_capi_true_object_p(SCM_OBJ_NULL));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_capi_false_object_p_1(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = SCM_TRUE_OBJ;

  cut_assert_false(scm_capi_false_object_p(bl));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_capi_false_object_p_2(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl);

  bl = SCM_FALSE_OBJ;

  cut_assert_true(scm_capi_false_object_p(bl));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_capi_false_object_p_3(void)
{
  cut_assert_false(scm_capi_false_object_p(SCM_OBJ_NULL));
  cut_assert_false(scm_capi_raised_p());
}

