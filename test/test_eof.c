#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
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
test_scm_eof_object_p_1(void)
{
  ScmObj eof = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&eof);

  eof = scm_api_eof();

  cut_assert_true(scm_capi_eof_object_p(eof));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_eof_object_p_2(void)
{
  ScmObj nil = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&nil);

  nil = scm_api_nil();

  cut_assert_false(scm_capi_eof_object_p(nil));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_eof_object_p_3(void)
{
  cut_assert_false(scm_capi_eof_object_p(SCM_OBJ_NULL));
  cut_assert_false(scm_capi_raised_p());
}
