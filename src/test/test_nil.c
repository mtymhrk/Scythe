#include <cutter.h>

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
test_scm_nil_p_1(void)
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&nil);

  nil = SCM_NIL_OBJ;

  cut_assert_true(scm_capi_nil_p(nil));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_nil_p_2(void)
{
  ScmObj eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&eof);

  eof = SCM_EOF_OBJ;

  cut_assert_false(scm_capi_nil_p(eof));
  cut_assert_false(scm_capi_raised_p());
}

void
test_scm_nil_p_3(void)
{
  cut_assert_false(scm_capi_nil_p(SCM_OBJ_NULL));
  cut_assert_false(scm_capi_raised_p());
}

