#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
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
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_eof_object_p_1(void)
{
  ScmObj eof = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&eof);

  eof = SCM_EOF_OBJ;

  cut_assert_true(scm_capi_eof_object_p(eof));
}

void
test_scm_eof_object_p_2(void)
{
  ScmObj nil = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&nil);

  nil = SCM_NIL_OBJ;

  cut_assert_false(scm_capi_eof_object_p(nil));
}

void
test_scm_eof_object_p_3(void)
{
  cut_assert_false(scm_capi_eof_object_p(SCM_OBJ_NULL));
}
