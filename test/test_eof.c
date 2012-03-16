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
  scm_capi_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_eof_new(void)
{
  ScmObj eof = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&eof);

  eof = scm_eof_new(SCM_MEM_HEAP);

  cut_assert_true(scm_obj_not_null_p(eof));
  cut_assert_true(scm_obj_type_p(eof, &SCM_EOF_TYPE_INFO));
}
