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
  scm_capi_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_nil_new(void)
{
  ScmObj nil = scm_nil_new(SCM_MEM_HEAP);

  cut_assert_true(scm_obj_not_null_p(nil));
}
