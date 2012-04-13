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
test_scm_bool_new(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP, true);

  cut_assert(scm_obj_not_null_p(bl));
  cut_assert(scm_obj_type_p(SCM_OBJ(bl), &SCM_BOOL_TYPE_INFO));
}

void
test_scm_bool_value_true(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP,true);

  cut_assert_true(scm_bool_value(bl));
}

void
test_scm_bool_value_false(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP,false);

  cut_assert_false(scm_bool_value(bl));
}
