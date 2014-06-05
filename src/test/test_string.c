#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "string.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_string_new(void)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  /* action */
  str = scm_string_new(SCM_MEM_HEAP,
                       "foo", sizeof("foo") - 1,
                       SCM_ENC_ASCII);
  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(str));
  cut_assert_true(scm_obj_type_p(str, &SCM_STRING_TYPE_INFO));
}
