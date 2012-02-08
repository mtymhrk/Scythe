#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "miscobjects.h"
#include "string.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}

void
test_scm_string_new(void)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  /* action */
  SCM_SETQ(str, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                     "foo", sizeof("foo") - 1,
                                     SCM_ENCODING_ASCII));
  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(str));
  cut_assert_true(scm_obj_type_p(str, &SCM_STRING_TYPE_INFO));
}

void
test_scm_string_is_string(void)
{
  ScmObj str = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &nil);

  /* preprocess */
  SCM_SETQ(str, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                     "foo", sizeof("foo") - 1,
                                     SCM_ENCODING_ASCII));
  SCM_SETQ(nil, SCM_OBJ(scm_nil_instance()));

  /* action and postcondition check */
  cut_assert_true(scm_string_is_string(str));
  cut_assert_false(scm_string_is_string(nil));
}
