#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "nil.h"
#include "string.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_construct());
  scm_vm_switch_vm(vm);
}

void
cut_shutdown(void)
{
  scm_vm_revert_vm();
  scm_vm_destruct(vm);
}

void
test_scm_string_construct(void)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  /* action */
  SCM_SETQ(str, scm_string_construct("foo", sizeof("foo") - 1,
                                     SCM_ENCODING_ASCII));
  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(str));
  cut_assert_true(SCM_OBJ_IS_TYPE(str, &SCM_STRING_TYPE_INFO));
}

void
test_scm_string_is_string(void)
{
  ScmObj str = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &nil);

  /* preprocess */
  SCM_SETQ(str, scm_string_construct("foo", sizeof("foo") - 1,
                                     SCM_ENCODING_ASCII));
  SCM_SETQ(nil, SCM_OBJ(scm_nil_construct()));

  /* action and postcondition check */
  cut_assert_true(scm_string_is_string(str));
  cut_assert_false(scm_string_is_string(nil));
}
