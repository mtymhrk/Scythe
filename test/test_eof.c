#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "miscobjects.h"

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
test_scm_eof_construct(void)
{
  ScmObj eof1 = SCM_OBJ_INIT, eof2 = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&eof1, eof2);

  SCM_SETQ(eof1, scm_eof_instance());
  SCM_SETQ(eof2, scm_eof_instance());

  cut_assert_not_null(eof1);
  cut_assert_not_null(eof2);

  cut_assert_true(scm_obj_is_same_instance(SCM_OBJ(eof1), SCM_OBJ(eof2)));
}

void
test_scm_eof_is_eof(void)
{
  cut_assert_true(scm_eof_is_eof(SCM_OBJ(scm_eof_instance())));
  cut_assert_false(scm_eof_is_eof(SCM_OBJ(scm_nil_instance())));
}
