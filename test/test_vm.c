#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "object.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_setup(void)
{
  vm = SCM_OBJ_INIT;

  SCM_SETQ(vm, scm_vm_new());

  scm_vm_switch_vm(vm);
}

void
cut_teardown(void)
{
  scm_vm_end(vm);
}

void
test_scm_vm_stack_push_primitive_value_0(void)
{
  long actual;

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)0L, false);

  actual = (long)scm_vm_stack_pop(vm);

  cut_assert(actual == 0L);
}

void
test_scm_vm_stack_push_primitive_value_SCM_SWORD_MAX(void)
{
  long actual;

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_SWORD_MAX, false);

  actual = (long)scm_vm_stack_pop(vm);

  cut_assert(actual == SCM_SWORD_MAX);
}

void
test_scm_vm_stack_push_primitive_value_SCM_SWORD_MIN(void)
{
  long actual;

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_SWORD_MIN, false);

  actual = (long)scm_vm_stack_pop(vm);

  cut_assert(actual == SCM_SWORD_MIN);
}


void
test_scm_vm_stack_push_scmobj_value(void)
{
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)scm_vm_nil_instance(), true);

  cut_assert(scm_obj_is_same_instance(scm_vm_nil_instance(),
                                      scm_vm_stack_pop(vm)));
}
