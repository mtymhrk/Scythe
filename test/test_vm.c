#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "object.h"
#include "iseq.h"
#include "api.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_setup(void)
{
  vm = SCM_OBJ_INIT;

  SCM_SETQ(vm, scm_vm_new());
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



void
test_scm_vm_run__op_immval(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT;
  scm_inst_t code;

  SCM_STACK_FRAME_PUSH(&iseq, &sym);

  /* preprocess */
  scm_vm_setup_system(vm);

  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));
  SCM_SETQ(sym, scm_api_make_symbol_ascii("cons"));

  code.immv1.op = SCM_OPCODE_IMMVAL;
  code.immv1.imm_idx = scm_iseq_set_immval(iseq, sym);

  scm_iseq_set_word(iseq, 0, code.iword);

  code.plain.op = SCM_OPCODE_STOP;
  code.plain.arg = 0;

  scm_iseq_set_word(iseq, 1, code.iword);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  cut_assert(SCM_OBJ_IS_SAME_INSTANCE(sym, SCM_VM_VAL(vm)));
}


