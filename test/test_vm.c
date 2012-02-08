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

  cut_assert(scm_obj_same_instance_p(scm_vm_nil_instance(),
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
  cut_assert(scm_obj_same_instance_p(sym, SCM_VM_VAL(vm)));
}


void
test_scm_vm_run__call_cons(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj cons_sym = SCM_OBJ_INIT;
  ScmObj car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;
  scm_inst_t code;
  size_t idx;

  SCM_STACK_FRAME_PUSH(&iseq, &cons_sym, &car, &cdr, &actual);

  /* preprocess */
  scm_vm_setup_system(vm);

  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));
  SCM_SETQ(cons_sym, scm_api_make_symbol_ascii("cons"));
  SCM_SETQ(car, scm_api_make_symbol_ascii("foo"));
  SCM_SETQ(cdr, scm_api_make_symbol_ascii("bar"));

  idx = 0;

  code.plain.op = SCM_OPCODE_FRAME;
  code.plain.arg = 0;
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.immv1.op = SCM_OPCODE_IMMVAL;
  code.immv1.imm_idx = scm_iseq_set_immval(iseq, cdr);
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.plain.op = SCM_OPCODE_PUSH;
  code.plain.arg = 0;
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.immv1.op = SCM_OPCODE_IMMVAL;
  code.immv1.imm_idx = scm_iseq_set_immval(iseq, car);
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.plain.op = SCM_OPCODE_PUSH;
  code.plain.arg = 0;
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  code.primv.primval = 2;
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.immv1.op = SCM_OPCODE_GREF;
  code.immv1.imm_idx = scm_iseq_set_immval(iseq, cons_sym);
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.plain.op = SCM_OPCODE_CALL;
  code.plain.arg = 0;
  scm_iseq_set_word(iseq, idx++, code.iword);

  code.plain.op = SCM_OPCODE_STOP;
  code.plain.arg = 0;
  scm_iseq_set_word(iseq, idx++, code.iword);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  SCM_SETQ(actual, SCM_VM_VAL(vm));
  cut_assert(scm_obj_type_p(actual, &SCM_PAIR_TYPE_INFO));
  cut_assert(scm_obj_same_instance_p(SCM_PAIR_CAR(actual), car));
  cut_assert(scm_obj_same_instance_p(SCM_PAIR_CDR(actual), cdr));
}
