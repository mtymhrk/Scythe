#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "object.h"
#include "iseq.h"
#include "api.h"
#include "pair.h"

static ScmEvaluator *ev;
static ScmObj vm;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_setup_current_vm(ev);
  vm = scm_vm__current_vm;
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_vm_stack_push_scmobj(void)
{
  scm_vm_stack_push(vm, scm_api_nil());

  cut_assert(scm_obj_same_instance_p(scm_api_nil(),
                                     scm_vm_stack_pop(vm)));
}


void
test_scm_vm_run__op_immval(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &sym);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);
  sym = scm_capi_make_symbol_from_cstr("cons", SCM_ENC_ASCII);

  scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, sym);
  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_HALT);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  cut_assert(scm_obj_same_instance_p(sym, SCM_VM(vm)->reg.val));
}


void
test_scm_vm_run__call_cons(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj cons_sym = SCM_OBJ_INIT;
  ScmObj car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &cons_sym, &car, &cdr, &actual);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);
  cons_sym = scm_capi_make_symbol_from_cstr("cons", SCM_ENC_ASCII);
  car = scm_capi_make_symbol_from_cstr("foo", SCM_ENC_ASCII);
  cdr = scm_capi_make_symbol_from_cstr("bar", SCM_ENC_ASCII);


  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_FRAME);
  scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, car);
  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
  scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, cdr);
  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
  scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_GREF, cons_sym);
  scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_CALL, 2);
  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_HALT);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  actual = SCM_VM(vm)->reg.val;
  cut_assert(scm_obj_type_p(actual, &SCM_PAIR_TYPE_INFO));
  cut_assert(scm_obj_same_instance_p(SCM_PAIR_CAR(actual), car));
  cut_assert(scm_obj_same_instance_p(SCM_PAIR_CDR(actual), cdr));
}
