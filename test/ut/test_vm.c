#include "../../src/vm.c"

#include "api.h"

#include "test.h"

TEST_GROUP(vm);

static ScmEvaluator *ev;
static ScmObj vm;

TEST_SETUP(vm)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  vm = scm_vm_current_vm();
}

TEST_TEAR_DOWN(vm)
{
  scm_capi_evaluator_end(ev);
}

TEST(vm, vm_run__op_immval)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &sym);

  /* preprocess */
  iseq = scm_api_make_iseq();
  sym = scm_capi_make_symbol_from_cstr("abc", SCM_ENC_SRC);

  scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, sym);
  scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_HALT);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  TEST_ASSERT_EQUAL_INT(1, SCM_VM(vm)->reg.vc);
  TEST_ASSERT_SCM_EQ(sym, SCM_VM(vm)->reg.val[0]);
}
