#include <cutter.h>
#include <assert.h>

#include "iseq.h"
#include "vm.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
  scm_vm_switch_vm(vm);
}

void
cut_shutdown(void)
{
  scm_vm_revert_vm();
  scm_vm_end(vm);
}


void
test_scm_iseq_new(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* action */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* postconditin check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(iseq));
  cut_assert_true(SCM_OBJ_IS_TYPE(iseq, &SCM_ISEQ_TYPE_INFO));
  cut_assert_not_null(SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_SIZE,SCM_ISEQ_SIZE(iseq));
  cut_assert_equal_uint(0,SCM_ISEQ_LENGTH(iseq));
}

void
test_scm_iseq_set_op(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_iseq_t *next_write =
    scm_iseq_set_op(iseq, SCM_ISEQ_SEQ(iseq), SCM_INST_CALL);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_LENGTH(iseq));
  SCM_INST_T actual;
  scm_iseq_t *next_read = scm_iseq_get_op(SCM_ISEQ_SEQ(iseq), &actual);
  cut_assert_equal_int(SCM_INST_CALL, actual);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_primval(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_iseq_t *next_write =
    scm_iseq_set_primval(iseq, SCM_ISEQ_SEQ(iseq), -1);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_LENGTH(iseq));
  int actual;
  scm_iseq_t *next_read = scm_iseq_get_primval(SCM_ISEQ_SEQ(iseq), &actual);
  cut_assert_equal_int(-1, actual);
  cut_assert_equal_pointer(next_read, next_write);
}
