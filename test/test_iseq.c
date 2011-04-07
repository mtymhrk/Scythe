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
  cut_assert_not_null(SCM_ISEQ_IMMVAL_VEC(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_SEQ_SIZE, SCM_ISEQ_SEQ_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_IMMVEC_SIZE,
                        SCM_ISEQ_VEC_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_VEC_LENGTH(iseq));

}

void
test_scm_iseq_set_word(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_iseq_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), (scm_iseq_t)12345);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  scm_iseq_t actual;
  scm_iseq_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual);
  cut_assert_equal_uint(12345, actual);
  cut_assert_equal_pointer(next_read, next_write);
}
