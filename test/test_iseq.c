#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "api.h"
#include "iseq.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  vm = scm_vm_new();
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}


void
test_scm_iseq_new(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* action */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* postconditin check */
  cut_assert_true(scm_obj_not_null_p(iseq));
  cut_assert_true(scm_obj_type_p(iseq, &SCM_ISEQ_TYPE_INFO));
  cut_assert_not_null(SCM_ISEQ_SEQ(iseq));
  cut_assert_not_null(SCM_ISEQ_IMMVAL_VEC(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_SEQ_SIZE, SCM_ISEQ_SEQ_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_IMMVS_SIZE,
                        SCM_ISEQ_VEC_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_VEC_LENGTH(iseq));

}

void
test_scm_iseq_push_uint8(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint8(iseq, 123);
  uint8_t actual = SCM_ISEQ_SEQ(iseq)[0];;

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(123, actual);
}

void
test_scm_iseq_push_uint32_1(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  uint32_t actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint32(iseq, UINT32_MAX);
  scm_iseq_get_uint32(iseq, 0, &actual);

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(4, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(UINT32_MAX, actual);
}

void
test_scm_iseq_push_uint32_2(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  uint32_t actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint32(iseq, 0);
  scm_iseq_get_uint32(iseq, 0, &actual);

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(4, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(0, actual);
}

void
test_scm_iseq_push_uint32_3(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  int32_t actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint32(iseq, INT32_MAX);
  scm_iseq_get_uint32(iseq, 0, (uint32_t *)&actual);

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(4, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(INT32_MAX, actual);
}

void
test_scm_iseq_push_uint32_4(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  int32_t actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint32(iseq, (uint32_t)INT32_MIN);
  scm_iseq_get_uint32(iseq, 0, (uint32_t *)&actual);

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(4, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(INT32_MIN, actual);
}

void
test_scm_iseq__expand_sequence_buffer(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  for (size_t idx = 0; idx < SCM_ISEQ_DEFAULT_SEQ_SIZE; idx++) {
    ssize_t r = scm_iseq_push_uint8(iseq, (uint8_t)(idx % 255));
    cut_assert_equal_int((int)idx, r);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_uint8(iseq, 255);

  /* postcondition check */
  cut_assert_equal_int(SCM_ISEQ_DEFAULT_SEQ_SIZE, rslt);
  cut_assert_true(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);

  uint8_t actual = SCM_ISEQ_SEQ(iseq)[SCM_ISEQ_DEFAULT_SEQ_SIZE];
  cut_assert_equal_uint(255, actual);

  for (size_t i = 0; i < SCM_ISEQ_DEFAULT_SEQ_SIZE; i++) {
    cut_assert_equal_uint(i % 255, SCM_ISEQ_SEQ(iseq)[i]);
  }
}

void
test_scm_iseq_push_immval_get_immval(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual1 = SCM_OBJ_INIT, actual2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val1, &val2, &actual1, &actual2);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val1 = scm_vm_nil_instance();
  val2 = scm_vm_eof_instance();

  /* action */
  ssize_t idx1 = scm_iseq_push_immval(iseq, val1);
  ssize_t idx2 = scm_iseq_push_immval(iseq, val2);
  actual1 = scm_iseq_get_immval(iseq, (size_t)idx1);
  actual2 = scm_iseq_get_immval(iseq, (size_t)idx2);

  /* postcondition check */
  cut_assert_true(idx1 >= 0);
  cut_assert_true(idx2 >= 0);
  cut_assert_true(idx1 != idx2);
  cut_assert_true(scm_obj_same_instance_p(val1, actual1));
  cut_assert_true(scm_obj_same_instance_p(val2, actual2));
}

void
test_scm_iseq_set_immval__update_successed(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual1 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val1, &val2, &actual1);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val1 = scm_vm_nil_instance();
  val2 = scm_vm_eof_instance();
  ssize_t idx1 = scm_iseq_push_immval(iseq, val1);

  /* action */
  ssize_t idx2 = scm_iseq_set_immval(iseq, (size_t)idx1, val2);
  actual1 = scm_iseq_get_immval(iseq, (size_t)idx1);

  /* postcondition check */
  cut_assert_true(idx1 >= 0);
  cut_assert_true(idx2 >= 0);
  cut_assert_equal_int(idx1, idx2);
  cut_assert_true(scm_obj_same_instance_p(val2, actual1));
}

void
test_scm_iseq__expand_object_vector(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val, &actual);

  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val = scm_vm_nil_instance();

  for (int i; i < SCM_ISEQ_DEFAULT_IMMVS_SIZE; i++) {
    ssize_t r = scm_iseq_push_immval(iseq, val);
    cut_assert_true(r >= 0);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_immval(iseq, val);

  /* postcondition check */
  cut_assert_true(rslt >= 0);
  cut_assert_true(SCM_ISEQ_VEC_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_IMMVS_SIZE);
  actual = scm_iseq_get_immval(iseq, (size_t)rslt);
    cut_assert_true(scm_obj_same_instance_p(val, actual));

  for (size_t i; i < SCM_ISEQ_DEFAULT_IMMVS_SIZE; i++) {
    actual = scm_iseq_get_immval(iseq, i);
    cut_assert_true(scm_obj_same_instance_p(val, actual));
  }
}
