#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "api.h"
#include "iseq.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
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
  cut_assert_not_null(SCM_ISEQ_SEQ_VEC(iseq));
  cut_assert_not_null(SCM_ISEQ_IDX_VEC(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_SEQ_SIZE, SCM_ISEQ_SEQ_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_ISEQ_DEFAULT_INDEX_SIZE,
                        SCM_ISEQ_IDX_CAPACITY(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_IDX_LENGTH(iseq));

}

void
test_scm_iseq_push_ushort(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_ushort(iseq, 123);
  unsigned short actual = scm_iseq_get_ushort(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(unsigned short), rslt);
  cut_assert_equal_uint(sizeof(unsigned short), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(123, actual);
}

void
test_scm_iseq_push_uint_1(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  unsigned int actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint(iseq, UINT32_MAX);
  actual = scm_iseq_get_uint(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(unsigned int), rslt);
  cut_assert_equal_uint(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(UINT32_MAX, actual);
}

void
test_scm_iseq_push_uint_2(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  unsigned int actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint(iseq, 0);
  actual = scm_iseq_get_uint(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(unsigned int), rslt);
  cut_assert_equal_uint(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(0, actual);
}

void
test_scm_iseq_push_uint_3(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  int actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint(iseq, INT32_MAX);
  actual = (int)scm_iseq_get_uint(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(unsigned int), rslt);
  cut_assert_equal_uint(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(INT32_MAX, actual);
}

void
test_scm_iseq_push_uint_4(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  int actual;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  ssize_t rslt = scm_iseq_push_uint(iseq, (unsigned int)INT32_MIN);
  actual = (int)scm_iseq_get_uint(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(unsigned int), rslt);
  cut_assert_equal_uint(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_int(INT32_MIN, actual);
}

void
test_scm_iseq__expand_sequence_buffer(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  for (size_t idx = 0; idx < SCM_ISEQ_DEFAULT_SEQ_SIZE / sizeof(short); idx++) {
    ssize_t r = scm_iseq_push_ushort(iseq, (uint8_t)(idx % 255));
    cut_assert_equal_int((int)(idx + 1) * (int)sizeof(short), r);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_ushort(iseq, 255);

  /* postcondition check */
  cut_assert_equal_int(SCM_ISEQ_DEFAULT_SEQ_SIZE + sizeof(short), rslt);
  cut_assert_true(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);

  unsigned short actual = scm_iseq_get_ushort(iseq, SCM_ISEQ_DEFAULT_SEQ_SIZE);
  cut_assert_equal_uint(255, actual);

  for (size_t i = 0; i < SCM_ISEQ_DEFAULT_SEQ_SIZE / sizeof(short); i++) {
    cut_assert_equal_uint(i % 255,
                          scm_iseq_get_ushort(iseq, i * sizeof(short)));;
  }
}

void
test_scm_iseq_push_obj_1(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &expected, &actual);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  expected = scm_api_nil();

  /* action */
  rslt = scm_iseq_push_obj(iseq, expected);
  actual = scm_iseq_get_obj(iseq, 0);

  /* postcondition check */
  cut_assert_equal_int(sizeof(ScmObj), rslt);
  cut_assert_equal_uint(sizeof(ScmObj), SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_true(scm_capi_eq_p(expected, actual));

  cut_assert_equal_uint(1, SCM_ISEQ_IDX_LENGTH(iseq));
  cut_assert_equal_uint(0, SCM_ISEQ_IDX_VEC(iseq)[0]);
}

void
test_scm_iseq__expand_object_vector(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val, &actual);

  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val = scm_api_nil();

  for (int i; i < SCM_ISEQ_DEFAULT_INDEX_SIZE; i++) {
    ssize_t r = scm_iseq_push_obj(iseq, val);
    cut_assert_true(r >= 0);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_obj(iseq, val);

  /* postcondition check */
  cut_assert_true(rslt >= 0);
  cut_assert_true(SCM_ISEQ_IDX_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_INDEX_SIZE);
  actual = scm_iseq_get_obj(iseq, SCM_ISEQ_IDX_VEC(iseq)[SCM_ISEQ_DEFAULT_INDEX_SIZE]);
  cut_assert_true(scm_obj_same_instance_p(val, actual));

  for (size_t i; i < SCM_ISEQ_DEFAULT_INDEX_SIZE; i++) {
    actual = scm_iseq_get_obj(iseq, SCM_ISEQ_IDX_VEC(iseq)[i]);
    cut_assert_true(scm_obj_same_instance_p(val, actual));
  }
}
