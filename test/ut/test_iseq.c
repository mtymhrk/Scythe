#include "../../src/iseq.c"

#include "api.h"

#include "test.h"

TEST_GROUP(iseq);

static ScmEvaluator *ev;
static ScmObj iseq;

TEST_SETUP(iseq)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);

  iseq = SCM_OBJ_NULL;
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(iseq));
  iseq = scm_iseq_new(SCM_MEM_HEAP);
}

TEST_TEAR_DOWN(iseq)
{
  scm_capi_evaluator_end(ev);
}

TEST(iseq, iseq_new)
{
  TEST_ASSERT_TRUE(scm_obj_not_null_p(iseq));
  TEST_ASSERT_TRUE(scm_obj_type_p(iseq, &SCM_ISEQ_TYPE_INFO));
  TEST_ASSERT_NOT_NULL(SCM_ISEQ_SEQ_VEC(iseq));
  TEST_ASSERT_NOT_NULL(SCM_ISEQ_IDX_VEC(iseq));
  TEST_ASSERT_EQUAL_UINT(SCM_ISEQ_DEFAULT_SEQ_SIZE, SCM_ISEQ_SEQ_CAPACITY(iseq));
  TEST_ASSERT_EQUAL_UINT(0, SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_UINT(SCM_ISEQ_DEFAULT_INDEX_SIZE,
                         SCM_ISEQ_IDX_CAPACITY(iseq));
  TEST_ASSERT_EQUAL_UINT(0, SCM_ISEQ_IDX_LENGTH(iseq));
}

TEST(iseq, iseq_push_ushort)
{
  ssize_t rslt = scm_iseq_push_ushort(iseq, 123);
  unsigned short actual = scm_iseq_get_ushort(iseq, 0);

  TEST_ASSERT_EQUAL_INT(sizeof(unsigned short), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(unsigned short), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(123, actual);
}

TEST(iseq, iseq_push_uint_1)
{
  ssize_t rslt = scm_iseq_push_uint(iseq, UINT32_MAX);
  unsigned int actual = scm_iseq_get_uint(iseq, 0);

  TEST_ASSERT_EQUAL_INT(sizeof(unsigned int), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(UINT32_MAX, actual);
}

TEST(iseq, iseq_push_uint_2)
{
  ssize_t rslt = scm_iseq_push_uint(iseq, 0);
  unsigned int actual = scm_iseq_get_uint(iseq, 0);

  TEST_ASSERT_EQUAL_INT(sizeof(unsigned int), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(0, actual);
}

TEST(iseq, iseq_push_uint_3)
{
  ssize_t rslt = scm_iseq_push_uint(iseq, INT32_MAX);
  int actual = (int)scm_iseq_get_uint(iseq, 0);

  TEST_ASSERT_EQUAL_INT(sizeof(unsigned int), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(INT32_MAX, actual);
}

TEST(iseq, iseq_push_uint_4)
{
  ssize_t rslt = scm_iseq_push_uint(iseq, (unsigned int)INT32_MIN);
  int actual = (int)scm_iseq_get_uint(iseq, 0);

  TEST_ASSERT_EQUAL_INT(sizeof(unsigned int), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(unsigned int), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(INT32_MIN, actual);
}

TEST(iseq, expand_sequence_buffer)
{
  for (size_t idx = 0; idx < SCM_ISEQ_DEFAULT_SEQ_SIZE / sizeof(short); idx++) {
    ssize_t r = scm_iseq_push_ushort(iseq, (uint8_t)(idx % 255));
    TEST_ASSERT_EQUAL_INT((int)(idx + 1) * (int)sizeof(short), r);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_ushort(iseq, 255);

  /* postcondition check */
  TEST_ASSERT_EQUAL_INT(SCM_ISEQ_DEFAULT_SEQ_SIZE + sizeof(short), rslt);
  TEST_ASSERT_TRUE(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);

  unsigned short actual = scm_iseq_get_ushort(iseq, SCM_ISEQ_DEFAULT_SEQ_SIZE);
  TEST_ASSERT_EQUAL_UINT(255, actual);

  for (size_t i = 0; i < SCM_ISEQ_DEFAULT_SEQ_SIZE / sizeof(short); i++) {
    TEST_ASSERT_EQUAL_UINT(i % 255,
                          scm_iseq_get_ushort(iseq, i * sizeof(short)));;
  }
}

TEST(iseq, iseq_push_obj_1)
{
  ScmObj actual = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&actual);

  /* action */
  rslt = scm_iseq_push_obj(iseq, SCM_NIL_OBJ);
  actual = scm_iseq_get_obj(iseq, 0);

  /* postcondition check */
  TEST_ASSERT_EQUAL_INT(sizeof(ScmObj), rslt);
  TEST_ASSERT_EQUAL_UINT(sizeof(ScmObj), SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, actual);

  TEST_ASSERT_EQUAL_UINT(1, SCM_ISEQ_IDX_LENGTH(iseq));
  TEST_ASSERT_EQUAL_UINT(0, SCM_ISEQ_IDX_VEC(iseq)[0]);
}

TEST(iseq, expand_object_vector)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&actual, &expected);

  expected = scm_capi_make_symbol_from_cstr("foo", SCM_ENC_SRC);

  for (int i = 0; i < SCM_ISEQ_DEFAULT_INDEX_SIZE; i++) {
    ssize_t r = scm_iseq_push_obj(iseq, SCM_NIL_OBJ);
    TEST_ASSERT(r >= 0);
  }

  /* action */
  ssize_t rslt = scm_iseq_push_obj(iseq, expected);
  actual = scm_iseq_get_obj(iseq, SCM_ISEQ_IDX_VEC(iseq)[SCM_ISEQ_DEFAULT_INDEX_SIZE]);

  /* postcondition check */
  TEST_ASSERT(rslt >= 0);
  TEST_ASSERT_TRUE(SCM_ISEQ_IDX_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_INDEX_SIZE);
  TEST_ASSERT_SCM_EQ(expected, actual);

  for (size_t i = 0; i < SCM_ISEQ_DEFAULT_INDEX_SIZE; i++) {
    actual = scm_iseq_get_obj(iseq, SCM_ISEQ_IDX_VEC(iseq)[i]);
    TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, actual);
  }
}
