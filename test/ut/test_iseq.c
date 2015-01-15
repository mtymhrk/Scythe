#include "iseq.c"

#include "scythe/api.h"

#include "test.h"

TEST_GROUP(iseq);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;
static ScmObj iseq;
static ScmObj iseq2;

TEST_SETUP(iseq)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);

  iseq = SCM_OBJ_NULL;
  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(iseq));
  iseq = scm_fcd_iseq_new(SCM_MEM_HEAP);

  iseq2 = SCM_OBJ_NULL;
  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(iseq2));
  iseq2 = scm_fcd_iseq_new(SCM_MEM_HEAP);
}

TEST_TEAR_DOWN(iseq)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(iseq, iseq_new)
{
  TEST_ASSERT_TRUE(scm_obj_not_null_p(iseq));
  TEST_ASSERT_TRUE(scm_obj_type_p(iseq, &SCM_ISEQ_TYPE_INFO));
  TEST_ASSERT_NOT_NULL(SCM_ISEQ_SEQ_VEC(iseq));
  TEST_ASSERT_NOT_NULL(SCM_ISEQ_OBJS_VEC(iseq));
  TEST_ASSERT_EQUAL_UINT(SCM_ISEQ_DEFAULT_SEQ_SIZE, SCM_ISEQ_SEQ_CAPACITY(iseq));
  TEST_ASSERT_EQUAL_UINT(0, SCM_ISEQ_SEQ_LENGTH(iseq));
  TEST_ASSERT_EQUAL_UINT(SCM_ISEQ_DEFAULT_OBJS_SIZE,
                         SCM_ISEQ_OBJS_CAPACITY(iseq));
  TEST_ASSERT_EQUAL_UINT(0, SCM_ISEQ_OBJS_LENGTH(iseq));
}

static ssize_t
push_inst_noopd(ScmObj iseq, scm_opcode_t op)
{
  struct scm_vm_inst_noopd inst = { .op = op };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), NULL, 0);
}

static ssize_t
push_inst_obj(ScmObj iseq, scm_opcode_t op, ScmObj opd1)
{
  struct scm_vm_inst_obj inst = { .op = op, .opd1 = opd1 };
  size_t objs[1] = { SCM_INST_OPD_OFFSET_OBJ_1 };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), objs, 1);
}

static ssize_t
push_inst_obj_obj(ScmObj iseq, scm_opcode_t op, ScmObj opd1, ScmObj opd2)
{
  struct scm_vm_inst_obj_obj inst = { .op = op, .opd1 = opd1, .opd2 = opd2 };
  size_t objs[2] = {
    SCM_INST_OPD_OFFSET_OBJ_OBJ_1, SCM_INST_OPD_OFFSET_OBJ_OBJ_2
  };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), objs, 2);
}

static ssize_t
push_inst_si(ScmObj iseq, scm_opcode_t op, int opd1)
{
  struct scm_vm_inst_si inst = { .op = op, .opd1 = opd1 };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), NULL, 0);
}

static ssize_t
push_inst_si_si(ScmObj iseq, scm_opcode_t op, int opd1, int opd2)
{
  struct scm_vm_inst_si_si inst = { .op = op, .opd1 = opd1, .opd2 = opd2 };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), NULL, 0);
}

static ssize_t
push_inst_si_si_obj(ScmObj iseq, scm_opcode_t op, int opd1, int opd2, ScmObj opd3)
{
  struct scm_vm_inst_si_si_obj inst = { .op = op, .opd1 = opd1, .opd2 = opd2, .opd3 = opd3 };
  size_t objs[1] = { SCM_INST_OPD_OFFSET_SI_SI_OBJ_3 };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), objs, 1);
}

static ssize_t
push_inst_iof(ScmObj iseq, scm_opcode_t op, int opd1)
{
  struct scm_vm_inst_iof inst = { .op = op, .opd1 = opd1 };
  return scm_iseq_push_inst(iseq, &inst, sizeof(inst), NULL, 0);
}

TEST(iseq, iseq_push_inst2__push)
{
  scm_byte_t *ip;
  int actual_op, actual_opd;

   TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_EFRAME,
                        push_inst_si(iseq, SCM_OPCODE_EFRAME, 123));

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI(ip, actual_opd);

  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_EFRAME, actual_op);
  TEST_ASSERT_EQUAL_INT(123, actual_opd);
}

TEST(iseq, iseq_push_inst2__push_obj)
{
  scm_byte_t *ip;
  ScmObj actual_opd1 = SCM_OBJ_INIT, actual_opd2 = SCM_OBJ_INIT;
  ScmRef ref;
  int actual_op;

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_GREF,
                        push_inst_obj_obj(iseq, SCM_OPCODE_GREF,
                                          SCM_TRUE_OBJ, SCM_FALSE_OBJ));

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip, actual_opd1, actual_opd2);

  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_GREF, actual_op);
  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, actual_opd1);
  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, actual_opd2);

  TEST_ASSERT_EQUAL_INT(SCM_INST_OPD_OFFSET_OBJ_OBJ_1,
                        SCM_ISEQ_OBJS_VEC(iseq)[0]);
  TEST_ASSERT_EQUAL_INT(SCM_INST_OPD_OFFSET_OBJ_OBJ_2,
                        SCM_ISEQ_OBJS_VEC(iseq)[1]);

  ref = SCM_REF_MAKE_FROM_PTR((scm_iseq_to_ip(iseq)
                               + SCM_ISEQ_OBJS_VEC(iseq)[0]));
  TEST_ASSERT_SCM_EQ(SCM_TRUE_OBJ, SCM_REF_DEREF(ref));

  ref = SCM_REF_MAKE_FROM_PTR((scm_iseq_to_ip(iseq)
                               + SCM_ISEQ_OBJS_VEC(iseq)[1]));
  TEST_ASSERT_SCM_EQ(SCM_FALSE_OBJ, SCM_REF_DEREF(ref));
}

TEST(iseq, expand_sequence_buffer)
{
  size_t sum;
  ssize_t ret_value;

  for (sum = 0; sum < SCM_ISEQ_DEFAULT_SEQ_SIZE; sum += SCM_INST_SZ_NOP)
    push_inst_noopd(iseq, SCM_OPCODE_NOP);

  ret_value =     push_inst_noopd(iseq, SCM_OPCODE_NOP);

  TEST_ASSERT_EQUAL_INT(sum + SCM_INST_SZ_NOP, ret_value);
  TEST_ASSERT_TRUE(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);
}

TEST(iseq, expand_index_buffer)
{
  for (size_t sum = 0; sum < SCM_ISEQ_DEFAULT_OBJS_SIZE; sum++)
    push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);

  push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);

  TEST_ASSERT_TRUE(SCM_ISEQ_OBJS_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_OBJS_SIZE);
}

static void
test_iseq_eq__equal(ScmObj iseq1, ScmObj iseq2)
{
  bool actual_rslt;
  TEST_ASSERT_EQUAL_INT(0, scm_iseq_eq(iseq, iseq2, &actual_rslt));
  TEST_ASSERT_TRUE(actual_rslt);
}

static void
test_iseq_eq__not_equal(ScmObj iseq1, ScmObj iseq2)
{
  bool actual_rslt;
  TEST_ASSERT_EQUAL_INT(0, scm_iseq_eq(iseq, iseq2, &actual_rslt));
  TEST_ASSERT_FALSE(actual_rslt);
}

TEST(iseq, iseq_eq__noopd__equal)
{
  push_inst_noopd(iseq, SCM_OPCODE_PUSH);
  push_inst_noopd(iseq2, SCM_OPCODE_PUSH);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__noopd__not_equal)
{
  push_inst_noopd(iseq, SCM_OPCODE_PUSH);
  push_inst_noopd(iseq2, SCM_OPCODE_NOP);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj__equal)
{
  push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  push_inst_obj(iseq2, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj__not_equal)
{
  push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  push_inst_obj(iseq2, SCM_OPCODE_IMMVAL, SCM_EOF_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj_obj__equal)
{
  push_inst_obj_obj(iseq, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  push_inst_obj_obj(iseq2, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj_obj__not_equal)
{
  push_inst_obj_obj(iseq, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  push_inst_obj_obj(iseq2, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_NIL_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si__equal)
{
  push_inst_si(iseq, SCM_OPCODE_CALL, 1);
  push_inst_si(iseq2, SCM_OPCODE_CALL, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si__not_equal)
{
  push_inst_si(iseq, SCM_OPCODE_CALL, 1);
  push_inst_si(iseq2, SCM_OPCODE_CALL, 10);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si__equal)
{
  push_inst_si_si(iseq, SCM_OPCODE_SREF, 1, 1);
  push_inst_si_si(iseq2, SCM_OPCODE_SREF, 1, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si__not_equal)
{
  push_inst_si_si(iseq, SCM_OPCODE_SREF, 1, 1);
  push_inst_si_si(iseq2, SCM_OPCODE_SREF, 1, -20);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si_obj__equal)
{
  push_inst_si_si_obj(iseq, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  push_inst_si_si_obj(iseq2, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si_obj__not_equal)
{
  push_inst_si_si_obj(iseq, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  push_inst_si_si_obj(iseq2, SCM_OPCODE_CLOSE, 1, 1, SCM_EOF_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__iof__equal)
{
  push_inst_iof(iseq, SCM_OPCODE_JMP, 1);
  push_inst_iof(iseq2, SCM_OPCODE_JMP, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__iof__not_equal)
{
  push_inst_iof(iseq, SCM_OPCODE_JMP, 1);
  push_inst_iof(iseq2, SCM_OPCODE_JMP, 100);
  test_iseq_eq__not_equal(iseq, iseq2);
}
