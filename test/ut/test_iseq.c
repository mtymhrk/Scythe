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

TEST(iseq, iseq_push_inst_noopd)
{
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  actual_ret_value = scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_PUSH);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_PUSH, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_PUSH, actual_op);
}

TEST(iseq, iseq_push_inst_obj)
{
  ScmObj actual_opd = SCM_OBJ_INIT;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  SCM_REFSTK_INIT_REG(&actual_opd);

  actual_ret_value = scm_iseq_push_inst_obj(iseq,
                                            SCM_OPCODE_IMMVAL,
                                            SCM_NIL_OBJ);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_OBJ(ip, actual_opd);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_IMMVAL, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_IMMVAL, actual_op);
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, actual_opd);
}

TEST(iseq, iseq_push_inst_obj__index)
{
  scm_iseq_push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);

  TEST_ASSERT_EQUAL_INT(1, SCM_ISEQ_OBJS_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(0, SCM_ISEQ_OBJS_VEC(iseq)[0]);
}

TEST(iseq, iseq_push_inst_obj_obj)
{
  ScmObj actual_opd1 = SCM_OBJ_INIT, actual_opd2 = SCM_OBJ_INIT;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  SCM_REFSTK_INIT_REG(&actual_opd1, &actual_opd2);

  actual_ret_value = scm_iseq_push_inst_obj_obj(iseq,
                                                SCM_OPCODE_GREF,
                                                SCM_NIL_OBJ,
                                                SCM_UNDEF_OBJ);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip, actual_opd1, actual_opd2);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_GREF, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_GREF, actual_op);
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, actual_opd1);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, actual_opd2);
}

TEST(iseq, iseq_push_inst_obj_obj__index)
{
  scm_iseq_push_inst_obj_obj(iseq, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_UNDEF_OBJ);

  TEST_ASSERT_EQUAL_INT(1, SCM_ISEQ_OBJS_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(0, SCM_ISEQ_OBJS_VEC(iseq)[0]);
}

TEST(iseq, iseq_push_inst_si)
{
  int actual_opd;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  actual_ret_value = scm_iseq_push_inst_si(iseq,
                                           SCM_OPCODE_CALL,
                                           128);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI(ip, actual_opd);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_CALL, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_CALL, actual_op);
  TEST_ASSERT_EQUAL_INT(128, actual_opd);
}

TEST(iseq, iseq_push_inst_si_si)
{
  int actual_opd1, actual_opd2;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  actual_ret_value = scm_iseq_push_inst_si_si(iseq,
                                              SCM_OPCODE_SREF,
                                              128,
                                              -64);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI_SI(ip, actual_opd1, actual_opd2);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_SREF, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_SREF, actual_op);
  TEST_ASSERT_EQUAL_INT(128, actual_opd1);
  TEST_ASSERT_EQUAL_INT(-64, actual_opd2);
}

TEST(iseq, iseq_push_inst_si_si_obj)
{
  ScmObj actual_opd3 = SCM_OBJ_INIT;
  int actual_opd1, actual_opd2;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  SCM_REFSTK_INIT_REG(&actual_opd3);

  actual_ret_value = scm_iseq_push_inst_si_si_obj(iseq,
                                                  SCM_OPCODE_CLOSE,
                                                  128,
                                                  -64,
                                                  SCM_NIL_OBJ);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip, actual_opd1, actual_opd2, actual_opd3);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_CLOSE, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_CLOSE, actual_op);
  TEST_ASSERT_EQUAL_INT(128, actual_opd1);
  TEST_ASSERT_EQUAL_INT(-64, actual_opd2);
  TEST_ASSERT_SCM_EQ(SCM_NIL_OBJ, actual_opd3);
}

TEST(iseq, iseq_push_inst_si_si_obj__index)
{
  scm_iseq_push_inst_si_si_obj(iseq, SCM_OPCODE_CLOSE, 128, -64, SCM_NIL_OBJ);

  TEST_ASSERT_EQUAL_INT(1, SCM_ISEQ_OBJS_LENGTH(iseq));
  TEST_ASSERT_EQUAL_INT(0, SCM_ISEQ_OBJS_VEC(iseq)[0]);
}

TEST(iseq, iseq_push_inst_iof)
{
  int actual_opd;
  scm_opcode_t actual_op;
  ssize_t actual_ret_value;
  scm_byte_t *ip;

  actual_ret_value = scm_iseq_push_inst_si(iseq,
                                           SCM_OPCODE_JMP,
                                           128);

  ip = scm_iseq_to_ip(iseq);
  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_IOF(ip, actual_opd);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_JMP, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_JMP, actual_op);
  TEST_ASSERT_EQUAL_INT(128, actual_opd);
}

TEST(iseq, iseq_push_dst)
{
  TEST_ASSERT_EQUAL_INT(0, scm_iseq_push_dst(iseq, 0));
  TEST_ASSERT_EQUAL_INT(0, scm_iseq_dst(iseq, 0));
}

TEST(iseq, iseq_push_dst__sorted)
{
  ssize_t o;

  o = scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_NOP);
  scm_iseq_push_dst(iseq, (size_t)o);
  scm_iseq_push_dst(iseq, 0);

  TEST_ASSERT_EQUAL_INT(0, scm_iseq_dst(iseq, 0));
  TEST_ASSERT_EQUAL_INT(o, scm_iseq_dst(iseq, 1));
}

TEST(iseq, expand_sequence_buffer)
{
  size_t sum;
  ssize_t ret_value;

  for (sum = 0; sum < SCM_ISEQ_DEFAULT_SEQ_SIZE; sum += SCM_INST_SZ_NOP)
    scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_NOP);

  ret_value = scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_NOP);

  TEST_ASSERT_EQUAL_INT(sum + SCM_INST_SZ_NOP, ret_value);
  TEST_ASSERT_TRUE(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);
}

TEST(iseq, expand_index_buffer)
{
  for (size_t sum = 0; sum < SCM_ISEQ_DEFAULT_OBJS_SIZE; sum++)
    scm_iseq_push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);

  scm_iseq_push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);

  TEST_ASSERT_TRUE(SCM_ISEQ_OBJS_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_OBJS_SIZE);
}

TEST(iseq, expand_dsts_buffer)
{
  for (size_t sum = 0;
       sum < SCM_ISEQ_DEFAULT_DSTS_SIZE + SCM_INST_SZ_NOP;
       sum += SCM_INST_SZ_NOP)
    scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_NOP);

  for (size_t i = 0; i < SCM_ISEQ_DEFAULT_DSTS_SIZE; i++)
    scm_iseq_push_dst(iseq, i);

  scm_iseq_push_dst(iseq, SCM_ISEQ_DEFAULT_DSTS_SIZE);

  TEST_ASSERT_TRUE(SCM_ISEQ_DSTS_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_DSTS_SIZE);
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
  scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_PUSH);
  scm_iseq_push_inst_noopd(iseq2, SCM_OPCODE_PUSH);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__noopd__not_equal)
{
  scm_iseq_push_inst_noopd(iseq, SCM_OPCODE_PUSH);
  scm_iseq_push_inst_noopd(iseq2, SCM_OPCODE_NOP);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj__equal)
{
  scm_iseq_push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  scm_iseq_push_inst_obj(iseq2, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj__not_equal)
{
  scm_iseq_push_inst_obj(iseq, SCM_OPCODE_IMMVAL, SCM_NIL_OBJ);
  scm_iseq_push_inst_obj(iseq2, SCM_OPCODE_IMMVAL, SCM_EOF_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj_obj__equal)
{
  scm_iseq_push_inst_obj_obj(iseq, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  scm_iseq_push_inst_obj_obj(iseq2, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__obj_obj__not_equal)
{
  scm_iseq_push_inst_obj_obj(iseq, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_EOF_OBJ);
  scm_iseq_push_inst_obj_obj(iseq2, SCM_OPCODE_GREF, SCM_NIL_OBJ, SCM_NIL_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si__equal)
{
  scm_iseq_push_inst_si(iseq, SCM_OPCODE_CALL, 1);
  scm_iseq_push_inst_si(iseq2, SCM_OPCODE_CALL, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si__not_equal)
{
  scm_iseq_push_inst_si(iseq, SCM_OPCODE_CALL, 1);
  scm_iseq_push_inst_si(iseq2, SCM_OPCODE_CALL, 10);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si__equal)
{
  scm_iseq_push_inst_si_si(iseq, SCM_OPCODE_SREF, 1, 1);
  scm_iseq_push_inst_si_si(iseq2, SCM_OPCODE_SREF, 1, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si__not_equal)
{
  scm_iseq_push_inst_si_si(iseq, SCM_OPCODE_SREF, 1, 1);
  scm_iseq_push_inst_si_si(iseq2, SCM_OPCODE_SREF, 1, -20);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si_obj__equal)
{
  scm_iseq_push_inst_si_si_obj(iseq, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  scm_iseq_push_inst_si_si_obj(iseq2, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__si_si_obj__not_equal)
{
  scm_iseq_push_inst_si_si_obj(iseq, SCM_OPCODE_CLOSE, 1, 1, SCM_NIL_OBJ);
  scm_iseq_push_inst_si_si_obj(iseq2, SCM_OPCODE_CLOSE, 1, 1, SCM_EOF_OBJ);
  test_iseq_eq__not_equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__iof__equal)
{
  scm_iseq_push_inst_iof(iseq, SCM_OPCODE_JMP, 1);
  scm_iseq_push_inst_iof(iseq2, SCM_OPCODE_JMP, 1);
  test_iseq_eq__equal(iseq, iseq2);
}

TEST(iseq, iseq_eq__iof__not_equal)
{
  scm_iseq_push_inst_iof(iseq, SCM_OPCODE_JMP, 1);
  scm_iseq_push_inst_iof(iseq2, SCM_OPCODE_JMP, 100);
  test_iseq_eq__not_equal(iseq, iseq2);
}
