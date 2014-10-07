#include "iseq.c"

#include "scythe/api.h"

#include "test.h"

TEST_GROUP(iseq);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;
static ScmObj iseq;

TEST_SETUP(iseq)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_ref_stack_save(&rsi);

  iseq = SCM_OBJ_NULL;
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(iseq));
  iseq = scm_iseq_new(SCM_MEM_HEAP);
}

TEST_TEAR_DOWN(iseq)
{
  scm_capi_ref_stack_restore(&rsi);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);

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
  SCM_VMINST_FETCH_OP(ip, actual_op);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);
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
  SCM_VMINST_FETCH_OP(ip, actual_op);
  SCM_VMINST_FETCH_OPD_IOF(ip, actual_opd);

  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_JMP, actual_ret_value);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_JMP, actual_op);
  TEST_ASSERT_EQUAL_INT(128, actual_opd);
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




