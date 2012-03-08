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
test_scm_iseq_set_word_get_word(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  int rslt = scm_iseq_set_word(iseq, 0, (scm_iword_t)12345);
  scm_iword_t actual = SCM_ISEQ_SEQ(iseq)[0];;

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(12345, actual);
}

void
test_scm_iseq_set_immv_get_immv_1(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.immv1.op = SCM_OPCODE_IMMVAL;
  inst.immv1.imm_idx = SCM_INST_IMMVAL_MAX;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv1.op);
  cut_assert_equal_int(SCM_INST_IMMVAL_MAX, actual.immv1.imm_idx);
}

void
test_scm_iseq_set_immv_get_immv_2(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.immv1.op = SCM_OPCODE_IMMVAL;
  inst.immv1.imm_idx  = 0;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv1.op);
  cut_assert_equal_int(0, actual.immv1.imm_idx);
}

void
test_scm_iseq_set_immv_get_immv_3(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.immv1.op = SCM_OPCODE_IMMVAL;
  inst.immv1.imm_idx  = -1;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv1.op);
  cut_assert_equal_int(-1, actual.immv1.imm_idx);
}

void
test_scm_iseq_set_immv_get_immv_4(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.immv1.op = SCM_OPCODE_IMMVAL;
  inst.immv1.imm_idx  = SCM_INST_IMMVAL_MIN;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv1.op);
  cut_assert_equal_int(SCM_INST_IMMVAL_MIN, actual.immv1.imm_idx);
}

void
test_scm_iseq_set_primv_get_primv_1(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval = SCM_INST_PRIMVAL_MAX;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(SCM_INST_PRIMVAL_MAX, actual.primv.primval);
}

void
test_scm_iseq_set_primv_get_primv_2(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = 0;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(0, actual.primv.primval);
}

void
test_scm_iseq_set_primv_get_primv_3(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = -1;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(-1, actual.primv.primval);
}

void
test_scm_iseq_set_primv_get_primv_4(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = SCM_INST_PRIMVAL_MIN;

  int rslt = scm_iseq_set_word(iseq, 0, inst.iword);
  scm_inst_t actual;
  actual.iword = SCM_ISEQ_SEQ(iseq)[0];

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(SCM_INST_PRIMVAL_MIN, actual.primv.primval);
}

void
test_scm_iseq__size_of_scm_inst_t_should_be_equal_to_scm_iword_t(void)
{
  cut_assert_equal_uint(sizeof(scm_iword_t), sizeof(scm_inst_t));
}

void
test_scm_iseq__expand_sequence_buffer(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  size_t idx;
  for (idx = 0; idx < SCM_ISEQ_DEFAULT_SEQ_SIZE; idx++) {
    int r = scm_iseq_set_word(iseq, idx, (scm_iword_t)idx);
    cut_assert_equal_int(0, r);
  }

  /* action */
  int rslt = scm_iseq_set_word(iseq, idx, (scm_iword_t)9999);

  /* postcondition check */
  cut_assert_equal_int(0, rslt);
  cut_assert_true(SCM_ISEQ_SEQ_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_SEQ_SIZE);

  scm_iword_t actual;
  actual = SCM_ISEQ_SEQ(iseq)[idx];
  cut_assert_equal_uint(9999, actual);

  for (unsigned int i = 0; i < SCM_ISEQ_DEFAULT_SEQ_SIZE; i++) {
    cut_assert_equal_uint(i, SCM_ISEQ_SEQ(iseq)[i]);
  }
}

void
test_scm_iseq_set_immval_get_immval(void)
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
  int idx1 = scm_iseq_set_immval(iseq, val1);
  int idx2 = scm_iseq_set_immval(iseq, val2);
  actual1 = scm_iseq_get_immval(iseq, idx1);
  actual2 = scm_iseq_get_immval(iseq, idx2);

  /* postcondition check */
  cut_assert_true(idx1 >= 0);
  cut_assert_true(idx2 >= 0);
  cut_assert_true(idx1 != idx2);
  cut_assert_true(scm_obj_same_instance_p(val1, actual1));
  cut_assert_true(scm_obj_same_instance_p(val2, actual2));
}

void
test_scm_iseq_update_immval__update_successed(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual1 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val1, &val2, &actual1);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val1 = scm_vm_nil_instance();
  val2 = scm_vm_eof_instance();
  int idx1 = scm_iseq_set_immval(iseq, val1);

  /* action */
  int idx2 = scm_iseq_update_immval(iseq, idx1, val2);
  actual1 = scm_iseq_get_immval(iseq, idx1);

  /* postcondition check */
  cut_assert_true(idx1 >= 0);
  cut_assert_true(idx2 >= 0);
  cut_assert_equal_int(idx1, idx2);
  cut_assert_true(scm_obj_same_instance_p(val2, actual1));
}

void
test_scm_iseq_update_immval__update_not_assigned_idx(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual1 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &val1, &val2, &actual1);

  /* preprocess */
  iseq = scm_iseq_new(SCM_MEM_HEAP);

  val1 = scm_vm_nil_instance();
  val2 = scm_vm_eof_instance();
  int idx1 = scm_iseq_set_immval(iseq, val1);

  /* action */
  int idx2 = scm_iseq_update_immval(iseq, idx1 + 1, val2);
  actual1 = scm_iseq_get_immval(iseq, idx1);

  /* postcondition check */
  cut_assert_true(idx1 >= 0);
  cut_assert_true(idx2 != 0);
  cut_assert_true(scm_obj_same_instance_p(val1, actual1));
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
    int r = scm_iseq_set_immval(iseq, val);
    cut_assert_true(r >= 0);
  }

  /* action */
  int rslt = scm_iseq_set_immval(iseq, val);

  /* postcondition check */
  cut_assert_true(rslt >= 0);
  cut_assert_true(SCM_ISEQ_VEC_CAPACITY(iseq) > SCM_ISEQ_DEFAULT_IMMVS_SIZE);
  actual = scm_iseq_get_immval(iseq, rslt);
    cut_assert_true(scm_obj_same_instance_p(val, actual));

  for (int i; i < SCM_ISEQ_DEFAULT_IMMVS_SIZE; i++) {
    actual = scm_iseq_get_immval(iseq, i);
    cut_assert_true(scm_obj_same_instance_p(val, actual));
  }
}


void
test_scm_iseq_list_to_iseq(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj port = SCM_OBJ_INIT;
  const char *str =
    "((nop)(stop)(call)(return)(frame)(push)(gref vvv)"
    "(gdef vvv)(gset vvv)(immval vvv)(push_primval 123)"
    "(label lbl)(jmp lbl)(asm ((nop))))";
  const uint8_t expected_codes[] = { SCM_OPCODE_NOP, SCM_OPCODE_STOP,
                                     SCM_OPCODE_CALL, SCM_OPCODE_RETURN,
                                     SCM_OPCODE_FRAME, SCM_OPCODE_PUSH,
                                     SCM_OPCODE_GREF, SCM_OPCODE_GDEF,
                                     SCM_OPCODE_GSET, SCM_OPCODE_IMMVAL,
                                     SCM_OPCODE_PUSH_PRIMVAL, SCM_OPCODE_JMP,
                                     SCM_OPCODE_IMMVAL };
  ScmObj actual_immv = SCM_OBJ_INIT;
  ScmObj expected_immv = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq, &lst, &port, &actual_immv, &expected_immv);

  expected_immv = scm_capi_make_symbol_from_cstr("vvv", SCM_ENC_ASCII);
  port = scm_capi_open_input_string_port_from_cstr(str, SCM_ENC_ASCII);
  lst = scm_api_read(port);

  iseq = scm_iseq_list_to_iseq(lst);

  cut_assert_true(scm_obj_not_null_p(iseq));
  cut_assert_true(scm_obj_type_p(iseq, &SCM_ISEQ_TYPE_INFO));

  for (size_t i = 0; i < sizeof(expected_codes)/sizeof(expected_codes[0]); i++) {
    scm_inst_t actual;
    int rslt;

    rslt = scm_iseq_get_word(iseq, i, &actual.iword);

    cut_assert_equal_int(0, rslt);
    cut_assert_equal_int(expected_codes[i], actual.plain.op);

    switch (actual.plain.op) {
    case SCM_OPCODE_GREF:
    case SCM_OPCODE_GDEF:
    case SCM_OPCODE_GSET:
      actual_immv = scm_iseq_get_immval(iseq, actual.immv1.imm_idx);
      cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      break;
    case SCM_OPCODE_IMMVAL:
      actual_immv = scm_iseq_get_immval(iseq, actual.immv1.imm_idx);
      if (i == 9)
        cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      else if (i == 12)
        cut_assert_true(scm_obj_type_p(actual_immv, &SCM_ISEQ_TYPE_INFO));
      else
        cut_assert(false);
      break;
    case SCM_OPCODE_PUSH_PRIMVAL:
      cut_assert_equal_int(123, actual.primv.primval);
      break;
    case SCM_OPCODE_JMP:
      cut_assert_equal_int(-1, actual.primv.primval);
      break;
    }
  }
}
