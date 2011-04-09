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
test_scm_iseq_set_word_get_word(void)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), (scm_iword_t)12345);
  scm_iword_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(12345, actual);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_immv_get_immv_1(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.immv.op = SCM_OPCODE_IMMVAL;
  inst.immv.imm_idx = SCM_INST_IMMVAL_MAX;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv.op);
  cut_assert_equal_int(SCM_INST_IMMVAL_MAX, actual.immv.imm_idx);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_immv_get_immv_2(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.immv.op = SCM_OPCODE_IMMVAL;
  inst.immv.imm_idx  = 0;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv.op);
  cut_assert_equal_int(0, actual.immv.imm_idx);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_immv_get_immv_3(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.immv.op = SCM_OPCODE_IMMVAL;
  inst.immv.imm_idx  = -1;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv.op);
  cut_assert_equal_int(-1, actual.immv.imm_idx);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_immv_get_immv_4(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.immv.op = SCM_OPCODE_IMMVAL;
  inst.immv.imm_idx  = SCM_INST_IMMVAL_MIN;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_IMMVAL, actual.immv.op);
  cut_assert_equal_int(SCM_INST_IMMVAL_MIN, actual.immv.imm_idx);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_primv_get_primv_1(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval = SCM_INST_PRIMVAL_MAX;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(SCM_INST_PRIMVAL_MAX, actual.primv.primval);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_primv_get_primv_2(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = 0;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(0, actual.primv.primval);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_primv_get_primv_3(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = -1;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(-1, actual.primv.primval);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq_set_primv_get_primv_4(void)
{
 ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  /* preprocess */
  SCM_SETQ(iseq, scm_iseq_new(SCM_MEM_ALLOC_HEAP));

  /* action */
  scm_inst_t inst;
  inst.primv.op = SCM_OPCODE_PUSH_PRIMVAL;
  inst.primv.primval  = SCM_INST_PRIMVAL_MIN;

  scm_iword_t *next_write =
    scm_iseq_set_word(iseq, SCM_ISEQ_SEQ(iseq), inst.iword);
  scm_inst_t actual;
  scm_iword_t *next_read = scm_iseq_get_word(SCM_ISEQ_SEQ(iseq), &actual.iword);

  /* postcondition check */
  cut_assert_not_null(next_write);
  cut_assert_equal_int(1, next_write - SCM_ISEQ_SEQ(iseq));
  cut_assert_equal_uint(1, SCM_ISEQ_SEQ_LENGTH(iseq));
  cut_assert_equal_uint(SCM_OPCODE_PUSH_PRIMVAL, actual.primv.op);
  cut_assert_equal_int(SCM_INST_PRIMVAL_MIN, actual.primv.primval);
  cut_assert_equal_pointer(next_read, next_write);
}

void
test_scm_iseq__size_of_scm_inst_t_should_be_equal_to_scm_iword_t(void)
{
  cut_assert_equal_uint(sizeof(scm_iword_t), sizeof(scm_inst_t));
}
