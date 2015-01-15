#include "fcd_assembler.c"

#include "scythe/object.h"
#include "scythe/fcd.h"

#include "test.h"

TEST_GROUP(fcd_assembler);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(fcd_assembler)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(fcd_assembler)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static ScmObj
make_iseq(const char *asmbl)
{
  ScmObj iseq = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq, &lst);

  lst = read_cstr(asmbl);

  iseq = scm_fcd_assemble(lst, SCM_OBJ_NULL);
  TEST_ASSERT_TRUE(scm_fcd_iseq_p(iseq));

  return iseq;
}

static void
check_following(scm_byte_t *ip)
{
  int op;

  op = SCM_VMINST_GET_OP(ip);
  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_NOP, op);
}

static void
test_assemble_noopd(const char *asmbl, uint8_t code)
{
  ScmObj iseq;
  scm_byte_t *ip;
  int actual_op;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_NOOPD(ip);

  TEST_ASSERT_EQUAL_INT(code, actual_op);

  check_following(ip);
}

static void
test_assemble_obj(const char *asmbl, uint8_t code, ScmObj obj)
{
  ScmObj iseq = SCM_OBJ_INIT, actual_immv = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int actual_op;

  SCM_REFSTK_INIT_REG(&obj,
                      &iseq, &actual_immv);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_OBJ(ip, actual_immv);

  TEST_ASSERT_EQUAL_INT(code, actual_op);
  TEST_ASSERT_SCM_EQUAL(obj, actual_immv);

  check_following(ip);
}

static void
test_assemble_obj_obj(const char *asmbl, uint8_t code, ScmObj obj1, ScmObj obj2)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj actual_immv1 = SCM_OBJ_INIT, actual_immv2 = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int actual_op;

  SCM_REFSTK_INIT_REG(&obj1, &obj2,
                      &iseq, &actual_immv1, &actual_immv2);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip, actual_immv1, actual_immv2);

  TEST_ASSERT_EQUAL_INT(code, actual_op);
  TEST_ASSERT_SCM_EQUAL(obj1, actual_immv1);
  TEST_ASSERT_SCM_EQUAL(obj2, actual_immv2);

  check_following(ip);
}

static void
test_assemble_si(const char *asmbl, uint8_t code, int si)
{
  ScmObj iseq = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int actual_op, actual_si;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI(ip, actual_si);

  TEST_ASSERT_EQUAL_INT(code, actual_op);
  TEST_ASSERT_EQUAL_INT(si, actual_si);

  check_following(ip);
}

static void
test_assemble_si_si(const char *asmbl, uint8_t code, int si1, int si2)
{
  ScmObj iseq = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int actual_op, actual_si1, actual_si2;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI_SI(ip, actual_si1, actual_si2);

  TEST_ASSERT_EQUAL_INT(code, actual_op);
  TEST_ASSERT_EQUAL_INT(si1, actual_si1);
  TEST_ASSERT_EQUAL_INT(si2, actual_si2);

  check_following(ip);
}

static void
test_assemble_iof(const char *asmbl, uint8_t code, int si, size_t dst)
{
  ScmObj iseq = SCM_OBJ_INIT;
  scm_byte_t *ip;
  const size_t *dsts;
  int actual_op, actual_si;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = make_iseq(asmbl);
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_IOF(ip, actual_si);

  TEST_ASSERT_EQUAL_INT(code, actual_op);
  TEST_ASSERT_EQUAL_INT(si, actual_si);

  dsts = scm_fcd_iseq_br_dsts(iseq);
  TEST_ASSERT_EQUAL_INT(dst, dsts[0]);

  check_following(ip);
}

TEST(fcd_assembler, nop)
{
  test_assemble_noopd("((nop)(nop))", SCM_OPCODE_NOP);
}

TEST(fcd_assembler, halt)
{
  test_assemble_noopd("((halt)(nop))", SCM_OPCODE_HALT);
}

TEST(fcd_assembler, undef)
{
  test_assemble_noopd("((undef)(nop))", SCM_OPCODE_UNDEF);
}

TEST(fcd_assembler, uninit)
{
  test_assemble_noopd("((uninit)(nop))", SCM_OPCODE_UNINIT);
}

TEST(fcd_assembler, cframe)
{
  test_assemble_si("((cframe (label 0))(nop)(label 0))",
                   SCM_OPCODE_CFRAME, SCM_INST_SZ_NOP);
}

TEST(fcd_assembler, eframe)
{
  test_assemble_si("((eframe 20)(nop))", SCM_OPCODE_EFRAME, 20);
}

TEST(fcd_assembler, epop)
{
  test_assemble_noopd("((epop)(nop))", SCM_OPCODE_EPOP);
}

TEST(fcd_assembler, eshift)
{
  test_assemble_si("((eshift -1)(nop))", SCM_OPCODE_ESHIFT, -1);
}

TEST(fcd_assembler, push)
{
  test_assemble_noopd("((push)(nop))", SCM_OPCODE_PUSH);
}

TEST(fcd_assembler, mvpush)
{
  test_assemble_noopd("((mvpush)(nop))", SCM_OPCODE_MVPUSH);
}

TEST(fcd_assembler, return)
{
  test_assemble_noopd("((return)(nop))", SCM_OPCODE_RETURN);
}

TEST(fcd_assembler, pcall)
{
  test_assemble_si("((pcall 5)(nop))", SCM_OPCODE_PCALL, 5);
}

TEST(fcd_assembler, call)
{
  test_assemble_si("((call 5)(nop))", SCM_OPCODE_CALL, 5);
}

TEST(fcd_assembler, tcall)
{
  test_assemble_si("((tcall 5)(nop))", SCM_OPCODE_TAIL_CALL, 5);
}

TEST(fcd_assembler, gref)
{
  ScmObj obj1 = SCM_OBJ_INIT, obj2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj1, &obj2);

  obj1 = read_cstr("vvv");
  obj2 = read_cstr("xxx");

  test_assemble_obj_obj("((gref vvv xxx)(nop))",
                        SCM_OPCODE_GREF, obj1, obj2);
}

TEST(fcd_assembler, gdef)
{
  ScmObj obj1 = SCM_OBJ_INIT, obj2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj1, &obj2);

  obj1 = read_cstr("vvv");
  obj2 = read_cstr("xxx");

  test_assemble_obj_obj("((gdef vvv xxx)(nop))",
                        SCM_OPCODE_GDEF, obj1, obj2);
}

TEST(fcd_assembler, gset)
{
  ScmObj obj1 = SCM_OBJ_INIT, obj2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj1, &obj2);

  obj1 = read_cstr("vvv");
  obj2 = read_cstr("xxx");

  test_assemble_obj_obj("((gset vvv xxx)(nop))",
                        SCM_OPCODE_GSET, obj1, obj2);
}

TEST(fcd_assembler, sref)
{
  test_assemble_si_si("((sref -4 12)(nop))", SCM_OPCODE_SREF, -4, 12);
}

TEST(fcd_assembler, sset)
{
  test_assemble_si_si("((sset -6 13)(nop))", SCM_OPCODE_SSET, -6, 13);
}

TEST(fcd_assembler, immval)
{
  ScmObj obj = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj);

  obj = read_cstr("aaa");

  test_assemble_obj("((immval aaa)(nop))", SCM_OPCODE_IMMVAL, obj) ;
}

TEST(fcd_assembler, label_jmp)
{
  test_assemble_iof("((label 0)(jmp (label 0))(nop))",
                    SCM_OPCODE_JMP, -(int)SCM_INST_SZ_JMP,
                    0);
}

TEST(fcd_assembler, label_jmpt)
{
  test_assemble_iof("((label 0)(jmpt (label 0))(nop))",
                    SCM_OPCODE_JMPT, -(int)SCM_INST_SZ_JMPT,
                    0);
}

TEST(fcd_assembler, label_jmpf)
{
  test_assemble_iof("((label 0)(jmpf (label 0))(nop))",
                    SCM_OPCODE_JMPF, -(int)SCM_INST_SZ_JMPF,
                    0);
}

TEST(fcd_assembler, box)
{
  test_assemble_si_si("((box -8 14)(nop))", SCM_OPCODE_BOX, -8, 14);
}

TEST(fcd_assembler, close)
{
  ScmObj iseq = SCM_OBJ_INIT, actual_immv = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int actual_op, actual_si1, actual_si2;

  SCM_REFSTK_INIT_REG(&iseq, &actual_immv);

  iseq = make_iseq("((close 11 21 ((nop)))(nop))");
  ip = scm_fcd_iseq_to_ip(iseq);

  actual_op = SCM_VMINST_GET_OP(ip);
  SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip, actual_si1, actual_si2, actual_immv);

  TEST_ASSERT_EQUAL_INT(SCM_OPCODE_CLOSE, actual_op);
  TEST_ASSERT_EQUAL_INT(11, actual_si1);
  TEST_ASSERT_EQUAL_INT(21, actual_si2);
  TEST_ASSERT_TRUE(scm_fcd_iseq_p(actual_immv));

  check_following(ip);
}

TEST(fcd_assembler, demine)
{
  test_assemble_si_si("((demine 15 16)(nop))", SCM_OPCODE_DEMINE, 15, 16);
}

TEST(fcd_assembler, emine)
{
  test_assemble_si("((emine 17)(nop))", SCM_OPCODE_EMINE, 17);
}

TEST(fcd_assembler, edemine)
{
  test_assemble_si_si("((edemine 18 19)(nop))", SCM_OPCODE_EDEMINE, 18, 19);
}

TEST(fcd_assembler, mrvc)
{
  test_assemble_si("((mrvc 5)(nop))", SCM_OPCODE_MRVC, 5);
}

TEST(fcd_assembler, mrve)
{
  test_assemble_noopd("((mrve)(nop))", SCM_OPCODE_MRVE);
}

TEST(fcd_assembler, mrve_size)
{
  TEST_ASSERT_EQUAL_INT(SCM_INST_SZ_NOP, SCM_INST_SZ_MRVE);
}
