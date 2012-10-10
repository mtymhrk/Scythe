#include <cutter.h>
#include <assert.h>

#include "vm.h"
#include "api.h"
#include "assembler.h"

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
test_scm_asm_assemble(void)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj lst = SCM_OBJ_INIT;
  ScmObj port = SCM_OBJ_INIT;
  const char *str =
    "((nop)(halt)(undef)(call 5)(tcall 2)(apply)(tapply)"
    "(return)(frame)(cframe)(eframe)"
    "(ecommit 20)(epop)(erebind 21)(push)(mvpush)"
    "(gref vvv)(gdef vvv)(gset vvv)(sref -4 12)(sset -6 13)"
    "(immval vvv)(label lbl)(jmp lbl)(jmpt lbl)(jmpf lbl)(asm ((nop)))(raise)"
    "(box -8 14)(close 10 20 vvv)(asm-close 11 21 ((nop)))"
    "(demine 15 16)(emine 17)(edemine 18 19)(arity 22))";
  const uint8_t expected_codes[] = { SCM_OPCODE_NOP, SCM_OPCODE_HALT,
                                     SCM_OPCODE_UNDEF, SCM_OPCODE_CALL,
                                     SCM_OPCODE_TAIL_CALL, SCM_OPCODE_APPLY,
                                     SCM_OPCODE_TAIL_APPLY, SCM_OPCODE_RETURN,
                                     SCM_OPCODE_FRAME, SCM_OPCODE_CFRAME,
                                     SCM_OPCODE_EFRAME, SCM_OPCODE_ECOMMIT,
                                     SCM_OPCODE_EPOP, SCM_OPCODE_EREBIND,
                                     SCM_OPCODE_PUSH, SCM_OPCODE_MVPUSH,
                                     SCM_OPCODE_GREF, SCM_OPCODE_GDEF,
                                     SCM_OPCODE_GSET, SCM_OPCODE_SREF,
                                     SCM_OPCODE_SSET, SCM_OPCODE_IMMVAL,
                                     SCM_OPCODE_JMP,  SCM_OPCODE_JMPT,
                                     SCM_OPCODE_JMPF, SCM_OPCODE_IMMVAL,
                                     SCM_OPCODE_RAISE, SCM_OPCODE_BOX,
                                     SCM_OPCODE_CLOSE, SCM_OPCODE_CLOSE,
                                     SCM_OPCODE_DEMINE, SCM_OPCODE_EMINE,
                                     SCM_OPCODE_EDEMINE, SCM_OPCODE_ARITY };
  ScmObj actual_immv = SCM_OBJ_INIT;
  ScmObj expected_immv = SCM_OBJ_INIT;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&iseq, &lst, &port, &actual_immv, &expected_immv);

  expected_immv = scm_capi_make_symbol_from_cstr("vvv", SCM_ENC_ASCII);
  port = scm_capi_open_input_string_from_cstr(str, SCM_ENC_ASCII);
  lst = scm_api_read(port);

  iseq = scm_asm_assemble(lst);

  cut_assert_false(scm_capi_null_value_p(iseq));
  cut_assert_true(scm_capi_iseq_p(iseq));

  ip = scm_capi_iseq_to_ip(iseq);

  for (size_t i = 0; i < sizeof(expected_codes)/sizeof(expected_codes[0]); i++) {
    uint8_t actual_op;
    int32_t actual_arg1, actual_arg2;;

    SCM_CAPI_INST_FETCH_OP(ip, actual_op);
    cut_assert_equal_int(expected_codes[i], actual_op);

    switch (actual_op) {
    case SCM_OPCODE_GREF:
    case SCM_OPCODE_GDEF:
    case SCM_OPCODE_GSET:
      ip = scm_capi_inst_fetch_oprand_obj(ip, SCM_CSETTER_L(actual_immv));
      cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      break;
    case SCM_OPCODE_IMMVAL:
      ip = scm_capi_inst_fetch_oprand_obj(ip, SCM_CSETTER_L(actual_immv));
      if (i == 21)
        cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      else if (i == 25)
        cut_assert_true(scm_capi_iseq_p(actual_immv));
      else
        cut_assert(false);
      break;
    case SCM_OPCODE_CALL:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(5, actual_arg1);
      break;
    case SCM_OPCODE_TAIL_CALL:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(2, actual_arg1);
      break;
    case SCM_OPCODE_ECOMMIT:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(20, actual_arg1);
      break;
    case SCM_OPCODE_EREBIND:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(21, actual_arg1);
      break;
    case SCM_OPCODE_SREF:
      ip = scm_capi_inst_fetch_oprand_si_si(ip, &actual_arg1, &actual_arg2);
      cut_assert_equal_int(-4, actual_arg1);
      cut_assert_equal_int(12, actual_arg2);
      break;
    case SCM_OPCODE_SSET:
      ip = scm_capi_inst_fetch_oprand_si_si(ip, &actual_arg1, &actual_arg2);
      cut_assert_equal_int(-6, actual_arg1);
      cut_assert_equal_int(13, actual_arg2);
      break;
    case SCM_OPCODE_BOX:
      ip = scm_capi_inst_fetch_oprand_si_si(ip, &actual_arg1, &actual_arg2);
      cut_assert_equal_int(-8, actual_arg1);
      cut_assert_equal_int(14, actual_arg2);
      break;
    case SCM_OPCODE_JMP:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(-6, actual_arg1);
      break;
    case SCM_OPCODE_JMPT:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(-12, actual_arg1);
      break;
    case SCM_OPCODE_JMPF:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(-18, actual_arg1);
      break;
    case SCM_OPCODE_CLOSE:
      ip = scm_capi_inst_fetch_oprand_si_si_obj(ip,
                                                &actual_arg1,
                                                &actual_arg2,
                                                SCM_CSETTER_L(actual_immv));
      if (i == 31) {
        cut_assert_equal_int(10, actual_arg1);
        cut_assert_equal_int(20, actual_arg2);
        cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      }
      else if (i == 32) {
        cut_assert_equal_int(11, actual_arg1);
        cut_assert_equal_int(21, actual_arg2);
        cut_assert_true(scm_capi_iseq_p(actual_immv));
      }
      break;
    case SCM_OPCODE_DEMINE:
      ip = scm_capi_inst_fetch_oprand_si_si(ip, &actual_arg1, &actual_arg2);
      cut_assert_equal_int(15, actual_arg1);
      cut_assert_equal_int(16, actual_arg2);
      break;
    case SCM_OPCODE_EMINE:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(17, actual_arg1);
      break;
    case SCM_OPCODE_EDEMINE:
      ip = scm_capi_inst_fetch_oprand_si_si(ip, &actual_arg1, &actual_arg2);
      cut_assert_equal_int(18, actual_arg1);
      cut_assert_equal_int(19, actual_arg2);
      break;
    case SCM_OPCODE_ARITY:
      ip = scm_capi_inst_fetch_oprand_si(ip, &actual_arg1);
      cut_assert_equal_int(22, actual_arg1);
      break;
    }
  }
}
