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
  scm_capi_setup_current_vm(ev);
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
    "((nop)(stop)(call 5)(return 0)(frame)(push)(gref vvv)"
    "(gdef vvv)(gset vvv)(immval vvv)"
    "(label lbl)(jmp lbl)(asm ((nop))))";
  const uint8_t expected_codes[] = { SCM_OPCODE_NOP, SCM_OPCODE_STOP,
                                     SCM_OPCODE_CALL, SCM_OPCODE_RETURN,
                                     SCM_OPCODE_FRAME, SCM_OPCODE_PUSH,
                                     SCM_OPCODE_GREF, SCM_OPCODE_GDEF,
                                     SCM_OPCODE_GSET, SCM_OPCODE_IMMVAL,
                                     SCM_OPCODE_JMP, SCM_OPCODE_IMMVAL };
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
    uint32_t actual_arg;

    SCM_CAPI_INST_FETCH_OP(ip, actual_op);

    cut_assert_equal_int(expected_codes[i], actual_op);

    switch (actual_op) {
    case SCM_OPCODE_GREF:
    case SCM_OPCODE_GDEF:
    case SCM_OPCODE_GSET:
      SCM_CAPI_INST_FETCH_UINT32(ip, actual_arg);
      actual_immv = scm_capi_iseq_ref_immval(iseq, actual_arg);
      cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      break;
    case SCM_OPCODE_IMMVAL:
      SCM_CAPI_INST_FETCH_UINT32(ip, actual_arg);
      actual_immv = scm_capi_iseq_ref_immval(iseq, actual_arg);
      if (i == 9)
        cut_assert_true(scm_capi_eq_p(expected_immv, actual_immv));
      else if (i == 11)
        cut_assert_true(scm_capi_iseq_p(actual_immv));
      else
        cut_assert(false);
      break;
    case SCM_OPCODE_CALL:
      SCM_CAPI_INST_FETCH_UINT32(ip, actual_arg);
      cut_assert_equal_uint(5, actual_arg);
      break;
    case SCM_OPCODE_RETURN:
      SCM_CAPI_INST_FETCH_UINT32(ip, actual_arg);
      cut_assert_equal_uint(0, actual_arg);
      break;
    case SCM_OPCODE_JMP:
      SCM_CAPI_INST_FETCH_UINT32(ip, actual_arg);
      cut_assert_equal_int(-6, (int32_t)actual_arg);
      break;
    }
  }
}
