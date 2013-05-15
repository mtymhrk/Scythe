#include <stdio.h>
#include <stdbool.h>

#include "object.h"
#include "api.h"
#include "assembler.h"
#include "compiler.h"

ScmTypeInfo SCM_COMPILER_TYPE_INFO = {
  .name                            = "compiler",
  .flags                           = SCM_TYPE_FLG_MMO,
  .pp_func                         = scm_cmpl_pretty_print,
  .obj_size                        = sizeof(ScmCompiler),
  .gc_ini_func                     = scm_cmpl_gc_initialize,
  .gc_fin_func                     = NULL,
  .gc_accept_func                  = scm_cmpl_gc_accept,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

int
scm_cmpl_initialize(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(cmpl)->label_id = 0;
  SCM_COMPILER(cmpl)->module = SCM_OBJ_NULL;

  return 0;
}

ScmObj
scm_cmpl_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj cmpl = SCM_OBJ_INIT;

  cmpl = scm_capi_mem_alloc(&SCM_COMPILER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cmpl)) return SCM_OBJ_NULL;

  if (scm_cmpl_initialize(cmpl) < 0)
    return SCM_OBJ_NULL;

  return cmpl;
}

void
scm_cmpl_select_module(ScmObj cmpl, ScmObj module)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_capi_module_p(module));

  SCM_SLOT_SETQ(ScmCompiler, cmpl, module, module);
}

ScmObj
scm_cmpl_compile(ScmObj cmpl, ScmObj exp)
{
  ScmObj env = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  ssize_t rdepth;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp,
                       &env, &next);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  env = scm_cmpl_env_new();
  if (scm_obj_null_p(env)) return SCM_OBJ_NULL;

  next = SCM_NIL_OBJ;
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(cmpl, exp, env, next, 1, false, true, &rdepth);
}

int
scm_cmpl_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char str[256];
  int rslt;

  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);

  snprintf(str, sizeof(str), "#<%s %p>", scm_obj_type_name(obj), (void *)obj);

  rslt = scm_capi_write_cstr(str, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(obj)->module = SCM_OBJ_NULL;
}

int
scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->module, mem);
}

#ifdef SCM_UNIT_TEST

void
scm_cmpl_ut_clear_label_id(void)
{
}

#endif

/**************************************************************************/
/* Compiler Framework and Helpers                                         */
/**************************************************************************/

static ScmObj
scm_cmpl_cons_inst_noopd(SCM_OPCODE_T op)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}

static ScmObj
scm_cmpl_cons_inst_obj(SCM_OPCODE_T op, ScmObj obj)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj,
                       &mne);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, obj);
}

static ScmObj
scm_cmpl_cons_inst_obj_obj(SCM_OPCODE_T op, ScmObj obj1, ScmObj obj2)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj1, &obj2,
                       &mne);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(3, mne, obj1, obj2);
}

static ScmObj
scm_cmpl_cons_inst_si(SCM_OPCODE_T op, scm_sword_t n)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(n);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_si_si(SCM_OPCODE_T op, scm_sword_t n1, scm_sword_t n2)
{
  ScmObj mne = SCM_OBJ_INIT, num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num1, &num2);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num1 = scm_capi_make_number_from_sword(n1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_capi_make_number_from_sword(n2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_capi_list(3, mne, num1, num2);
}

static ScmObj
scm_cmpl_cons_inst_si_si_obj(SCM_OPCODE_T op,
                             scm_sword_t n1, scm_sword_t n2, ScmObj obj)
{
  ScmObj mne = SCM_OBJ_INIT, num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj,
                       &mne, &num1, &num2);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num1 = scm_capi_make_number_from_sword(n1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_capi_make_number_from_sword(n2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_capi_list(4, mne, num1, num2, obj);
}

ScmObj
scm_cmpl_cons_inst_nop(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_NOP);
}

ScmObj
scm_cmpl_push_inst_nop(ScmObj next)
{
  ScmObj inst_nop = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_nop);

  inst_nop = scm_cmpl_cons_inst_nop();
  if (scm_obj_null_p(inst_nop)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_nop, next);
}

ScmObj
scm_cmpl_cons_inst_undef(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_UNDEF);
}

ScmObj
scm_cmpl_push_inst_undef(ScmObj next)
{
  ScmObj inst_undef = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_undef);

  inst_undef = scm_cmpl_cons_inst_undef();
  if (scm_obj_null_p(inst_undef)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_undef, next);
}

ScmObj
scm_cmpl_cons_inst_call(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_CALL, narg);
}

ScmObj
scm_cmpl_push_inst_call(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_call = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_call);

  inst_call = scm_cmpl_cons_inst_call(narg);
  if (scm_obj_null_p(inst_call)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_call, next);
}

ScmObj
scm_cmpl_cons_inst_tcall(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_TAIL_CALL, narg);
}

ScmObj
scm_cmpl_push_inst_tcall(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_tcall = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_tcall);

  inst_tcall = scm_cmpl_cons_inst_tcall(narg);
  if (scm_obj_null_p(inst_tcall)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_tcall, next);
}

ScmObj
scm_cmpl_cons_inst_return(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_RETURN);
}

ScmObj
scm_cmpl_push_inst_return(ScmObj next)
{
  ScmObj inst_ret = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_ret);

  inst_ret = scm_cmpl_cons_inst_return();
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_ret, next);
}

ScmObj
scm_cmpl_cons_inst_frame(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_FRAME);
}

ScmObj
scm_cmpl_push_inst_frame(ScmObj next)
{
  ScmObj inst_frame = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_frame);

  inst_frame = scm_cmpl_cons_inst_frame();
  if (scm_obj_null_p(inst_frame)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_frame, next);
}

ScmObj
scm_cmpl_cons_inst_cframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_CFRAME);
}

ScmObj
scm_cmpl_push_inst_cframe(ScmObj next)
{
  ScmObj inst_cframe = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_cframe);

  inst_cframe = scm_cmpl_cons_inst_cframe();
  if (scm_obj_null_p(inst_cframe)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_cframe, next);
}

ScmObj
scm_cmpl_cons_inst_eframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_EFRAME);
}

ScmObj
scm_cmpl_push_inst_eframe(ScmObj next)
{
  ScmObj inst_eframe = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_eframe);

  inst_eframe = scm_cmpl_cons_inst_eframe();
  if (scm_obj_null_p(inst_eframe)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_eframe, next);
}

ScmObj
scm_cmpl_cons_inst_ecommit(scm_sword_t narg)
{
  return scm_cmpl_cons_inst_si(SCM_OPCODE_ECOMMIT, narg);
}

ScmObj
scm_cmpl_push_inst_ecommit(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_ecommit = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_ecommit);

  inst_ecommit = scm_cmpl_cons_inst_ecommit(narg);
  if (scm_obj_null_p(inst_ecommit)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_ecommit, next);
}

ScmObj
scm_cmpl_cons_inst_epop(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_EPOP);
}

ScmObj
scm_cmpl_push_inst_epop(ScmObj next)
{
  ScmObj inst_epop = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_epop);

  inst_epop = scm_cmpl_cons_inst_epop();
  if (scm_obj_null_p(inst_epop)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_epop, next);
}

ScmObj
scm_cmpl_cons_inst_erebind(scm_sword_t narg)
{
  return scm_cmpl_cons_inst_si(SCM_OPCODE_EREBIND, narg);
}

ScmObj
scm_cmpl_push_inst_erebind(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_erebind = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_erebind);

  inst_erebind = scm_cmpl_cons_inst_erebind(narg);
  if (scm_obj_null_p(inst_erebind)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_erebind, next);
}

ScmObj
scm_cmpl_cons_inst_immval(ScmObj val)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_IMMVAL, val);
}

ScmObj
scm_cmpl_push_inst_immval(ScmObj val, ScmObj next)
{
  ScmObj inst_immval = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val, &next,
                       &inst_immval);

  inst_immval = scm_cmpl_cons_inst_immval(val);
  if (scm_obj_null_p(inst_immval)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_immval, next);
}

ScmObj
scm_cmpl_cons_inst_push(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_PUSH);
}

ScmObj
scm_cmpl_push_inst_push(ScmObj next)
{
  ScmObj inst_push = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_push);

  inst_push = scm_cmpl_cons_inst_push();
  if (scm_obj_null_p(inst_push)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_push, next);
}

ScmObj
scm_cmpl_cons_inst_gref(ScmObj sym, ScmObj module)
{
  return scm_cmpl_cons_inst_obj_obj(SCM_OPCODE_GREF, sym, module);
}

ScmObj
scm_cmpl_push_inst_gref(ScmObj sym, ScmObj module, ScmObj next)
{
  ScmObj inst_gref = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gref);

  inst_gref = scm_cmpl_cons_inst_gref(sym, module);
  if (scm_obj_null_p(inst_gref)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gref, next);
}

ScmObj
scm_cmpl_cons_inst_gdef(ScmObj sym, ScmObj module)
{
  return scm_cmpl_cons_inst_obj_obj(SCM_OPCODE_GDEF, sym, module);
}

ScmObj
scm_cmpl_push_inst_gdef(ScmObj sym, ScmObj module, ScmObj next)
{
  ScmObj inst_gdef = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gdef);

  inst_gdef = scm_cmpl_cons_inst_gdef(sym, module);
  if (scm_obj_null_p(inst_gdef)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gdef, next);
}

ScmObj
scm_cmpl_cons_inst_gset(ScmObj sym, ScmObj module)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  return scm_cmpl_cons_inst_obj_obj(SCM_OPCODE_GSET, sym, module);
}

ScmObj
scm_cmpl_push_inst_gset(ScmObj sym, ScmObj module, ScmObj next)
{
  ScmObj inst_gset = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gset);

  inst_gset = scm_cmpl_cons_inst_gset(sym, module);
  if (scm_obj_null_p(inst_gset)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gset, next);
}

ScmObj
scm_cmpl_cons_inst_sref(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SREF, idx, layer);
}

ScmObj
scm_cmpl_push_inst_sref(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_sref = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_sref);

  inst_sref = scm_cmpl_cons_inst_sref(idx, layer);
  if (scm_obj_null_p(inst_sref)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_sref, next);
}

ScmObj
scm_cmpl_cons_inst_sset(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SSET, idx, layer);
}

ScmObj
scm_cmpl_push_inst_sset(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_sset = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_sset);

  inst_sset = scm_cmpl_cons_inst_sset(idx, layer);
  if (scm_obj_null_p(inst_sset)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_sset, next);
}

ScmObj
scm_cmpl_cons_inst_jmp(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMP, lbl);
}

ScmObj
scm_cmpl_push_inst_jmp(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmp);

  inst_jmp = scm_cmpl_cons_inst_jmp(lbl);
  if (scm_obj_null_p(inst_jmp)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmp, next);
}

ScmObj
scm_cmpl_cons_inst_jmpt(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMPT, lbl);
}

ScmObj
scm_cmpl_push_inst_jmpt(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmpt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmpt);

  inst_jmpt = scm_cmpl_cons_inst_jmpt(lbl);
  if (scm_obj_null_p(inst_jmpt)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmpt, next);
}

ScmObj
scm_cmpl_cons_inst_jmpf(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMPF, lbl);
}

ScmObj
scm_cmpl_push_inst_jmpf(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmpf = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmpf);

  inst_jmpf = scm_cmpl_cons_inst_jmpf(lbl);
  if (scm_obj_null_p(inst_jmpf)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmpf, next);
}

ScmObj
scm_cmpl_cons_inst_box(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_BOX, idx, layer);
}

ScmObj
scm_cmpl_push_inst_box(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_box = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_box);

  inst_box = scm_cmpl_cons_inst_box(idx, layer);
  if (scm_obj_null_p(inst_box)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_box, next);
}

ScmObj
scm_cmpl_cons_inst_demine(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_DEMINE, idx, layer);
}

ScmObj
scm_cmpl_push_inst_demine(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_demine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_demine);

  inst_demine = scm_cmpl_cons_inst_demine(idx, layer);
  if (scm_obj_null_p(inst_demine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_demine, next);
}

ScmObj
scm_cmpl_cons_inst_emine(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_EMINE, narg);
}

ScmObj
scm_cmpl_push_inst_emine(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_emine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_emine);

  inst_emine = scm_cmpl_cons_inst_emine(narg);
  if (scm_obj_null_p(inst_emine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_emine, next);
}

ScmObj
scm_cmpl_cons_inst_edemine(scm_sword_t narg, scm_sword_t layer)
{
  scm_assert(narg >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_EDEMINE, narg, layer);
}

ScmObj
scm_cmpl_push_inst_edemine(scm_sword_t narg, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_edemine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_edemine);

  inst_edemine = scm_cmpl_cons_inst_edemine(narg, layer);
  if (scm_obj_null_p(inst_edemine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_edemine, next);
}

ScmObj
scm_cmpl_cons_inst_arity(scm_sword_t arity)
{
  return scm_cmpl_cons_inst_si(SCM_OPCODE_ARITY, arity);
}

ScmObj
scm_cmpl_push_inst_arity(scm_sword_t arity, ScmObj next)
{
  ScmObj inst_arity = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_arity);

  inst_arity = scm_cmpl_cons_inst_arity(arity);
  if (scm_obj_null_p(inst_arity)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_arity, next);
}

ScmObj
scm_cmpl_cons_inst_label(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_ASM_PI_LABEL, lbl);
}

ScmObj
scm_cmpl_push_inst_label(ScmObj lbl, ScmObj next)
{
  ScmObj inst_label = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_label);

  inst_label = scm_cmpl_cons_inst_label(lbl);
  if (scm_obj_null_p(inst_label)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_label, next);
}

ScmObj
scm_cmpl_cons_inst_asm_close(scm_sword_t nr_free,
                             scm_sword_t arity, ScmObj code)
{
  scm_assert(nr_free >= 0);

  return scm_cmpl_cons_inst_si_si_obj(SCM_ASM_PI_ASM_CLOSE,
                                      nr_free, arity, code);
}

ScmObj
scm_cmpl_push_inst_asm_close(scm_sword_t nr_free, scm_sword_t arity,
                             ScmObj code, ScmObj next)
{
  ScmObj inst_asm_close = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_asm_close);

  inst_asm_close = scm_cmpl_cons_inst_asm_close(nr_free, arity, code);
  if (scm_obj_null_p(inst_asm_close)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_asm_close, next);
}

ScmObj
scm_cmpl_push_cont_arity_check(int arity, ScmObj next)
{
  SCM_STACK_FRAME_PUSH(&next);

  if (arity <= 1) {
    next = scm_cmpl_push_inst_arity(arity, next);
  }
  else {
    for (size_t i = 0; i < SCM_INST_SZ_ARITY / SCM_OPSIZE; i++) {
      next = scm_cmpl_push_inst_nop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }

  return next;
}

ScmObj
scm_cmpl_env_new(void)
{
  return SCM_NIL_OBJ;
}

ScmObj
scm_cmpl_env_cons(ScmObj vars, bool vparam, ScmObj env)
{
  ScmObj rib = SCM_OBJ_INIT, assigned = SCM_OBJ_INIT, fls = SCM_OBJ_INIT;
  ScmObj vp_flg = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&vars, &env,
                       &rib, &assigned, &fls,
                       &vp_flg);

  scm_assert(scm_capi_pair_p(vars) || scm_capi_vector_p(vars));
  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));

  if (scm_capi_pair_p(vars)) {
    vars = scm_api_list_to_vector(vars);
    if (scm_obj_null_p(vars)) return SCM_OBJ_NULL;
  }

  len = scm_capi_vector_length(vars);
  if (len < 0) return SCM_OBJ_NULL;

  fls = SCM_FALSE_OBJ;
  if (scm_obj_null_p(fls)) return SCM_OBJ_NULL;

  assigned = scm_capi_make_vector((size_t)len, fls);
  if (scm_obj_null_p(assigned)) return SCM_OBJ_NULL;

  vp_flg = vparam ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
  if (scm_obj_null_p(vp_flg)) return SCM_OBJ_NULL;

  rib = scm_capi_list(3, vars, assigned, vp_flg);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  rib = scm_api_list_to_vector(rib);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  return scm_api_cons(rib, env);
}

ScmObj
scm_cmpl_env_outer(ScmObj env)
{
  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));

  if (scm_capi_nil_p(env))
    return env;

  return scm_api_cdr(env);
}

int
scm_cmpl_env_vparam_flg(ScmObj env, size_t layer, bool *flg)
{
  ScmObj itr = SCM_OBJ_INIT, rib = SCM_OBJ_INIT, vparam = SCM_OBJ_INIT;;
  size_t i;

  SCM_STACK_FRAME_PUSH(&env,
                       &itr, &rib, &vparam);

  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));
  scm_assert(flg != NULL);

  for (i = 0, itr = env;
       i < layer && scm_capi_pair_p(itr);
       i++, itr = scm_api_cdr(itr))
    ;

  rib = scm_api_car(itr);
  if (scm_obj_null_p(rib)) return -1;

  vparam = scm_capi_vector_ref(rib, 2);
  if (scm_obj_null_p(vparam)) return -1;

  *flg = scm_capi_true_object_p(vparam);

  return 0;
}

int
scm_cmpl_env_assigned_flg(ScmObj env, size_t idx, size_t layer, bool *flg)
{
  ScmObj itr = SCM_OBJ_INIT, rib = SCM_OBJ_INIT, assigned = SCM_OBJ_INIT;
  ScmObj fo = SCM_OBJ_INIT;
  size_t i;

  SCM_STACK_FRAME_PUSH(&env,
                       &itr, &rib, &assigned,
                       &fo);

  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));
  scm_assert(flg != NULL);

  for (i = 0, itr = env;
       i < layer && scm_capi_pair_p(itr);
       i++, itr = scm_api_cdr(itr))
    ;

  if (scm_obj_null_p(itr)) return -1;

  rib = scm_api_car(itr);
  if (scm_obj_null_p(rib)) return -1;

  assigned = scm_capi_vector_ref(rib, 1);
  if (scm_obj_null_p(assigned)) return -1;

  fo = scm_capi_vector_ref(assigned, idx);
  if (scm_obj_null_p(fo)) return -1;

  *flg = scm_capi_true_object_p(fo);

  return 0;
}

int
scm_cmpl_env_resolv(ScmObj env, ScmObj sym, bool assigned,
                    ssize_t *idx, ssize_t *layer)
{
  ScmObj itr = SCM_OBJ_INIT, rib = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj as_flgs = SCM_OBJ_INIT,  var = SCM_OBJ_INIT, tr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env, &sym,
                       &itr, &rib, &vars,
                       &as_flgs, &var, &tr);

  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(idx != NULL);
  scm_assert(layer != NULL);

  for (itr = env, *layer = 0;
       scm_capi_pair_p(itr);
       itr = scm_api_cdr(itr), (*layer)++) {
    ssize_t len;

    rib = scm_api_car(itr);

    if (scm_obj_null_p(rib)) return -1;

    vars = scm_capi_vector_ref(rib, 0);
    if (scm_obj_null_p(vars)) return -1;

    len = scm_capi_vector_length(vars);
    if (len < 0) return -1;

    for (*idx = 0; *idx < len; (*idx)++) {
      var = scm_capi_vector_ref(vars, (size_t)*idx);
      if (scm_obj_null_p(var)) return -1;

      if (scm_capi_eq_p(var, sym)) {
        if (assigned) {

          tr = SCM_TRUE_OBJ;
          if (scm_obj_null_p(tr)) return -1;

          as_flgs = scm_capi_vector_ref(rib, 1);
          if (scm_obj_null_p(as_flgs)) return -1;

          tr = scm_capi_vector_set(as_flgs, (size_t)*idx, tr);
          if (scm_obj_null_p(tr)) return -1;
        }
        return 0;
      }
    }
  }

  if (scm_obj_null_p(itr)) return -1;

  *idx = -1;
  *layer = -1;

  return 0;
}

ScmObj
scm_cmpl_current_module_name(ScmObj cmpl)
{
  ScmObj module = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&cmpl,
                       &module);

  module = scm_api_current_module(cmpl);
  if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

  return scm_api_module_name(module);
}

ScmObj
scm_cmpl_gen_label(ScmObj cmpl, const char *prefix)
{
  char str[256];

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (prefix != NULL)
    snprintf(str, sizeof(str), "lbl_%s_%u",
             prefix, SCM_COMPILER(cmpl)->label_id++);
  else
    snprintf(str, sizeof(str), "lbl_%u",
             SCM_COMPILER(cmpl)->label_id++);

  return scm_capi_make_symbol_from_cstr(str, SCM_ENC_ASCII);
}

enum { SCM_CMPL_CORE_SYNTAX_REFERENCE,
       SCM_CMPL_CORE_SYNTAX_SELF_EVAL,
       SCM_CMPL_CORE_SYNTAX_APPLICATION };

static ScmObj scm_cmpl_core_syntaxes[] = { SCM_OBJ_NULL,
                                           SCM_OBJ_NULL,
                                           SCM_OBJ_NULL };

static ScmObj
scm_cmpl_compile_reference(ScmObj cmpl, ScmObj exp, ScmObj env,
                           ScmObj next, int arity,
                           bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj module = SCM_OBJ_INIT;
  ssize_t idx, layer;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &module);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  scm_assert(scm_capi_symbol_p(exp));

  rslt = scm_cmpl_env_resolv(env, exp, false, &idx, &layer);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (idx > SCM_SWORD_MAX || layer > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  *rdepth = (layer >= 0) ? layer : -1;

  if (idx >= 0) {
    return scm_cmpl_push_inst_sref(idx, layer, next);
  }
  else {
    module = scm_cmpl_current_module_name(cmpl);
    if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

    return scm_cmpl_push_inst_gref(exp, module, next);
  }
}

static ScmObj
scm_cmpl_compile_self_eval(ScmObj cmpl, ScmObj exp, ScmObj env,
                           ScmObj next, int arity,
                           bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  *rdepth = -1;

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_push_inst_immval(exp, next);
}

static int
scm_cmpl_decons_application(ScmObj exp,
                            scm_csetter_t *proc, scm_csetter_t *args)
{
  ScmObj po = SCM_OBJ_INIT, ao = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &po, &ao);

  scm_assert(scm_capi_pair_p(exp));

  po = scm_api_car(exp);
  if (scm_obj_null_p(po)) return -1;

  ao = scm_api_cdr(exp);
  if (scm_obj_null_p(ao)) return -1;

  scm_csetter_setq(proc, po);
  scm_csetter_setq(args, ao);

  return 0;
}

static ScmObj
scm_cmpl_cmpl_application(ScmObj cmpl, ScmObj proc, ScmObj args,
                          ScmObj env, ScmObj next, int arity,
                          bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj elm = SCM_OBJ_INIT;
  ssize_t nr_args, rd;

  SCM_STACK_FRAME_PUSH(&cmpl, &proc, &args, &env, &next,
                       &elm);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  nr_args = scm_capi_vector_length(args);
  if (nr_args < 0) return SCM_OBJ_NULL;

  if (tail_p) {
    next = scm_cmpl_push_inst_tcall((scm_sword_t)nr_args, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }
  else {
    next = scm_cmpl_push_cont_arity_check(arity, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_call((scm_sword_t)nr_args, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp(cmpl, proc, env, next, 1,
                              false, toplevel_p, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  for (ssize_t i = nr_args; i > 0; i--) {
    elm = scm_capi_vector_ref(args, (size_t)i - 1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_push(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(cmpl, elm, env, next, 1,
                                false, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  if (!tail_p) {
    if (nr_args > 0)
      return scm_cmpl_push_inst_frame(next);
    else
      return scm_cmpl_push_inst_cframe(next);
  }
  else if (nr_args > 0) {
    return scm_cmpl_push_inst_eframe(next);
  }
  else {
    return next;
  }
}

static ScmObj
scm_cmpl_compile_application(ScmObj cmpl, ScmObj exp, ScmObj env,
                             ScmObj next, int arity,
                             bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &proc, &args);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_application(exp,
                                     SCM_CSETTER_L(proc),
                                     SCM_CSETTER_L(args));
  if (rslt < 0) return SCM_OBJ_NULL;

  args = scm_api_list_to_vector(args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  return scm_cmpl_cmpl_application(cmpl, proc, args, env, next, arity,
                                   tail_p, toplevel_p, rdepth);
}

static ScmObj
scm_cmpl_make_syntax(const char *key, ScmSyntaxHandlerFunc handler)
{
  ScmObj sym = SCM_OBJ_INIT;

  scm_assert(key != NULL);
  scm_assert(handler != NULL);

  sym = scm_capi_make_symbol_from_cstr(key, SCM_ENC_ASCII);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  return scm_capi_make_syntax(sym, handler);
}

static int
scm_cmpl_init_syntaxes(int id)
{
  static const char * const keys[] = { "reference", "self-eval", "application"};
  static ScmSyntaxHandlerFunc const handlers[] =
    { scm_cmpl_compile_reference,
      scm_cmpl_compile_self_eval,
      scm_cmpl_compile_application };

  scm_assert(id >= 0 && (size_t)id < sizeof(keys)/sizeof(keys[0]));
  scm_assert(sizeof(keys)/sizeof(keys[0])
             == sizeof(handlers)/sizeof(handlers[0]));

  if (scm_obj_null_p(scm_cmpl_core_syntaxes[id])) {
    ScmRef r = SCM_REF_MAKE(scm_cmpl_core_syntaxes[id]);
    r = scm_capi_mem_register_extra_rfrn(r);
    if (r == NULL) return -1;
    scm_cmpl_core_syntaxes[id] = scm_cmpl_make_syntax(keys[id], handlers[id]);
  }

  return 0;
}

ScmObj
scm_cmpl_syntax(ScmObj cmpl, ScmObj exp, ScmObj env,
                bool tail_p, bool toplevel_p)
{
  ScmObj key = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env,
                       &key, &syx, &mod);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (scm_capi_symbol_p(exp)) {
    int rslt = scm_cmpl_init_syntaxes(SCM_CMPL_CORE_SYNTAX_REFERENCE);
    if (rslt < 0) return SCM_OBJ_NULL;

    return scm_cmpl_core_syntaxes[SCM_CMPL_CORE_SYNTAX_REFERENCE];
  }
  else if (scm_capi_pair_p(exp)) {
    ssize_t idx, layer;
    int rslt;

    key = scm_api_car(exp);
    if (scm_obj_null_p(key)) return SCM_OBJ_NULL;

    if (scm_capi_symbol_p(key)) {
      rslt = scm_cmpl_env_resolv(env, key, false, &idx, &layer);
      if (rslt < 0) return SCM_OBJ_NULL;

      if (idx < 0) {
        mod = scm_api_current_module(cmpl);
        if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

        rslt = scm_capi_global_syx_ref(mod, key, SCM_CSETTER_L(syx));
        if (rslt < 0) return SCM_OBJ_NULL;

        if (scm_obj_not_null_p(syx))
          return syx;
      }
    }

    rslt = scm_cmpl_init_syntaxes(SCM_CMPL_CORE_SYNTAX_APPLICATION);
    if (rslt < 0) return SCM_OBJ_NULL;

    return scm_cmpl_core_syntaxes[SCM_CMPL_CORE_SYNTAX_APPLICATION];
  }
  else {
    int rslt = scm_cmpl_init_syntaxes(SCM_CMPL_CORE_SYNTAX_SELF_EVAL);
    if (rslt < 0) return SCM_OBJ_NULL;

    return scm_cmpl_core_syntaxes[SCM_CMPL_CORE_SYNTAX_SELF_EVAL];
  }
}

ScmObj
scm_cmpl_compile_exp(ScmObj cmpl, ScmObj exp, ScmObj env,
                     ScmObj next, int arity,
                     bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj syntax = SCM_OBJ_INIT;
  ScmSyntaxHandlerFunc handler;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &syntax);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  syntax = scm_cmpl_syntax(cmpl, exp, env, tail_p, toplevel_p);
  if (scm_obj_null_p(syntax)) return SCM_OBJ_NULL;

  rslt = scm_capi_syntax_handler(syntax, &handler);
  if (rslt < 0) return SCM_OBJ_NULL;

  return handler(cmpl, exp, env, next, arity, tail_p, toplevel_p, rdepth);
}


/************************************************************************/
/* Scheme Base Syntax                                                   */
/************************************************************************/

static ScmObj
scm_cmpl_stack_new(void)
{
  ScmObj n = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&n, &nil);

  n = scm_capi_make_number_from_sword(0);
  if (scm_obj_null_p(n)) return SCM_OBJ_NULL;

  nil = SCM_NIL_OBJ;
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  return scm_api_cons(nil, n);
}

static int
scm_cmpl_stack_inc_cnt(ScmObj stack)
{
  ScmObj n = SCM_OBJ_INIT;
  scm_sword_t cnt;
  int rslt;

  SCM_STACK_FRAME_PUSH(&stack,
                       &n);

  n = scm_api_cdr(stack);
  if (scm_obj_null_p(n)) return -1;

  rslt = scm_capi_num_to_sword(n, &cnt);
  if (rslt < 0) return -1;

  cnt++;

  n = scm_capi_make_number_from_sword(cnt);
  if (scm_obj_null_p(n)) return -1;

  n = scm_api_set_cdr(stack, n);
  if (scm_obj_null_p(n)) return -1;

  return 0;
}

static int
scm_cmpl_stack_dec_cnt(ScmObj stack)
{
  ScmObj n = SCM_OBJ_INIT;
  scm_sword_t cnt;
  int rslt;

  SCM_STACK_FRAME_PUSH(&stack,
                       &n);

  n = scm_api_cdr(stack);
  if (scm_obj_null_p(n)) return -1;

  rslt = scm_capi_num_to_sword(n, &cnt);
  if (rslt < 0) return -1;

  cnt--;

  n = scm_capi_make_number_from_sword(cnt);
  if (scm_obj_null_p(n)) return -1;

  n = scm_api_set_cdr(stack, n);
  if (scm_obj_null_p(n)) return -1;

  return 0;
}

static int
scm_cmpl_stack_push(ScmObj stack, ScmObj obj)
{
  ScmObj st = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&stack,
                       &st, &obj);

  st = scm_api_car(stack);
  if (scm_obj_null_p(st)) return -1;

  st = scm_api_cons(obj, st);
  if (scm_obj_null_p(st)) return -1;

  st = scm_api_set_car(stack, st);
  if (scm_obj_null_p(st)) return -1;

  return scm_cmpl_stack_inc_cnt(stack);
}

static int
scm_cmpl_stack_pop(ScmObj stack)
{
  ScmObj st = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&stack,
                       &st);

  st = scm_api_car(stack);
  if (scm_obj_null_p(st)) return -1;

  st = scm_api_cdr(st);
  if (scm_obj_null_p(st)) return -1;

  st = scm_api_set_car(stack, st);
  if (scm_obj_null_p(st)) return -1;

  return scm_cmpl_stack_dec_cnt(stack);
}

static ScmObj
scm_cmpl_stack_head(ScmObj stack)
{
  ScmObj st = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&stack,
                       &st);

  st = scm_api_car(stack);
  if (scm_obj_null_p(st)) return SCM_OBJ_NULL;

  return scm_api_car(st);
}

static bool
scm_cmpl_stack_empty_p(ScmObj stack)
{
  ScmObj st = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&stack,
                       &st);

  st = scm_api_car(stack);
  if (scm_obj_null_p(st)) return false;

  return scm_capi_nil_p(st);
}

static ssize_t
scm_cmpl_stack_len(ScmObj stack)
{
  ScmObj n = SCM_OBJ_INIT;
  scm_sword_t l;
  int rslt;

  SCM_STACK_FRAME_PUSH(&stack,
                       &n);
  n = scm_api_cdr(stack);
  if (scm_obj_null_p(n)) return -1;

  rslt = scm_capi_num_to_sword(n, &l);
  if (rslt < 0) return -1;

  return (ssize_t)l;
}

static ScmObj
scm_cmpl_stack_to_vec(ScmObj stack)
{
  ScmObj vec = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&stack,
                       &vec, &lst, &e);

  len = scm_cmpl_stack_len(stack);
  if (len < 0) return SCM_OBJ_NULL;

  vec = scm_capi_make_vector((size_t)len, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  lst = scm_api_car(stack);
  if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;

  for (ssize_t i = 0; i < len; i++) {
    e = scm_api_car(lst);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    e = scm_capi_vector_set(vec, (size_t)i, e);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    lst = scm_api_cdr(lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return vec;
}

static ScmObj
scm_cmpl_stack_to_rvec(ScmObj stack)
{
  ScmObj vec = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&stack,
                       &vec, &lst, &e);

  len = scm_cmpl_stack_len(stack);
  if (len < 0) return SCM_OBJ_NULL;

  vec = scm_capi_make_vector((size_t)len, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  lst = scm_api_car(stack);
  if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;

  for (ssize_t i = len; i > 0; i--) {
    e = scm_api_car(lst);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    e = scm_capi_vector_set(vec, (size_t)i - 1, e);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    lst = scm_api_cdr(lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return vec;
}

enum { SCM_CMPL_BASE_SYNTAX_DEFINITION, SCM_CMPL_BASE_SYNTAX_QUOTE,
       SCM_CMPL_BASE_SYNTAX_LAMBDA, SCM_CMPL_BASE_SYNTAX_ASSIGNMENT,
       SCM_CMPL_BASE_SYNTAX_IF, SCM_CMPL_BASE_SYNTAX_COND,
       SCM_CMPL_SYNTAXL_AND, SCM_CMPL_BASE_SYNTAX_OR,
       SCM_CMPL_BASE_SYNTAX_WHEN, SCM_CMPL_BASE_SYNTAX_UNLESS,
       SCM_CMPL_BASE_SYNTAX_LET, SCM_CMPL_BASE_SYNTAX_LET_A,
       SCM_CMPL_BASE_SYNTAX_LETREC, SCM_CMPL_BASE_SYNTAX_LETREC_A,
       SCM_CMPL_BASE_SYNTAX_BEGIN, SCM_CMPL_BASE_SYNTAX_DO,
       SCM_CMPL_NR_BASE_SYNTAX };

static ScmObj scm_cmpl_base_syntaxes[SCM_CMPL_NR_BASE_SYNTAX] =
  { SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL,
    SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL,
    SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL,
    SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL, SCM_OBJ_NULL };

static bool
scm_cmpl_syntax_eq_p(ScmObj syntax, int id)
{
  scm_assert(0 <= id && id < SCM_CMPL_NR_BASE_SYNTAX );

  return scm_capi_eq_p(syntax, scm_cmpl_base_syntaxes[id]);
}

static ScmObj scm_cmpl_compile_empty(ScmObj cmpl, ScmObj exp, ScmObj env,
                                     ScmObj next, int arity,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static ScmObj scm_cmpl_compile_exp_list(ScmObj cmpl, ScmObj exp_lst, ScmObj env,
                                        ScmObj next, int arity,
                                        bool tail_p, bool toplevel_p,
                                        ssize_t *rdepth);
static int scm_cmpl_decons_body(ScmObj cmpl, ScmObj body, ScmObj env,
                                bool tail_p, bool toplevel_p,
                                scm_csetter_t *vars, scm_csetter_t *inits,
                                scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_body(ScmObj cmpl, ScmObj body, ScmObj env,
                                    ScmObj next, int arity,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static ScmObj scm_cmpl_normalize_definition(ScmObj exp);
static int scm_cmpl_decons_definition(ScmObj exp,
                                      scm_csetter_t *var, scm_csetter_t *val);
static ScmObj scm_cmpl_compile_definition(ScmObj cmpl, ScmObj exp, ScmObj env,
                                          ScmObj next, int arity,
                                          bool tail_p, bool toplevel_p,
                                          ssize_t *rdepth);
static int scm_cmpl_decons_quote(ScmObj exp, scm_csetter_t *obj);
static ScmObj scm_cmpl_compile_quote(ScmObj cmpl, ScmObj exp, ScmObj env,
                                     ScmObj next, int arity,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static int scm_cmpl_decons_lambda(ScmObj exp,
                                  scm_csetter_t *formals, scm_csetter_t *body);
static ScmObj scm_cmpl_cmpl_lambda(ScmObj cmpl,
                                   ScmObj params, bool vparam_p, ScmObj body,
                                   ScmObj env, ScmObj next, int arity,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static ScmObj scm_cmpl_cmpl_closure_body(ScmObj cmpl, ScmObj body, ScmObj env,
                                         ScmObj next, int arity, bool tail_p,
                                         bool toplevel_p, size_t nr_param,
                                         ssize_t *rdepth);
static ScmObj scm_cmpl_compile_lambda(ScmObj cmpl, ScmObj exp, ScmObj env,
                                      ScmObj next, int arity,
                                      bool tail_p, bool toplevel_p,
                                      ssize_t *rdepth);
static int scm_cmpl_decons_assignment(ScmObj exp,
                                      scm_csetter_t *var, scm_csetter_t *val);
static ScmObj scm_cmpl_compile_assignment(ScmObj cmpl, ScmObj exp, ScmObj env,
                                          ScmObj next, int arity,
                                          bool tail_p, bool toplevel_p,
                                          ssize_t *rdepth);
static int scm_cmpl_decons_if(ScmObj exp, scm_csetter_t *cond,
                              scm_csetter_t *conse, scm_csetter_t *alter);
static ScmObj scm_cmpl_compile_if(ScmObj cmpl, ScmObj exp, ScmObj env,
                                  ScmObj next, int arity,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);
static int scm_cmpl_decons_cond(ScmObj exp,
                                scm_csetter_t *tests, scm_csetter_t *expss,
                                bool *else_exist_p);
static ScmObj scm_cmpl_compile_cond(ScmObj cmpl, ScmObj exp, ScmObj env,
                                    ScmObj next, int arity,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static int scm_cmpl_decons_and(ScmObj exp, scm_csetter_t *tests);
static ScmObj scm_cmpl_compile_and(ScmObj cmpl, ScmObj exp, ScmObj env,
                                   ScmObj next, int arity,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static int scm_cmpl_decons_or(ScmObj exp, scm_csetter_t *tests);
static ScmObj scm_cmpl_compile_or(ScmObj cmpl, ScmObj exp, ScmObj env,
                                  ScmObj next, int arity,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);
static int scm_cmpl_decons_when(ScmObj exp,
                                scm_csetter_t *test, scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_when(ScmObj cmpl, ScmObj exp, ScmObj env,
                                    ScmObj next, int arity,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static int scm_cmpl_decons_unless(ScmObj exp,
                                  scm_csetter_t *test, scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_unless(ScmObj cmpl, ScmObj exp, ScmObj env,
                                      ScmObj next, int arity,
                                      bool tail_p, bool toplevel_p,
                                      ssize_t *rdepth);
static int scm_cmpl_decons_let(ScmObj exp, int syntax,
                               scm_csetter_t *name, scm_csetter_t *vars,
                               scm_csetter_t *inits, scm_csetter_t *body);
static ScmObj scm_cmpl_compile_let(ScmObj cmpl, ScmObj exp, ScmObj env,
                                   ScmObj next, int arity,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static ScmObj scm_cmpl_compile_let_a(ScmObj cmpl, ScmObj exp, ScmObj env,
                                     ScmObj next, int arity,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static ScmObj scm_cmpl_compile_letrec(ScmObj cmpl, ScmObj exp, ScmObj env,
                                      ScmObj next, int arity,
                                      bool tail_p, bool toplevel_p,
                                      ssize_t *rdepth);
static ScmObj scm_cmpl_compile_letrec_a(ScmObj cmpl, ScmObj exp, ScmObj env,
                                        ScmObj next, int arity,
                                        bool tail_p, bool toplevel_p,
                                        ssize_t *rdepth);
static int scm_cmpl_decons_begin(ScmObj exp, scm_csetter_t *exp_lst);
static ScmObj scm_cmpl_compile_begin(ScmObj cmpl,ScmObj exp, ScmObj env,
                                     ScmObj next, int arity,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static int scm_cmpl_decons_do(ScmObj exp,
                              scm_csetter_t *vars, scm_csetter_t *inits,
                              scm_csetter_t *steps, scm_csetter_t *test,
                              scm_csetter_t *exps, scm_csetter_t *cmds);
static ScmObj scm_cmpl_compile_do(ScmObj cmpl, ScmObj exp, ScmObj env,
                                  ScmObj next, int arity,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);

static ScmObj
scm_cmpl_compile_empty(ScmObj cmpl, ScmObj exp, ScmObj env,
                       ScmObj next, int arity,
                       bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  *rdepth = -1;

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_push_inst_undef(next);
}

static ScmObj
scm_cmpl_compile_exp_list(ScmObj cmpl, ScmObj exp_lst, ScmObj env,
                          ScmObj next, int arity,
                          bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj exp_vec = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, code = SCM_OBJ_INIT;
  ssize_t rd, len;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp_lst, &env, &next,
                       &exp_vec, &exp, &code);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  exp_vec = scm_api_list_to_vector(exp_lst);
  if (scm_obj_null_p(exp_vec)) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(exp_vec);
  if (len < 0) return SCM_OBJ_NULL;

  if (len == 0)
    return scm_cmpl_compile_empty(cmpl, exp_lst, env, next, arity,
                                  tail_p, toplevel_p, rdepth);

  *rdepth = -1;
  code = next;
  for (ssize_t i = len; i > 0; i--) {
    exp = scm_capi_vector_ref(exp_vec, (size_t)i - 1);
    if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

    code = scm_cmpl_compile_exp(cmpl, exp, env,
                                code, arity, tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(code)) return SCM_OBJ_NULL;

    tail_p = false;
    arity = -1;

    if (rd > *rdepth) *rdepth = rd;
  }

  return code;
}

static int
scm_cmpl_decons_body(ScmObj cmpl, ScmObj body, ScmObj env,
                     bool tail_p, bool toplevel_p,
                     scm_csetter_t *vars, scm_csetter_t *inits,
                     scm_csetter_t *exps)
{
  ScmObj var_stack = SCM_OBJ_INIT, init_stack = SCM_OBJ_INIT;
  ScmObj exps_stack = SCM_OBJ_INIT;
  ScmObj exp_lst = SCM_OBJ_INIT, exp = SCM_OBJ_INIT;
  ScmObj var = SCM_OBJ_INIT, init = SCM_OBJ_INIT;
  ScmObj vvec = SCM_OBJ_INIT, ivec = SCM_OBJ_INIT, evec = SCM_OBJ_INIT;
  ScmObj syntax = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ssize_t len;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &body, &env,
                       &var_stack, &init_stack,
                       &exps_stack,
                       &exp_lst, &exp,
                       &var, &init,
                       &vvec, &ivec, &evec,
                       &syntax, &nil);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  var_stack = scm_cmpl_stack_new();
  if (scm_obj_null_p(var_stack)) return -1;

  init_stack = scm_cmpl_stack_new();
  if (scm_obj_null_p(init_stack)) return -1;

  exps_stack = scm_cmpl_stack_new();
  if (scm_obj_null_p(exps_stack)) return -1;

  rslt = scm_cmpl_stack_push(exps_stack, body);
  if (rslt < 0) return -1;

  while (!scm_cmpl_stack_empty_p(exps_stack)) {
    exp_lst = scm_cmpl_stack_head(exps_stack);
    if (scm_obj_null_p(exp_lst)) return -1;

    rslt = scm_cmpl_stack_pop(exps_stack);
    if (rslt < 0) return -1;

    while (scm_capi_pair_p(exp_lst)) {
      exp = scm_api_car(exp_lst);
      if (scm_obj_null_p(exp)) return -1;

      syntax = scm_cmpl_syntax(cmpl, exp, env, tail_p, toplevel_p);
      if (scm_obj_null_p(syntax)) return -1;

      if (scm_cmpl_syntax_eq_p(syntax, SCM_CMPL_BASE_SYNTAX_DEFINITION)) {
        exp_lst = scm_api_cdr(exp_lst);
        if (scm_obj_null_p(exp_lst)) return -1;

        exp = scm_cmpl_normalize_definition(exp);
        if (scm_obj_null_p(exp)) return -1;

        rslt = scm_cmpl_decons_definition(exp,
                                          SCM_CSETTER_L(var),
                                          SCM_CSETTER_L(init));
        if (rslt < 0) return -1;

        rslt = scm_cmpl_stack_push(var_stack, var);
        if (rslt < 0) return -1;

        rslt = scm_cmpl_stack_push(init_stack, init);
        if (rslt < 0) return -1;
      }
      else if (scm_cmpl_syntax_eq_p(syntax, SCM_CMPL_BASE_SYNTAX_BEGIN)) {
        exp_lst = scm_api_cdr(exp_lst);
        if (scm_obj_null_p(exp_lst)) return -1;

        if (scm_capi_pair_p(exp_lst)) {
          rslt = scm_cmpl_stack_push(exps_stack, exp_lst);
          if (rslt < 0) return -1;
        }
        else if (!scm_capi_nil_p(exp_lst)) {
          scm_capi_error("Compiler: malformed <body>", 0);
          return -1;
        }

        rslt = scm_cmpl_decons_begin(exp, SCM_CSETTER_L(exp_lst));
        if (rslt < 0) return -1;
      }
      else {
        rslt = scm_cmpl_stack_push(exps_stack, exp_lst);
        if (rslt < 0) return -1;

        goto done;
      }
    }

    if (!scm_capi_nil_p(exp_lst)) {
      scm_capi_error("Compiler: malformed <body>", 0);
      return -1;
    }
  }

 done:

  len = scm_cmpl_stack_len(exps_stack);
  if (len < 0) return -1;

  if (len == 0) {
    nil = SCM_NIL_OBJ;
    if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

    rslt = scm_cmpl_stack_push(exps_stack, nil);
    if (rslt < 0) return -1;
  }

  vvec = scm_cmpl_stack_to_rvec(var_stack);
  if (scm_obj_null_p(vvec)) return SCM_OBJ_NULL;

  ivec = scm_cmpl_stack_to_rvec(init_stack);
  if (scm_obj_null_p(ivec)) return SCM_OBJ_NULL;

  evec = scm_cmpl_stack_to_vec(exps_stack);
  if (scm_obj_null_p(evec)) return SCM_OBJ_NULL;

  scm_csetter_setq(vars, vvec);
  scm_csetter_setq(inits, ivec);
  scm_csetter_setq(exps, evec);

  return 0;
}

static ScmObj
scm_cmpl_compile_body(ScmObj cmpl, ScmObj body, ScmObj env,
                      ScmObj next, int arity,
                      bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT, expls = SCM_OBJ_INIT;
  ScmObj exp = SCM_OBJ_INIT, exp_lst = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, nr_exps, rd;
  bool tl_p;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &body, &env, &next,
                       &vars, &inits, &expls,
                       &exp, &exp_lst, &new_env);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_body(cmpl, body, env, tail_p, toplevel_p,
                              SCM_CSETTER_L(vars), SCM_CSETTER_L(inits),
                              SCM_CSETTER_L(expls));
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    new_env = scm_cmpl_env_cons(vars, false, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    if (!tail_p) {
      next = scm_cmpl_push_inst_epop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }
  else {
    new_env = env;
  }

  nr_exps = scm_capi_vector_length(expls);
  if (nr_exps < 0) return SCM_OBJ_NULL;

  *rdepth = -1;
  tl_p = tail_p;
  for (ssize_t i = nr_exps; i > 0; i--) {
    exp_lst = scm_capi_vector_ref(expls, (size_t)i - 1);
    if (scm_obj_null_p(exp_lst)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp_list(cmpl, exp_lst, new_env, next, arity,
                                     tl_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;

    tl_p = false;
    arity = -1;
  }

  if (nr_vars > 0) {
    if (nr_vars > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      next = scm_cmpl_push_inst_demine((scm_sword_t)i - 1, 0, next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      exp = scm_capi_vector_ref(inits, i - 1);
      if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, exp, new_env, next, 1,
                                  false, toplevel_p, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_emine(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (nr_vars > 0 && *rdepth >= 0) (*rdepth)--;

  return next;
}

static int
scm_cmpl_decons_definition(ScmObj exp, scm_csetter_t *var, scm_csetter_t *val)
{
  ScmObj second = SCM_OBJ_INIT, third = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &second, &third, &tmp);

  scm_assert(scm_capi_pair_p(exp));
  scm_assert(var != NULL);
  scm_assert(val != NULL);

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  second = scm_api_car(tmp);
  if (scm_obj_null_p(second)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  third = scm_api_car(tmp);
  if (scm_obj_null_p(third)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_nil_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed define", 0);
    return SCM_OBJ_NULL;
  }

  scm_csetter_setq(var, second);
  scm_csetter_setq(val, third);

  return 0;
}

static ScmObj
scm_cmpl_normalize_definition(ScmObj exp)
{
  ScmObj first = SCM_OBJ_INIT, second = SCM_OBJ_INIT, third = SCM_OBJ_INIT;
  ScmObj name = SCM_OBJ_INIT, form = SCM_OBJ_INIT;
  ScmObj lambda = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &first, &second, &third,
                       &name, &form,
                       &lambda,
                       &tmp);

  lambda = scm_capi_make_symbol_from_cstr("lambda", SCM_ENC_ASCII);
  if (scm_obj_null_p(lambda)) return SCM_OBJ_NULL;

  first = scm_api_car(exp);
  if (scm_obj_null_p(first)) return SCM_OBJ_NULL;

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;

  second = scm_api_car(tmp);
  if (scm_obj_null_p(second)) return SCM_OBJ_NULL;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;

  third = scm_api_car(tmp);
  if (scm_obj_null_p(third)) return SCM_OBJ_NULL;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;

  if (!scm_capi_nil_p(tmp)) {
    scm_capi_error("Compiler: syntax error: malformed define", 0);
    return SCM_OBJ_NULL;
  }

  while (scm_capi_pair_p(second)) {
    name = scm_api_car(second);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

    form = scm_api_cdr(second);
    if (scm_obj_null_p(form)) return SCM_OBJ_NULL;

    third = scm_capi_list(3, lambda, form, third);
    if (scm_obj_null_p(third)) return SCM_OBJ_NULL;

    second = name;
  }

  return scm_capi_list(3, first, second, third);
}

static ScmObj
scm_cmpl_compile_definition(ScmObj cmpl,ScmObj exp, ScmObj env,
                            ScmObj next, int arity,
                            bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT, module = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &var, &val, &module);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (!toplevel_p) {
    scm_capi_error("Compiler: definition can appear "
                   "in the toplevel or beginning of a <body>", 0);
    return SCM_OBJ_NULL;
  }

  exp = scm_cmpl_normalize_definition(exp);
  if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_decons_definition(exp,
                                    SCM_CSETTER_L(var), SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (!scm_capi_symbol_p(var)) {
    scm_capi_error("Compiler: syntax error: malformed define", 0);
    return SCM_OBJ_NULL;
  }

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  module = scm_cmpl_current_module_name(cmpl);
  if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_gdef(var, module, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(cmpl, val, env, next, 1,
                              false, toplevel_p, rdepth);
}

static int
scm_cmpl_decons_quote(ScmObj exp, scm_csetter_t *obj)
{
  ScmObj oo = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &oo, &tmp);

  scm_assert(scm_capi_pair_p(exp));

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed quote", 0);
    return -1;
  }

  oo = scm_api_car(tmp);
  if (scm_obj_null_p(oo)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_nil_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed quote", 0);
    return -1;
  }

  scm_csetter_setq(obj, oo);

  return 0;
}

static ScmObj
scm_cmpl_compile_quote(ScmObj cmpl, ScmObj exp, ScmObj env,
                       ScmObj next, int arity,
                       bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj obj = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &obj);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_quote(exp, SCM_CSETTER_L(obj));
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_cmpl_compile_self_eval(cmpl, obj, env, next, arity,
                                    tail_p, toplevel_p, rdepth);
}

static int
scm_cmpl_decons_lambda(ScmObj exp, scm_csetter_t *formals, scm_csetter_t *body)
{
  ScmObj fo = SCM_OBJ_INIT, bo = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &fo, &bo, &tmp);

  scm_assert(scm_capi_pair_p(exp));

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed lambda", 0);
    return -1;
  }

  fo = scm_api_car(tmp);
  if (scm_obj_null_p(fo)) return -1;

  bo = scm_api_cdr(tmp);
  if (scm_obj_null_p(fo)) return -1;

  if (formals != NULL)
    scm_csetter_setq(formals, fo);

  if (body != NULL)
    scm_csetter_setq(body, bo);

  return 0;
}

static ScmObj
scm_cmpl_parse_lambda_formals(ScmObj formals, bool *vparam_p)
{
  ScmObj i = SCM_OBJ_INIT, e = SCM_OBJ_INIT, v = SCM_OBJ_INIT, r = SCM_OBJ_INIT;
  size_t n, j;
  bool variable;

  SCM_STACK_FRAME_PUSH(&formals, &i, &e, &v, &r);

  for (i = formals, n = 0;
       scm_capi_pair_p(i);
       i = scm_api_cdr(i), n++) {
    e = scm_api_car(i);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;
    if (!scm_capi_symbol_p(e)) {
      scm_capi_error("compile: syntax error: malformed lambda", 0);
      return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(i)) return SCM_OBJ_NULL;

  if (scm_capi_nil_p(i)) {
    variable = false;
  }
  else if (scm_capi_symbol_p(i)) {
    variable = true;
    n++;
  }
  else {
    scm_capi_error("compile: syntax error: malformed lambda", 0);
    return SCM_OBJ_NULL;
  }

  v = scm_capi_make_vector(n, SCM_OBJ_NULL);
  if (scm_obj_null_p(v)) return SCM_OBJ_NULL;

  j = 0;
  for (i = formals;
       scm_capi_pair_p(i);
       i = scm_api_cdr(i)) {
    e = scm_api_car(i);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    r = scm_capi_vector_set(v, j++, e);
    if (scm_obj_null_p(r)) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(i)) return SCM_OBJ_NULL;

  if (variable) {
    r = scm_capi_vector_set(v, j++, i);
    if (scm_obj_null_p(r)) return SCM_OBJ_NULL;
  }

  if (vparam_p != NULL) *vparam_p = variable;

  return v;
}

static ScmObj
scm_cmpl_cmpl_closure_body(ScmObj cmpl, ScmObj body, ScmObj env,
                           ScmObj next, int arity,
                           bool tail_p, bool toplevel_p, size_t nr_param,
                           ssize_t *rdepth)
{
  ScmObj body_code = SCM_OBJ_INIT;
  bool assigned;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &body, &env,
                       &body_code);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  body_code = scm_cmpl_compile_body(cmpl, body, env, next, arity,
                                    tail_p, toplevel_p, rdepth);
  if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;

  if (nr_param > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  for (size_t i = 0; i < nr_param; i++) {
    rslt = scm_cmpl_env_assigned_flg(env, i, 0, &assigned);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (assigned) {
      body_code = scm_cmpl_push_inst_box((scm_sword_t)i, 0, body_code);
      if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;
    }
  }

  return body_code;
}

static ScmObj
scm_cmpl_cmpl_lambda(ScmObj cmpl, ScmObj params, bool vparam_p, ScmObj body,
                     ScmObj env, ScmObj next, int arity,
                     bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj new_env = SCM_OBJ_INIT, body_code = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ssize_t nr_params, func_arity;

  SCM_STACK_FRAME_PUSH(&cmpl, &params, &body, &env, &next,
                       &new_env, &body_code, &nil);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  nil = SCM_NIL_OBJ;
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  nr_params = scm_capi_vector_length(params);
  if (nr_params < 0) return SCM_OBJ_NULL;

  if (nr_params > 0) {
    new_env = scm_cmpl_env_cons(params, vparam_p, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;
  }
  else
    new_env = env;

  body_code = scm_cmpl_cmpl_closure_body(cmpl, body, new_env, nil, -1,
                                         true, false, (size_t)nr_params,
                                         rdepth);
  if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;

  if (nr_params > 0 && *rdepth >= 0) (*rdepth)--;

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (*rdepth >= 0 && *rdepth > SCM_SWORD_MAX - 1) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  func_arity = vparam_p ? -nr_params : nr_params;

  return scm_cmpl_push_inst_asm_close((*rdepth >= 0) ? *rdepth + 1 : 0,
                                      func_arity, body_code, next);
}

static ScmObj
scm_cmpl_compile_lambda(ScmObj cmpl, ScmObj exp, ScmObj env,
                        ScmObj next, int arity,
                        bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj formals = SCM_OBJ_INIT, body = SCM_OBJ_INIT, params = SCM_OBJ_INIT;
  bool vparam_p;

  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &formals, &body, &params);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_lambda(exp,
                                SCM_CSETTER_L(formals), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  params = scm_cmpl_parse_lambda_formals(formals, &vparam_p);
  if (scm_obj_null_p(params)) return SCM_OBJ_NULL;

  return scm_cmpl_cmpl_lambda(cmpl, params, vparam_p, body, env, next, arity,
                              tail_p, toplevel_p, rdepth);
}

static int
scm_cmpl_decons_assignment(ScmObj exp, scm_csetter_t *var, scm_csetter_t *val)
{
  ScmObj ro = SCM_OBJ_INIT, lo = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &ro, &ro, &tmp);

  scm_assert(scm_capi_pair_p(exp));

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed assignment", 0);
    return -1;
  }

  ro = scm_api_car(tmp);
  if (scm_obj_null_p(ro)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed assignment", 0);
    return -1;
  }

  lo = scm_api_car(tmp);
  if (scm_obj_null_p(lo)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_nil_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed assignment", 0);
    return -1;
  }

  scm_csetter_setq(var, ro);
  scm_csetter_setq(val, lo);

  return 0;
}

static ScmObj
scm_cmpl_compile_assignment(ScmObj cmpl, ScmObj exp, ScmObj env,
                            ScmObj next, int arity,
                            bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT, module = SCM_OBJ_INIT;
  int rslt;
  ssize_t idx, layer, rd;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &var, &val, &module);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_assignment(exp,
                                    SCM_CSETTER_L(var), SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (!scm_capi_symbol_p(var)) {
    scm_capi_error("Compiler: syntax error: malformed set!", 0);
    return SCM_OBJ_NULL;
  }

  rslt = scm_cmpl_env_resolv(env, var, true, &idx, &layer);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (idx > SCM_SWORD_MAX || layer > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (idx >= 0) {
    next = scm_cmpl_push_inst_sset(idx, layer, next);
  }
  else {
    module = scm_cmpl_current_module_name(cmpl);
    if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_gset(var, module, next);
  }

  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  *rdepth = (layer >= 0) ? layer : -1;

  next = scm_cmpl_compile_exp(cmpl, val, env, next, 1, false, toplevel_p, &rd);;
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  return next;
}

static int
scm_cmpl_decons_if(ScmObj exp, scm_csetter_t *cond,
                   scm_csetter_t *conse, scm_csetter_t *alter)
{
  ScmObj cd = SCM_OBJ_INIT, cs = SCM_OBJ_INIT, al = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &cd, &cs, &al, &tmp);

  scm_assert(scm_capi_pair_p(exp));

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed if", 0);
    return -1;
  }

  cd = scm_api_car(tmp);
  if (scm_obj_null_p(cd)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_pair_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed if", 0);
    return -1;
  }

  cs = scm_api_car(tmp);
  if (scm_obj_null_p(cs)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (scm_capi_nil_p(tmp)) {
    al = SCM_OBJ_NULL;
  }
  else if (scm_capi_pair_p(tmp)) {
    al = scm_api_car(tmp);
    if (scm_obj_null_p(al)) return -1;

    tmp = scm_api_cdr(tmp);
    if (scm_obj_null_p(tmp)) return -1;

    if (!scm_capi_nil_p(tmp)) {
      scm_capi_error("compile: syntax error: malformed if", 0);
      return -1;
    }
  }
  else {
    scm_capi_error("compile: syntax error: malformed if", 0);
    return -1;
  }

  if (cond != NULL)
    scm_csetter_setq(cond, cd);

  if (conse != NULL)
    scm_csetter_setq(conse, cs);

  if (alter != NULL)
    scm_csetter_setq(alter, al);

  return 0;
}

static ScmObj
scm_cmpl_compile_if(ScmObj cmpl, ScmObj exp, ScmObj env, ScmObj next, int arity,
                    bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj cond = SCM_OBJ_INIT, conse = SCM_OBJ_INIT, alter = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &cond, &conse, &alter,
                       &lbl_junc, &lbl_alt);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_if(exp, SCM_CSETTER_L(cond),
                            SCM_CSETTER_L(conse), SCM_CSETTER_L(alter));

  if (rslt < 0) return SCM_OBJ_NULL;

  if (!tail_p) {
    /* if 分岐後の合流地点のラベル定義を next 直前に追加 */
    lbl_junc = scm_cmpl_gen_label(cmpl, "if-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  *rdepth = -1;
  if (scm_obj_not_null_p(alter)) {
    /* alternative 節を付加 */
    next = scm_cmpl_compile_exp(cmpl, alter, env, next, arity,
                                tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }
  else {
    next = scm_cmpl_compile_empty(cmpl, alter, env, next, arity,
                                  tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  if (rd > *rdepth) *rdepth = rd;

  /* condition 節実行後に alternative 節に条件ジャンプするためのラベル定義
     を追加 */
  lbl_alt = scm_cmpl_gen_label(cmpl, "if-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  if (!tail_p) {
    /* consequnece 節実行後に合流地点へジャンプする命令を追加 */
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  /* consequence 節を付加 */
  next = scm_cmpl_compile_exp(cmpl, conse, env, next, arity,
                              tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  /* condition 節が false value の場合に alternative 節直前にジャンプする
     命令の作成 */
  next = scm_cmpl_push_inst_jmpf(lbl_alt, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  /* conditio 節を付加 */
  next = scm_cmpl_compile_exp(cmpl, cond, env, next, 1,
                              false, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  return next;
}

static int
scm_cmpl_decons_cond(ScmObj exp, scm_csetter_t *tests, scm_csetter_t *expss,
                     bool *else_exist_p)
{
  ScmObj clauses = SCM_OBJ_INIT, tvec = SCM_OBJ_INIT, evec = SCM_OBJ_INIT;
  ScmObj cls = SCM_OBJ_INIT, tst = SCM_OBJ_INIT, exs = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT, arrow_sym = SCM_OBJ_INIT, else_sym = SCM_OBJ_INIT;
  ssize_t nr_clauses;

  SCM_STACK_FRAME_PUSH(&exp,
                       &clauses, &tvec, &evec,
                       &cls, &tst, &exs,
                       &tmp, &arrow_sym, &else_sym);

  else_sym= scm_capi_make_symbol_from_cstr("else", SCM_ENC_ASCII);
  if (scm_obj_null_p(else_sym)) return -1;

  arrow_sym = scm_capi_make_symbol_from_cstr("=>", SCM_ENC_ASCII);
  if (scm_obj_null_p(arrow_sym)) return -1;

  clauses = scm_api_cdr(exp);

  nr_clauses = scm_capi_length(clauses);
  if (nr_clauses < 0) return -1;

  tvec = scm_capi_make_vector((size_t)nr_clauses, SCM_OBJ_NULL);
  if (scm_obj_null_p(tvec)) return -1;

  evec = scm_capi_make_vector((size_t)nr_clauses, SCM_OBJ_NULL);
  if (scm_obj_null_p(evec)) return -1;

  *else_exist_p = false;
  for (size_t i = 0; i < (size_t)nr_clauses; i++) {
    cls = scm_api_car(clauses);
    if (scm_obj_null_p(cls)) return -1;

    tst = scm_api_car(cls);
    if (scm_obj_null_p(tst)) return -1;

    if (scm_capi_eq_p(else_sym, tst)) {
      if (*else_exist_p) {
        scm_capi_error("malformed cond", 0);
        return -1;
      }

      *else_exist_p = true;
    }

    exs = scm_api_cdr(cls);
    if (scm_obj_null_p(exs)) return -1;

    if (scm_capi_pair_p(exs)) {
      tmp = scm_api_car(exs);
      if (scm_obj_null_p(tmp)) return -1;

      if (scm_capi_eq_p(arrow_sym, tmp)) {
        if (*else_exist_p) {
          scm_capi_error("Compiler: malformed cond", 0);
          return -1;
        }

        exs = scm_api_cdr(exs);
        if (scm_obj_null_p(exs)) return -1;

        exs = scm_api_car(exs);
        if (scm_obj_null_p(exs)) return -1;

        /* (<test> => <expression>) 形式の場合、<expression> を vector で包む。
         * (<test> <expression> ...) 形式の場合はリストが evec に設定されるが
         * その形式と区別するため。
         */
        exs = scm_capi_make_vector(1, exs);
        if (scm_obj_null_p(exs)) return -1;
      }
    }

    tmp = scm_capi_vector_set(tvec, i, tst);
    if (scm_obj_null_p(tmp)) return -1;

    tmp = scm_capi_vector_set(evec, i, exs);
    if (scm_obj_null_p(tmp)) return -1;

    clauses = scm_api_cdr(clauses);
    if (scm_obj_null_p(clauses)) return -1;
  }

  scm_csetter_setq(tests, tvec);
  scm_csetter_setq(expss, evec);

  return 0;
}

static ScmObj
scm_cmpl_cmpl_cond_clause_exp(ScmObj cmpl, ScmObj exp, ScmObj label,
                              ScmObj env, ScmObj next, int arity,
                              bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next);

  *rdepth = -1;

  if (!scm_capi_nil_p(label)) {
    next = scm_cmpl_push_inst_jmp(label, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (scm_capi_pair_p(exp)) {
    return scm_cmpl_compile_exp_list(cmpl, exp, env, next, arity,
                                     tail_p, toplevel_p, rdepth);
  }
  else if (scm_capi_nil_p(exp)){
    if (tail_p)
      return scm_cmpl_push_inst_return(next);
    else
      return next;
  }
  else {
    exp = scm_capi_vector_ref(exp, 0);
    if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

    if (tail_p) {
      next = scm_cmpl_push_inst_tcall(1, next);
      if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
    }
    else {
      next = scm_cmpl_push_cont_arity_check(arity, next);
      if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

      next = scm_cmpl_push_inst_call(1, next);
      if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
    }

    next = scm_cmpl_compile_exp(cmpl, exp, env, next, 1,
                                false, toplevel_p, rdepth);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_push(next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    if (tail_p)
      return scm_cmpl_push_inst_eframe(next);
    else
      return scm_cmpl_push_inst_frame(next);
  }
}

static ScmObj
scm_cmpl_make_cond_clause_label(ScmObj cmpl,
                                ScmObj labels, size_t idx, const char *prefix)
{
  ScmObj lbl = SCM_OBJ_INIT, ro = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&labels,
                       &lbl, &ro);

  lbl = scm_cmpl_gen_label(cmpl, prefix);
  if (scm_obj_null_p(lbl)) return SCM_OBJ_NULL;

  ro = scm_capi_vector_set(labels, idx, lbl);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  return lbl;
}

static ScmObj
scm_cmpl_push_cond_clause(ScmObj cmpl, ScmObj exp, ScmObj junc,
                          ScmObj labels, size_t idx,
                          ScmObj env, ScmObj next, int arity,
                          bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj lbl_cls = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &junc, &labels, &env, &next,
                       &lbl_cls);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  next = scm_cmpl_cmpl_cond_clause_exp(cmpl, exp, junc, env, next, arity,
                                       tail_p, toplevel_p, rdepth);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  lbl_cls = scm_cmpl_make_cond_clause_label(cmpl, labels, idx, "cond-c");
  if (scm_obj_null_p(lbl_cls)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_cls, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  return next;
}

static ScmObj
scm_cmpl_cmpl_cond_clause_test(ScmObj cmpl, ScmObj test, ScmObj label,
                               ScmObj env, ScmObj next, int arity,
                               bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&cmpl, &test, &label, &env, &next);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (!scm_capi_nil_p(label)) {
    next = scm_cmpl_push_inst_jmpt(label, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_compile_exp(cmpl, test, env, next, 1,
                              false, toplevel_p, rdepth);
}

static ScmObj
scm_cmpl_compile_cond(ScmObj cmpl, ScmObj exp, ScmObj env,
                      ScmObj next, int arity,
                      bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_INIT, expss = SCM_OBJ_INIT;
  ScmObj texp = SCM_OBJ_INIT, eexp = SCM_OBJ_INIT;
  ScmObj labels = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_INIT, lbl_cls = SCM_OBJ_INIT;
  ScmObj lbl = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ssize_t nr_clauses, rd;
  bool else_exist_p;
  int rslt, nr_cls_exp_code;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &tests, &expss,
                       &texp, &eexp,
                       &labels, &lbl_junc, &lbl_cls,
                       &lbl, &nil);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  *rdepth = -1;

  nil = SCM_NIL_OBJ;
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_decons_cond(exp, SCM_CSETTER_L(tests), SCM_CSETTER_L(expss),
                              &else_exist_p);
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_clauses = scm_capi_vector_length(tests);
  if (nr_clauses < 0) return SCM_OBJ_NULL;

  if (nr_clauses == 0)
    return scm_cmpl_compile_empty(cmpl, exp, env, next, arity,
                                  tail_p, toplevel_p,rdepth);

  labels = scm_capi_make_vector((size_t)nr_clauses, nil);
  if (scm_obj_null_p(labels)) return SCM_OBJ_NULL;


  /*
   * cond 式評価後の合流地点のラベルを設定。cond が tail-expression の場合は、
   * 各節の <expression> をコンパイルしたコードの部分から直接 return するので
   * 合流地点のラベルは設定しない。
   */
  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label(cmpl, "cond-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }
  else {
    lbl_junc = nil;
  }

  nr_cls_exp_code = 0;

  /*
   * else 節に <expression> が記載されている場合はその式をコンパイルし、その
   * コードへジャンプするためのラベルを設定する。式の評価後、cond が
   *  tail-expression なら return する。tail-expression でないなら、何もしな
   * い (このコード直後がちょうど合流地点になる)
   */
  if (else_exist_p) {
    eexp = scm_capi_vector_ref(expss, (size_t)nr_clauses - 1);
    if (scm_obj_null_p(eexp)) return SCM_OBJ_NULL;

    if (!scm_capi_nil_p(eexp)) {
      next = scm_cmpl_push_cond_clause(cmpl, eexp, nil, labels,
                                       (size_t)nr_clauses - 1,
                                       env, next, arity,
                                       tail_p, toplevel_p, &rd);
      if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
      nr_cls_exp_code++;
    }
  }

  /*
   * else 節よりも前に記載しれている節の <expression> 部分をコンパイルし、その
   * コードへジャンプするためのラベルを設定する。<expression> が空の場合でも、
   * cond が tail-expression の場合には return 命令だけを出力してラベルを設定
   * する。<expression> の評価後、cond が tail-expression なら return する。
   * tail-expression でないなら合流地点へジャンプする。ただし、その
   *  <expression> が最後の節である場合 (具体的には else 節がない、または else
   * 節に <expression> の記載がない cond 式の else 節を除いた最後の節の場合)
   * は、ジャンプしない (そのコードの直後が合流地点ちょうど合流地点になる)
   */
  for (size_t i = (size_t)nr_clauses - (else_exist_p ? 1 : 0); i > 0; i--) {
    eexp = scm_capi_vector_ref(expss, i - 1);
    if (scm_obj_null_p(eexp)) return SCM_OBJ_NULL;

    if (!scm_capi_nil_p(eexp) || tail_p) {
      lbl = (!tail_p && nr_cls_exp_code > 0) ? lbl_junc : nil;

      next = scm_cmpl_push_cond_clause(cmpl, eexp, lbl, labels, i - 1,
                                       env, next, arity,
                                       tail_p, toplevel_p, &rd);
      if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
      nr_cls_exp_code++;
    }
  }

  /*
   * else 節よりも前に記載しれている節の <test> が全て偽だった場合のコードを
   * 出力する。else 節に <expression> の記載があれば、それをコンパイルした
   * コードのところへジャンプする。else 節の <expression> が空、あるいは
   * else 節そのものがない場合は、undef を実行し return あるいは合流地点へ
   * ジャンプする。ただし、ジャンプしなくても、<expression> のコードあるいは
   * 合流地点へいける場合はジャンプしない。
   */
  lbl_cls = nil;
  if (else_exist_p) {
    lbl_cls = scm_capi_vector_ref(labels, (size_t)nr_clauses - 1);
    if (scm_obj_null_p(lbl_cls)) return SCM_OBJ_NULL;
  }

  if (scm_capi_nil_p(lbl_cls)) {
    if (tail_p)
      next = scm_cmpl_push_inst_return(next);
    else if (nr_cls_exp_code > 0)
      next = scm_cmpl_push_inst_jmp(lbl_junc, next);

    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_undef(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }
  else if (nr_cls_exp_code > 1) {
    next = scm_cmpl_push_inst_jmp(lbl_cls, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  /*
   * else 節よりも前に記載されている節の <test> 部分をコンパイルする。
   * <test> の評価結果が真の場合、その節に <expression> 部分の記載があれば、
   * それをコンパイルしたコードのところへジャンプする。記載がない場合は合流
   * 地点へジャンプする。
   */
  for (size_t i = (size_t)nr_clauses - (else_exist_p ? 1 : 0); i > 0; i--) {
    texp = scm_capi_vector_ref(tests, i - 1);
    if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

    lbl_cls = scm_capi_vector_ref(labels, i - 1);
    if (scm_obj_null_p(lbl_cls)) return SCM_OBJ_NULL;

    lbl = (scm_capi_nil_p(lbl_cls)) ? lbl_junc : lbl_cls;
    next = scm_cmpl_cmpl_cond_clause_test(cmpl, texp, lbl, env, next, 1,
                                          tail_p, toplevel_p, &rd);

    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  return next;
}

static int
scm_cmpl_decons_and(ScmObj exp, scm_csetter_t *tests)
{
  ScmObj to = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&exp,
                       &to);

  to = scm_api_cdr(exp);
  if (scm_obj_null_p(to)) return -1;

  to = scm_api_list_to_vector(to);
  if (scm_obj_null_p(to)) return -1;

  scm_csetter_setq(tests, to);

  return 0;
}

static ScmObj
scm_cmpl_compile_and(ScmObj cmpl, ScmObj exp, ScmObj env,
                     ScmObj next, int arity,
                     bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_NULL, texp = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_NULL;
  ssize_t len, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &tests, &texp, &lbl_junc);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  *rdepth = -1;

  rslt = scm_cmpl_decons_and(exp, SCM_CSETTER_L(tests));
  if (rslt < 0) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(tests);
  if (len < 0) return SCM_OBJ_NULL;

  if (len != 1 && tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (len == 0)
    return scm_cmpl_push_inst_immval(SCM_TRUE_OBJ, next);

  if (len > 1) {
    lbl_junc = scm_cmpl_gen_label(cmpl, "and-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  texp = scm_capi_vector_ref(tests, (size_t)len - 1);
  if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(cmpl, texp, env, next, arity,
                              tail_p, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  for (size_t i = (size_t)len - 1; i > 0; i--) {
    next = scm_cmpl_push_inst_jmpf(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    texp = scm_capi_vector_ref(tests, i - 1);
    if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(cmpl, texp, env, next, 1,
                                false, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  return next;
}

static int
scm_cmpl_decons_or(ScmObj exp, scm_csetter_t *tests)
{
  ScmObj to = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&exp,
                       &to);

  to = scm_api_cdr(exp);
  if (scm_obj_null_p(to)) return -1;

  to = scm_api_list_to_vector(to);
  if (scm_obj_null_p(to)) return -1;

  scm_csetter_setq(tests, to);

  return 0;
}

static ScmObj
scm_cmpl_compile_or(ScmObj cmpl, ScmObj exp, ScmObj env, ScmObj next, int arity,
                    bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_NULL, texp = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_NULL;
  ssize_t len, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &tests, &texp, &lbl_junc);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  *rdepth = -1;

  rslt = scm_cmpl_decons_or(exp, SCM_CSETTER_L(tests));
  if (rslt < 0) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(tests);
  if (len < 0) return SCM_OBJ_NULL;

  if (len != 1 && tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (len == 0)
    return scm_cmpl_push_inst_immval(SCM_FALSE_OBJ, next);

  if (len > 1) {
    lbl_junc = scm_cmpl_gen_label(cmpl, "or-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  texp = scm_capi_vector_ref(tests, (size_t)len - 1);
  if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(cmpl, texp, env, next, arity,
                              tail_p, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  for (size_t i = (size_t)len - 1; i > 0; i--) {
    next = scm_cmpl_push_inst_jmpt(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    texp = scm_capi_vector_ref(tests, i - 1);
    if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(cmpl, texp, env, next, 1,
                                false, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  return next;
}

static int
scm_cmpl_decons_when(ScmObj exp, scm_csetter_t *test, scm_csetter_t *exps)
{
  ScmObj tmp = SCM_OBJ_INIT, to = SCM_OBJ_INIT, eo = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &tmp, &to, &eo);

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(exp)) return -1;

  to = scm_api_car(tmp);
  if (scm_obj_null_p(to)) return -1;

  eo = scm_api_cdr(tmp);
  if (scm_obj_null_p(eo)) return -1;

  scm_csetter_setq(test, to);
  scm_csetter_setq(exps, eo);

  return 0;
}

static ScmObj
scm_cmpl_compile_when(ScmObj cmpl, ScmObj exp, ScmObj env,
                      ScmObj next, int arity,
                      bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj test = SCM_OBJ_INIT, exps = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &test, &exps, &lbl_junc, &lbl_alt);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_when(exp, SCM_CSETTER_L(test), SCM_CSETTER_L(exps));
  if (rslt < 0) return SCM_OBJ_NULL;

  *rdepth = -1;

  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label(cmpl, "when-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_push_inst_undef(next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_alt = scm_cmpl_gen_label(cmpl, "when-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (!tail_p) {
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp_list(cmpl, exps, env, next, arity,
                                   tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_jmpf(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(cmpl, test, env, next, 1, false, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  return next;
}

static int
scm_cmpl_decons_unless(ScmObj exp, scm_csetter_t *test, scm_csetter_t *exps)
{
  ScmObj tmp = SCM_OBJ_INIT, to = SCM_OBJ_INIT, eo = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &tmp, &to, &eo);

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(exp)) return -1;

  to = scm_api_car(tmp);
  if (scm_obj_null_p(to)) return -1;

  eo = scm_api_cdr(tmp);
  if (scm_obj_null_p(eo)) return -1;

  scm_csetter_setq(test, to);
  scm_csetter_setq(exps, eo);

  return 0;
}

static ScmObj
scm_cmpl_compile_unless(ScmObj cmpl, ScmObj exp, ScmObj env,
                        ScmObj next, int arity,
                        bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj test = SCM_OBJ_INIT, exps = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &test, &exps, &lbl_junc, &lbl_alt);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_unless(exp, SCM_CSETTER_L(test), SCM_CSETTER_L(exps));
  if (rslt < 0) return SCM_OBJ_NULL;

  *rdepth = -1;

  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label(cmpl, "unless-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_push_inst_undef(next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_alt = scm_cmpl_gen_label(cmpl, "unless-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (!tail_p) {
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp_list(cmpl, exps, env, next, arity,
                                   tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_jmpt(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(cmpl, test, env, next, 1, false, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  return next;
}

static int
scm_cmpl_decons_let_var_init_pair(ScmObj var_init,
                                  scm_csetter_t *var, scm_csetter_t *init)
{
  ScmObj vo = SCM_OBJ_INIT, io = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&var_init,
                       &vo, &io, &tmp);

  scm_assert(scm_capi_pair_p(var_init));

  vo = scm_api_car(var_init);
  if (scm_obj_null_p(vo)) return -1;

  tmp = scm_api_cdr(var_init);
  if (scm_obj_null_p(tmp)) return -1;

  io = scm_api_car(tmp);
  if (scm_obj_null_p(io)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (!scm_capi_nil_p(tmp)) {
    scm_capi_error("compile: syntax error: malformed let", 0);
    return -1;
  }

  scm_csetter_setq(var, vo);
  scm_csetter_setq(init, io);

  return 0;
}

static int
scm_cmpl_decons_let_bindings(ScmObj bindings,
                             scm_csetter_t *vars, scm_csetter_t *inits)
{
  ScmObj vi_pair = SCM_OBJ_INIT, var = SCM_OBJ_INIT, init = SCM_OBJ_INIT;
  ScmObj vars_vec = SCM_OBJ_INIT, inits_vec = SCM_OBJ_INIT;
  ScmObj cur = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;
  ssize_t nr_bindings;
  size_t idx;
  int rslt;

  SCM_STACK_FRAME_PUSH(&bindings,
                       &vi_pair, &var, &init,
                       &vars_vec, &inits_vec,
                       &cur, &tmp);

  scm_assert(scm_capi_nil_p(bindings) || scm_capi_pair_p(bindings));

  nr_bindings = scm_capi_length(bindings);
  if (nr_bindings < 0) return -1;

  vars_vec = scm_capi_make_vector((size_t)nr_bindings, SCM_OBJ_NULL);
  if (scm_obj_null_p(vars_vec)) return -1;

  inits_vec = scm_capi_make_vector((size_t)nr_bindings, SCM_OBJ_NULL);
  if (scm_obj_null_p(inits_vec)) return -1;

  for (idx = 0, cur = bindings;
       scm_capi_pair_p(cur);
       idx++, cur = scm_api_cdr(cur)) {

    vi_pair = scm_api_car(cur);
    if (scm_obj_null_p(vi_pair)) return -1;

    rslt = scm_cmpl_decons_let_var_init_pair(vi_pair,
                                             SCM_CSETTER_L(var),
                                             SCM_CSETTER_L(init));
    if (rslt < 0) return -1;

    tmp = scm_capi_vector_set(vars_vec, idx, var);
    if (scm_obj_null_p(tmp)) return -1;

    tmp = scm_capi_vector_set(inits_vec, idx, init);
    if (scm_obj_null_p(tmp)) return -1;
  }

  if (scm_obj_null_p(cur)) return -1;

  scm_csetter_setq(vars, vars_vec);
  scm_csetter_setq(inits, inits_vec);

  return 0;
}

static int
scm_cmpl_decons_let(ScmObj exp, int syntax,
                    scm_csetter_t *name, scm_csetter_t *vars,
                    scm_csetter_t *inits, scm_csetter_t *body)
{
  ScmObj nao = SCM_OBJ_INIT, bio = SCM_OBJ_INIT, boo = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &nao, &bio, &boo,
                       &tmp);

  scm_assert(scm_capi_pair_p(exp));

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  nao = scm_api_car(tmp);
  if (scm_obj_null_p(nao)) return -1;

  if (syntax == SCM_CMPL_BASE_SYNTAX_LET && scm_capi_symbol_p(nao)) {
    tmp = scm_api_cdr(tmp);
    if (scm_obj_null_p(tmp)) return -1;

    bio = scm_api_car(tmp);
    if (scm_obj_null_p(bio)) return -1;
  }
  else {
    bio = nao;
    nao = SCM_OBJ_NULL;
  }

  boo = scm_api_cdr(tmp);
  if (scm_obj_null_p(bio)) return -1;

  scm_csetter_setq(name, nao);
  scm_csetter_setq(body, boo);

  if (!(scm_capi_nil_p(bio) || scm_capi_pair_p(bio))) {
    switch (syntax) {
    case SCM_CMPL_BASE_SYNTAX_LET:
      scm_capi_error("Compiler: syntax error: malformed let", 0);
      break;
    }
    return -1;
  }

  return scm_cmpl_decons_let_bindings(bio, vars, inits);
}

static ScmObj
scm_cmpl_cmpl_named_let_body(ScmObj cmpl, ScmObj name,
                             ScmObj vars, ScmObj inits, ScmObj body,
                             ScmObj env , ScmObj next, int arity,
                             bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  /*
   * named let は以下の式と等価なものにコンパイルする。この関数では letrec*
   * 以降の部分相当のコードを出力する。上部の let 部分は通常の let と共通に
   * 処理する。
   *
   *  (let ((var init) ...)
   *    (letrec* (({name} (lambda {vars} . {body})))
   *      ({name} . {vars})))
   *
   */

  ScmObj letrec_vars = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t rd;

  SCM_STACK_FRAME_PUSH(&cmpl, &name, &vars, &inits, &body, &env, &next,
                       &letrec_vars, &new_env);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (!tail_p) {
    next = scm_cmpl_push_inst_epop(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  letrec_vars = scm_capi_make_vector(1, name);
  if (scm_obj_null_p(letrec_vars)) return SCM_OBJ_NULL;

  new_env = scm_cmpl_env_cons(letrec_vars, false, env);
  if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

  next = scm_cmpl_cmpl_application(cmpl, name, vars, new_env,
                                   next, arity, tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_demine(0, 0, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_cmpl_lambda(cmpl, vars, false, body,
                              new_env, next, 1, false, false, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_emine(1, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (*rdepth >= 0) (*rdepth)--;

  return next;
}

static ScmObj
scm_cmpl_compile_let(ScmObj cmpl, ScmObj exp, ScmObj env,
                     ScmObj next, int arity,
                     bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, bindings = SCM_OBJ_INIT, body = SCM_OBJ_INIT;
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT;
  ScmObj converted = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &name, &bindings, &body,
                       &vars, &inits, &ini_exp,
                       &converted, &new_env);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_BASE_SYNTAX_LET,
                             SCM_CSETTER_L(name), SCM_CSETTER_L(vars),
                             SCM_CSETTER_L(inits), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    new_env = scm_cmpl_env_cons(vars, false, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    if (!tail_p) {
      next = scm_cmpl_push_inst_epop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }
  else {
    new_env = env;
  }

  if (scm_obj_null_p(name))
    next = scm_cmpl_cmpl_closure_body(cmpl, body, new_env, next, arity,
                                      tail_p, false, (size_t)nr_vars,
                                      rdepth);
  else
    next = scm_cmpl_cmpl_named_let_body(cmpl, name, vars, inits, body,
                                        new_env, next, arity,
                                        tail_p, false, rdepth);

  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (nr_vars > 0 && *rdepth >= 0) (*rdepth)--;

  if (nr_vars > 0) {
    if (nr_vars > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    next = scm_cmpl_push_inst_ecommit(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      next = scm_cmpl_push_inst_push(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      ini_exp = scm_capi_vector_ref(inits, i - 1);
      if (scm_obj_null_p(ini_exp)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, ini_exp, env, next, 1,
                                  false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}

static ScmObj
scm_cmpl_compile_let_a(ScmObj cmpl, ScmObj exp, ScmObj env,
                       ScmObj next, int arity,
                       bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, bindings = SCM_OBJ_INIT, body = SCM_OBJ_INIT;
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT;
  ScmObj var = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  bool assigned;
  int rslt;


  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &name, &bindings, &body,
                       &vars, &inits,
                       &var,  &ini_exp, &new_env);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_BASE_SYNTAX_LET_A,
                             SCM_CSETTER_L(name), SCM_CSETTER_L(vars),
                             SCM_CSETTER_L(inits), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  new_env = env;
  for (size_t i = 0; i < (size_t)nr_vars; i++) {
    var = scm_capi_vector_ref(vars, i);
    if (scm_obj_null_p(var)) return SCM_OBJ_NULL;

    var = scm_capi_make_vector(1, var);
    if (scm_obj_null_p(var)) return SCM_OBJ_NULL;

    new_env = scm_cmpl_env_cons(var, false, new_env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    if (!tail_p) {
      next = scm_cmpl_push_inst_epop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }

  next = scm_cmpl_cmpl_closure_body(cmpl, body, new_env, next, arity,
                                    tail_p, false, (nr_vars > 0) ? 1 : 0,
                                    rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (*rdepth >= nr_vars)
    *rdepth -= nr_vars;
  else
    *rdepth = -1;

  for (ssize_t i = nr_vars; i > 0; i--) {
    if (i < nr_vars) {
      rslt = scm_cmpl_env_assigned_flg(new_env, 0, 0, &assigned);
      if (rslt < 0) return SCM_OBJ_NULL;

      if (assigned) {
        next = scm_cmpl_push_inst_box(0, 0, next);
        if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
      }
    }

    next = scm_cmpl_push_inst_ecommit(1, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_push(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    new_env = scm_cmpl_env_outer(new_env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    ini_exp = scm_capi_vector_ref(inits, (size_t)i - 1);
    if (scm_obj_null_p(ini_exp)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(cmpl, ini_exp, new_env, next, 1,
                                false, false, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd >= i && rd - i > *rdepth)
       *rdepth = rd - i;

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}

static ScmObj
scm_cmpl_compile_letrec(ScmObj cmpl, ScmObj exp, ScmObj env,
                        ScmObj next, int arity,
                        bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, body = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj inits = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &name, &body, &vars,
                       &inits, &ini_exp, &new_env);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_BASE_SYNTAX_LETREC,
                             SCM_CSETTER_L(name), SCM_CSETTER_L(vars),
                             SCM_CSETTER_L(inits), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    new_env = scm_cmpl_env_cons(vars, false, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    if (!tail_p) {
      next = scm_cmpl_push_inst_epop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }
  else {
    new_env = env;
  }

  next = scm_cmpl_compile_body(cmpl, body, new_env, next, arity,
                               tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    if (nr_vars > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    next = scm_cmpl_push_inst_edemine(nr_vars, 0, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      next = scm_cmpl_push_inst_push(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      ini_exp = scm_capi_vector_ref(inits, i - 1);
      if (scm_obj_null_p(ini_exp)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, ini_exp, new_env, next, 1,
                                  false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_emine(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (nr_vars > 0 && *rdepth >= 0) (*rdepth)--;

  return next;
}

static ScmObj
scm_cmpl_compile_letrec_a(ScmObj cmpl, ScmObj exp, ScmObj env,
                          ScmObj next, int arity,
                          bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, body = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj inits = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &name, &body, &vars,
                       &inits, &new_env, &ini_exp);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_BASE_SYNTAX_LETREC_A,
                             SCM_CSETTER_L(name), SCM_CSETTER_L(vars),
                             SCM_CSETTER_L(inits), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    new_env = scm_cmpl_env_cons(vars, false, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

    if (!tail_p) {
      next = scm_cmpl_push_inst_epop(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }
  else {
    new_env = env;
  }

  next = scm_cmpl_compile_body(cmpl, body, new_env, next, arity,
                               tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    if (nr_vars > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      next = scm_cmpl_push_inst_demine((scm_sword_t)i - 1, 0, next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      ini_exp = scm_capi_vector_ref(inits, i - 1);
      if (scm_obj_null_p(ini_exp)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, ini_exp, new_env, next, 1,
                                  false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_emine(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (nr_vars > 0 && *rdepth >= 0) (*rdepth)--;

  return next;
}

static int
scm_cmpl_decons_begin(ScmObj exp, scm_csetter_t *exp_lst)
{
  ScmObj eo;

  SCM_STACK_FRAME_PUSH(&exp,
                       &eo);

  scm_assert(scm_capi_pair_p(exp));

  eo = scm_api_cdr(exp);
  if (scm_obj_null_p(eo)) return -1;

  scm_csetter_setq(exp_lst, eo);

  return 0;
}

static ScmObj
scm_cmpl_compile_begin(ScmObj cmpl, ScmObj exp, ScmObj env,
                       ScmObj next, int arity,
                       bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj exp_lst = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &exp_lst);

  rslt = scm_cmpl_decons_begin(exp, SCM_CSETTER_L(exp_lst));
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp_list(cmpl, exp_lst, env, next, arity,
                                   tail_p, toplevel_p, rdepth);
}

static int
scm_cmpl_decons_do_vis(ScmObj vis, scm_csetter_t *var,
                       scm_csetter_t *init, scm_csetter_t *step)
{
  ScmObj v = SCM_OBJ_INIT, i = SCM_OBJ_INIT, s = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vis,
                       &v, &i, &s,
                       &tmp);

  v = scm_api_car(vis);
  if (scm_obj_null_p(v)) return -1;

  tmp = scm_api_cdr(vis);
  if (scm_obj_null_p(tmp)) return -1;

  i = scm_api_car(tmp);
  if (scm_obj_null_p(i)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  if (scm_capi_nil_p(tmp)) {
    s = v;
  }
  else {
    s = scm_api_car(tmp);
    if (scm_obj_null_p(s)) return -1;
  }

  scm_csetter_setq(var, v);
  scm_csetter_setq(init, i);
  scm_csetter_setq(step, s);

  return 0;
}

static int
scm_cmpl_decons_do(ScmObj exp, scm_csetter_t *vars, scm_csetter_t *inits,
                   scm_csetter_t *steps, scm_csetter_t *test,
                   scm_csetter_t *exps, scm_csetter_t *cmds)
{
  ScmObj var_cls = SCM_OBJ_INIT, tst_cls = SCM_OBJ_INIT, cmd_cls = SCM_OBJ_INIT;
  ScmObj vvec = SCM_OBJ_INIT, ivec = SCM_OBJ_INIT, svec = SCM_OBJ_INIT;
  ScmObj vis = SCM_OBJ_INIT, ve = SCM_OBJ_INIT, ie = SCM_OBJ_INIT;
  ScmObj se = SCM_OBJ_INIT, te = SCM_OBJ_INIT, el = SCM_OBJ_INIT;
  ScmObj ro = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;
  ssize_t nr_vars;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp,
                       &var_cls, &tst_cls, &cmd_cls,
                       &vvec, &ivec, &svec,
                       &vis, &ve, &ie,
                       &se, &te, &el,
                       &ro, &tmp);

  tmp = scm_api_cdr(exp);
  if (scm_obj_null_p(tmp)) return -1;

  var_cls = scm_api_car(tmp);
  if (scm_obj_null_p(var_cls)) return -1;

  tmp = scm_api_cdr(tmp);
  if (scm_obj_null_p(tmp)) return -1;

  tst_cls = scm_api_car(tmp);
  if (scm_obj_null_p(tst_cls)) return -1;

  cmd_cls = scm_api_cdr(tmp);
  if (scm_obj_null_p(cmd_cls)) return -1;

  nr_vars = scm_capi_length(var_cls);
  if (nr_vars < 0) return -1;

  vvec = scm_capi_make_vector((size_t)nr_vars, SCM_OBJ_NULL);
  if (scm_obj_null_p(vvec)) return -1;

  ivec = scm_capi_make_vector((size_t)nr_vars, SCM_OBJ_NULL);
  if (scm_obj_null_p(ivec)) return -1;

  svec = scm_capi_make_vector((size_t)nr_vars, SCM_OBJ_NULL);
  if (scm_obj_null_p(svec)) return -1;

  tmp = var_cls;
  for (size_t i = 0; i < (size_t)nr_vars; i++) {
    vis = scm_api_car(tmp);
    if (scm_obj_null_p(vis)) return -1;

    rslt = scm_cmpl_decons_do_vis(vis, SCM_CSETTER_L(ve),
                                  SCM_CSETTER_L(ie), SCM_CSETTER_L(se));
    if (rslt < 0) return -1;

    ro = scm_capi_vector_set(vvec, i, ve);
    if (scm_obj_null_p(ro)) return -1;

    ro = scm_capi_vector_set(ivec, i, ie);
    if (scm_obj_null_p(ro)) return -1;

    ro = scm_capi_vector_set(svec, i, se);
    if (scm_obj_null_p(ro)) return -1;

    tmp = scm_api_cdr(tmp);
    if (scm_obj_null_p(tmp)) return -1;
  }

  te = scm_api_car(tst_cls);
  if (scm_obj_null_p(te)) return -1;

  el = scm_api_cdr(tst_cls);
  if (scm_obj_null_p(el)) return -1;

  scm_csetter_setq(vars, vvec);
  scm_csetter_setq(inits, ivec);
  scm_csetter_setq(steps, svec);
  scm_csetter_setq(test, te);
  scm_csetter_setq(exps, el);
  scm_csetter_setq(cmds, cmd_cls);

  return 0;
}

static ScmObj
scm_cmpl_compile_do(ScmObj cmpl, ScmObj exp, ScmObj env,
                    ScmObj next, int arity,
                    bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT, steps = SCM_OBJ_INIT;
  ScmObj test = SCM_OBJ_INIT, exps = SCM_OBJ_INIT, cmds = SCM_OBJ_INIT;
  ScmObj ve = SCM_OBJ_INIT, ie = SCM_OBJ_INIT, se = SCM_OBJ_INIT;
  ScmObj lbl_start = SCM_OBJ_INIT, lbl_end = SCM_OBJ_INIT;
  ScmObj new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, idx, layer, rd;
  bool assigned;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cmpl, &exp, &env, &next,
                       &vars, &inits, &steps,
                       &test, &exps, &cmds,
                       &ve, &ie, &se,
                       &lbl_start, &lbl_end, &new_env);

  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  rslt = scm_cmpl_decons_do(exp,
                            SCM_CSETTER_L(vars), SCM_CSETTER_L(inits),
                            SCM_CSETTER_L(steps), SCM_CSETTER_L(test),
                            SCM_CSETTER_L(exps), SCM_CSETTER_L(cmds));
  if (rslt < 0) return SCM_OBJ_NULL;


  nr_vars = scm_capi_vector_length(vars);
  if (nr_vars < 0) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    new_env = scm_cmpl_env_cons(vars, false, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;
  }
  else {
    new_env = env;
  }

  if (!tail_p && nr_vars > 0) {
    next = scm_cmpl_push_inst_epop(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp_list(cmpl, exps, new_env, next, arity,
                                   tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_end = scm_cmpl_gen_label(cmpl, "do-e");
  if (scm_obj_null_p(lbl_end)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_end, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_start = scm_cmpl_gen_label(cmpl, "do-s");
  if (scm_obj_null_p(lbl_start)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_jmp(lbl_start, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (nr_vars > 0) {
    if (nr_vars - 1 > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    next = scm_cmpl_push_inst_erebind(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      ve = scm_capi_vector_ref(vars, i - 1);
      if (scm_obj_null_p(ve)) return SCM_OBJ_NULL;

      se = scm_capi_vector_ref(steps, i - 1);
      if (scm_obj_null_p(se)) return SCM_OBJ_NULL;

      /* 変数の重複チェック */
      rslt = scm_cmpl_env_resolv(new_env, ve, false, &idx, &layer);
      if (rslt < 0) return SCM_OBJ_NULL;

      if (idx != (ssize_t)i - 1 || layer != 0) {
        scm_capi_error("Compiler: malformed do", 0);
        return SCM_OBJ_NULL;
      }

      next = scm_cmpl_push_inst_push(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, se, new_env, next, 1,
                                  false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (!scm_capi_nil_p(cmds)) {
    next = scm_cmpl_compile_exp_list(cmpl, cmds, new_env, next, -1,
                                     false, false, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  next = scm_cmpl_push_inst_jmpt(lbl_end, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(cmpl, test, new_env, next, 1, false, false, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  for (size_t i = (size_t)nr_vars; i > 0; i--) {
    rslt = scm_cmpl_env_assigned_flg(new_env, i - 1, 0, &assigned);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (assigned) {
      next = scm_cmpl_push_inst_box((scm_sword_t)i - 1, 0, next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
    }
  }

  next = scm_cmpl_push_inst_label(lbl_start, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (*rdepth >= 0) (*rdepth)--;

  if (nr_vars > 0) {
    next = scm_cmpl_push_inst_ecommit(nr_vars, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    for (size_t i = (size_t)nr_vars; i > 0; i--) {
      ie = scm_capi_vector_ref(inits, i - 1);
      if (scm_obj_null_p(ie)) return SCM_OBJ_NULL;

      next = scm_cmpl_push_inst_push(next);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      next = scm_cmpl_compile_exp(cmpl, ie, env, next, 1, false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}


/**************************************************************************/
/* Scheme Base Module                                                     */
/**************************************************************************/

static const char * const scm_cmpl_base_syntax_keywords[] =
  { "define", "quote", "lambda", "set!", "if", "cond", "and", "or", "when",
    "unless", "let", "let*", "letrec", "letrec*", "begin", "do" };

static ScmSyntaxHandlerFunc scm_cmpl_compile_funcs[] =
  {  scm_cmpl_compile_definition,
     scm_cmpl_compile_quote,
     scm_cmpl_compile_lambda,
     scm_cmpl_compile_assignment,
     scm_cmpl_compile_if,
     scm_cmpl_compile_cond,
     scm_cmpl_compile_and,
     scm_cmpl_compile_or,
     scm_cmpl_compile_when,
     scm_cmpl_compile_unless,
     scm_cmpl_compile_let,
     scm_cmpl_compile_let_a,
     scm_cmpl_compile_letrec,
     scm_cmpl_compile_letrec_a,
     scm_cmpl_compile_begin,
     scm_cmpl_compile_do };

int
scm_cmpl_define_syntax(ScmObj module)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT;
  ScmRef r;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &syx);

  for (int i = 0; i < SCM_CMPL_NR_BASE_SYNTAX; i++) {
    sym = scm_capi_make_symbol_from_cstr(scm_cmpl_base_syntax_keywords[i],
                                         SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    syx = scm_capi_make_syntax(sym, scm_cmpl_compile_funcs[i]);
    if (scm_obj_null_p(syx)) return -1;

    rslt = scm_capi_define_global_syx(module, sym, syx, true);
    if (rslt < 0) return -1;

    r = SCM_REF_MAKE(scm_cmpl_base_syntaxes[i]);
    r = scm_capi_mem_register_extra_rfrn(r);
    if (r == SCM_REF_NULL) return -1;

    scm_cmpl_base_syntaxes[i] = syx;
  }

  return 0;
}
