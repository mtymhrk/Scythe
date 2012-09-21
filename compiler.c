#include <stdio.h>
#include <stdbool.h>

#include "object.h"
#include "api.h"
#include "assembler.h"
#include "compiler.h"

/* XXX: マルチスレッド対応の場合には TLS にする */
static unsigned int label_id = 0;

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
scm_cmpl_cons_inst_si_obj(SCM_OPCODE_T op, scm_sword_t n, ScmObj obj)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj,
                       &mne, &num);

  mne = scm_asm_mnemonic(op);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(n);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(3, mne, num, obj);
}

static ScmObj
scm_cmpl_cons_inst_undef(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_UNDEF);
}

static ScmObj
scm_cmpl_push_inst_undef(ScmObj next)
{
  ScmObj inst_undef = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_undef);

  inst_undef = scm_cmpl_cons_inst_undef();
  if (scm_obj_null_p(inst_undef)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_undef, next);
}

static ScmObj
scm_cmpl_cons_inst_call(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_CALL, narg);
}

static ScmObj
scm_cmpl_push_inst_call(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_call = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_call);

  inst_call = scm_cmpl_cons_inst_call(narg);
  if (scm_obj_null_p(inst_call)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_call, next);
}

static ScmObj
scm_cmpl_cons_inst_tcall(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_TAIL_CALL, narg);
}

static ScmObj
scm_cmpl_push_inst_tcall(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_tcall = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_tcall);

  inst_tcall = scm_cmpl_cons_inst_tcall(narg);
  if (scm_obj_null_p(inst_tcall)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_tcall, next);
}

static ScmObj
scm_cmpl_cons_inst_return(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_RETURN);
}

static ScmObj
scm_cmpl_push_inst_return(ScmObj next)
{
  ScmObj inst_ret = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_ret);

  inst_ret = scm_cmpl_cons_inst_return();
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_ret, next);
}

static ScmObj
scm_cmpl_cons_inst_frame(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_FRAME);
}

static ScmObj
scm_cmpl_push_inst_frame(ScmObj next)
{
  ScmObj inst_frame = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_frame);

  inst_frame = scm_cmpl_cons_inst_frame();
  if (scm_obj_null_p(inst_frame)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_frame, next);
}

static ScmObj
scm_cmpl_cons_inst_cframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_CFRAME);
}

static ScmObj
scm_cmpl_push_inst_cframe(ScmObj next)
{
  ScmObj inst_cframe = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_cframe);

  inst_cframe = scm_cmpl_cons_inst_cframe();
  if (scm_obj_null_p(inst_cframe)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_cframe, next);
}

static ScmObj
scm_cmpl_cons_inst_eframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_EFRAME);
}

static ScmObj
scm_cmpl_push_inst_eframe(ScmObj next)
{
  ScmObj inst_eframe = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_eframe);

  inst_eframe = scm_cmpl_cons_inst_eframe();
  if (scm_obj_null_p(inst_eframe)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_eframe, next);
}

static ScmObj
scm_cmpl_cons_inst_ecommit(scm_sword_t narg)
{
  return scm_cmpl_cons_inst_si(SCM_OPCODE_ECOMMIT, narg);
}

static ScmObj
scm_cmpl_push_inst_ecommit(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_ecommit = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_ecommit);

  inst_ecommit = scm_cmpl_cons_inst_ecommit(narg);
  if (scm_obj_null_p(inst_ecommit)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_ecommit, next);
}

static ScmObj
scm_cmpl_cons_inst_epop(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_EPOP);
}

static ScmObj
scm_cmpl_push_inst_epop(ScmObj next)
{
  ScmObj inst_epop = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_epop);

  inst_epop = scm_cmpl_cons_inst_epop();
  if (scm_obj_null_p(inst_epop)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_epop, next);
}

static ScmObj
scm_cmpl_cons_inst_erebind(scm_sword_t narg)
{
  return scm_cmpl_cons_inst_si(SCM_OPCODE_EREBIND, narg);
}

static ScmObj
scm_cmpl_push_inst_erebind(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_erebind = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_erebind);

  inst_erebind = scm_cmpl_cons_inst_erebind(narg);
  if (scm_obj_null_p(inst_erebind)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_erebind, next);
}

static ScmObj
scm_cmpl_cons_inst_immval(ScmObj val)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_IMMVAL, val);
}

static ScmObj
scm_cmpl_push_inst_immval(ScmObj val, ScmObj next)
{
  ScmObj inst_immval = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val, &next,
                       &inst_immval);

  inst_immval = scm_cmpl_cons_inst_immval(val);
  if (scm_obj_null_p(inst_immval)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_immval, next);
}

static ScmObj
scm_cmpl_cons_inst_push(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_PUSH);
}

static ScmObj
scm_cmpl_push_inst_push(ScmObj next)
{
  ScmObj inst_push = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_push);

  inst_push = scm_cmpl_cons_inst_push();
  if (scm_obj_null_p(inst_push)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_push, next);
}

static ScmObj
scm_cmpl_cons_inst_gref(ScmObj sym)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GREF, sym);
}

static ScmObj
scm_cmpl_push_inst_gref(ScmObj sym, ScmObj next)
{
  ScmObj inst_gref = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gref);

  inst_gref = scm_cmpl_cons_inst_gref(sym);
  if (scm_obj_null_p(inst_gref)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gref, next);
}

static ScmObj
scm_cmpl_cons_inst_gdef(ScmObj sym)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GDEF, sym);
}

static ScmObj
scm_cmpl_push_inst_gdef(ScmObj sym, ScmObj next)
{
  ScmObj inst_gdef = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gdef);

  inst_gdef = scm_cmpl_cons_inst_gdef(sym);
  if (scm_obj_null_p(inst_gdef)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gdef, next);
}

static ScmObj
scm_cmpl_cons_inst_gset(ScmObj sym)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GSET, sym);
}

static ScmObj
scm_cmpl_push_inst_gset(ScmObj sym, ScmObj next)
{
  ScmObj inst_gset = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &next,
                       &inst_gset);

  inst_gset = scm_cmpl_cons_inst_gset(sym);
  if (scm_obj_null_p(inst_gset)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_gset, next);
}

static ScmObj
scm_cmpl_cons_inst_sref(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SREF, idx, layer);
}

static ScmObj
scm_cmpl_push_inst_sref(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_sref = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_sref);

  inst_sref = scm_cmpl_cons_inst_sref(idx, layer);
  if (scm_obj_null_p(inst_sref)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_sref, next);
}

static ScmObj
scm_cmpl_cons_inst_sset(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SSET, idx, layer);
}

static ScmObj
scm_cmpl_push_inst_sset(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_sset = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_sset);

  inst_sset = scm_cmpl_cons_inst_sset(idx, layer);
  if (scm_obj_null_p(inst_sset)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_sset, next);
}

static ScmObj
scm_cmpl_cons_inst_jmp(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMP, lbl);
}

static ScmObj
scm_cmpl_push_inst_jmp(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmp);

  inst_jmp = scm_cmpl_cons_inst_jmp(lbl);
  if (scm_obj_null_p(inst_jmp)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmp, next);
}

static ScmObj
scm_cmpl_cons_inst_jmpt(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMPT, lbl);
}

static ScmObj
scm_cmpl_push_inst_jmpt(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmpt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmpt);

  inst_jmpt = scm_cmpl_cons_inst_jmpt(lbl);
  if (scm_obj_null_p(inst_jmpt)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmpt, next);
}

static ScmObj
scm_cmpl_cons_inst_jmpf(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMPF, lbl);
}

static ScmObj
scm_cmpl_push_inst_jmpf(ScmObj lbl, ScmObj next)
{
  ScmObj inst_jmpf = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_jmpf);

  inst_jmpf = scm_cmpl_cons_inst_jmpf(lbl);
  if (scm_obj_null_p(inst_jmpf)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_jmpf, next);
}

static ScmObj
scm_cmpl_cons_inst_box(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_BOX, idx, layer);
}

static ScmObj
scm_cmpl_push_inst_box(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_box = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_box);

  inst_box = scm_cmpl_cons_inst_box(idx, layer);
  if (scm_obj_null_p(inst_box)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_box, next);
}

static ScmObj
scm_cmpl_cons_inst_demine(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_DEMINE, idx, layer);
}

static ScmObj
scm_cmpl_push_inst_demine(scm_sword_t idx, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_demine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_demine);

  inst_demine = scm_cmpl_cons_inst_demine(idx, layer);
  if (scm_obj_null_p(inst_demine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_demine, next);
}

static ScmObj
scm_cmpl_cons_inst_emine(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_EMINE, narg);
}

static ScmObj
scm_cmpl_push_inst_emine(scm_sword_t narg, ScmObj next)
{
  ScmObj inst_emine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_emine);

  inst_emine = scm_cmpl_cons_inst_emine(narg);
  if (scm_obj_null_p(inst_emine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_emine, next);
}

static ScmObj
scm_cmpl_cons_inst_edemine(scm_sword_t narg, scm_sword_t layer)
{
  scm_assert(narg >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_EDEMINE, narg, layer);
}

static ScmObj
scm_cmpl_push_inst_edemine(scm_sword_t narg, scm_sword_t layer, ScmObj next)
{
  ScmObj inst_edemine = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_edemine);

  inst_edemine = scm_cmpl_cons_inst_edemine(narg, layer);
  if (scm_obj_null_p(inst_edemine)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_edemine, next);
}

static ScmObj
scm_cmpl_cons_inst_label(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_ASM_PI_LABEL, lbl);
}

static ScmObj
scm_cmpl_push_inst_label(ScmObj lbl, ScmObj next)
{
  ScmObj inst_label = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &next,
                       &inst_label);

  inst_label = scm_cmpl_cons_inst_label(lbl);
  if (scm_obj_null_p(inst_label)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_label, next);
}

static ScmObj
scm_cmpl_cons_inst_asm_close(scm_sword_t nr_free, ScmObj code)
{
  scm_assert(nr_free >= 0);

  return scm_cmpl_cons_inst_si_obj(SCM_ASM_PI_ASM_CLOSE, nr_free, code);
}

static ScmObj
scm_cmpl_push_inst_asm_close(scm_sword_t nr_free, ScmObj code, ScmObj next)
{
  ScmObj inst_asm_close = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&next,
                       &inst_asm_close);

  inst_asm_close = scm_cmpl_cons_inst_asm_close(nr_free, code);
  if (scm_obj_null_p(inst_asm_close)) return SCM_OBJ_NULL;

  return scm_api_cons(inst_asm_close, next);
}

static ScmObj
scm_cmpl_env_new(void)
{
  return scm_api_nil();
}

static ScmObj
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

  fls = scm_api_false();
  if (scm_obj_null_p(fls)) return SCM_OBJ_NULL;

  assigned = scm_capi_make_vector((size_t)len, fls);
  if (scm_obj_null_p(assigned)) return SCM_OBJ_NULL;

  vp_flg = vparam ? scm_api_true() : scm_api_false();
  if (scm_obj_null_p(vp_flg)) return SCM_OBJ_NULL;

  rib = scm_capi_list(3, vars, assigned, vp_flg);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  rib = scm_api_list_to_vector(rib);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  return scm_api_cons(rib, env);
}

static ScmObj
scm_cmpl_env_outer(ScmObj env)
{
  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));

  if (scm_capi_nil_p(env))
    return env;

  return scm_api_cdr(env);
}

static int
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

static int
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

          tr = scm_api_true();
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

static ScmObj
scm_cmpl_stack_new(void)
{
  ScmObj n = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&n, &nil);

  n = scm_capi_make_number_from_sword(0);
  if (scm_obj_null_p(n)) return SCM_OBJ_NULL;

  nil = scm_api_nil();
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

static ScmObj
scm_cmpl_gen_label(const char *prefix)
{
  char str[256];

  if (prefix != NULL)
    snprintf(str, sizeof(str), "lbl_%s_%u", prefix, label_id++);
  else
    snprintf(str, sizeof(str), "lbl_%u", label_id++);

  return scm_capi_make_symbol_from_cstr(str, SCM_ENC_ASCII);
}

static ScmObj scm_cmpl_compile_empty(ScmObj exp, ScmObj env, ScmObj next,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static ScmObj scm_cmpl_compile_exp_list(ScmObj exp_lst, ScmObj env, ScmObj next,
                                        bool tail_p, bool toplevel_p,
                                        ssize_t *rdepth);
static int scm_cmpl_decons_body(ScmObj body, ScmObj env, bool tail_p,
                                bool toplevel_p,
                                scm_csetter_t *vars, scm_csetter_t *inits,
                                scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_body(ScmObj body, ScmObj env, ScmObj next,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static ScmObj scm_cmpl_normalize_definition(ScmObj exp);
static int scm_cmpl_decons_definition(ScmObj exp,
                                      scm_csetter_t *var, scm_csetter_t *val);
static ScmObj scm_cmpl_compile_definition(ScmObj exp, ScmObj env, ScmObj next,
                                          bool tail_p, bool toplevel_p,
                                          ssize_t *rdepth);
static ScmObj scm_cmpl_compile_reference(ScmObj exp, ScmObj env, ScmObj next,
                                         bool tail_p, bool toplevel_p,
                                         ssize_t *rdepth);
static ScmObj scm_cmpl_compile_self_eval(ScmObj exp, ScmObj env, ScmObj next,
                                         bool tail_p, bool toplevel_p,
                                         ssize_t *rdepth);
static int scm_cmpl_decons_quote(ScmObj exp, scm_csetter_t *obj);
static ScmObj scm_cmpl_compile_quote(ScmObj exp, ScmObj env, ScmObj next,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static int scm_cmpl_decons_application(ScmObj exp,
                                       scm_csetter_t *proc,
                                       scm_csetter_t *args);
static ScmObj scm_cmpl_cmpl_application(ScmObj proc, ScmObj args,
                                        ScmObj env, ScmObj next,
                                        bool tail_p, bool toplevel_p,
                                        ssize_t *rdepth);
static ScmObj scm_cmpl_compile_application(ScmObj exp, ScmObj env, ScmObj next,
                                           bool tail_p, bool toplevel_p,
                                           ssize_t *rdepth);
static int scm_cmpl_decons_lambda(ScmObj exp,
                                  scm_csetter_t *formals, scm_csetter_t *body);
static ScmObj scm_cmpl_cmpl_lambda(ScmObj params, bool vparam_p, ScmObj body,
                                   ScmObj env, ScmObj next,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static ScmObj scm_cmpl_cmpl_closure_body(ScmObj body, ScmObj env,
                                         ScmObj next, bool tail_p,
                                         bool toplevel_p, size_t nr_param,
                                         ssize_t *rdepth);
static ScmObj scm_cmpl_compile_lambda(ScmObj exp, ScmObj env, ScmObj next,
                                      bool tail_p, bool toplevel_p,
                                      ssize_t *rdepth);
static int scm_cmpl_decons_assignment(ScmObj exp,
                                      scm_csetter_t *var, scm_csetter_t *val);
static ScmObj scm_cmpl_compile_assignment(ScmObj exp, ScmObj env, ScmObj next,
                                          bool tail_p, bool toplevel_p,
                                          ssize_t *rdepth);
static int scm_cmpl_decons_if(ScmObj exp, scm_csetter_t *cond,
                              scm_csetter_t *conse, scm_csetter_t *alter);
static ScmObj scm_cmpl_compile_if(ScmObj exp, ScmObj env, ScmObj next,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);
static int scm_cmpl_decons_cond(ScmObj exp,
                                scm_csetter_t *tests, scm_csetter_t *expss,
                                bool *else_exist_p);
static ScmObj scm_cmpl_compile_cond(ScmObj exp, ScmObj env, ScmObj next,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static int scm_cmpl_decons_and(ScmObj exp, scm_csetter_t *tests);
static ScmObj scm_cmpl_compile_and(ScmObj exp, ScmObj env, ScmObj next,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static int scm_cmpl_decons_or(ScmObj exp, scm_csetter_t *tests);
static ScmObj scm_cmpl_compile_or(ScmObj exp, ScmObj env, ScmObj next,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);
static int scm_cmpl_decons_when(ScmObj exp,
                                scm_csetter_t *test, scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_when(ScmObj exp, ScmObj env, ScmObj next,
                                    bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static int scm_cmpl_decons_unless(ScmObj exp,
                                  scm_csetter_t *test, scm_csetter_t *exps);
static ScmObj scm_cmpl_compile_unless(ScmObj exp, ScmObj env, ScmObj next,
                                      bool tail_p, bool toplevel_p,
                                    ssize_t *rdepth);
static int scm_cmpl_decons_let(ScmObj exp, int syntax,
                               scm_csetter_t *name, scm_csetter_t *vars,
                               scm_csetter_t *inits, scm_csetter_t *body);
static ScmObj scm_cmpl_compile_let(ScmObj exp, ScmObj env, ScmObj next,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);
static ScmObj scm_cmpl_compile_let_a(ScmObj exp, ScmObj env, ScmObj next,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static ScmObj scm_cmpl_compile_letrec(ScmObj exp, ScmObj env, ScmObj next,
                                      bool tail_p, bool toplevel_p,
                                      ssize_t *rdepth);
static ScmObj scm_cmpl_compile_letrec_a(ScmObj exp, ScmObj env, ScmObj next,
                                        bool tail_p, bool toplevel_p,
                                        ssize_t *rdepth);
static int scm_cmpl_decons_begin(ScmObj exp, scm_csetter_t *exp_lst);
static ScmObj scm_cmpl_compile_begin(ScmObj exp, ScmObj env, ScmObj next,
                                     bool tail_p, bool toplevel_p,
                                     ssize_t *rdepth);
static int scm_cmpl_decons_do(ScmObj exp,
                              scm_csetter_t *vars, scm_csetter_t *inits,
                              scm_csetter_t *steps, scm_csetter_t *test,
                              scm_csetter_t *exps, scm_csetter_t *cmds);
static ScmObj scm_cmpl_compile_do(ScmObj exp, ScmObj env, ScmObj next,
                                  bool tail_p, bool toplevel_p,
                                  ssize_t *rdepth);
static ScmObj scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj next,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth);

enum { SCM_CMPL_SYNTAX_DEFINITION, SCM_CMPL_SYNTAX_REFERENCE,
       SCM_CMPL_SYNTAX_SELF_EVAL, SCM_CMPL_SYNTAX_QUOTE,
       SCM_CMPL_SYNTAX_APPLICATION, SCM_CMPL_SYNTAX_LAMBDA,
       SCM_CMPL_SYNTAX_ASSIGNMENT, SCM_CMPL_SYNTAX_IF,
       SCM_CMPL_SYNTAX_COND, SCM_CMPL_SYNTAXL_AND,
       SCM_CMPL_SYNTAX_OR, SCM_CMPL_SYNTAX_WHEN,
       SCM_CMPL_SYNTAX_UNLESS, SCM_CMPL_SYNTAX_LET,
       SCM_CMPL_SYNTAX_LET_A, SCM_CMPL_SYNTAX_LETREC,
       SCM_CMPL_SYNTAX_LETREC_A, SCM_CMPL_SYNTAX_BEGIN,
       SCM_CMPL_SYNTAX_DO, SCM_CMPL_NR_SYNTAX };

static const char *scm_cmpl_syntax_keywords[] =
  { "define", NULL, NULL, "quote", NULL, "lambda", "set!", "if",
    "cond", "and", "or", "when", "unless", "let", "let*", "letrec",
    "letrec*", "begin", "do" };

static ScmObj (*scm_cmpl_compile_funcs[])(ScmObj exp, ScmObj env, ScmObj next,
                                          bool tail_p, bool toplevel_p,
                                          ssize_t *rdepth)
= {
  scm_cmpl_compile_definition,
  scm_cmpl_compile_reference,
  scm_cmpl_compile_self_eval,
  scm_cmpl_compile_quote,
  scm_cmpl_compile_application,
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
  scm_cmpl_compile_do,
};


static int
scm_cmpl_syntax_id(ScmObj exp, ScmObj env, bool tail_p, bool toplevel_p)
{
  ScmObj key = SCM_OBJ_INIT, syx = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env,
                       &key, &syx);

  if (scm_capi_symbol_p(exp)) {
    return SCM_CMPL_SYNTAX_REFERENCE;
  }
  else if (scm_capi_pair_p(exp)) {
    ssize_t idx, layer;
    int rslt;

    key = scm_api_car(exp);
    if (scm_obj_null_p(key)) return -1;

    if (scm_capi_symbol_p(key)) {
      rslt = scm_cmpl_env_resolv(env, key, false, &idx, &layer);
      if (rslt < 0) return -1;

      if (idx < 0) {
        rslt = scm_capi_global_var_ref(key, SCM_CSETTER_L(syx));
        if (rslt < 0) return -1;

        if (scm_capi_syntax_p(syx)) {
          int id = scm_capi_syntax_id(syx);
          if (id < 0) return -1;

          return id;
        }
      }
    }

    return SCM_CMPL_SYNTAX_APPLICATION;
  }
  else {
    return SCM_CMPL_SYNTAX_SELF_EVAL;
  }
}

static ScmObj
scm_cmpl_compile_empty(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                       bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&exp, &env, &next);

  *rdepth = -1;

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_push_inst_undef(next);
}

static ScmObj
scm_cmpl_compile_exp_list(ScmObj exp_lst, ScmObj env, ScmObj next, bool tail_p,
                          bool toplevel_p, ssize_t *rdepth)
{
  ScmObj exp_vec = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, code = SCM_OBJ_INIT;
  ssize_t rd, len;

  SCM_STACK_FRAME_PUSH(&exp_lst, &env, &next,
                       &exp_vec, &exp, &code);

  exp_vec = scm_api_list_to_vector(exp_lst);
  if (scm_obj_null_p(exp_vec)) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(exp_vec);
  if (len < 0) return SCM_OBJ_NULL;

  if (len == 0)
    return scm_cmpl_compile_empty(exp_lst, env, next,
                                  tail_p, toplevel_p, rdepth);

  *rdepth = -1;
  code = next;
  for (ssize_t i = len; i > 0; i--) {
    exp = scm_capi_vector_ref(exp_vec, (size_t)i - 1);
    if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

    code = scm_cmpl_compile_exp(exp, env, code, tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(code)) return SCM_OBJ_NULL;

    tail_p = false;

    if (rd > *rdepth) *rdepth = rd;
  }

  return code;
}

static int
scm_cmpl_decons_body(ScmObj body, ScmObj env, bool tail_p, bool toplevel_p,
                     scm_csetter_t *vars, scm_csetter_t *inits,
                     scm_csetter_t *exps)
{
  ScmObj var_stack = SCM_OBJ_INIT, init_stack = SCM_OBJ_INIT;
  ScmObj exps_stack = SCM_OBJ_INIT;
  ScmObj exp_lst = SCM_OBJ_INIT, exp = SCM_OBJ_INIT;
  ScmObj var = SCM_OBJ_INIT, init = SCM_OBJ_INIT;
  ScmObj vvec = SCM_OBJ_INIT, ivec = SCM_OBJ_INIT, evec = SCM_OBJ_INIT;
  ScmObj nil = SCM_OBJ_INIT;
  ssize_t len;
  int id, rslt;

  SCM_STACK_FRAME_PUSH(&body, &env,
                       &var_stack, &init_stack,
                       &exps_stack,
                       &exp_lst, &exp,
                       &var, &init,
                       &vvec, &ivec, &evec,
                       &nil);

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

      id = scm_cmpl_syntax_id(exp, env, tail_p, toplevel_p);
      if (id < 0) return -1;

      if (id == SCM_CMPL_SYNTAX_DEFINITION) {
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
      else if (id == SCM_CMPL_SYNTAX_BEGIN) {
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
    nil = scm_api_nil();
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
scm_cmpl_compile_body(ScmObj body, ScmObj env, ScmObj next, bool tail_p,
                      bool toplevel_p, ssize_t *rdepth)
{
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT, expls = SCM_OBJ_INIT;
  ScmObj exp = SCM_OBJ_INIT, exp_lst = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, nr_exps, rd;
  bool tl_p;
  int rslt;

  SCM_STACK_FRAME_PUSH(&body, &env, &next,
                       &vars, &inits, &expls,
                       &exp, &exp_lst, &new_env);

  rslt = scm_cmpl_decons_body(body, env, tail_p, toplevel_p,
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

    next = scm_cmpl_compile_exp_list(exp_lst, new_env, next,
                                     tl_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;

    tl_p = false;
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

      next = scm_cmpl_compile_exp(exp, new_env, next, false, toplevel_p, &rd);
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
scm_cmpl_compile_definition(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                            bool toplevel_p, ssize_t *rdepth)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &var, &val);

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

  next = scm_cmpl_push_inst_gdef(var, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(val, env, next, false, toplevel_p, rdepth);
}

static ScmObj
scm_cmpl_compile_reference(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                           bool toplevel_p, ssize_t *rdepth)
{
  ssize_t idx, layer;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next);

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

  if (idx >= 0)
    return scm_cmpl_push_inst_sref(idx, layer, next);
  else
    return scm_cmpl_push_inst_gref(exp, next);
}

static ScmObj
scm_cmpl_compile_self_eval(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                           bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&exp, &env, &next);

  *rdepth = -1;

  if (tail_p) {
    next = scm_cmpl_push_inst_return(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_push_inst_immval(exp, next);
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
scm_cmpl_compile_quote(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                       bool toplevel_p, ssize_t *rdepth)
{
  ScmObj obj = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &obj);

  rslt = scm_cmpl_decons_quote(exp, SCM_CSETTER_L(obj));
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_cmpl_compile_self_eval(obj, env, next, tail_p, toplevel_p, rdepth);
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
scm_cmpl_cmpl_application(ScmObj proc, ScmObj args,
                          ScmObj env, ScmObj next, bool tail_p, bool toplevel_p,
                          ssize_t *rdepth)
{
  ScmObj elm = SCM_OBJ_INIT;
  ssize_t nr_args, rd;

  SCM_STACK_FRAME_PUSH(&proc, &args, &env, &next,
                       &elm);

  nr_args = scm_capi_vector_length(args);
  if (nr_args < 0) return SCM_OBJ_NULL;

  if (tail_p)
    next = scm_cmpl_push_inst_tcall((scm_sword_t)nr_args, next);
  else
    next = scm_cmpl_push_inst_call((scm_sword_t)nr_args, next);

  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(proc, env, next, false, toplevel_p, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  for (ssize_t i = nr_args; i > 0; i--) {
    elm = scm_capi_vector_ref(args, (size_t)i - 1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_push(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(elm, env, next, false, toplevel_p, &rd);
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
scm_cmpl_compile_application(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                             bool toplevel_p, ssize_t *rdepth)
{
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &proc, &args);

  rslt = scm_cmpl_decons_application(exp,
                                     SCM_CSETTER_L(proc),
                                     SCM_CSETTER_L(args));
  if (rslt < 0) return SCM_OBJ_NULL;

  args = scm_api_list_to_vector(args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  return scm_cmpl_cmpl_application(proc, args,
                                   env, next, tail_p, toplevel_p, rdepth);
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
scm_cmpl_cmpl_closure_body(ScmObj body, ScmObj env, ScmObj next,
                           bool tail_p, bool toplevel_p, size_t nr_param,
                           ssize_t *rdepth)
{
  ScmObj body_code = SCM_OBJ_INIT;
  bool assigned;
  int rslt;

  SCM_STACK_FRAME_PUSH(&body, &env,
                       &body_code);

  body_code = scm_cmpl_compile_body(body, env, next,
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
scm_cmpl_cmpl_lambda(ScmObj params, bool vparam_p, ScmObj body,
                     ScmObj env, ScmObj next, bool tail_p, bool toplevel_p,
                     ssize_t *rdepth)
{
  ScmObj new_env = SCM_OBJ_INIT, body_code = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ssize_t nr_params;

  SCM_STACK_FRAME_PUSH(&params, &body, &env, &next,
                       &new_env, &body_code, &nil);

  nil = scm_api_nil();
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  nr_params = scm_capi_vector_length(params);
  if (nr_params < 0) return SCM_OBJ_NULL;

  if (nr_params > 0) {
    new_env = scm_cmpl_env_cons(params, vparam_p, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;
  }
  else
    new_env = env;

  body_code = scm_cmpl_cmpl_closure_body(body, new_env, nil,
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

  return scm_cmpl_push_inst_asm_close((*rdepth >= 0) ? *rdepth + 1 : 0,
                                      body_code, next);
}

static ScmObj
scm_cmpl_compile_lambda(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                        bool toplevel_p, ssize_t *rdepth)
{
  ScmObj formals = SCM_OBJ_INIT, body = SCM_OBJ_INIT, params = SCM_OBJ_INIT;
  bool vparam_p;

  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &formals, &body, &params);

  rslt = scm_cmpl_decons_lambda(exp,
                                SCM_CSETTER_L(formals), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  params = scm_cmpl_parse_lambda_formals(formals, &vparam_p);
  if (scm_obj_null_p(params)) return SCM_OBJ_NULL;

  return scm_cmpl_cmpl_lambda(params, vparam_p, body, env, next,
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
scm_cmpl_compile_assignment(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                            bool toplevel_p, ssize_t *rdepth)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;
  ssize_t idx, layer, rd;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &var, &val);

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

  if (idx >= 0)
    next = scm_cmpl_push_inst_sset(idx, layer, next);
  else
    next = scm_cmpl_push_inst_gset(var, next);

  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  *rdepth = (layer >= 0) ? layer : -1;

  next = scm_cmpl_compile_exp(val, env, next, false, toplevel_p, &rd);;
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
scm_cmpl_compile_if(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                    bool toplevel_p, ssize_t *rdepth)
{
  ScmObj cond = SCM_OBJ_INIT, conse = SCM_OBJ_INIT, alter = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &cond, &conse, &alter,
                       &lbl_junc, &lbl_alt);

  rslt = scm_cmpl_decons_if(exp, SCM_CSETTER_L(cond),
                            SCM_CSETTER_L(conse), SCM_CSETTER_L(alter));

  if (rslt < 0) return SCM_OBJ_NULL;

  if (!tail_p) {
    /* if 分岐後の合流地点のラベル定義を next 直前に追加 */
    lbl_junc = scm_cmpl_gen_label("if-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  *rdepth = -1;
  if (scm_obj_not_null_p(alter)) {
    /* alternative 節を付加 */
    next = scm_cmpl_compile_exp(alter, env, next, tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }
  else {
    next = scm_cmpl_compile_empty(alter, env, next, tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  if (rd > *rdepth) *rdepth = rd;

  /* condition 節実行後に alternative 節に条件ジャンプするためのラベル定義
     を追加 */
  lbl_alt = scm_cmpl_gen_label("if-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  if (!tail_p) {
    /* consequnece 節実行後に合流地点へジャンプする命令を追加 */
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;
  }

  /* consequence 節を付加 */
  next = scm_cmpl_compile_exp(conse, env, next, tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  /* condition 節が false value の場合に alternative 節直前にジャンプする
     命令の作成 */
  next = scm_cmpl_push_inst_jmpf(lbl_alt, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  /* conditio 節を付加 */
  next = scm_cmpl_compile_exp(cond, env, next, false, toplevel_p, &rd);

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
scm_cmpl_cmpl_cond_clause_exp(ScmObj exp, ScmObj label, ScmObj env, ScmObj next,
                              bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&exp, &env, &next);

  *rdepth = -1;

  if (!scm_capi_nil_p(label)) {
    next = scm_cmpl_push_inst_jmp(label, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (scm_capi_pair_p(exp)) {
    return scm_cmpl_compile_exp_list(exp, env, next,
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

    if (tail_p)
      next = scm_cmpl_push_inst_tcall(1, next);
    else
      next = scm_cmpl_push_inst_call(1, next);

    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(exp, env, next, false, toplevel_p, rdepth);
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
scm_cmpl_make_cond_clause_label(ScmObj labels, size_t idx, const char *prefix)
{
  ScmObj lbl = SCM_OBJ_INIT, ro = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&labels,
                       &lbl, &ro);

  lbl = scm_cmpl_gen_label(prefix);
  if (scm_obj_null_p(lbl)) return SCM_OBJ_NULL;

  ro = scm_capi_vector_set(labels, idx, lbl);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  return lbl;
}

static ScmObj
scm_cmpl_push_cond_clause(ScmObj exp, ScmObj junc, ScmObj labels, size_t idx,
                          ScmObj env, ScmObj next,
                          bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj lbl_cls = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &junc, &labels, &env, &next,
                       &lbl_cls);

  next = scm_cmpl_cmpl_cond_clause_exp(exp, junc, env, next,
                                       tail_p, toplevel_p, rdepth);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  lbl_cls = scm_cmpl_make_cond_clause_label(labels, idx, "cond-c");
  if (scm_obj_null_p(lbl_cls)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_cls, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  return next;
}

static ScmObj
scm_cmpl_cmpl_cond_clause_test(ScmObj test, ScmObj label,
                               ScmObj env, ScmObj next,
                               bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  SCM_STACK_FRAME_PUSH(&test, &label, &env, &next);

  if (!scm_capi_nil_p(label)) {
    next = scm_cmpl_push_inst_jmpt(label, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_cmpl_compile_exp(test, env, next, false, toplevel_p, rdepth);
}

static ScmObj
scm_cmpl_compile_cond(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                      bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_INIT, expss = SCM_OBJ_INIT;
  ScmObj texp = SCM_OBJ_INIT, eexp = SCM_OBJ_INIT;
  ScmObj labels = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_INIT, lbl_cls = SCM_OBJ_INIT;
  ScmObj lbl = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ssize_t nr_clauses, rd;
  bool else_exist_p;
  int rslt, nr_cls_exp_code;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &tests, &expss,
                       &texp, &eexp,
                       &labels, &lbl_junc, &lbl_cls,
                       &lbl, &nil);

  *rdepth = -1;

  nil = scm_api_nil();
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_decons_cond(exp, SCM_CSETTER_L(tests), SCM_CSETTER_L(expss),
                              &else_exist_p);
  if (rslt < 0) return SCM_OBJ_NULL;

  nr_clauses = scm_capi_vector_length(tests);
  if (nr_clauses < 0) return SCM_OBJ_NULL;

  if (nr_clauses == 0)
    return scm_cmpl_compile_empty(exp, env, next, tail_p, toplevel_p,rdepth);

  labels = scm_capi_make_vector((size_t)nr_clauses, nil);
  if (scm_obj_null_p(labels)) return SCM_OBJ_NULL;


  /*
   * cond 式評価後の合流地点のラベルを設定。cond が tail-expression の場合は、
   * 各節の <expression> をコンパイルしたコードの部分から直接 return するので
   * 合流地点のラベルは設定しない。
   */
  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label("cond-j");
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
      next = scm_cmpl_push_cond_clause(eexp, nil, labels,
                                       (size_t)nr_clauses - 1,
                                       env, next, tail_p, toplevel_p, &rd);
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

      next = scm_cmpl_push_cond_clause(eexp, lbl, labels, i - 1,
                                       env, next, tail_p, toplevel_p, &rd);
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
    next = scm_cmpl_cmpl_cond_clause_test(texp, lbl, env, next,
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
scm_cmpl_compile_and(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                     bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_NULL, texp = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_NULL;
  ssize_t len, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &tests, &texp, &lbl_junc);

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
    return scm_cmpl_push_inst_immval(scm_api_true(), next);

  if (len > 1) {
    lbl_junc = scm_cmpl_gen_label("and-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  texp = scm_capi_vector_ref(tests, (size_t)len - 1);
  if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(texp, env, next, tail_p, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  for (size_t i = (size_t)len - 1; i > 0; i--) {
    next = scm_cmpl_push_inst_jmpf(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    texp = scm_capi_vector_ref(tests, i - 1);
    if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(texp, env, next, false, toplevel_p, &rd);
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
scm_cmpl_compile_or(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                     bool toplevel_p, ssize_t *rdepth)
{
  ScmObj tests = SCM_OBJ_NULL, texp = SCM_OBJ_INIT, lbl_junc = SCM_OBJ_NULL;
  ssize_t len, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &tests, &texp, &lbl_junc);

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
    return scm_cmpl_push_inst_immval(scm_api_false(), next);

  if (len > 1) {
    lbl_junc = scm_cmpl_gen_label("or-j");
    if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

    next = scm_cmpl_push_inst_label(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  texp = scm_capi_vector_ref(tests, (size_t)len - 1);
  if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(texp, env, next, tail_p, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  for (size_t i = (size_t)len - 1; i > 0; i--) {
    next = scm_cmpl_push_inst_jmpt(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    texp = scm_capi_vector_ref(tests, i - 1);
    if (scm_obj_null_p(texp)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(texp, env, next, false, toplevel_p, &rd);
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
scm_cmpl_compile_when(ScmObj exp, ScmObj env, ScmObj next,
                      bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj test = SCM_OBJ_INIT, exps = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &test, &exps, &lbl_junc, &lbl_alt);

  rslt = scm_cmpl_decons_when(exp, SCM_CSETTER_L(test), SCM_CSETTER_L(exps));
  if (rslt < 0) return SCM_OBJ_NULL;

  *rdepth = -1;

  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label("when-j");
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

  lbl_alt = scm_cmpl_gen_label("when-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (!tail_p) {
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp_list(exps, env, next, tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_jmpf(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(test, env, next, false, toplevel_p, &rd);
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
scm_cmpl_compile_unless(ScmObj exp, ScmObj env, ScmObj next,
                        bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj test = SCM_OBJ_INIT, exps = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &test, &exps, &lbl_junc, &lbl_alt);

  rslt = scm_cmpl_decons_unless(exp, SCM_CSETTER_L(test), SCM_CSETTER_L(exps));
  if (rslt < 0) return SCM_OBJ_NULL;

  *rdepth = -1;

  if (!tail_p) {
    lbl_junc = scm_cmpl_gen_label("unless-j");
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

  lbl_alt = scm_cmpl_gen_label("unless-a");
  if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (!tail_p) {
    next = scm_cmpl_push_inst_jmp(lbl_junc, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  next = scm_cmpl_compile_exp_list(exps, env, next, tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_jmpt(lbl_alt, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(test, env, next, false, toplevel_p, &rd);
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

  if (syntax == SCM_CMPL_SYNTAX_LET && scm_capi_symbol_p(nao)) {
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
    case SCM_CMPL_SYNTAX_LET:
      scm_capi_error("Compiler: syntax error: malformed let", 0);
      break;
    }
    return -1;
  }

  return scm_cmpl_decons_let_bindings(bio, vars, inits);
}

static ScmObj
scm_cmpl_cmpl_named_let_body(ScmObj name,
                             ScmObj vars, ScmObj inits, ScmObj body,
                             ScmObj env , ScmObj next,
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

  SCM_STACK_FRAME_PUSH(&name, &vars, &inits, &body, &env, &next,
                       &letrec_vars, &new_env);

  if (!tail_p) {
    next = scm_cmpl_push_inst_epop(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  letrec_vars = scm_capi_make_vector(1, name);
  if (scm_obj_null_p(letrec_vars)) return SCM_OBJ_NULL;

  new_env = scm_cmpl_env_cons(letrec_vars, false, env);
  if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

  next = scm_cmpl_cmpl_application(name, vars, new_env,
                                   next, tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_demine(0, 0, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_cmpl_lambda(vars, false, body,
                              new_env, next, false, false, &rd);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  next = scm_cmpl_push_inst_emine(1, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  if (*rdepth >= 0) (*rdepth)--;

  return next;
}

static ScmObj
scm_cmpl_compile_let(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                     bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, bindings = SCM_OBJ_INIT, body = SCM_OBJ_INIT;
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT;
  ScmObj converted = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &name, &bindings, &body,
                       &vars, &inits, &ini_exp,
                       &converted, &new_env);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_SYNTAX_LET,
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
    next = scm_cmpl_cmpl_closure_body(body, new_env, next,
                                      tail_p, false, (size_t)nr_vars,
                                      rdepth);
  else
    next = scm_cmpl_cmpl_named_let_body(name, vars, inits, body,
                                        new_env, next, tail_p, false, rdepth);

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

      next = scm_cmpl_compile_exp(ini_exp, env, next, false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}

static ScmObj
scm_cmpl_compile_let_a(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                       bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, bindings = SCM_OBJ_INIT, body = SCM_OBJ_INIT;
  ScmObj vars = SCM_OBJ_INIT, inits = SCM_OBJ_INIT;
  ScmObj var = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  bool assigned;
  int rslt;


  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &name, &bindings, &body,
                       &vars, &inits,
                       &var,  &ini_exp, &new_env);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_SYNTAX_LET_A,
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

  next = scm_cmpl_cmpl_closure_body(body, new_env, next,
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

    next = scm_cmpl_compile_exp(ini_exp, new_env, next, false, false, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd >= i && rd - i > *rdepth)
       *rdepth = rd - i;

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}

static ScmObj
scm_cmpl_compile_letrec(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                        bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, body = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj inits = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &name, &body, &vars,
                       &inits, &ini_exp, &new_env);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_SYNTAX_LETREC,
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

  next = scm_cmpl_compile_body(body, new_env, next, tail_p, false, rdepth);
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

      next = scm_cmpl_compile_exp(ini_exp, new_env, next, false, false, &rd);
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
scm_cmpl_compile_letrec_a(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                          bool toplevel_p, ssize_t *rdepth)
{
  ScmObj name = SCM_OBJ_INIT, body = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj inits = SCM_OBJ_INIT, new_env = SCM_OBJ_INIT, ini_exp = SCM_OBJ_INIT;
  ssize_t nr_vars, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &name, &body, &vars,
                       &inits, &new_env, &ini_exp);

  rslt = scm_cmpl_decons_let(exp, SCM_CMPL_SYNTAX_LETREC_A,
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

  next = scm_cmpl_compile_body(body, new_env, next, tail_p, false, rdepth);
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

      next = scm_cmpl_compile_exp(ini_exp, new_env, next, false, false, &rd);
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
scm_cmpl_compile_begin(ScmObj exp, ScmObj env, ScmObj next,
                       bool tail_p, bool toplevel_p, ssize_t *rdepth)
{
  ScmObj exp_lst = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &exp_lst);

  rslt = scm_cmpl_decons_begin(exp, SCM_CSETTER_L(exp_lst));
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp_list(exp_lst, env, next,
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
scm_cmpl_compile_do(ScmObj exp, ScmObj env, ScmObj next,
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

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &vars, &inits, &steps,
                       &test, &exps, &cmds,
                       &ve, &ie, &se,
                       &lbl_start, &lbl_end, &new_env);

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

  next = scm_cmpl_compile_exp_list(exps, new_env, next, tail_p, false, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_end = scm_cmpl_gen_label("do-e");
  if (scm_obj_null_p(lbl_end)) return SCM_OBJ_NULL;

  next = scm_cmpl_push_inst_label(lbl_end, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  lbl_start = scm_cmpl_gen_label("do-s");
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

      next = scm_cmpl_compile_exp(se, new_env, next, false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (!scm_capi_nil_p(cmds)) {
    next = scm_cmpl_compile_exp_list(cmds, new_env, next, false, false, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  next = scm_cmpl_push_inst_jmpt(lbl_end, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(test, new_env, next, false, false, &rd);
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

      next = scm_cmpl_compile_exp(ie, env, next, false, false, &rd);
      if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

      if (rd > *rdepth) *rdepth = rd;
    }

    next = scm_cmpl_push_inst_eframe(next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
}

static ScmObj
scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                     bool toplevel_p, ssize_t *rdepth)
{
  int syntax_id;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next);

  syntax_id = scm_cmpl_syntax_id(exp, env, tail_p, toplevel_p);
  return scm_cmpl_compile_funcs[syntax_id](exp, env, next, tail_p,
                                           toplevel_p, rdepth);
}

ScmObj
scm_cmpl_compile(ScmObj exp)
{
  ScmObj env = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  ssize_t rdepth;

  SCM_STACK_FRAME_PUSH(&exp,
                       &env, &next);

  env = scm_cmpl_env_new();
  if (scm_obj_null_p(env)) return SCM_OBJ_NULL;

  next = scm_api_nil();
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(exp, env, next, false, true, &rdepth);
}

#ifdef SCM_UNIT_TEST

void
scm_cmpl_ut_clear_label_id(void)
{
  label_id = 0;
}

#endif


/*******************************************************/

int
scm_core_syntx_system_setup(void)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, rslt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &rslt);

  for (int i = 0; i < SCM_CMPL_NR_SYNTAX; i++) {
    if (scm_cmpl_syntax_keywords[i] == NULL) continue;

    sym = scm_capi_make_symbol_from_cstr(scm_cmpl_syntax_keywords[i],
                                         SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    syx = scm_api_make_syntax(i, scm_cmpl_syntax_keywords[i]);
    if (scm_obj_null_p(syx)) return -1;

    rslt = scm_api_global_var_define(sym, syx);
    if (scm_obj_null_p(rslt)) return -1;
  }

  return 0;
}

