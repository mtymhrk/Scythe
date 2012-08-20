#include <stdio.h>
#include <stdbool.h>

#include "object.h"
#include "api.h"
#include "assembler.h"
#include "compiler.h"

/* XXX: マルチスレッド対応の場合には TLS にする */
static unsigned int label_id = 0;

ScmObj scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                            bool toplevel_p, ssize_t *rdepth);

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
scm_cmpl_cons_inst_call(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_CALL, narg);
}

static ScmObj
scm_cmpl_cons_inst_tcall(scm_sword_t narg)
{
  scm_assert(narg >= 0);

  return scm_cmpl_cons_inst_si(SCM_OPCODE_TAIL_CALL, narg);
}

static ScmObj
scm_cmpl_cons_inst_return(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_RETURN);
}

static ScmObj
scm_cmpl_cons_inst_frame(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_FRAME);
}

static ScmObj
scm_cmpl_cons_inst_cframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_CFRAME);
}

static ScmObj
scm_cmpl_cons_inst_eframe(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_EFRAME);
}

static ScmObj
scm_cmpl_cons_inst_immval(ScmObj val)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_IMMVAL, val);
}

static ScmObj
scm_cmpl_cons_inst_push(void)
{
  return scm_cmpl_cons_inst_noopd(SCM_OPCODE_PUSH);
}

static ScmObj
scm_cmpl_cons_inst_gref(ScmObj sym)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GREF, sym);
}

static ScmObj
scm_cmpl_cons_inst_gdef(ScmObj sym)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GDEF, sym);
}

static ScmObj
scm_cmpl_cons_inst_gset(ScmObj sym)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  return scm_cmpl_cons_inst_obj(SCM_OPCODE_GSET, sym);
}

static ScmObj
scm_cmpl_cons_inst_sref(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SREF, idx, layer);
}

static ScmObj
scm_cmpl_cons_inst_sset(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_SSET, idx, layer);
}

static ScmObj
scm_cmpl_cons_inst_jmp(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMP, lbl);
}

static ScmObj
scm_cmpl_cons_inst_jmpf(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_OPCODE_JMPF, lbl);
}

static ScmObj
scm_cmpl_cons_inst_box(scm_sword_t idx, scm_sword_t layer)
{
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  return scm_cmpl_cons_inst_si_si(SCM_OPCODE_BOX, idx, layer);
}

static ScmObj
scm_cmpl_cons_inst_label(ScmObj lbl)
{
  return scm_cmpl_cons_inst_obj(SCM_ASM_PI_LABEL, lbl);
}

static ScmObj
scm_cmpl_cons_inst_asm_close(scm_sword_t nr_free, ScmObj code)
{
  scm_assert(nr_free >= 0);

  return scm_cmpl_cons_inst_si_obj(SCM_ASM_PI_ASM_CLOSE, nr_free, code);
}

static ScmObj
scm_cmpl_env_new(void)
{
  return scm_api_nil();
}

static ScmObj
scm_cmpl_env_cons(ScmObj vars, bool vparam, ScmObj env)
{
  ScmObj rib = SCM_OBJ_INIT, rebound = SCM_OBJ_INIT, fls = SCM_OBJ_INIT;
  ScmObj vp_flg = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&vars, &env,
                       &rib, &rebound, &fls,
                       &vp_flg);

  scm_assert(scm_capi_pair_p(vars) || scm_capi_vector_p(vars));
  scm_assert(scm_capi_nil_p(env) || scm_capi_pair_p(env));

  if (scm_capi_pair_p(vars)) {
    vars = scm_api_list_to_vector(vars);
    if (scm_obj_null_p(vars)) return SCM_OBJ_NULL;
  }

  len = scm_capi_vector_length(vars);
  if (len < 0) return SCM_OBJ_NULL;

  fls = scm_api_bool_false();
  if (scm_obj_null_p(fls)) return SCM_OBJ_NULL;

  rebound = scm_capi_make_vector((size_t)len, fls);
  if (scm_obj_null_p(rebound)) return SCM_OBJ_NULL;

  vp_flg = vparam ? scm_api_bool_true() : scm_api_bool_false();
  if (scm_obj_null_p(vp_flg)) return SCM_OBJ_NULL;

  rib = scm_capi_list(3, vars, rebound, vp_flg);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  rib = scm_api_list_to_vector(rib);
  if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

  return scm_api_cons(rib, env);
}

static int
scm_cmpl_env_rebound_flg(ScmObj env, size_t idx, size_t layer, bool *flg)
{
  ScmObj itr = SCM_OBJ_INIT, rib = SCM_OBJ_INIT, rebound = SCM_OBJ_INIT;
  ScmObj fo = SCM_OBJ_INIT;
  size_t i;

  SCM_STACK_FRAME_PUSH(&env,
                       &itr, &rib, &rebound,
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

  rebound = scm_capi_vector_ref(rib, 1);
  if (scm_obj_null_p(rebound)) return -1;

  fo = scm_capi_vector_ref(rebound, idx);
  if (scm_obj_null_p(fo)) return -1;

  *flg = scm_capi_true_p(fo);

  return 0;
}

static int
scm_cmpl_env_resolv(ScmObj env, ScmObj sym, bool rebound,
                    ssize_t *idx, ssize_t *layer)
{
  ScmObj itr = SCM_OBJ_INIT, rib = SCM_OBJ_INIT, vars = SCM_OBJ_INIT;
  ScmObj rb_flgs = SCM_OBJ_INIT,  var = SCM_OBJ_INIT, tr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env, &sym,
                       &itr, &rib, &vars,
                       &rb_flgs, &var, &tr);

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
        if (rebound) {

          tr = scm_api_bool_true();
          if (scm_obj_null_p(tr)) return -1;

          rb_flgs = scm_capi_vector_ref(rib, 1);
          if (scm_obj_null_p(rb_flgs)) return -1;

          tr = scm_capi_vector_set(rb_flgs, (size_t)*idx, tr);
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
scm_cmpl_gen_label(const char *prefix)
{
  char str[256];

  if (prefix != NULL)
    snprintf(str, sizeof(str), "lbl_%s_%u", prefix, label_id++);
  else
    snprintf(str, sizeof(str), "lbl_%u", label_id++);

  return scm_capi_make_symbol_from_cstr(str, SCM_ENC_ASCII);
}

static ScmObj
scm_cmpl_compile_exp_list(ScmObj exp_lst, ScmObj env, ScmObj next, bool tail_p,
                          bool toplevel_p, ssize_t *rdepth)
{
  ScmObj exp_vec = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, code = SCM_OBJ_INIT;
  ssize_t rd;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&exp_lst, &env, &next,
                       &exp_vec, &exp, &code);

  exp_vec = scm_api_list_to_vector(exp_lst);
  if (scm_obj_null_p(exp_vec)) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(exp_vec);
  if (len < 0) return SCM_OBJ_NULL;

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
scm_cmpl_decons_definision(ScmObj exp, scm_csetter_t *var, scm_csetter_t *val)
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
scm_cmpl_normalize_definizion(ScmObj exp)
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
scm_cmpl_compile_definision(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                            bool toplevel_p, ssize_t *rdepth)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ScmObj inst_gdef = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &var, &val,
                       &inst_gdef);

  exp = scm_cmpl_normalize_definizion(exp);
  if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_decons_definision(exp,
                                    SCM_CSETTER_L(var), SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (!scm_capi_symbol_p(var)) {
    scm_capi_error("Compiler: syntax error: malformed define", 0);
    return SCM_OBJ_NULL;
  }

  if (!toplevel_p) {
    scm_capi_error("Compiler: internal definition is not supported", 0);
    return SCM_OBJ_NULL;
  }

  inst_gdef = scm_cmpl_cons_inst_gdef(var);
  if (scm_obj_null_p(inst_gdef)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_gdef, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(val, env, next, false, toplevel_p, rdepth);
}

static ScmObj
scm_cmpl_compile_reference(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                           bool toplevel_p, ssize_t *rdepth)
{
  ScmObj inst_ref = SCM_OBJ_INIT;
  ssize_t idx, layer;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &inst_ref);

  scm_assert(scm_capi_symbol_p(exp));

  rslt = scm_cmpl_env_resolv(env, exp, false, &idx, &layer);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (idx > SCM_SWORD_MAX || layer > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  if (idx >= 0)
    inst_ref = scm_cmpl_cons_inst_sref(idx, layer);
  else
    inst_ref = scm_cmpl_cons_inst_gref(exp);

  if (scm_obj_null_p(inst_ref)) return SCM_OBJ_NULL;

  *rdepth = (layer >= 0) ? layer : -1;

  return scm_api_cons(inst_ref, next);
}

static ScmObj
scm_cmpl_compile_self_eval(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                           bool toplevel_p, ssize_t *rdepth)
{
  ScmObj inst_immval = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &inst_immval);

  inst_immval = scm_cmpl_cons_inst_immval(exp);
  if (scm_obj_null_p(inst_immval)) return SCM_OBJ_NULL;

  *rdepth = -1;
  return scm_api_cons(inst_immval, next);
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
scm_cmpl_compile_application(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                             bool toplevel_p, ssize_t *rdepth)
{
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj inst_push = SCM_OBJ_INIT, inst_call = SCM_OBJ_INIT;
  ScmObj inst_frame = SCM_OBJ_INIT;
  ssize_t nr_args, rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &proc, &args, &elm,
                       &inst_push, &inst_call,
                       &inst_frame);

  rslt = scm_cmpl_decons_application(exp,
                                     SCM_CSETTER_L(proc),
                                     SCM_CSETTER_L(args));
  if (rslt < 0) return SCM_OBJ_NULL;

  args = scm_api_list_to_vector(args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  nr_args = scm_capi_vector_length(args);
  if (nr_args < 0) return SCM_OBJ_NULL;

  if (tail_p)
    inst_call = scm_cmpl_cons_inst_tcall((scm_sword_t)nr_args);
  else
    inst_call = scm_cmpl_cons_inst_call((scm_sword_t)nr_args);

  if (scm_obj_null_p(inst_call)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_call, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(proc, env, next, false, toplevel_p, rdepth);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  inst_push = scm_cmpl_cons_inst_push();
  if (scm_obj_null_p(inst_push)) return SCM_OBJ_NULL;

  for (ssize_t i = nr_args; i > 0; i--) {
    elm = scm_capi_vector_ref(args, (size_t)i - 1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    next = scm_api_cons(inst_push, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(elm, env, next, false, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;
  }

  inst_frame = SCM_OBJ_NULL;
  if (!tail_p) {
    inst_frame = (nr_args > 0) ?
      scm_cmpl_cons_inst_frame() : scm_cmpl_cons_inst_cframe();
    if (scm_obj_null_p(inst_frame)) return SCM_OBJ_NULL;
  }
  else if (nr_args > 0) {
    inst_frame = scm_cmpl_cons_inst_eframe();
    if (scm_obj_null_p(inst_frame)) return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(inst_frame)) {
    next = scm_api_cons(inst_frame, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return next;
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
scm_cmpl_make_closure_code(ScmObj body, ScmObj env,
                           size_t nr_param, ssize_t *rdepth)
{
  ScmObj inst_ret = SCM_OBJ_INIT, inst_box = SCM_OBJ_INIT;
  ScmObj inst_close = SCM_OBJ_INIT;
  ScmObj body_code = SCM_OBJ_INIT;
  scm_sword_t nr_env;
  bool rebound;
  int rslt;

  SCM_STACK_FRAME_PUSH(&body, &env,
                       &inst_ret, &inst_box,
                       &inst_close,
                       &body_code);

  inst_ret = scm_cmpl_cons_inst_return();
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  inst_ret = scm_capi_list(1, inst_ret);
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  /* XXX: クロージャの本体最後が tail-call であっても本体のアセンブラコードの最
   *      後には無条件で return 命令が付与される。 */
  body_code = scm_cmpl_compile_exp_list(body, env, inst_ret,
                                        true, false, rdepth);
  if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;

  for (size_t i = 0; i < nr_param; i++) {
    rslt = scm_cmpl_env_rebound_flg(env, i, 0, &rebound);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (rebound) {
      inst_box = scm_cmpl_cons_inst_box((scm_sword_t)i, 0);
      if (scm_obj_null_p(inst_box)) return SCM_OBJ_NULL;

      body_code = scm_api_cons(inst_box, body_code);
      if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;
    }
  }

  if (*rdepth > SCM_SWORD_MAX - 1) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  if (*rdepth >= 0)
    nr_env = (nr_param > 0) ? *rdepth : *rdepth + 1;
  else
    nr_env = 0;

  return scm_cmpl_cons_inst_asm_close(nr_env, body_code);
}

static ScmObj
scm_cmpl_compile_lambda(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                        bool toplevel_p, ssize_t *rdepth)
{
  ScmObj formals = SCM_OBJ_INIT, body = SCM_OBJ_INIT, params = SCM_OBJ_INIT;
  ScmObj new_env = SCM_OBJ_INIT;
  ScmObj inst_close = SCM_OBJ_INIT, inst_ref = SCM_OBJ_INIT;
  ssize_t nr_params;
  bool vparam_p;

  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &formals, &body, &params,
                       &new_env,
                       &inst_close, &inst_ref);

  rslt = scm_cmpl_decons_lambda(exp,
                                SCM_CSETTER_L(formals), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  params = scm_cmpl_parse_lambda_formals(formals, &vparam_p);
  if (scm_obj_null_p(params)) return SCM_OBJ_NULL;

  nr_params = scm_capi_vector_length(params);
  if (nr_params < 0) return SCM_OBJ_NULL;

  if (nr_params > 0) {
    new_env = scm_cmpl_env_cons(params, vparam_p, env);
    if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;
  }
  else
    new_env = env;

  inst_close = scm_cmpl_make_closure_code(body, new_env,
                                          (size_t)nr_params, rdepth);
  if (scm_obj_null_p(inst_close)) return SCM_OBJ_NULL;

  if (nr_params > 0 && *rdepth >= 0) (*rdepth)--;

  next = scm_api_cons(inst_close, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return next;
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
  ScmObj inst_set = SCM_OBJ_INIT;
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

  if (idx >= 0)
    inst_set = scm_cmpl_cons_inst_sset(idx, layer);
  else
    inst_set = scm_cmpl_cons_inst_gset(var);

  if (scm_obj_null_p(inst_set)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_set, next);
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
  ScmObj jmp_junc = SCM_OBJ_INIT, jmpf = SCM_OBJ_INIT;
  ScmObj def_lbl = SCM_OBJ_INIT;
  ssize_t rd;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &cond, &conse, &alter,
                       &lbl_junc, &lbl_alt,
                       &jmp_junc, &jmpf,
                       &def_lbl);

  rslt = scm_cmpl_decons_if(exp, SCM_CSETTER_L(cond),
                            SCM_CSETTER_L(conse), SCM_CSETTER_L(alter));

  if (rslt < 0) return SCM_OBJ_NULL;

  /* if 分岐後の合流地点のラベル定義を next 直前に追加 */
  lbl_junc = scm_cmpl_gen_label("if-j");
  if (scm_obj_null_p(lbl_junc)) return SCM_OBJ_NULL;

  def_lbl = scm_cmpl_cons_inst_label(lbl_junc);
  if (scm_obj_null_p(def_lbl)) return SCM_OBJ_NULL;

  next = scm_api_cons(def_lbl, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  *rdepth = -1;
  if (scm_obj_not_null_p(alter)) {
    /* alternative 節を付加 */
    next = scm_cmpl_compile_exp(alter, env, next, tail_p, toplevel_p, &rd);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    if (rd > *rdepth) *rdepth = rd;

    /* condition 節実行後に alternative 節に条件ジャンプするためのラベル定義
       を追加 */
    lbl_alt = scm_cmpl_gen_label("if-a");
    if (scm_obj_null_p(lbl_alt)) return SCM_OBJ_NULL;

    def_lbl = scm_cmpl_cons_inst_label(lbl_alt);
    if (scm_obj_null_p(def_lbl)) return SCM_OBJ_NULL;

    next = scm_api_cons(def_lbl, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    /* consequnece 節実行後に合流地点へジャンプする命令を追加 */
    jmp_junc = scm_cmpl_cons_inst_jmp(lbl_junc);
    if (scm_obj_null_p(jmp_junc)) return  SCM_OBJ_NULL;

    next = scm_api_cons(jmp_junc, next);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

    /* condition 節が false value の場合に alternative 節直前にジャンプする
       命令の作成 */
    jmpf = scm_cmpl_cons_inst_jmpf(lbl_alt);
    if (scm_obj_null_p(jmpf)) return  SCM_OBJ_NULL;
  }
  else {
    /* condition 節が false value の場合に合流地点にジャンプする命令の作成 */
    jmpf = scm_cmpl_cons_inst_jmpf(lbl_junc);
    if (scm_obj_null_p(jmpf)) return  SCM_OBJ_NULL;
  }

  /* consequence 節を付加 */
  next = scm_cmpl_compile_exp(conse, env, next, tail_p, toplevel_p, &rd);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  if (rd > *rdepth) *rdepth = rd;

  /* condition 節実行後の条件ジャンプ命令を付加 */
  next = scm_api_cons(jmpf, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  /* conditio 節を付加 */
  next = scm_cmpl_compile_exp(cond, env, next, false, toplevel_p, &rd);

  if (rd > *rdepth) *rdepth = rd;

  return next;
}


enum { SCM_CMPL_DEFINITION, SCM_CMPL_SYNTAX_REFERENCE,
       SCM_CMPL_SYNTAX_SELF_EVAL, SCM_CMPL_SYNTAX_QUOTE,
       SCM_CMPL_SYNTAX_APPLICATION, SCM_CMPL_SYNTAX_LAMBDA,
       SCM_CMPL_SYNTAX_ASSIGNMENT, SCM_CMPL_SYNTAX_IF,
       SCM_CMPL_NR_SYNTAX };

const char *scm_cmpl_syntax_keywords[] = { "define", NULL, NULL, "quote", NULL,
                                           "lambda", "set!", "if" };
ScmObj (*scm_cmpl_compile_funcs[])(ScmObj exp, ScmObj env, ScmObj next,
                                   bool tail_p, bool toplevel_p,
                                   ssize_t *rdepth)
= {
  scm_cmpl_compile_definision,
  scm_cmpl_compile_reference,
  scm_cmpl_compile_self_eval,
  scm_cmpl_compile_quote,
  scm_cmpl_compile_application,
  scm_cmpl_compile_lambda,
  scm_cmpl_compile_assignment,
  scm_cmpl_compile_if,
};

ScmObj
scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj next, bool tail_p,
                     bool toplevel_p, ssize_t *rdepth)
{
  ScmObj key = SCM_OBJ_INIT, syx = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env, &next,
                       &key, &syx);

  if (scm_capi_symbol_p(exp)) {
    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_REFERENCE](exp, env,
                                                             next,
                                                             tail_p,
                                                             toplevel_p,
                                                             rdepth);
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
        rslt = scm_capi_global_var_ref(key, SCM_CSETTER_L(syx));
        if (rslt < 0) return SCM_OBJ_NULL;

        if (scm_capi_syntax_p(syx)) {
          int id = scm_capi_syntax_id(syx);
          if (id < 0) return SCM_OBJ_NULL;

          return scm_cmpl_compile_funcs[id](exp, env, next, tail_p,
                                            toplevel_p, rdepth);
        }
      }
    }

    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_APPLICATION](exp, env,
                                                               next,
                                                               tail_p,
                                                               toplevel_p,
                                                               rdepth);
  }
  else {
    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_SELF_EVAL](exp, env,
                                                             next,
                                                             tail_p,
                                                             toplevel_p,
                                                             rdepth);
  }
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
