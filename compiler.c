#include <stdio.h>
#include <stdbool.h>

#include "object.h"
#include "api.h"
#include "assembler.h"
#include "compiler.h"

/* XXX: マルチスレッド対応の場合には TLS にする */
static unsigned int label_id = 0;



static int scm_cmpl_find_free_and_assign_exp(ScmObj exp, ScmObj vars,
                                             ScmObj env, ScmObj rib,
                                             ScmObj free, ScmObj assign);
ScmObj scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj sv,
                            ScmObj next, bool tail_p);



static ScmObj
scm_cmpl_cons_inst_halt(void)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_HALT);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}

static ScmObj
scm_cmpl_cons_inst_call(scm_sword_t narg)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  scm_assert(narg >= 0);

  mne = scm_asm_mnemonic(SCM_OPCODE_CALL);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(narg);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_tcall(scm_sword_t narg, scm_sword_t cf_narg)
{
  ScmObj mne = SCM_OBJ_INIT, num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num1, &num2);

  scm_assert(narg >= 0);

  mne = scm_asm_mnemonic(SCM_OPCODE_TAIL_CALL);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num1 = scm_capi_make_number_from_sword(narg);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_capi_make_number_from_sword(cf_narg);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_capi_list(3, mne, num1, num2);
}

static ScmObj
scm_cmpl_cons_inst_return(scm_sword_t narg)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  scm_assert(narg >= 0);

  mne = scm_asm_mnemonic(SCM_OPCODE_RETURN);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(narg);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_frame(void)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_FRAME);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}

static ScmObj
scm_cmpl_cons_inst_immval(ScmObj val)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_IMMVAL);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, val);
}

static ScmObj
scm_cmpl_cons_inst_push(void)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_PUSH);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}

static ScmObj
scm_cmpl_cons_inst_gref(ScmObj sym)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_GREF);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, sym);
}

static ScmObj
scm_cmpl_cons_inst_gdef(ScmObj sym)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_GDEF);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, sym);
}

static ScmObj
scm_cmpl_cons_inst_gset(ScmObj sym)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_GSET);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, sym);
}

static ScmObj
scm_cmpl_cons_inst_sref(scm_sword_t idx)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(SCM_OPCODE_SREF);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(idx);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_sset(scm_sword_t idx)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(SCM_OPCODE_SSET);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(idx);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_cref(scm_sword_t idx)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(SCM_OPCODE_CREF);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(idx);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_cset(scm_sword_t idx)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(SCM_OPCODE_CSET);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(idx);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_jmp(ScmObj lbl)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_JMP);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, lbl);
}

static ScmObj
scm_cmpl_cons_inst_jmpf(ScmObj lbl)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_JMPF);

  return scm_capi_list(2, mne, lbl);
}

static ScmObj
scm_cmpl_cons_inst_raise(void)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_RAISE);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}

static ScmObj
scm_cmpl_cons_inst_box(scm_sword_t idx)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne, &num);

  mne = scm_asm_mnemonic(SCM_OPCODE_BOX);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(idx);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, num);
}

static ScmObj
scm_cmpl_cons_inst_unbox(void)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mne);

  mne = scm_asm_mnemonic(SCM_OPCODE_UNBOX);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(1, mne);
}


static ScmObj
scm_cmpl_cons_inst_label(ScmObj lbl)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lbl, &mne);

  mne = scm_asm_mnemonic(SCM_ASM_PI_LABEL);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, lbl);
}

static ScmObj
scm_cmpl_cons_inst_asm(ScmObj code)
{
  ScmObj mne = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&code, &mne);

  mne = scm_asm_mnemonic(SCM_ASM_PI_ASM);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  return scm_capi_list(2, mne, code);
}

static ScmObj
scm_cmpl_cons_inst_asm_close(scm_sword_t nr_free, ScmObj code)
{
  ScmObj mne = SCM_OBJ_INIT, num = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&code, &mne, &num);

  mne = scm_asm_mnemonic(SCM_ASM_PI_ASM_CLOSE);
  if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

  num = scm_capi_make_number_from_sword(nr_free);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_capi_list(3, mne, num, code);
}

static ScmObj
scm_cmpl_set_empty(void)
{
  return scm_api_nil();
}

static int
scm_cmpl_set_is_included(ScmObj set, ScmObj elm, bool *rslt)
{
  ScmObj node = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  bool r;

  SCM_STACK_FRAME_PUSH(&set, &elm, &node, &x);

  scm_assert(scm_obj_not_null_p(set));
  scm_assert(scm_obj_not_null_p(elm));

  for (node = set; scm_capi_pair_p(node); node = scm_api_cdr(node)) {
    x = scm_api_car(node);
    if (scm_obj_null_p(x)) return -1;

    r = scm_capi_eq_p(x, elm);
    if (r) {
      if (rslt != NULL) *rslt = true;
      return 0;
    }
  }

  if (scm_obj_null_p(node)) return -1;

  if (rslt != NULL) *rslt = false;

  return 0;
}

static ScmObj
scm_cmpl_set_add(ScmObj set, ScmObj elm)
{
  bool inc;
  int r;

  SCM_STACK_FRAME_PUSH(&set, &elm);

  scm_assert(scm_capi_nil_p(set) || scm_capi_pair_p(set));
  scm_assert(scm_obj_not_null_p(elm));

  r = scm_cmpl_set_is_included(set, elm, &inc);
  if (r < 0) return SCM_OBJ_NULL;

  if (inc)
    return set;
  else
    return scm_api_cons(elm, set);
}

static ScmObj
scm_cmpl_set_add_vec(ScmObj set, ScmObj vec)
{
  ScmObj elm = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&set, &vec, &elm);

  scm_assert(scm_capi_nil_p(set) || scm_capi_pair_p(set));
  scm_assert(scm_capi_vector_p(vec));

  len = scm_capi_vector_length(vec);
  if (len < 0) return SCM_OBJ_NULL;

  for (ssize_t n = 0; n < len; n++) {
    elm = scm_capi_vector_ref(vec, (size_t)n);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    set = scm_cmpl_set_add(set, elm);
    if (scm_obj_null_p(set)) return SCM_OBJ_NULL;
  }

  return set;
}

static ScmObj
scm_cmpl_set_union(ScmObj s1, ScmObj s2)
{
  ScmObj elm = SCM_OBJ_INIT, rest = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&s1, &s2, &elm, &rest);

  scm_assert(scm_capi_nil_p(s1) || scm_capi_pair_p(s1));
  scm_assert(scm_capi_nil_p(s2) || scm_capi_pair_p(s2));

  while  (!scm_capi_nil_p(s1)) {
    elm = scm_api_car(s1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    s1 = scm_api_cdr(s1);
    if (scm_obj_null_p(s1)) return SCM_OBJ_NULL;

    s2 = scm_cmpl_set_add(s2, elm);
  }

  return s2;
}

static ScmObj
scm_cmpl_set_minus(ScmObj s1, ScmObj s2)
{
  ScmObj set = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  bool inc;
  int rslt;

  SCM_STACK_FRAME_PUSH(&s1, &s2, &set, &elm);

  scm_assert(scm_capi_nil_p(s1) || scm_capi_pair_p(s1));
  scm_assert(scm_capi_nil_p(s2) || scm_capi_pair_p(s2));

  set = scm_cmpl_set_empty();
  if (scm_obj_null_p(set)) return SCM_OBJ_NULL;

  while  (!scm_capi_nil_p(s1)) {
    elm = scm_api_car(s1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    rslt = scm_cmpl_set_is_included(s2, elm, &inc);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (!inc) {
      set = scm_cmpl_set_add(set, elm);
      if (scm_obj_null_p(set)) return SCM_OBJ_NULL;
    }

    s1 = scm_api_cdr(s1);
  }

  return set;
}

static ScmObj
scm_cmpl_set_minus_vec(ScmObj set, ScmObj vec)
{
  ScmObj s = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  ssize_t len;
  bool inc;

  SCM_STACK_FRAME_PUSH(&set, &vec, &s, &elm);

  scm_assert(scm_capi_nil_p(set) || scm_capi_pair_p(set));
  scm_assert(scm_capi_vector_p(vec));

  s = scm_cmpl_set_empty();
  if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(vec);
  if (len < 0) return SCM_OBJ_NULL;

  while  (!scm_capi_nil_p(set)) {
    elm = scm_api_car(set);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    inc = false;
    for (ssize_t i = 0; i < len; i++) {
      x = scm_capi_vector_ref(vec, (size_t)i);
      if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

      if (scm_capi_eq_p(x, elm)) {
        inc = true;
        break;
      }
    }

    if (!inc) {
      s = scm_cmpl_set_add(s, elm);
      if (scm_obj_null_p(s)) return SCM_OBJ_NULL;
    }

    set = scm_api_cdr(set);
  }

  return s;
}

static ScmObj
scm_cmpl_set_intersect(ScmObj s1, ScmObj s2)
{
  ScmObj set = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, rest = SCM_OBJ_INIT;
  bool inc;
  int rslt;

  SCM_STACK_FRAME_PUSH(&s1, &s2, &set, &elm, &rest);


  scm_assert(scm_capi_nil_p(s1) || scm_capi_pair_p(s1));
  scm_assert(scm_capi_nil_p(s2) || scm_capi_pair_p(s2));

  set = scm_cmpl_set_empty();
  if (scm_obj_null_p(set)) return SCM_OBJ_NULL;

  while  (!scm_capi_nil_p(s1)) {
    elm = scm_api_car(s1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    rslt = scm_cmpl_set_is_included(s2, elm, &inc);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (inc) {
      set = scm_cmpl_set_add(set, elm);
      if (scm_obj_null_p(set)) return SCM_OBJ_NULL;
    }

    s1 =scm_api_cdr(s1);
  }

  return set;
}

static ScmObj
scm_cmpl_set_to_vec(ScmObj set)
{
  return scm_api_list_to_vector(set);
}

static ScmObj
scm_cmpl_env_cons(ScmObj bound, ScmObj free)
{
  scm_assert(scm_capi_vector_p(bound));
  scm_assert(scm_capi_vector_p(free));

  return scm_api_cons(bound, free);
}

static int
scm_cmpl_env_find_bound(ScmObj env, ScmObj var, ssize_t *idx)
{
  ScmObj bvs = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&env, &var, &bvs, &v);

  scm_assert(idx != NULL);

  bvs = scm_api_car(env);
  if (scm_obj_null_p(bvs)) return -1;

  len = scm_capi_vector_length(bvs);
  if (len < 0) return -1;

  for (ssize_t n = 0; n < len; n++) {
    v = scm_capi_vector_ref(bvs, (size_t)n);
    if (scm_obj_null_p(v)) return -1;

    if (scm_capi_eq_p(v, var)) {
      *idx = n;
      return 0;
    }
  }

  *idx = -1;
  return 0;
}

static ScmObj
scm_cmpl_env_bound_vars(ScmObj env)
{
  scm_assert(scm_capi_pair_p(env));

  return scm_api_car(env);
}

static ScmObj
scm_cmpl_env_free_vars(ScmObj env)
{
  scm_assert(scm_capi_pair_p(env));

  return scm_api_cdr(env);
}

static ssize_t
scm_cmpl_env_nr_bound_vars(ScmObj env)
{
  ScmObj bound = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env,
                       &bound);

  scm_assert(scm_capi_pair_p(env));

  bound = scm_cmpl_env_bound_vars(env);
  if (scm_obj_null_p(bound)) return -1;

  return scm_capi_vector_length(bound);
}

static ssize_t
scm_cmpl_env_nr_free_vars(ScmObj env)
{
  ScmObj freev = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env,
                       &freev);

  scm_assert(scm_capi_pair_p(env));

  freev = scm_cmpl_env_free_vars(env);
  if (scm_obj_null_p(freev)) return -1;

  return scm_capi_vector_length(freev);
}

static ScmObj
scm_cmpl_env_ref_bound_var(ScmObj env, size_t i)
{
  ScmObj bound = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env,
                       &bound);

  scm_assert(scm_capi_pair_p(env));

  bound = scm_cmpl_env_bound_vars(env);
  if (scm_obj_null_p(bound)) return SCM_OBJ_NULL;

  return scm_capi_vector_ref(bound, i);
}

static ScmObj
scm_cmpl_env_ref_free_var(ScmObj env, size_t i)
{
  ScmObj freev = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&env,
                       &freev);

  scm_assert(scm_capi_pair_p(env));

  freev = scm_cmpl_env_free_vars(env);
  if (scm_obj_null_p(freev)) return SCM_OBJ_NULL;

  return scm_capi_vector_ref(freev, i);
}

enum { SCM_CMPL_REF_GLOBAL, SCM_CMPL_REF_BOUND, SCM_CMPL_REF_FREE};

static int
scm_cmpl_env_resolv_ref(ScmObj env, ScmObj sym, int *type, size_t *idx)
{
  ScmObj var = SCM_OBJ_NULL;
  ssize_t nr_bound, nr_free;

  scm_assert(type != NULL);
  scm_assert(idx != NULL);

  SCM_STACK_FRAME_PUSH(&env, &sym,
                       &var);

  nr_bound = scm_cmpl_env_nr_bound_vars(env);
  if (nr_bound < 0) return -1;

  for (ssize_t i = 0; i < nr_bound; i++) {
    var = scm_cmpl_env_ref_bound_var(env, (size_t)i);
    if (scm_obj_null_p(var)) return -1;

    if (scm_capi_eq_p(var, sym)) {
      *type = SCM_CMPL_REF_BOUND;
      *idx = (size_t)i;
      return 0;
    }
  }

  nr_free = scm_cmpl_env_nr_free_vars(env);
  if (nr_free < 0) return -1;

  for (ssize_t i = 0; i < nr_free; i++) {
    var = scm_cmpl_env_ref_free_var(env, (size_t)i);
    if (scm_obj_null_p(var)) return -1;

    if (scm_capi_eq_p(var, sym)) {
      *type = SCM_CMPL_REF_FREE;
      *idx = (size_t)i;
      return 0;
    }
  }

  *type = SCM_CMPL_REF_GLOBAL;
  *idx = 0;

  return 0;
}

static inline ssize_t
scm_cmpl_bound_var_idx(ssize_t level, ssize_t idx)
{
  return level + idx;
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

static int
scm_cmpl_add_sym_into_free_if_needed(ScmObj sym, ScmObj env,
                                     ScmObj bound, ScmObj free)
{
  ScmObj set = SCM_OBJ_INIT;
  bool inc;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &env, &bound, &free,
                       &set);

  scm_assert(scm_capi_symbol_p(sym));

  rslt = scm_cmpl_set_is_included(bound, sym, &inc);
  if (rslt < 0) return -1;

  if (!inc) {
    ssize_t idx;

    rslt = scm_cmpl_env_find_bound(env, sym, &idx);
    if (rslt < 0) return -1;

    if (idx >= 0) {
      set = scm_api_car(free);
      if (scm_obj_null_p(set)) return -1;

      set = scm_cmpl_set_add(set, sym);
      if (scm_obj_null_p(set)) return -1;

      set = scm_api_set_car(free, set);
      if (scm_obj_null_p(set)) return -1;
    }
  }

  return 0;
}

static int
scm_cmpl_add_sym_into_assign_if_needed(ScmObj sym, ScmObj formal, ScmObj assign)
{
  ScmObj set = SCM_OBJ_INIT;
  bool inc;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &formal, &assign,
                       &set);

  scm_assert(scm_capi_symbol_p(sym));

  rslt = scm_cmpl_set_is_included(formal, sym, &inc);
  if (rslt < 0) return -1;

  if (inc) {
    set = scm_api_car(assign);
    if (scm_obj_null_p(set)) return -1;

    set = scm_cmpl_set_add(set, sym);
    if (scm_obj_null_p(set)) return -1;

    set = scm_api_set_car(assign, set);
    if (scm_obj_null_p(set)) return -1;
  }

  return 0;
}

static int
scm_cmpl_find_free_and_assign_exp_list(ScmObj exp_lst, ScmObj env,
                                       ScmObj bound, ScmObj formal,
                                       ScmObj free, ScmObj assign)
{
  ScmObj i = SCM_OBJ_INIT, exp = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp_lst, &env, &bound, &formal, &free, &assign,
                       &i, &exp);

  for (i = exp_lst; scm_capi_pair_p(i); i = scm_api_cdr(i)) {
    exp = scm_api_car(i);
    if (scm_obj_null_p(exp)) return -1;

    rslt = scm_cmpl_find_free_and_assign_exp(exp, env,
                                             bound, formal, free, assign);
    if (rslt < 0) return -1;
  }

  if (scm_obj_null_p(i)) {
    return -1;
  }
  else if (!scm_capi_nil_p(i)) {
    scm_capi_error("compile: syntax error: malformed expression list", 0);
    return -1;
  }

  return 0;
}

static ScmObj
scm_cmpl_compile_exp_list(ScmObj exp_lst, ScmObj env, ScmObj sv,
                          ScmObj next, bool tail_p)
{
  ScmObj exp_vec = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, code = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&exp_lst, &env, &sv, &next,
                       &exp_vec, &exp, &code);

  exp_vec = scm_api_list_to_vector(exp_lst);
  if (scm_obj_null_p(exp_vec)) return SCM_OBJ_NULL;

  len = scm_capi_vector_length(exp_vec);
  if (len < 0) return SCM_OBJ_NULL;

  code = next;
  for (ssize_t i = len; i > 0; i--) {
    exp = scm_capi_vector_ref(exp_vec, (size_t)i - 1);
    if (scm_obj_null_p(exp)) return SCM_OBJ_NULL;

    code = scm_cmpl_compile_exp(exp, env, sv, code, tail_p);
    if (scm_obj_null_p(code)) return SCM_OBJ_NULL;

    tail_p = false;
  }

  return code;
}

static int
scm_cmpl_find_free_and_assign_reference(ScmObj exp, ScmObj env,
                                        ScmObj bound, ScmObj formal,
                                        ScmObj free, ScmObj assign)
{
  return scm_cmpl_add_sym_into_free_if_needed(exp, env, bound, free);
}

static ScmObj
scm_cmpl_compile_reference(ScmObj exp, ScmObj env, ScmObj sv,
                           ScmObj next, bool tail_p)
{
  ScmObj inst_ref = SCM_OBJ_INIT, inst_unbox = SCM_OBJ_INIT;
  int type, rslt;
  size_t idx;
  ssize_t nr_bound, s_idx;
  bool inc;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &inst_ref, &inst_unbox);

  scm_assert(scm_capi_symbol_p(exp));

  rslt = scm_cmpl_env_resolv_ref(env, exp, &type, &idx);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (idx > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  switch (type) {
  case SCM_CMPL_REF_GLOBAL:
    inst_ref = scm_cmpl_cons_inst_gref(exp);
    break;
  case SCM_CMPL_REF_BOUND:
    nr_bound = scm_cmpl_env_nr_bound_vars(env);
    if (nr_bound < 0) return SCM_OBJ_NULL;

    s_idx = scm_cmpl_bound_var_idx(-nr_bound, (ssize_t)idx);
    if (s_idx < SCM_SWORD_MIN || SCM_SWORD_MAX < s_idx) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    inst_ref = scm_cmpl_cons_inst_sref((scm_sword_t)s_idx);
    break;
  case SCM_CMPL_REF_FREE:
    inst_ref = scm_cmpl_cons_inst_cref((scm_sword_t)idx);
    break;
  }

  if (scm_obj_null_p(inst_ref)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_set_is_included(sv, exp, &inc);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (inc) {
    inst_unbox = scm_cmpl_cons_inst_unbox();
    if (scm_obj_null_p(inst_unbox)) return SCM_OBJ_NULL;

    next = scm_api_cons(inst_unbox, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  return scm_api_cons(inst_ref, next);
}

static int
scm_cmpl_find_free_and_assign_self_eval(ScmObj exp, ScmObj env,
                                        ScmObj bound, ScmObj formal,
                                        ScmObj free, ScmObj assign)
{
  return 0;
}

static ScmObj
scm_cmpl_compile_self_eval(ScmObj exp, ScmObj env, ScmObj sv,
                           ScmObj next, bool tail_p)
{
  ScmObj inst_immval = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &inst_immval);

  inst_immval = scm_cmpl_cons_inst_immval(exp);
  if (scm_obj_null_p(inst_immval)) return SCM_OBJ_NULL;

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

static int
scm_cmpl_find_free_and_assign_quote(ScmObj exp, ScmObj env,
                                    ScmObj bound, ScmObj formal,
                                    ScmObj free, ScmObj assign)
{
  return 0;
}

static ScmObj
scm_cmpl_compile_quote(ScmObj exp, ScmObj env, ScmObj sv,
                       ScmObj next, bool tail_p)
{
  ScmObj obj = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &obj);

  rslt = scm_cmpl_decons_quote(exp, SCM_CSETTER_L(obj));
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_cmpl_compile_self_eval(obj, env, sv, next, tail_p);
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

static int
scm_cmpl_find_free_and_assign_application(ScmObj exp, ScmObj env,
                                          ScmObj bound, ScmObj formal,
                                          ScmObj free, ScmObj assign)
{
  ScmObj lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &bound, &formal, &free, &assign,
                       &lst, &elm);

  scm_assert(scm_capi_pair_p(exp));

  for (lst = exp; scm_capi_pair_p(lst); lst = scm_api_cdr(lst)) {
    elm = scm_api_car(lst);
    if (scm_obj_null_p(elm)) return -1;

    rslt = scm_cmpl_find_free_and_assign_exp(elm, env,
                                             bound, formal, free, assign);
    if (rslt < 0) return -1;
  }

  if (scm_obj_null_p(lst)) return -1;

  if (!scm_capi_nil_p(lst)) {
    scm_capi_error("compile: syntax error: malformed application", 0);
    return -1;
  }

  return 0;
}

static ScmObj
scm_cmpl_compile_application(ScmObj exp, ScmObj env, ScmObj sv,
                             ScmObj next, bool tail_p)
{
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj inst_push = SCM_OBJ_INIT, inst_call = SCM_OBJ_INIT;
  ScmObj inst_frame = SCM_OBJ_INIT;
  ssize_t nr_args;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
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

  if (tail_p) {
    ssize_t nr_lvars = scm_cmpl_env_nr_bound_vars(env);
    if (nr_lvars < 0) return SCM_OBJ_NULL;

    inst_call = scm_cmpl_cons_inst_tcall((scm_sword_t)nr_args,
                                         (scm_sword_t)nr_lvars);
  }
  else {
    inst_call = scm_cmpl_cons_inst_call((scm_sword_t)nr_args);
  }

  if (scm_obj_null_p(inst_call)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_call, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  next = scm_cmpl_compile_exp(proc, env, sv, next, false);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  inst_push = scm_cmpl_cons_inst_push();
  if (scm_obj_null_p(inst_push)) return SCM_OBJ_NULL;

  for (ssize_t i = nr_args; i > 0; i--) {
    elm = scm_capi_vector_ref(args, (size_t)i - 1);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    next = scm_api_cons(inst_push, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_cmpl_compile_exp(elm, env, sv, next, false);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

  if (!tail_p) {
    inst_frame = scm_cmpl_cons_inst_frame();
    if (scm_obj_null_p(inst_frame)) return SCM_OBJ_NULL;

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
scm_cmpl_parse_lambda_formals(ScmObj formals, bool *var_p)
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

  if (var_p != NULL) *var_p = variable;

  return v;
}

static int
scm_cmpl_find_free_and_assign_lambda_body(ScmObj body, ScmObj env,
                                          ScmObj bound, ScmObj formal,
                                          ScmObj free, ScmObj assign)
{
  scm_assert(scm_capi_nil_p(body) || scm_capi_pair_p(body));

  /* 内部 define 未対応 */
  return scm_cmpl_find_free_and_assign_exp_list(body, env,
                                                bound, formal, free, assign);
}

static int
scm_cmpl_find_free_and_assign_lambda(ScmObj exp, ScmObj env,
                                     ScmObj bound, ScmObj formal,
                                     ScmObj free, ScmObj assign)
{
  ScmObj form = SCM_OBJ_INIT, body = SCM_OBJ_INIT, vs = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &bound, &formal, &free, &assign,
                       &form, &body, &vs);

  rslt = scm_cmpl_decons_lambda(exp, SCM_CSETTER_L(form), SCM_CSETTER_L(body));
  if (rslt < 0) return -1;

  vs = scm_cmpl_parse_lambda_formals(form, NULL);
  if (scm_obj_null_p(vs)) return -1;

  bound = scm_cmpl_set_add_vec(bound, vs);
  if (scm_obj_null_p(bound)) return -1;

  formal = scm_cmpl_set_minus_vec(formal, vs);
  if (scm_obj_null_p(bound)) return -1;

  rslt = scm_cmpl_find_free_and_assign_lambda_body(body, env,
                                                   bound, formal, free, assign);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_cmpl_collect_free_and_assign(ScmObj body, ScmObj env, ScmObj lvars,
                                 scm_csetter_t *fv, scm_csetter_t *av)
{
  ScmObj bound = SCM_OBJ_INIT, formal = SCM_OBJ_INIT;
  ScmObj fvars = SCM_OBJ_INIT, avars = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&body, &env, &lvars,
                       &bound, &formal,
                       &fvars, &avars);

  bound = scm_cmpl_set_empty();
  if (scm_obj_null_p(bound)) return -1;

  bound = scm_cmpl_set_add_vec(bound, lvars);
  if (scm_obj_null_p(bound)) return -1;

  formal = scm_cmpl_set_empty();
  if (scm_obj_null_p(formal)) return -1;

  formal = scm_cmpl_set_add_vec(formal, lvars);
  if (scm_obj_null_p(formal)) return -1;

  fvars = scm_cmpl_set_empty();
  if (scm_obj_null_p(fvars)) return -1;

  fvars = scm_capi_list(1, fvars);
  if (scm_obj_null_p(fvars)) return -1;

  avars = scm_cmpl_set_empty();
  if (scm_obj_null_p(avars)) return -1;

  avars = scm_capi_list(1, avars);
  if (scm_obj_null_p(avars)) return -1;

  /* 内部 define 未対応 */
  rslt = scm_cmpl_find_free_and_assign_exp_list(body, env,
                                                bound, formal, fvars, avars);
  if (rslt < 0) return -1;

  fvars = scm_api_car(fvars);
  if (scm_obj_null_p(fvars)) return -1;

  avars = scm_api_car(avars);
  if (scm_obj_null_p(avars)) return -1;

  scm_csetter_setq(fv, fvars);
  scm_csetter_setq(av, avars);

  return 0;
}

static ScmObj
scm_cmpl_make_closure_code(ScmObj body, ScmObj new_env, ScmObj new_sv)
{
  ScmObj inst_ret = SCM_OBJ_INIT, inst_box = SCM_OBJ_INIT;
  ScmObj inst_close = SCM_OBJ_INIT;
  ScmObj body_code = SCM_OBJ_INIT;
  ScmObj elm = SCM_OBJ_INIT;
  ssize_t nr_lvars, nr_fvars;
  bool inc;
  int rslt;
  ssize_t nr_bound;

  SCM_STACK_FRAME_PUSH(&body, &new_env, &new_sv,
                       &inst_ret, &inst_box,
                       &inst_close,
                       &inst_close,
                       &body_code,
                       &elm);

  nr_lvars = scm_cmpl_env_nr_bound_vars(new_env);
  if (nr_lvars < 0)  return SCM_OBJ_NULL;

  inst_ret = scm_cmpl_cons_inst_return(nr_lvars);
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  inst_ret = scm_capi_list(1, inst_ret);
  if (scm_obj_null_p(inst_ret)) return SCM_OBJ_NULL;

  body_code = scm_cmpl_compile_exp_list(body,
                                        new_env, new_sv, inst_ret, true);
  if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;

  nr_bound = scm_cmpl_env_nr_bound_vars(new_env);
  if (nr_bound < 0) return SCM_OBJ_NULL;

  for (ssize_t i = 0; i < nr_lvars; i++) {
    elm = scm_cmpl_env_ref_bound_var(new_env, (size_t)i);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    rslt = scm_cmpl_set_is_included(new_sv, elm, &inc);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (inc) {
      ssize_t s_idx = scm_cmpl_bound_var_idx(-nr_bound, i);
      if (s_idx < SCM_SWORD_MIN || SCM_SWORD_MAX < s_idx) {
        scm_capi_error("Compiler: inner index overflow", 0);
        return SCM_OBJ_NULL;
      }

      inst_box = scm_cmpl_cons_inst_box(s_idx);
      if (scm_obj_null_p(inst_box)) return SCM_OBJ_NULL;

      body_code = scm_api_cons(inst_box, body_code);
      if (scm_obj_null_p(body_code)) return SCM_OBJ_NULL;
    }
  }

  nr_fvars = scm_cmpl_env_nr_free_vars(new_env);
  if (nr_fvars < 0) return SCM_OBJ_NULL;

  inst_close = scm_cmpl_cons_inst_asm_close(nr_fvars, body_code);
  if (scm_obj_null_p(inst_close)) return SCM_OBJ_NULL;

  return inst_close;
}

static ScmObj
scm_cmpl_new_env(ScmObj lvars, ScmObj fvars)
{
  ScmObj fvec = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lvars, &fvars,
                       &fvec);

  scm_assert(scm_capi_vector_p(lvars));

  fvec = scm_cmpl_set_to_vec(fvars);
  if (scm_obj_null_p(fvec)) return SCM_OBJ_NULL;

  return scm_cmpl_env_cons(lvars, fvec);
}

static ScmObj
scm_cmpl_new_empty_env(void)
{
  ScmObj lvars = SCM_OBJ_INIT, fvars = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lvars, &fvars);

  lvars = scm_capi_make_vector(0, SCM_OBJ_NULL);
  if (scm_obj_null_p(lvars)) return SCM_OBJ_NULL;

  fvars = scm_cmpl_set_empty();
  if (scm_obj_null_p(fvars)) return SCM_OBJ_NULL;

  return scm_cmpl_new_env(lvars, fvars);
}

static ScmObj
scm_cmpl_new_sv(ScmObj sv, ScmObj fvars, ScmObj avars)
{
  SCM_STACK_FRAME_PUSH(&sv, &fvars, &avars);

  sv = scm_cmpl_set_intersect(sv, fvars);
  if (scm_obj_null_p(sv)) return SCM_OBJ_NULL;

  return scm_cmpl_set_union(sv, avars);
}

static ScmObj
scm_cmpl_new_empty_sv(void)
{
  ScmObj empty = SCM_OBJ_INIT;

  empty = scm_cmpl_set_empty();

  return scm_cmpl_new_sv(empty, empty, empty);
}

static ScmObj
scm_cmpl_compile_lambda(ScmObj exp, ScmObj env, ScmObj sv,
                        ScmObj next, bool tail_p)
{
  ScmObj form = SCM_OBJ_INIT, body = SCM_OBJ_INIT, lvars = SCM_OBJ_INIT;
  ScmObj fvars = SCM_OBJ_INIT, avars = SCM_OBJ_INIT;
  ScmObj new_env = SCM_OBJ_INIT, new_sv = SCM_OBJ_INIT;
  ScmObj inst_close = SCM_OBJ_INIT, inst_ref = SCM_OBJ_INIT;
  ScmObj inst_push = SCM_OBJ_INIT;
  ScmObj fv = SCM_OBJ_INIT;
  bool variable_p;
  ssize_t nr_fvars;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &form, &body, &lvars,
                       &fvars, &avars,
                       &new_env, &new_sv,
                       &inst_close, &inst_ref,
                       &inst_push,
                       &fv);

  rslt = scm_cmpl_decons_lambda(exp,
                                SCM_CSETTER_L(form), SCM_CSETTER_L(body));
  if (rslt < 0) return SCM_OBJ_NULL;

  lvars = scm_cmpl_parse_lambda_formals(form, &variable_p);
  if (scm_obj_null_p(lvars)) return SCM_OBJ_NULL;

  rslt = scm_cmpl_collect_free_and_assign(body, env, lvars,
                                          SCM_CSETTER_L(fvars),
                                          SCM_CSETTER_L(avars));

  if (rslt < 0) return SCM_OBJ_NULL;

  new_env = scm_cmpl_new_env(lvars, fvars);
  if (scm_obj_null_p(new_env)) return SCM_OBJ_NULL;

  new_sv = scm_cmpl_new_sv(sv, fvars, avars);
  if (scm_obj_null_p(new_sv)) return SCM_OBJ_NULL;

  inst_close = scm_cmpl_make_closure_code(body, new_env, new_sv);
  if (scm_obj_null_p(inst_close)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_close, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  nr_fvars = scm_cmpl_env_nr_free_vars(new_env);
  if (nr_fvars < 0) return SCM_OBJ_NULL;

  inst_push = scm_cmpl_cons_inst_push();
  if (scm_obj_null_p(inst_push)) return SCM_OBJ_NULL;

  for (ssize_t i = nr_fvars; i > 0; i--) {
    int ref_type;
    size_t idx;
    ssize_t nr_bound, s_idx;

    fv = scm_cmpl_env_ref_free_var(new_env, (size_t)i - 1);
    if (scm_obj_null_p(fv)) return SCM_OBJ_NULL;

    rslt = scm_cmpl_env_resolv_ref(env, fv, &ref_type, &idx);
    if (rslt < 0) return SCM_OBJ_NULL;

    if (idx > SCM_SWORD_MAX) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    switch (ref_type) {
    case SCM_CMPL_REF_GLOBAL:
      scm_assert(false);         /* must not happen */
      break;
    case SCM_CMPL_REF_BOUND:
      nr_bound = scm_cmpl_env_nr_bound_vars(env);
      if (nr_bound < 0) return SCM_OBJ_NULL;

      s_idx = scm_cmpl_bound_var_idx(-nr_bound, (ssize_t)idx);
      if (s_idx < SCM_SWORD_MIN || SCM_SWORD_MAX < s_idx) {
        scm_capi_error("Compiler: inner index overflow", 0);
        return SCM_OBJ_NULL;
      }

      inst_ref = scm_cmpl_cons_inst_sref((scm_sword_t)s_idx);
      break;
    case SCM_CMPL_REF_FREE:
      inst_ref = scm_cmpl_cons_inst_cref((scm_sword_t)idx);
      break;
    }

    if (scm_obj_null_p(inst_ref)) return SCM_OBJ_NULL;

    next = scm_api_cons(inst_push, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

    next = scm_api_cons(inst_ref, next);
    if (scm_obj_null_p(next)) return SCM_OBJ_NULL;
  }

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

static int
scm_cmpl_find_free_and_assign_assignment(ScmObj exp, ScmObj env,
                                         ScmObj bound, ScmObj formal,
                                         ScmObj free, ScmObj assign)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &bound, &formal, &free, &assign,
                       &var, &val);

  rslt = scm_cmpl_decons_assignment(exp,
                                    SCM_CSETTER_L(var), SCM_CSETTER_L(val));
  if (rslt < 0) return -1;

  rslt = scm_cmpl_add_sym_into_free_if_needed(var, env, bound, free);
  if (rslt < 0) return -1;

  rslt = scm_cmpl_add_sym_into_assign_if_needed(var, formal, assign);
  if (rslt < 0) return -1;

  rslt = scm_cmpl_find_free_and_assign_exp(val, env, bound, formal,
                                           free, assign);
  if (rslt < 0) return -1;

  return 0;
}

static ScmObj
scm_cmpl_compile_assignment(ScmObj exp, ScmObj env, ScmObj sv,
                            ScmObj next, bool tail_p)
{
  ScmObj var = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ScmObj inst_set = SCM_OBJ_INIT;
  int rslt, type;
  size_t idx;
  ssize_t nr_bound, s_idx;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &var, &val);

  rslt = scm_cmpl_decons_assignment(exp,
                                    SCM_CSETTER_L(var), SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  rslt = scm_cmpl_env_resolv_ref(env, var, &type, &idx);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (idx > SCM_SWORD_MAX) {
    scm_capi_error("Compiler: inner index overflow", 0);
    return SCM_OBJ_NULL;
  }

  switch (type) {
  case SCM_CMPL_REF_GLOBAL:
    inst_set = scm_cmpl_cons_inst_gset(var);
    break;
  case SCM_CMPL_REF_BOUND:
    nr_bound = scm_cmpl_env_nr_bound_vars(env);
    if (nr_bound < 0) return SCM_OBJ_NULL;

    s_idx = scm_cmpl_bound_var_idx(-nr_bound, (ssize_t)idx);
    if (s_idx < SCM_SWORD_MIN || SCM_SWORD_MAX < s_idx) {
      scm_capi_error("Compiler: inner index overflow", 0);
      return SCM_OBJ_NULL;
    }

    inst_set = scm_cmpl_cons_inst_sset((scm_sword_t)s_idx);
    break;
  case SCM_CMPL_REF_FREE:
    inst_set = scm_cmpl_cons_inst_cset((scm_sword_t)idx);
    break;
  }

  if (scm_obj_null_p(inst_set)) return SCM_OBJ_NULL;

  next = scm_api_cons(inst_set, next);
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(val, env, sv, next, false);
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

static int
scm_cmpl_find_free_and_assign_if(ScmObj exp, ScmObj env,
                                 ScmObj bound, ScmObj formal,
                                 ScmObj free, ScmObj assign)
{
  ScmObj cond = SCM_OBJ_INIT, conse = SCM_OBJ_INIT, alter = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &bound, &formal, &free, &assign,
                       &cond, &conse, &alter);

  rslt = scm_cmpl_decons_if(exp, SCM_CSETTER_L(exp),
                            SCM_CSETTER_L(conse), SCM_CSETTER_L(alter));
  if (rslt < 0) return -1;

  rslt = scm_cmpl_find_free_and_assign_exp(cond, env,
                                           bound, formal, free, assign);
  if (rslt < 0) return -1;

  rslt = scm_cmpl_find_free_and_assign_exp(conse, env,
                                           bound, formal, free, assign);
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(alter)) {
    rslt = scm_cmpl_find_free_and_assign_exp(alter, env,
                                             bound, formal, free, assign);
    if (rslt < 0) return -1;
  }

  return 0;
}

static ScmObj
scm_cmpl_compile_if(ScmObj exp, ScmObj env, ScmObj sv, ScmObj next, bool tail_p)
{
  ScmObj cond = SCM_OBJ_INIT, conse = SCM_OBJ_INIT, alter = SCM_OBJ_INIT;
  ScmObj lbl_junc = SCM_OBJ_INIT, lbl_alt = SCM_OBJ_INIT;
  ScmObj jmp_junc = SCM_OBJ_INIT, jmpf = SCM_OBJ_INIT;
  ScmObj def_lbl = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
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

  if (scm_obj_not_null_p(alter)) {
    /* alternative 節を付加 */
    next = scm_cmpl_compile_exp(alter, env, sv, next, tail_p);
    if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

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
  next = scm_cmpl_compile_exp(conse, env, sv, next, tail_p);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  /* condition 節実行後の条件ジャンプ命令を付加 */
  next = scm_api_cons(jmpf, next);
  if (scm_obj_null_p(next)) return  SCM_OBJ_NULL;

  /* conditio 節を付加 */
  return scm_cmpl_compile_exp(cond, env, sv, next, false);
}


enum { SCM_CMPL_SYNTAX_REFERENCE, SCM_CMPL_SYNTAX_SELF_EVAL,
       SCM_CMPL_SYNTAX_QUOTE, SCM_CMPL_SYNTAX_APPLICATION,
       SCM_CMPL_SYNTAX_LAMBDA, SCM_CMPL_SYNTAX_ASSIGNMENT,
       SCM_CMPL_SYNTAX_IF, SCM_CMPL_NR_SYNTAX };

const char *scm_cmpl_syntax_keywords[] = { NULL, NULL, "quote", NULL, "lambda",
                                           "set!", "if" };
ScmObj (*scm_cmpl_compile_funcs[])(ScmObj exp, ScmObj env, ScmObj sv,
                                   ScmObj next, bool tail_p) = {
  scm_cmpl_compile_reference,
  scm_cmpl_compile_self_eval,
  scm_cmpl_compile_quote,
  scm_cmpl_compile_application,
  scm_cmpl_compile_lambda,
  scm_cmpl_compile_assignment,
  scm_cmpl_compile_if
};

int (*scm_cmpl_find_funcs[])(ScmObj exp, ScmObj env,
                                  ScmObj bound, ScmObj formal,
                                  ScmObj free, ScmObj assign) = {
  scm_cmpl_find_free_and_assign_reference,
  scm_cmpl_find_free_and_assign_self_eval,
  scm_cmpl_find_free_and_assign_quote,
  scm_cmpl_find_free_and_assign_application,
  scm_cmpl_find_free_and_assign_lambda,
  scm_cmpl_find_free_and_assign_assignment,
  scm_cmpl_find_free_and_assign_if
};

static int
scm_cmpl_find_free_and_assign_exp(ScmObj exp, ScmObj env,
                                  ScmObj bound, ScmObj formal,
                                  ScmObj free, ScmObj assign)
{
  ScmObj key = SCM_OBJ_INIT, syx = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env, &bound, &formal, &free, &assign,
                       &key, &syx);

  if (scm_capi_symbol_p(exp)) {
    return scm_cmpl_find_funcs[SCM_CMPL_SYNTAX_REFERENCE](exp, env,
                                                          bound, formal,
                                                          free, assign);
  }
  else if (scm_capi_pair_p(exp)) {
    int type, rslt;
    size_t idx;

    key = scm_api_car(exp);
    if (scm_obj_null_p(key)) return SCM_OBJ_NULL;

    if (scm_capi_symbol_p(key)) {
      rslt = scm_cmpl_env_resolv_ref(env, key, &type, &idx);
      if (rslt < 0) return SCM_OBJ_NULL;

      if (type == SCM_CMPL_REF_GLOBAL) {
        rslt = scm_capi_global_var_ref(key, SCM_CSETTER_L(syx));
        if (rslt < 0) return SCM_OBJ_NULL;

        if (scm_capi_syntax_p(syx)) {
          int id = scm_capi_syntax_id(syx);
          if (id < 0) return SCM_OBJ_NULL;

          return scm_cmpl_find_funcs[id](exp, env, bound, formal, free, assign);
        }
      }
    }

    return scm_cmpl_find_funcs[SCM_CMPL_SYNTAX_APPLICATION](exp, env,
                                                            bound, formal,
                                                            free, assign);
  }

  return scm_cmpl_find_funcs[SCM_CMPL_SYNTAX_SELF_EVAL](exp, env,
                                                        bound, formal,
                                                        free, assign);
}

ScmObj
scm_cmpl_compile_exp(ScmObj exp, ScmObj env, ScmObj sv,
                     ScmObj next, bool tail_p)
{
  ScmObj key = SCM_OBJ_INIT, syx = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp, &env, &sv, &next,
                       &key, &syx);

  if (scm_capi_symbol_p(exp)) {
    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_REFERENCE](exp, env,
                                                             sv, next, tail_p);
  }
  else if (scm_capi_pair_p(exp)) {
    int type, rslt;
    size_t idx;

    key = scm_api_car(exp);
    if (scm_obj_null_p(key)) return SCM_OBJ_NULL;

    if (scm_capi_symbol_p(key)) {
      rslt = scm_cmpl_env_resolv_ref(env, key, &type, &idx);
      if (rslt < 0) return SCM_OBJ_NULL;

      if (type == SCM_CMPL_REF_GLOBAL) {
        rslt = scm_capi_global_var_ref(key, SCM_CSETTER_L(syx));
        if (rslt < 0) return SCM_OBJ_NULL;

        if (scm_capi_syntax_p(syx)) {
          int id = scm_capi_syntax_id(syx);
          if (id < 0) return SCM_OBJ_NULL;

          return scm_cmpl_compile_funcs[id](exp, env, sv, next, tail_p);
        }
      }
    }

    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_APPLICATION](exp, env,
                                                               sv, next,
                                                               tail_p);
  }
  else {
    return scm_cmpl_compile_funcs[SCM_CMPL_SYNTAX_SELF_EVAL](exp, env,
                                                             sv, next, tail_p);
  }
}

ScmObj
scm_cmpl_compile(ScmObj exp)
{
  ScmObj env = SCM_OBJ_INIT, sv = SCM_OBJ_INIT;
  ScmObj next = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&exp,
                       &env, &sv,
                       &next);

  env = scm_cmpl_new_empty_env();
  if (scm_obj_null_p(env)) return SCM_OBJ_NULL;

  sv = scm_cmpl_set_empty();
  if (scm_obj_null_p(sv)) return SCM_OBJ_NULL;

  next = scm_api_nil();
  if (scm_obj_null_p(next)) return SCM_OBJ_NULL;

  return scm_cmpl_compile_exp(exp, env, sv, next, true);
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
