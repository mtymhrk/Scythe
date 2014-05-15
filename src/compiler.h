#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

typedef struct ScmCompilerRec ScmCompiler;

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))

#include "object.h"

struct ScmCompilerRec {
  ScmObjHeader header;
  int label_id;
  ScmObj module;
};

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

int scm_cmpl_initialize(ScmObj cmpl);
ScmObj scm_cmpl_new(SCM_MEM_TYPE_T mtype);
void scm_cmpl_select_module(ScmObj cmpl, ScmObj module);
ScmObj scm_cmpl_compile(ScmObj cmpl, ScmObj exp);

void scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_cmpl_current_module(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->module;
}

#ifdef SCM_UNIT_TEST

void scm_cmpl_ut_clear_label_id(void);

#endif


/**************************************************************************/
/* Compiler Framework and Helpers                                         */
/**************************************************************************/

/* functions to construct instractions */
ScmObj scm_cmpl_cons_inst_nop(void);
ScmObj scm_cmpl_push_inst_nop(ScmObj next);
ScmObj scm_cmpl_cons_inst_undef(void);
ScmObj scm_cmpl_push_inst_undef(ScmObj next);
ScmObj scm_cmpl_cons_inst_call(scm_sword_t narg);
ScmObj scm_cmpl_push_inst_call(scm_sword_t narg, ScmObj next);
ScmObj scm_cmpl_cons_inst_tcall(scm_sword_t narg);
ScmObj scm_cmpl_push_inst_tcall(scm_sword_t narg, ScmObj next);
ScmObj scm_cmpl_cons_inst_return(void);
ScmObj scm_cmpl_push_inst_return(ScmObj next);
ScmObj scm_cmpl_cons_inst_frame(void);
ScmObj scm_cmpl_push_inst_frame(ScmObj next);
ScmObj scm_cmpl_cons_inst_cframe(void);
ScmObj scm_cmpl_push_inst_cframe(ScmObj next);
ScmObj scm_cmpl_cons_inst_eframe(void);
ScmObj scm_cmpl_push_inst_eframe(ScmObj next);
ScmObj scm_cmpl_cons_inst_ecommit(scm_sword_t narg);
ScmObj scm_cmpl_push_inst_ecommit(scm_sword_t narg, ScmObj next);
ScmObj scm_cmpl_cons_inst_epop(void);
ScmObj scm_cmpl_push_inst_epop(ScmObj next);
ScmObj scm_cmpl_cons_inst_erebind(scm_sword_t narg);
ScmObj scm_cmpl_push_inst_erebind(scm_sword_t narg, ScmObj next);
ScmObj scm_cmpl_cons_inst_immval(ScmObj val);
ScmObj scm_cmpl_push_inst_immval(ScmObj val, ScmObj next);
ScmObj scm_cmpl_cons_inst_push(void);
ScmObj scm_cmpl_push_inst_push(ScmObj next);
ScmObj scm_cmpl_cons_inst_gref(ScmObj sym, ScmObj module);
ScmObj scm_cmpl_push_inst_gref(ScmObj sym, ScmObj module, ScmObj next);
ScmObj scm_cmpl_cons_inst_gdef(ScmObj sym, ScmObj module);
ScmObj scm_cmpl_push_inst_gdef(ScmObj sym, ScmObj module, ScmObj next);
ScmObj scm_cmpl_cons_inst_gset(ScmObj sym, ScmObj module);
ScmObj scm_cmpl_push_inst_gset(ScmObj sym, ScmObj module, ScmObj next);
ScmObj scm_cmpl_cons_inst_sref(scm_sword_t idx, scm_sword_t layer);
ScmObj scm_cmpl_push_inst_sref(scm_sword_t idx, scm_sword_t layer, ScmObj next);
ScmObj scm_cmpl_cons_inst_sset(scm_sword_t idx, scm_sword_t layer);
ScmObj scm_cmpl_push_inst_sset(scm_sword_t idx, scm_sword_t layer, ScmObj next);
ScmObj scm_cmpl_cons_inst_jmp(ScmObj lbl);
ScmObj scm_cmpl_push_inst_jmp(ScmObj lbl, ScmObj next);
ScmObj scm_cmpl_cons_inst_jmpt(ScmObj lbl);
ScmObj scm_cmpl_push_inst_jmpt(ScmObj lbl, ScmObj next);
ScmObj scm_cmpl_cons_inst_jmpf(ScmObj lbl);
ScmObj scm_cmpl_push_inst_jmpf(ScmObj lbl, ScmObj next);
ScmObj scm_cmpl_cons_inst_box(scm_sword_t idx, scm_sword_t layer);
ScmObj scm_cmpl_push_inst_box(scm_sword_t idx, scm_sword_t layer, ScmObj next);
ScmObj scm_cmpl_cons_inst_demine(scm_sword_t idx, scm_sword_t layer);
ScmObj scm_cmpl_push_inst_demine(scm_sword_t idx,
                                 scm_sword_t layer, ScmObj next);
ScmObj scm_cmpl_cons_inst_emine(scm_sword_t narg);
ScmObj scm_cmpl_push_inst_emine(scm_sword_t narg, ScmObj next);
ScmObj scm_cmpl_cons_inst_edemine(scm_sword_t narg, scm_sword_t layer);
ScmObj scm_cmpl_push_inst_edemine(scm_sword_t narg,
                                  scm_sword_t layer, ScmObj next);
ScmObj scm_cmpl_cons_inst_arity(scm_sword_t arity);
ScmObj scm_cmpl_push_inst_arity(scm_sword_t arity, ScmObj next);
ScmObj scm_cmpl_cons_inst_label(ScmObj lbl);
ScmObj scm_cmpl_push_inst_label(ScmObj lbl, ScmObj next);
ScmObj scm_cmpl_cons_inst_asm_close(scm_sword_t nr_free,
                                    scm_sword_t arity, ScmObj code);
ScmObj scm_cmpl_push_inst_asm_close(scm_sword_t nr_free, scm_sword_t arity,
                                    ScmObj code, ScmObj next);
ScmObj scm_cmpl_push_cont_arity_check(int arity, ScmObj next);

/* functions to operate a environment */
ScmObj scm_cmpl_env_new(void);
ScmObj scm_cmpl_env_cons(ScmObj vars, bool vparam, ScmObj env);
ScmObj scm_cmpl_env_outer(ScmObj env);
int scm_cmpl_env_vparam_flg(ScmObj env, size_t layer, bool *flg);
int scm_cmpl_env_assigned_flg(ScmObj env, size_t idx, size_t layer, bool *flg);
int scm_cmpl_env_resolv(ScmObj env, ScmObj sym, bool ssigned,
                        ssize_t *idx, ssize_t *layer);


ScmObj scm_cmpl_current_module_name(ScmObj cmpl);

ScmObj scm_cmpl_gen_label(ScmObj cmpl, const char *prefix);

ScmObj scm_cmpl_syntax(ScmObj cmpl, ScmObj exp, ScmObj env,
                       bool tail_p, bool toplevel_p);

ScmObj scm_cmpl_compile_exp(ScmObj cmpl, ScmObj exp, ScmObj env,
                            ScmObj next, int arity,
                            bool tail_p, bool toplevel_p, ssize_t *rdepth);



/**************************************************************************/
/*                                                                        */
/**************************************************************************/

int scm_cmpl_define_syntax(ScmObj module);


#endif /* INCLUDE_COMPILER_H__ */
