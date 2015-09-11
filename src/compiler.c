#include <stdio.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/assembler.h"
#include "scythe/compiler.h"


/*************************************************************************/
/* Compiler                                                              */
/*************************************************************************/

ScmTypeInfo SCM_COMPILER_TYPE_INFO = {
  .name                            = "compiler",
  .flags                           = SCM_TYPE_FLG_MMO,
  .obj_print_func                  = NULL,
  .obj_size                        = sizeof(ScmCompiler),
  .gc_ini_func                     = scm_cmpl_gc_initialize,
  .gc_fin_func                     = NULL,
  .gc_accept_func                  = scm_cmpl_gc_accept,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

int
scm_cmpl_initialize(ScmObj cmpl, ScmObj env)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(env));

  SCM_SLOT_SETQ(ScmCompiler, cmpl, env, env);
  SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, SCM_NIL_OBJ);

  return 0;
}

void
scm_cmpl_set_env(ScmObj cmpl, ScmObj env)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(env));

  SCM_SLOT_SETQ(ScmCompiler, cmpl, env, env);
}

void
scm_cmpl_set_expr(ScmObj cmpl, ScmObj expr)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (scm_obj_null_p(expr))
    SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, SCM_NIL_OBJ);
  else
    SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, expr);
}

void
scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(obj)->env = SCM_OBJ_NULL;
  SCM_COMPILER(obj)->expr = SCM_OBJ_NULL;
}

int
scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->env, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->expr, mem);
}


/*************************************************************************/
/* Compiler (interface)                                                  */
/*************************************************************************/

static ScmObj
norm_cmpl_arg_mod(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&mod,
                      &name);

  if (scm_fcd_module_p(mod))
    return mod;

  if (scm_obj_null_p(mod)) {
    name = scm_fcd_make_symbol_from_cstr("main", SCM_ENC_SRC);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;

    name = scm_fcd_cons(name, SCM_NIL_OBJ);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
  }
  else if (scm_fcd_symbol_p(mod)) {
    name = scm_fcd_cons(mod, SCM_NIL_OBJ);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
  }
  else if (scm_fcd_pair_p(mod)) {
    name = mod;
  }
  else {
    scm_fcd_error("no such a module", 1, mod);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_find_module(name, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_fcd_error("no such a module", 1, mod);
    return SCM_OBJ_NULL;
  }

  return mod;
}

extern inline bool
scm_fcd_compiler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_COMPILER_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_compiler_P(ScmObj obj)
{
  return scm_fcd_compiler_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_compiler_new(scm_mem_type_t mtype, ScmObj env)
{
  ScmObj cmpl = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&env,
                      &cmpl);

  if (scm_obj_null_p(env)) {
    env = norm_cmpl_arg_mod(env);
    if (scm_obj_null_p(env)) return SCM_OBJ_NULL;
  }

  cmpl = scm_fcd_mem_alloc(&SCM_COMPILER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cmpl)) return SCM_OBJ_NULL;

  if (scm_cmpl_initialize(cmpl, env) < 0)
    return SCM_OBJ_NULL;

  return cmpl;
}

ScmObj
scm_fcd_make_compiler(ScmObj env)
{
  return scm_fcd_compiler_new(SCM_MEM_HEAP, env);
}

ScmObj
scm_fcd_compiler_base_env(ScmObj cmpl)
{
  scm_assert(scm_fcd_compiler_p(cmpl));
  return scm_cmpl_env(cmpl);
}

ScmObj
scm_fcd_compiler_current_expr(ScmObj cmpl)
{
  scm_assert(scm_fcd_compiler_p(cmpl));
  return scm_cmpl_expr(cmpl);
}

ScmObj
scm_fcd_compiler_select_base_env_i(ScmObj cmpl, ScmObj env)
{
  SCM_REFSTK_INIT_REG(&cmpl, &env);

  scm_assert(scm_fcd_compiler_p(cmpl));
  scm_assert(scm_obj_not_null_p(env));

  scm_cmpl_set_env(cmpl, env);
  return SCM_UNDEF_OBJ;
}

ScmObj
scm_fcd_compiler_select_module_i(ScmObj cmpl, ScmObj mod)
{
  SCM_REFSTK_INIT_REG(&cmpl, &mod);

  scm_assert(scm_fcd_compiler_p(cmpl));

  mod = norm_cmpl_arg_mod(mod);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  scm_cmpl_set_env(cmpl, mod);
  return SCM_UNDEF_OBJ;
}

void
scm_fcd_compiler_select_expr_i(ScmObj cmpl, ScmObj expr)
{
  scm_assert(scm_fcd_compiler_p(cmpl));
  scm_assert(scm_obj_not_null_p(expr));

  scm_cmpl_set_expr(cmpl, expr);
}


/*************************************************************************/
/* Quasiquotation                                                        */
/*************************************************************************/

ScmTypeInfo SCM_QQTMPLNODE_TYPE_INFO = {
  .name                = "qq-tmpl-node",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmQQTmplNode),
  .gc_ini_func         = scm_qqtn_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_qqtn_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static inline void
scm_qqtn_update(ScmObj node, int kind, ScmObj obj)
{
  scm_assert_obj_type(node, &SCM_QQTMPLNODE_TYPE_INFO);
  scm_assert(scm_qqtn_valid_kind_p(kind));
  scm_assert(kind != SCM_QQ_TMPL_NODE_LITERAL || scm_obj_not_null_p(obj));

  SCM_QQTMPLNODE(node)->kind = kind;
  if (kind == SCM_QQ_TMPL_NODE_LITERAL)
    SCM_SLOT_SETQ(ScmQQTmplNode, node, obj, obj);
  else
    SCM_QQTMPLNODE(node)->obj = SCM_OBJ_NULL;
}

int
scm_qqtn_initialize(ScmObj node, int kind, ScmObj obj)
{
  scm_qqtn_update(node, kind, obj);
  return 0;
}

void
scm_qqtn_update_contents(ScmObj node, int kind, ScmObj obj)
{
  scm_qqtn_update(node, kind, obj);
}

void
scm_qqtn_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_QQTMPLNODE_TYPE_INFO);

  SCM_QQTMPLNODE(obj)->kind = SCM_QQ_TMPL_NODE_UNQUOTE;
  SCM_QQTMPLNODE(obj)->obj = SCM_OBJ_NULL;
}

int
scm_qqtn_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_QQTMPLNODE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (SCM_QQTMPLNODE(obj)->kind == SCM_QQ_TMPL_NODE_LITERAL)
    return SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_QQTMPLNODE(obj)->obj, mem);
  else
    return SCM_GC_REF_HANDLER_VAL_INIT;
}


ScmTypeInfo SCM_QQTMPL_TYPE_INFO = {
  .name                = "qq-template",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmQQTmpl),
  .gc_ini_func         = scm_qqtmpl_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_qqtmpl_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_qqtmpl_initialize(ScmObj qqtmpl, ScmObj tmpl)
{
  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));

  SCM_SLOT_SETQ(ScmQQTmpl, qqtmpl, orig, tmpl);
  SCM_QQTMPL(qqtmpl)->compiled = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmQQTmpl, qqtmpl, expr, scm_fcd_make_vector(0, SCM_OBJ_NULL));
  if (scm_obj_null_p(SCM_QQTMPL(qqtmpl)->expr)) return -1;

  return 0;
}

size_t
scm_qqtmpl_nr_unquoted_expr(ScmObj qqtmpl)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  return scm_fcd_vector_length(SCM_QQTMPL(qqtmpl)->expr);
}

ScmObj
scm_qqtmpl_unquoted_expr(ScmObj qqtmpl, size_t n)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(n < scm_fcd_vector_length(SCM_QQTMPL(qqtmpl)->expr));

  return scm_fcd_vector_ref(SCM_QQTMPL(qqtmpl)->expr, n);
}

ssize_t
scm_qqtmpl_push_unquoted_expr(ScmObj qqtmpl, ScmObj expr)
{
  size_t n;
  int r;

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(expr));

  n = scm_fcd_vector_length(SCM_QQTMPL(qqtmpl)->expr);
  r = scm_fcd_vector_push(SCM_QQTMPL(qqtmpl)->expr, expr);
  if (r < 0) return -1;

  return (ssize_t)n;
}

int
scm_qqtmpl_compiled(ScmObj qqtmpl, ScmObj compiled)
{
  int r;

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(compiled));

  SCM_SLOT_SETQ(ScmQQTmpl, qqtmpl, compiled, compiled);
  r = scm_fcd_vector_contract_redundant_space(SCM_QQTMPL(qqtmpl)->expr);
  if (r < 0) return -1;

  return 0;
}

void
scm_qqtmpl_chg_orig_template(ScmObj qqtmpl, ScmObj tmpl)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));

  SCM_SLOT_SETQ(ScmQQTmpl, qqtmpl, orig, tmpl);
}

static int scm_qqtmpl_compiled_eq(ScmObj c1, ScmObj c2, bool *rslt);

static int
scm_qqtmpl_compiled_eq__list(ScmObj c1, ScmObj c2, bool *rslt)
{
  int r;

  scm_assert(scm_fcd_pair_p(c1));
  scm_assert(scm_fcd_pair_p(c2));
  scm_assert(rslt != NULL);

  r = scm_qqtmpl_compiled_eq(scm_fcd_car(c1), scm_fcd_car(c2), rslt);
  if (r < 0) return -1;
  else if (!*rslt) return 0;

  return scm_qqtmpl_compiled_eq(scm_fcd_cdr(c1), scm_fcd_cdr(c2), rslt);
}

static int
scm_qqtmpl_compiled_eq__vector(ScmObj c1, ScmObj c2, bool *rslt)
{
  size_t n;

  scm_assert(scm_fcd_vector_p(c1));
  scm_assert(scm_fcd_vector_p(c2));
  scm_assert(rslt != NULL);

  n = scm_fcd_vector_length(c1);
  if (n != scm_fcd_vector_length(c2)) {
    *rslt = false;
    return 0;
  }

  for (size_t i = 0; i < n; i++) {
    int r = scm_qqtmpl_compiled_eq(scm_fcd_vector_ref(c1, i),
                                   scm_fcd_vector_ref(c2, i),
                                   rslt);
    if (r < 0) return -1;
    else if (!*rslt) return 0;
  }

  return 0;
}

static int
scm_qqtmpl_compiled_eq(ScmObj c1, ScmObj c2, bool *rslt)
{
  scm_assert(scm_obj_not_null_p(c1));
  scm_assert(scm_obj_not_null_p(c2));
  scm_assert(rslt != NULL);

  if (scm_fcd_eq_p(c1, c2))
    goto equal;

  if (!scm_type_info_same_p(scm_obj_type(c1), scm_obj_type(c2)))
    goto not_equal;

  if (scm_fcd_pair_p(c1)) {
    return scm_qqtmpl_compiled_eq__list(c1, c2, rslt);
  }
  else if (scm_fcd_vector_p(c1)) {
    return scm_qqtmpl_compiled_eq__vector(c1, c2, rslt);
  }
  else if (scm_fcd_qqtmplnode_p(c1)) {
    if (scm_qqtn_kind(c1) != scm_qqtn_kind(c2))
      goto not_equal;
    if (scm_qqtn_kind(c1) == SCM_QQ_TMPL_NODE_LITERAL)
      return scm_fcd_equal(scm_qqtn_object(c1), scm_qqtn_object(c2), rslt);
    else
      goto equal;
  }
  else
    return scm_fcd_equal(c1, c2, rslt);

 equal:
  *rslt = true;
  return 0;

 not_equal:
  *rslt = false;
  return 0;
}

int
scm_qqtmpl_eq(ScmObj qqtmpl1, ScmObj qqtmpl2, bool *rslt)
{
  int r;

  scm_assert_obj_type(qqtmpl1, &SCM_QQTMPL_TYPE_INFO);
  scm_assert_obj_type(qqtmpl2, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(rslt != NULL);

  if (scm_fcd_eq_p(qqtmpl1, qqtmpl2)) {
    *rslt = true;
    return 0;
  }

  r = scm_fcd_equal(SCM_QQTMPL(qqtmpl1)->orig, SCM_QQTMPL(qqtmpl2)->orig, rslt);
  if (r < 0) return -1;
  else if (!*rslt) return 0;

  r = scm_fcd_equal(SCM_QQTMPL(qqtmpl1)->expr, SCM_QQTMPL(qqtmpl2)->expr, rslt);
  if (r < 0) return -1;
  else if (!*rslt) return 0;

  if (scm_obj_null_p(SCM_QQTMPL(qqtmpl1)->compiled)) {
    *rslt = scm_obj_null_p(SCM_QQTMPL(qqtmpl2)->compiled);
    return 0;
  }

  return scm_qqtmpl_compiled_eq(SCM_QQTMPL(qqtmpl1)->compiled,
                                SCM_QQTMPL(qqtmpl2)->compiled,
                                rslt);
}

void
scm_qqtmpl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_QQTMPL(obj)->orig = SCM_OBJ_NULL;
  SCM_QQTMPL(obj)->compiled = SCM_OBJ_NULL;
  SCM_QQTMPL(obj)->expr = SCM_OBJ_NULL;
}

int
scm_qqtmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_QQTMPL(obj)->orig, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_QQTMPL(obj)->compiled, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_QQTMPL(obj)->expr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

static ScmObj scm_cmpl_compile_qq_tmpl_or_spl(ScmObj qqtmpl, ScmObj tmpl,
                                              size_t depth);

static ScmObj
scm_cmpl_compile_qq_tmpl_quasi(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  ScmObj cdr = SCM_OBJ_INIT, x = SCM_OBJ_INIT;

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_pair_p(tmpl));

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &cdr, &x);

  cdr = scm_fcd_cdr(tmpl);
  if (!scm_fcd_pair_p(cdr) || !scm_fcd_nil_p(scm_fcd_cdr(cdr))) {
    scm_fcd_error("failed to compile <qq template>: malformed quasiquote", 0);
    return SCM_OBJ_NULL;
  }

  x = scm_cmpl_compile_qq_tmpl(qqtmpl, scm_fcd_car(cdr), depth + 1);
  if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, scm_fcd_car(tmpl), x);
}

static ScmObj
scm_cmpl_compile_qq_tmpl_unquote_internal(ScmObj qqtmpl, ScmObj tmpl,
                                          size_t depth, int kind)
{
  ScmObj cdr = SCM_OBJ_INIT, x = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &cdr, &x);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_pair_p(tmpl));
  scm_assert(kind == SCM_QQ_TMPL_NODE_UNQUOTE
             || kind == SCM_QQ_TMPL_NODE_UNQ_SPL);

  cdr = scm_fcd_cdr(tmpl);
  if (!scm_fcd_pair_p(cdr) || !scm_fcd_nil_p(scm_fcd_cdr(cdr))) {
    if (kind == SCM_QQ_TMPL_NODE_UNQUOTE)
      scm_fcd_error("failed to compile <qq template>: malformed unquote", 0);
    else
      scm_fcd_error("failed to compile <qq template>: "
                    "malformed unquote-splicing", 0);
    return SCM_OBJ_NULL;
  }

  if (depth > 0) {
    x = scm_cmpl_compile_qq_tmpl(qqtmpl, scm_fcd_car(cdr), depth - 1);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

    return scm_fcd_list(2, scm_fcd_car(tmpl), x);
  }
  else {
    ssize_t n = scm_qqtmpl_push_unquoted_expr(qqtmpl, scm_fcd_car(cdr));
    if (n < 0) return SCM_OBJ_NULL;

    return scm_fcd_qqtmplnode_new(SCM_MEM_HEAP, kind, SCM_OBJ_NULL);
  }
}

static ScmObj
scm_cmpl_compile_qq_tmpl_unquote(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  return scm_cmpl_compile_qq_tmpl_unquote_internal(qqtmpl, tmpl, depth,
                                                   SCM_QQ_TMPL_NODE_UNQUOTE);
}

static ScmObj
scm_cmpl_compile_qq_tmpl_quo_spl(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  return scm_cmpl_compile_qq_tmpl_unquote_internal(qqtmpl, tmpl, depth,
                                                   SCM_QQ_TMPL_NODE_UNQ_SPL);
}

static bool
scm_cmpl_qqtn_literal_p(ScmObj obj)
{
  if (scm_obj_type_p(obj, &SCM_QQTMPLNODE_TYPE_INFO))
    return (scm_qqtn_kind(obj) == SCM_QQ_TMPL_NODE_LITERAL);
  else
    return !scm_fcd_pair_p(obj) && !scm_fcd_vector_p(obj);
}

static int
scm_cmpl_qq_tmpl_syntax(ScmObj obj, ScmObj *syms, int n)
{
  ScmObj car = SCM_OBJ_INIT;

  scm_assert(scm_fcd_pair_p(obj));
  scm_assert(syms != NULL);

  car = scm_fcd_car(obj);
  for (int i = 0; i < n; i++) {
    if (scm_fcd_eq_p(car, syms[i])) {
      ScmObj cdr = scm_fcd_cdr(obj);
      if (scm_fcd_pair_p(cdr) && scm_fcd_nil_p(scm_fcd_cdr(cdr)))
        return i;
      else
        return -1;
    }
  }
  return -1;
}

static ScmObj
scm_cmpl_compile_qq_tmpl_list(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  enum { NR_SYNTAX = 3 };
  static const struct {
    int kind; ScmObj (*func)(ScmObj, size_t, ScmObj);
  } syntax_tbl[NR_SYNTAX] = {
    { SCM_CACHED_SYM_QUASIQUOTE, scm_cmpl_compile_qq_tmpl_quasi },
    { SCM_CACHED_SYM_UNQUOTE, scm_cmpl_compile_qq_tmpl_unquote },
    { SCM_CACHED_SYM_UNQUOTE_SPLICING, scm_cmpl_compile_qq_tmpl_quo_spl }
  };

  ScmObj lst = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, tail = SCM_OBJ_INIT;
  ScmObj syntax_sym[NR_SYNTAX] = { SCM_OBJ_INIT, SCM_OBJ_INIT, SCM_OBJ_INIT, };
  bool literal_only;
  int idx;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &lst, &x,
                      &head, &tail);
  SCM_REFSTK_REG_ARY(syntax_sym, NR_SYNTAX);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_pair_p(tmpl));

  for (int i = 0; i < NR_SYNTAX; i++) {
    syntax_sym[i] = scm_fcd_cached_symbol(syntax_tbl[i].kind);
    if (scm_obj_null_p(syntax_sym[i])) return SCM_OBJ_NULL;
  }

  if ((idx = scm_cmpl_qq_tmpl_syntax(tmpl, syntax_sym, NR_SYNTAX)) >= 0)
    return syntax_tbl[idx].func(qqtmpl, tmpl, depth);

  head = tail = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_NIL_OBJ);
  if (scm_obj_null_p(tail)) return SCM_OBJ_NULL;

  lst = tmpl;
  literal_only = true;
  while (scm_fcd_pair_p(lst)) {
    if ((idx = scm_cmpl_qq_tmpl_syntax(lst, syntax_sym, NR_SYNTAX)) >= 0) {
      if (syntax_tbl[idx].kind == SCM_CACHED_SYM_UNQUOTE_SPLICING) {
        scm_fcd_error("failed to compile <qq template>: "
                      "invalid unquote-splicing", 0);
        return SCM_OBJ_NULL;
      }
      x = syntax_tbl[idx].func(qqtmpl, lst, depth);
      if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
      literal_only = (literal_only && scm_cmpl_qqtn_literal_p(x));
      scm_fcd_set_cdr_i(tail, x);
      break;
    }

    x = scm_cmpl_compile_qq_tmpl_or_spl(qqtmpl, scm_fcd_car(lst), depth);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
    literal_only = (literal_only && scm_cmpl_qqtn_literal_p(x));

    x = scm_fcd_cons(x, SCM_NIL_OBJ);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

    scm_fcd_set_cdr_i(tail, x);
    tail = x;
    lst = scm_fcd_cdr(lst);
  }

  if (!scm_fcd_nil_p(lst)) {
    x = scm_cmpl_compile_qq_tmpl(qqtmpl, lst, depth);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
    literal_only = (literal_only && scm_cmpl_qqtn_literal_p(x));
    scm_fcd_set_cdr_i(tail, x);
  }

  if (literal_only)
    return scm_fcd_qqtmplnode_new(SCM_MEM_HEAP, SCM_QQ_TMPL_NODE_LITERAL, tmpl);
  else
    return scm_fcd_cdr(head);
}

static ScmObj
scm_cmpl_compile_qq_tmpl_vector(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  ScmObj new_vec = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  size_t n;
  bool literal_only;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &new_vec, &x);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_vector_p(tmpl));

  n = scm_fcd_vector_length(tmpl);
  new_vec = scm_fcd_make_vector(n, SCM_OBJ_NULL);
  if (scm_obj_null_p(new_vec)) return SCM_OBJ_NULL;

  literal_only = true;

  for (size_t i = 0; i < n; i++) {
    x = scm_fcd_vector_ref(tmpl, i);
    x = scm_cmpl_compile_qq_tmpl_or_spl(qqtmpl, x, depth);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
    scm_fcd_vector_set_i(new_vec, i, x);
    literal_only = (literal_only && scm_cmpl_qqtn_literal_p(x));
  }

  if (literal_only)
    return scm_fcd_qqtmplnode_new(SCM_MEM_HEAP, SCM_QQ_TMPL_NODE_LITERAL, tmpl);
  else
    return new_vec;
}

static ScmObj
scm_cmpl_compile_qq_tmpl_or_spl(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));

  if (scm_fcd_pair_p(tmpl)) {
    return scm_cmpl_compile_qq_tmpl_list(qqtmpl, tmpl, depth);
  }
  else if (scm_fcd_vector_p(tmpl)) {
    return scm_cmpl_compile_qq_tmpl_vector(qqtmpl, tmpl, depth);
  }
  else {
    return tmpl;
  }
}

ScmObj
scm_cmpl_compile_qq_tmpl(ScmObj qqtmpl, ScmObj tmpl, size_t depth)
{
  ScmObj obj = SCM_OBJ_INIT;

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));

  obj = scm_cmpl_compile_qq_tmpl_or_spl(qqtmpl, tmpl, depth);
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  if (scm_obj_type_p(obj, &SCM_QQTMPLNODE_TYPE_INFO)
      && scm_qqtn_kind(obj) == SCM_QQ_TMPL_NODE_UNQ_SPL) {
    scm_fcd_error("failed to compile <qq template>: invalid unquote-splicing",
                  0);
    return SCM_OBJ_NULL;
  }

  return obj;
}

static ScmObj scm_cmpl_substitute_qq_tmpl_or_spl(ScmObj qqtmpl, ScmObj tmpl,
                                                 scm_csetter_t *values,
                                                 bool *spl);

static ScmObj
scm_cmpl_sub_qq_tmpl_node(ScmObj qqtmpl, ScmObj tmpl,
                          scm_csetter_t *values, bool *spl)
{
  ScmObj obj = SCM_OBJ_INIT, v = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &obj);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert_obj_type(tmpl, &SCM_QQTMPLNODE_TYPE_INFO);
  scm_assert(spl != NULL);

  switch (scm_qqtn_kind(tmpl)) {
  case SCM_QQ_TMPL_NODE_LITERAL:
    obj = scm_qqtn_object(tmpl);
    break;
  case SCM_QQ_TMPL_NODE_UNQUOTE: /* fall through */
  case SCM_QQ_TMPL_NODE_UNQ_SPL:
    if (values == NULL)
      goto too_few_values;
    v = scm_csetter_val(values);
    if (!scm_fcd_pair_p(v))
      goto too_few_values;
    obj = scm_fcd_car(v);
    scm_csetter_setq(values, scm_fcd_cdr(v));
    break;
  default:
    scm_assert(false);        /* must not happen */
    return SCM_OBJ_NULL;
    break;
  }

  *spl = (scm_qqtn_kind(tmpl) == SCM_QQ_TMPL_NODE_UNQ_SPL);
  return obj;

 too_few_values:
  scm_fcd_error("failed to substitute <qq template>: too few values", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_cmpl_substitute_qq_tmpl_list(ScmObj qqtmpl, ScmObj tmpl,
                                 scm_csetter_t *values)
{
  ScmObj lst = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, tail = SCM_OBJ_INIT;
  bool spl;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &lst, &x, &head, &tail);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_pair_p(tmpl));

  head = tail = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_NIL_OBJ);
  if (scm_obj_null_p(tail)) return SCM_OBJ_NULL;

  lst = tmpl;
  while (scm_fcd_pair_p(lst)) {
    x = scm_cmpl_substitute_qq_tmpl_or_spl(qqtmpl, scm_fcd_car(lst),
                                           values, &spl);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

    if (spl) {
      scm_fcd_set_cdr_i(tail, x);
      for (; scm_fcd_pair_p(scm_fcd_cdr(tail)); tail = scm_fcd_cdr(tail))
        ;
      if (!scm_fcd_nil_p(scm_fcd_cdr(tail))) {
        scm_fcd_error("failed to substitute <qq template>: "
                      "unquoted expression does not evaluated to list", 0);
        return SCM_OBJ_NULL;
      }
    }
    else {
      x = scm_fcd_cons(x, SCM_NIL_OBJ);
      if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
      scm_fcd_set_cdr_i(tail, x);
      tail = x;
    }

    lst = scm_fcd_cdr(lst);
  }

  if (!scm_fcd_nil_p(lst)) {
    x = scm_cmpl_substitute_qq_tmpl(qqtmpl, lst, values);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
    scm_fcd_set_cdr_i(tail, x);
  }

  return scm_fcd_cdr(head);
}

static ScmObj
scm_cmpl_substitute_qq_tmpl_vector(ScmObj qqtmpl, ScmObj tmpl,
                                   scm_csetter_t *values)
{
  ScmObj vec = SCM_OBJ_INIT, t = SCM_OBJ_INIT, x = SCM_OBJ_INIT;
  size_t n;
  bool spl;
  int r;

  SCM_REFSTK_INIT_REG(&qqtmpl, &tmpl,
                      &vec, &t, &x);

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_fcd_vector_p(tmpl));

  vec = scm_fcd_make_vector(0, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  n = scm_fcd_vector_length(tmpl);
  for (size_t i = 0; i < n; i++) {
    t = scm_fcd_vector_ref(tmpl, i);
    x = scm_cmpl_substitute_qq_tmpl_or_spl(qqtmpl, t, values, &spl);
    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

    if (spl) {
      for (; scm_fcd_pair_p(x); x = scm_fcd_cdr(x)) {
        r = scm_fcd_vector_push(vec, scm_fcd_car(x));
        if (r < 0) return SCM_OBJ_NULL;
      }
      if (!scm_fcd_nil_p(x)) {
        scm_fcd_error("failed to substitute <qq template>: "
                      "unquoted expression does not evaluated to list", 0);
        return SCM_OBJ_NULL;
      }
    }
    else {
      r = scm_fcd_vector_push(vec, x);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  r = scm_fcd_vector_contract_redundant_space(vec);
  if (r < 0) return SCM_OBJ_NULL;

  return vec;
}

static ScmObj
scm_cmpl_substitute_qq_tmpl_or_spl(ScmObj qqtmpl, ScmObj tmpl,
                                   scm_csetter_t *values, bool *spl)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));
  scm_assert(spl != NULL);

  *spl = false;
  if (scm_obj_type_p(tmpl, &SCM_QQTMPLNODE_TYPE_INFO))
    return scm_cmpl_sub_qq_tmpl_node(qqtmpl, tmpl, values, spl);
  else if (scm_fcd_pair_p(tmpl))
    return scm_cmpl_substitute_qq_tmpl_list(qqtmpl, tmpl, values);
  else if (scm_fcd_vector_p(tmpl))
    return scm_cmpl_substitute_qq_tmpl_vector(qqtmpl, tmpl, values);
  else
    return tmpl;
}

ScmObj
scm_cmpl_substitute_qq_tmpl(ScmObj qqtmpl, ScmObj tmpl, scm_csetter_t *values)
{
  ScmObj obj = SCM_OBJ_INIT;
  bool spl;

  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(tmpl));

  obj = scm_cmpl_substitute_qq_tmpl_or_spl(qqtmpl, tmpl, values, &spl);
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  if (spl) {
    scm_fcd_error("failed to substitue <qq template>: "
                  "invalid unquote-splicing", 0);
    return SCM_OBJ_NULL;
  }

  return obj;
}


/*************************************************************************/
/* Quasiquotation (interface)                                            */
/*************************************************************************/

bool
scm_fcd_qqtmplnode_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_QQTMPLNODE_TYPE_INFO);
}

ScmObj
scm_fcd_qqtmplnode_new(scm_mem_type_t mtype, int kind, ScmObj obj)
{
  ScmObj node = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj,
                      &node);

  node = scm_fcd_mem_alloc(&SCM_QQTMPLNODE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(node)) return SCM_OBJ_NULL;

  if (scm_qqtn_initialize(node, kind, obj) < 0)
    return SCM_OBJ_NULL;

  return node;
}

ScmObj
scm_fcd_make_qqtmplnode_for_unmarshal(void)
{
  return scm_fcd_qqtmplnode_new(SCM_MEM_HEAP,
                                SCM_QQ_TMPL_NODE_UNQUOTE, SCM_OBJ_NULL);
}

void
scm_fcd_qqtmplnode_get_contents_for_marshal(ScmObj node,
                                            int *kind, scm_csetter_t *obj)
{
  scm_assert(scm_fcd_qqtmplnode_p(node));
  scm_assert(kind != NULL);
  scm_assert(obj != NULL);

  *kind = scm_qqtn_kind(node);
  if (scm_obj_null_p(scm_qqtn_object(node)))
    scm_csetter_setq(obj, SCM_UNDEF_OBJ);
  else
    scm_csetter_setq(obj, scm_qqtn_object(node));
}

int
scm_fcd_qqtmplnode_setup_for_unmarshal(ScmObj node, int kind, ScmObj obj)
{
  if (!scm_qqtn_valid_kind_p(kind)) {
    scm_fcd_error("failed to setup qq template node: invalid kind value", 0);
    return -1;
  }

  scm_qqtn_update_contents(node, kind,
                           scm_fcd_undef_object_p(obj) ? SCM_OBJ_NULL : obj);
  return 0;
}

bool
scm_fcd_qqtmpl_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_QQTMPL_TYPE_INFO);
}

ScmObj
scm_fcd_qqtmpl_new(scm_mem_type_t mtype, ScmObj tmpl)
{
  ScmObj qq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&tmpl,
                      &qq);

  scm_assert(scm_obj_not_null_p(tmpl));

  qq = scm_fcd_mem_alloc(&SCM_QQTMPL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(qq)) return SCM_OBJ_NULL;

  if (scm_qqtmpl_initialize(qq, tmpl) < 0)
    return SCM_OBJ_NULL;

  return qq;
}

ScmObj
scm_fcd_make_qqtmpl_for_unmarshal(void)
{
  return scm_fcd_qqtmpl_new(SCM_MEM_HEAP, SCM_NIL_OBJ);
}

int
scm_fcd_qqtmpl_get_contents_for_marshal(ScmObj qq,
                                        scm_csetter_t *tmpl,
                                        scm_csetter_t *compiled,
                                        scm_csetter_t *expr)
{
  ScmObj vec = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&qq,
                      &vec, &e);

  scm_assert(scm_fcd_qqtmpl_p(qq));
  scm_assert(tmpl != NULL);
  scm_assert(compiled != NULL);
  scm_assert(expr != NULL);

  scm_csetter_setq(tmpl, scm_qqtmpl_template(qq));

  e = scm_qqtmpl_compiled_template(qq);
  if (scm_obj_null_p(e))
    scm_csetter_setq(compiled, SCM_UNDEF_OBJ);
  else
    scm_csetter_setq(compiled, e);

  n = scm_fcd_qqtmpl_nr_unquoted_expr(qq);
  vec = scm_fcd_make_vector(n, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return -1;

  for (size_t i = 0; i < n; i++) {
    e = scm_qqtmpl_unquoted_expr(qq, i);
    scm_fcd_vector_set_i(vec, i , e);
  }

  scm_csetter_setq(expr, vec);

  return 0;
}

int
scm_fcd_qqtmpl_setup_for_unmarshal(ScmObj qq,
                                   ScmObj tmpl, ScmObj compiled, ScmObj expr)
{
  ScmObj e = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&qq, &tmpl, &compiled, &expr,
                      &e);

  scm_assert(scm_fcd_qqtmpl_p(qq));
  scm_assert(scm_obj_not_null_p(tmpl));
  scm_assert(scm_obj_not_null_p(tmpl));
  scm_assert(scm_fcd_vector_p(expr));

  scm_qqtmpl_chg_orig_template(qq, tmpl);

  n = scm_fcd_vector_length(expr);
  for (size_t i = 0; i < n; i++) {
    ssize_t r;

    e = scm_fcd_vector_ref(expr, i);
    r = scm_qqtmpl_push_unquoted_expr(qq, e);
    if (r < 0) return -1;
  }

  if (scm_obj_not_null_p(compiled) && !scm_fcd_undef_object_p(compiled))
    scm_qqtmpl_compiled(qq, compiled);

  return 0;
}

ScmObj
scm_fcd_qqtmpl_template(ScmObj qq)
{
  scm_assert(scm_fcd_qqtmpl_p(qq));

  return scm_qqtmpl_template(qq);
}

size_t
scm_fcd_qqtmpl_nr_unquoted_expr(ScmObj qq)
{
  scm_assert(scm_fcd_qqtmpl_p(qq));

  return scm_qqtmpl_nr_unquoted_expr(qq);
}

ScmObj
scm_fcd_qqtmpl_unquoted_expr(ScmObj qq, size_t n)
{
  scm_assert(scm_fcd_qqtmpl_p(qq));
  scm_assert(n < scm_qqtmpl_nr_unquoted_expr(qq));

  return scm_qqtmpl_unquoted_expr(qq, n);
}

/* XXX: Marshal/Unmarshal の動作確認に使用する目的で作成した等価性評価関数な
 *      ので、テスト目的以外には使用しない
 */
int
scm_fcd_qqtmpl_eq(ScmObj qq1, ScmObj qq2, bool *rslt)
{
  scm_assert(scm_fcd_qqtmpl_p(qq1));
  scm_assert(scm_fcd_qqtmpl_p(qq2));
  scm_assert(rslt != NULL);

  return scm_qqtmpl_eq(qq1, qq2, rslt);
}

ScmObj
scm_fcd_compile_qq_template(ScmObj tmpl)
{
  ScmObj qq = SCM_OBJ_NULL, compiled = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&tmpl,
                      &qq, &compiled);

  scm_assert(scm_obj_not_null_p(tmpl));

  qq = scm_fcd_qqtmpl_new(SCM_MEM_HEAP, tmpl);
  if (scm_obj_null_p(qq)) return SCM_OBJ_NULL;

  compiled = scm_cmpl_compile_qq_tmpl(qq, tmpl, 0);
  if (scm_obj_null_p(compiled)) return SCM_OBJ_NULL;

  r = scm_qqtmpl_compiled(qq, compiled);
  if (r < 0) return SCM_OBJ_NULL;

  return qq;
}

ScmObj
scm_fcd_substitute_qq_template(ScmObj qq, ScmObj values)
{
  SCM_REFSTK_INIT_REG(&qq, &values);

  scm_assert(scm_fcd_qqtmpl_p(qq));
  scm_assert(scm_obj_not_null_p(values));

  return scm_cmpl_substitute_qq_tmpl(qq, scm_qqtmpl_compiled_template(qq),
                                     SCM_CSETTER_L(values));
}


/*************************************************************************/
/* Identifier                                                            */
/*************************************************************************/

ScmTypeInfo SCM_IDENTIFIER_TYPE_INFO = {
  .name                = "identifier",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_ident_obj_print,
  .obj_size            = sizeof(ScmIdentifier),
  .gc_ini_func         = scm_ident_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_ident_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};


int
scm_ident_initialize(ScmObj ident, ScmObj name, ScmObj env)
{
  scm_assert_obj_type(ident, &SCM_IDENTIFIER_TYPE_INFO);
  scm_assert(scm_fcd_symbol_p(name));
  scm_assert(scm_obj_not_null_p(env));

  SCM_IDENT_SET_NAME(ident, name);
  SCM_IDENT_SET_ENV(ident, env);

  return 0;
}

int
scm_ident_obj_print(ScmObj obj, ScmObj port, int kind, ScmObjPrintHandler handler)
{
  char fmt[64];

  snprintf(fmt, sizeof(fmt), "#<ident ~a %lx>", SCM_IDENT_ENV(obj));
  return scm_fcd_pformat_cstr(port, fmt, SCM_IDENT_NAME(obj), SCM_OBJ_NULL);
}

void
scm_ident_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_IDENTIFIER_TYPE_INFO);

  SCM_IDENT_SET_NAME(obj, SCM_OBJ_NULL);
  SCM_IDENT_SET_ENV(obj, SCM_OBJ_NULL);
}

int
scm_ident_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_IDENTIFIER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_IDENT_NAME(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_IDENT_ENV(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return 0;
}


/*************************************************************************/
/* Identifier (interface)                                                */
/*************************************************************************/

bool
scm_fcd_identifier_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_IDENTIFIER_TYPE_INFO);
}

ScmObj
scm_fcd_identifier_P(ScmObj obj)
{
  return (scm_fcd_identifier_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_identifier_new(scm_mem_type_t mtype, ScmObj name, ScmObj env)
{
  ScmObj ident = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name, &env,
                      &ident);

  scm_assert(scm_fcd_symbol_p(name));
  scm_assert(scm_obj_not_null_p(env));

  ident = scm_fcd_mem_alloc(&SCM_IDENTIFIER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(ident)) return SCM_OBJ_NULL;

  if (scm_ident_initialize(ident, name, env) < 0)
    return SCM_OBJ_NULL;

  return ident;
}

ScmObj
scm_fcd_make_identifier(ScmObj name, ScmObj env)
{
  return scm_fcd_identifier_new(SCM_MEM_HEAP, name, env);
}

ScmObj
scm_fcd_identifier_name(ScmObj ident)
{
  return scm_ident_name(ident);
}

ScmObj
scm_fcd_identifier_env(ScmObj ident)
{
  return scm_ident_env(ident);
}
