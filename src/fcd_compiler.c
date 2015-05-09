#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/compiler.h"


/*************************************************************************/
/* Compiler                                                              */
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
scm_fcd_compiler_new(SCM_MEM_TYPE_T mtype, ScmObj module)
{
  ScmObj cmpl = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&module,
                      &cmpl);

  module = norm_cmpl_arg_mod(module);
  if (scm_obj_null_p(module)) return SCM_OBJ_NULL;

  cmpl = scm_fcd_mem_alloc(&SCM_COMPILER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cmpl)) return SCM_OBJ_NULL;

  if (scm_cmpl_initialize(cmpl, module) < 0)
    return SCM_OBJ_NULL;

  return cmpl;
}

ScmObj
scm_fcd_make_compiler(ScmObj mod)
{
  return scm_fcd_compiler_new(SCM_MEM_HEAP, mod);
}

ScmObj
scm_fcd_compiler_current_module(ScmObj cmpl)
{
  scm_assert(scm_fcd_compiler_p(cmpl));
  return scm_cmpl_module(cmpl);
}

ScmObj
scm_fcd_compiler_current_expr(ScmObj cmpl)
{
  scm_assert(scm_fcd_compiler_p(cmpl));
  return scm_cmpl_expr(cmpl);
}

ScmObj
scm_fcd_compiler_select_module_i(ScmObj cmpl, ScmObj mod)
{
  SCM_REFSTK_INIT_REG(&cmpl, &mod);

  scm_assert(scm_fcd_compiler_p(cmpl));

  mod = norm_cmpl_arg_mod(mod);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  scm_cmpl_set_module(cmpl, mod);
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

bool
scm_fcd_qqtmplnode_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_QQTMPLNODE_TYPE_INFO);
}

ScmObj
scm_fcd_qqtmplnode_new(SCM_MEM_TYPE_T mtype, int kind, ScmObj obj)
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
scm_fcd_qqtmpl_new(SCM_MEM_TYPE_T mtype, ScmObj tmpl)
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
  scm_assert(scm_fcd_qqtmpl_p(qq));
  scm_assert(scm_obj_not_null_p(values));

  return scm_cmpl_substitute_qq_tmpl(qq, scm_qqtmpl_compiled_template(qq),
                                     SCM_CSETTER_L(values));
}


/*************************************************************************/
/* Identifier                                                            */
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
scm_fcd_identifier_new(SCM_MEM_TYPE_T mtype, ScmObj name, ScmObj env)
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
