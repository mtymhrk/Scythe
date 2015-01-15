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
