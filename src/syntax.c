#include <string.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/syntax.h"

/*************************************************************************/
/* Syntax                                                                */
/*************************************************************************/

ScmTypeInfo SCM_SYNTAX_TYPE_INFO = {
  .name                = "syntax",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_syntax_obj_print,
  .obj_size            = sizeof(ScmSyntax),
  .gc_ini_func         = scm_syntax_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_syntax_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_syntax_initialize(ScmObj syx, ScmObj key, ScmObj handler)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);
  scm_assert(scm_fcd_symbol_p(key));
  scm_assert(scm_obj_not_null_p(handler));

  SCM_SLOT_SETQ(ScmSyntax, syx, keyword, key);
  SCM_SLOT_SETQ(ScmSyntax, syx, handler, handler);

  return 0;
}

int
scm_syntax_obj_print(ScmObj obj, ScmObj port, int kind,
                     ScmObjPrintHandler handler)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  return scm_fcd_pformat_cstr(port, "#<syntax ~a>",
                               SCM_SYNTAX(obj)->keyword, SCM_OBJ_NULL);
}

void
scm_syntax_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  SCM_SYNTAX(obj)->keyword = SCM_OBJ_NULL;
  SCM_SYNTAX(obj)->handler = SCM_OBJ_NULL;
}

int
scm_syntax_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYNTAX(obj)->keyword);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYNTAX(obj)->handler);
}


/*************************************************************************/
/* Syntax (interface)                                                    */
/*************************************************************************/

bool
scm_fcd_syntax_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ? true : false);
}

ScmObj
scm_fcd_syntax_P(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ?
          SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_syntax_new(scm_mem_type_t mtype, ScmObj key, ScmObj handler)
{
  ScmObj syx = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&key, &handler,
                      &syx);

  scm_assert(scm_fcd_symbol_p(key));
  scm_assert(scm_obj_not_null_p(handler));

  syx = scm_fcd_mem_alloc(&SCM_SYNTAX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(syx)) return SCM_OBJ_NULL;

  rslt = scm_syntax_initialize(syx, key, handler);
  if (rslt < 0) return SCM_OBJ_NULL;

  return syx;
}

ScmObj
scm_fcd_make_syntax(ScmObj keyword, ScmObj handler)
{
  scm_assert(scm_fcd_symbol_p(keyword));
  scm_assert(scm_obj_not_null_p(handler));
  return scm_fcd_syntax_new(SCM_MEM_HEAP, keyword, handler);
}

ScmObj
scm_fcd_syntax_keyword(ScmObj syx)
{
  scm_assert(scm_fcd_syntax_p(syx));
  return scm_syntax_keyword(syx);
}

ScmObj
scm_fcd_syntax_handler(ScmObj syx)
{
  scm_assert(scm_fcd_syntax_p(syx));
  return scm_syntax_handler(syx);
}


/*************************************************************************/
/* Macro                                                                 */
/*************************************************************************/

ScmTypeInfo SCM_MACRO_TYPE_INFO = {
  .name = "macro",
  .flags = SCM_TYPE_FLG_MMO,
  .obj_print_func = NULL,
  .obj_size = sizeof(ScmMacro),
  .gc_ini_func = scm_macro_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_macro_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

int
scm_macro_initialize(ScmObj macro, ScmObj transformer, ScmObj env)
{
  scm_assert_obj_type(macro, &SCM_MACRO_TYPE_INFO);
  scm_assert(scm_fcd_procedure_p(transformer));
  scm_assert(scm_obj_not_null_p(env));

  SCM_MACRO_SET_TRANSFORMER(macro, transformer);
  SCM_MACRO_SET_ENV(macro, env);

  return 0;
}

int
scm_macro_trmp_transformer(ScmObj macro, ScmObj form)
{
  ScmObj args = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&macro, &form,
                      &args);

  scm_assert_obj_type(macro, &SCM_MACRO_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(form));

  args = scm_fcd_cons(form, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  return scm_fcd_trampolining(SCM_MACRO_TRANSFORMER(macro), args,
                              SCM_OBJ_NULL, SCM_OBJ_NULL);
}

void
scm_macro_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_MACRO_TYPE_INFO);

  SCM_MACRO_SET_TRANSFORMER(obj, SCM_OBJ_NULL);
  SCM_MACRO_SET_ENV(obj, SCM_OBJ_NULL);
}

int
scm_macro_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_MACRO_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_MACRO_TRANSFORMER(obj));
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_MACRO_ENV(obj));
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return 0;
}


/*************************************************************************/
/* Macro (interface)                                                     */
/*************************************************************************/

bool
scm_fcd_macro_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MACRO_TYPE_INFO);
}

ScmObj
scm_fcd_macro_P(ScmObj obj)
{
  return (scm_fcd_macro_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_macro_new(scm_mem_type_t mtype, ScmObj transformer, ScmObj env)
{
  ScmObj macro = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&transformer, &env,
                      &macro);

  scm_assert(scm_fcd_procedure_p(transformer));
  scm_assert(scm_obj_not_null_p(env));

  macro = scm_fcd_mem_alloc(&SCM_MACRO_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(macro)) return SCM_OBJ_NULL;

  if (scm_macro_initialize(macro, transformer, env) < 0)
    return SCM_OBJ_NULL;

  return macro;
}

ScmObj
scm_fcd_make_macro(ScmObj transformer, ScmObj env)
{
  return scm_fcd_macro_new(SCM_MEM_HEAP, transformer, env);
}

ScmObj
scm_fcd_macro_env(ScmObj macro)
{
  scm_assert(scm_fcd_macro_p(macro));

  return scm_macro_env(macro);
}

int
scm_fcd_trmp_macro_transformer(ScmObj macro, ScmObj form)
{
  scm_assert(scm_fcd_macro_p(macro));
  scm_assert(scm_obj_not_null_p(form));

  return scm_macro_trmp_transformer(macro, form);
}
