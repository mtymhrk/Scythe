#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/syntax.h"

/*************************************************************************/
/* Syntax                                                                */
/*************************************************************************/

extern inline bool
scm_fcd_syntax_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ? true : false);
}

extern inline ScmObj
scm_fcd_syntax_P(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ?
          SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_syntax_new(SCM_MEM_TYPE_T mtype, ScmObj key, ScmObj handler)
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

extern inline ScmObj
scm_fcd_syntax_keyword(ScmObj syx)
{
  scm_assert(scm_fcd_syntax_p(syx));
  return scm_syntax_keyword(syx);
}

extern inline ScmObj
scm_fcd_syntax_handler(ScmObj syx)
{
  scm_assert(scm_fcd_syntax_p(syx));
  return scm_syntax_handler(syx);
}


/*************************************************************************/
/* Macro                                                                 */
/*************************************************************************/

extern inline bool
scm_fcd_macro_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MACRO_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_macro_P(ScmObj obj)
{
  return (scm_fcd_macro_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_macro_new(SCM_MEM_TYPE_T mtype, ScmObj transformer, ScmObj env)
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
