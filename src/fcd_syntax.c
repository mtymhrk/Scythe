#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/syntax.h"

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
scm_fcd_make_syntax(ScmObj keyword, ScmObj handler)
{
  scm_assert(scm_fcd_symbol_p(keyword));
  scm_assert(scm_obj_not_null_p(handler));
  return scm_syntax_new(SCM_MEM_HEAP, keyword, handler);
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
