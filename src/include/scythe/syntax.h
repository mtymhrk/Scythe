#ifndef INCLUDE_SYNTAX_H__
#define INCLUDE_SYNTAX_H__

#include <stdbool.h>

typedef struct ScmSyntaxRec ScmSyntax;
typedef struct ScmMacroRec ScmMacro;

#define SCM_SYNTAX(obj) ((ScmSyntax *)(obj))
#define SCM_MACRO(obj) ((ScmMacro *)(obj))

#include "scythe/object.h"

/*************************************************************************/
/* Syntax                                                                */
/*************************************************************************/

#define SCM_SYNTAX_KEYWORD_LEN_MAX 64

struct ScmSyntaxRec {
  ScmObjHeader header;
  ScmObj keyword;
  ScmObj handler;
};

extern ScmTypeInfo SCM_SYNTAX_TYPE_INFO;

int scm_syntax_initialize(ScmObj syx, ScmObj key, ScmObj handler);

int scm_syntax_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_syntax_gc_initialize(ScmObj obj, ScmObj mem);
int scm_syntax_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_syntax_keyword(ScmObj syx)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);

  return SCM_SYNTAX(syx)->keyword;
}

static inline ScmObj
scm_syntax_handler(ScmObj syx)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);

  return SCM_SYNTAX(syx)->handler;
}


/*************************************************************************/
/* Macro                                                                 */
/*************************************************************************/

struct ScmMacroRec {
  ScmObjHeader header;
  ScmObj transformer;
  ScmObj env;
};

extern ScmTypeInfo SCM_MACRO_TYPE_INFO;

#define SCM_MACRO_TRANSFORMER(macro) (SCM_MACRO(macro)->transformer)
#define SCM_MACRO_ENV(macro) (SCM_MACRO(macro)->env)
#define SCM_MACRO_SET_TRANSFORMER(macro, t) \
  SCM_SLOT_SETQ(ScmMacro, macro, transformer, t)
#define SCM_MACRO_SET_ENV(macro, e) \
  SCM_SLOT_SETQ(ScmMacro, macro, env, e)

int scm_macro_initialize(ScmObj macro, ScmObj transformer, ScmObj env);
int scm_macro_trmp_transformer(ScmObj macro, ScmObj form);
void scm_macro_gc_initialize(ScmObj obj, ScmObj mem);
int scm_macro_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_macro_env(ScmObj macro)
{
  scm_assert_obj_type(macro, &SCM_MACRO_TYPE_INFO);

  return SCM_MACRO_ENV(macro);
}


#endif /* INCLUDE_SYNTAX_H__ */
