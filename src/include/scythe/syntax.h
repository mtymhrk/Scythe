#ifndef INCLUDE_SYNTAX_H__
#define INCLUDE_SYNTAX_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"


/*************************************************************************/
/* Syntax                                                                */
/*************************************************************************/

#define SCM_SYNTAX_KEYWORD_LEN_MAX 64

typedef struct ScmSyntaxRec ScmSyntax;

struct ScmSyntaxRec {
  ScmObjHeader header;
  ScmObj keyword;
  ScmObj handler;
};

#define SCM_SYNTAX(obj) ((ScmSyntax *)(obj))

extern ScmTypeInfo SCM_SYNTAX_TYPE_INFO;

ScmObj scm_syntax_P(ScmObj obj);
int scm_syntax_initialize(ScmObj syx, ScmObj key, ScmObj handler);
ScmObj scm_syntax_new(scm_mem_type_t mtype, ScmObj key, ScmObj handler);
int scm_syntax_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_syntax_gc_initialize(ScmObj obj);
int scm_syntax_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_syntax_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ? true : false);
}

static inline ScmObj
scm_make_syntax(ScmObj keyword, ScmObj handler)
{
  return scm_syntax_new(SCM_MEM_HEAP, keyword, handler);
}

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

typedef struct ScmMacroRec ScmMacro;

struct ScmMacroRec {
  ScmObjHeader header;
  ScmObj transformer;
  ScmObj env;
};

#define SCM_MACRO(obj) ((ScmMacro *)(obj))

extern ScmTypeInfo SCM_MACRO_TYPE_INFO;

#define SCM_MACRO_TRANSFORMER(macro) (SCM_MACRO(macro)->transformer)
#define SCM_MACRO_ENV(macro) (SCM_MACRO(macro)->env)
#define SCM_MACRO_SET_TRANSFORMER(macro, t) \
  SCM_SLOT_SETQ(ScmMacro, macro, transformer, t)
#define SCM_MACRO_SET_ENV(macro, e) \
  SCM_SLOT_SETQ(ScmMacro, macro, env, e)

ScmObj scm_macro_P(ScmObj obj);
int scm_macro_initialize(ScmObj macro, ScmObj transformer, ScmObj env);
ScmObj scm_macro_new(scm_mem_type_t mtype, ScmObj transformer, ScmObj env);
int scm_macro_trmp_transformer(ScmObj macro, ScmObj form);
void scm_macro_gc_initialize(ScmObj obj);
int scm_macro_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_macro_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MACRO_TYPE_INFO);
}

static inline ScmObj
scm_make_macro(ScmObj transformer, ScmObj env)
{
  return scm_macro_new(SCM_MEM_HEAP, transformer, env);
}

static inline ScmObj
scm_macro_env(ScmObj macro)
{
  scm_assert_obj_type(macro, &SCM_MACRO_TYPE_INFO);

  return SCM_MACRO_ENV(macro);
}


#endif /* INCLUDE_SYNTAX_H__ */
