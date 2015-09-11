#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

typedef struct ScmCompilerRec ScmCompiler;
typedef struct ScmQQTmplNodeRec ScmQQTmplNode;
typedef struct ScmQQTmplRec ScmQQTmpl;
typedef struct ScmIdentifierRec ScmIdentifier;

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))
#define SCM_QQTMPLNODE(obj) ((ScmQQTmplNode *)(obj))
#define SCM_QQTMPL(obj) ((ScmQQTmpl *)(obj))
#define SCM_IDENTIFIER(obj) ((ScmIdentifier *)(obj))

#include "scythe/object.h"


/*************************************************************************/
/* Compiler                                                              */
/*************************************************************************/

struct ScmCompilerRec {
  ScmObjHeader header;
  ScmObj env;
  ScmObj expr;
};

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

int scm_cmpl_initialize(ScmObj cmpl, ScmObj module);
void scm_cmpl_set_env(ScmObj cmpl, ScmObj env);
void scm_cmpl_set_expr(ScmObj cmpl, ScmObj expr);

void scm_cmpl_gc_initialize(ScmObj obj);
int scm_cmpl_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_cmpl_env(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->env;
}

static inline ScmObj
scm_cmpl_expr(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->expr;
}


/*************************************************************************/
/* Quasiquotation                                                        */
/*************************************************************************/

enum {
  SCM_QQ_TMPL_NODE_LITERAL,
  SCM_QQ_TMPL_NODE_UNQUOTE,
  SCM_QQ_TMPL_NODE_UNQ_SPL,
};

struct ScmQQTmplNodeRec {
  ScmObjHeader header;
  int kind;
  ScmObj obj;
};

extern ScmTypeInfo SCM_QQTMPLNODE_TYPE_INFO;

int scm_qqtn_initialize(ScmObj node, int kind, ScmObj obj);
void scm_qqtn_update_contents(ScmObj node, int kind, ScmObj obj);
void scm_qqtn_gc_initialize(ScmObj obj);
int scm_qqtn_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_qqtn_valid_kind_p(int kind)
{
  return (kind == SCM_QQ_TMPL_NODE_LITERAL
          || kind == SCM_QQ_TMPL_NODE_UNQUOTE
          || kind == SCM_QQ_TMPL_NODE_UNQ_SPL);
}

static inline int
scm_qqtn_kind(ScmObj node)
{
  scm_assert_obj_type(node, &SCM_QQTMPLNODE_TYPE_INFO);
  return SCM_QQTMPLNODE(node)->kind;
}

static inline ScmObj
scm_qqtn_object(ScmObj node)
{
  scm_assert_obj_type(node, &SCM_QQTMPLNODE_TYPE_INFO);
  return SCM_QQTMPLNODE(node)->obj;
}

struct ScmQQTmplRec {
  ScmObjHeader header;
  ScmObj orig;
  ScmObj compiled;
  ScmObj expr;
};

extern ScmTypeInfo SCM_QQTMPL_TYPE_INFO;

int scm_qqtmpl_initialize(ScmObj qqtmpl, ScmObj tmpl);
size_t scm_qqtmpl_nr_unquoted_expr(ScmObj qqtmpl);
ScmObj scm_qqtmpl_unquoted_expr(ScmObj qqtmpl, size_t n);
ssize_t scm_qqtmpl_push_unquoted_expr(ScmObj qqtmpl, ScmObj expr);
int scm_qqtmpl_compiled(ScmObj qqtmpl, ScmObj compiled);
void scm_qqtmpl_chg_orig_template(ScmObj qqtmpl, ScmObj tmpl);
int scm_qqtmpl_eq(ScmObj qqtmpl1, ScmObj qqtmpl2, bool *rslt);
void scm_qqtmpl_gc_initialize(ScmObj obj);
int scm_qqtmpl_gc_accept(ScmObj obj, ScmGCRefHandler handler);


static inline ScmObj
scm_qqtmpl_template(ScmObj qqtmpl)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  return SCM_QQTMPL(qqtmpl)->orig;
}

static inline ScmObj
scm_qqtmpl_compiled_template(ScmObj qqtmpl)
{
  scm_assert_obj_type(qqtmpl, &SCM_QQTMPL_TYPE_INFO);
  return SCM_QQTMPL(qqtmpl)->compiled;
}


ScmObj scm_cmpl_compile_qq_tmpl(ScmObj qqtmpl, ScmObj tmpl, size_t depth);
ScmObj scm_cmpl_substitute_qq_tmpl(ScmObj qqtmpl, ScmObj tmpl,
                                   scm_csetter_t *values);


/*************************************************************************/
/* Identifier                                                            */
/*************************************************************************/

struct ScmIdentifierRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj env;
};

extern ScmTypeInfo SCM_IDENTIFIER_TYPE_INFO;

#define SCM_IDENT_NAME(ident) (SCM_IDENTIFIER(ident)->name)
#define SCM_IDENT_ENV(ident) (SCM_IDENTIFIER(ident)->env)
#define SCM_IDENT_SET_NAME(ident, n) \
  SCM_SLOT_SETQ(ScmIdentifier, ident, name, n)
#define SCM_IDENT_SET_ENV(ident, e) \
  SCM_SLOT_SETQ(ScmIdentifier, ident, env, e)

int scm_ident_initialize(ScmObj ident, ScmObj name, ScmObj env);
int scm_ident_obj_print(ScmObj obj, ScmObj port, int kind, ScmObjPrintHandler handler);
void scm_ident_gc_initialize(ScmObj obj);
int scm_ident_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_ident_name(ScmObj ident)
{
  scm_assert_obj_type(ident, &SCM_IDENTIFIER_TYPE_INFO);
  return SCM_IDENT_NAME(ident);
}

static inline ScmObj
scm_ident_env(ScmObj ident)
{
  scm_assert_obj_type(ident, &SCM_IDENTIFIER_TYPE_INFO);
  return SCM_IDENT_ENV(ident);
}


#endif /* INCLUDE_COMPILER_H__ */
