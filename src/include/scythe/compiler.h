#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"


/*************************************************************************/
/* Compiler                                                              */
/*************************************************************************/

typedef struct ScmCompilerRec ScmCompiler;

struct ScmCompilerRec {
  ScmObjHeader header;
  ScmObj env;
  ScmObj expr;
};

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

ScmObj scm_compiler_P(ScmObj obj);
int scm_cmpl_initialize(ScmObj cmpl, ScmObj module);
ScmObj scm_compiler_new(scm_mem_type_t mtype, ScmObj env);
void scm_cmpl_select_base_env(ScmObj cmpl, ScmObj env);
int scm_cmpl_select_module(ScmObj cmpl, ScmObj mod);
void scm_cmpl_select_expr(ScmObj cmpl, ScmObj expr);

void scm_cmpl_gc_initialize(ScmObj obj);
int scm_cmpl_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_compiler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_COMPILER_TYPE_INFO);
}

static inline ScmObj
scm_make_compiler(ScmObj env)
{
  return scm_compiler_new(SCM_MEM_HEAP, env);
}

static inline ScmObj
scm_cmpl_base_env(ScmObj cmpl)
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

typedef struct ScmQQTmplNodeRec ScmQQTmplNode;

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

#define SCM_QQTMPLNODE(obj) ((ScmQQTmplNode *)(obj))

extern ScmTypeInfo SCM_QQTMPLNODE_TYPE_INFO;

int scm_qqtn_initialize(ScmObj node, int kind, ScmObj obj);
ScmObj scm_qqtn_new(scm_mem_type_t mtype, int kind, ScmObj obj);
void scm_qqtn_update_contents(ScmObj node, int kind, ScmObj obj);
void scm_qqtn_get_contents_for_marshal(ScmObj node, int *kind, scm_csetter_t *obj);
int scm_qqtn_setup_for_unmarshal(ScmObj node, int kind, ScmObj obj);
void scm_qqtn_gc_initialize(ScmObj obj);
int scm_qqtn_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_qqtmplnode_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_QQTMPLNODE_TYPE_INFO);
}

static inline ScmObj
scm_make_qqtmplnode_for_unmarshal(void)
{
  return scm_qqtn_new(SCM_MEM_HEAP, SCM_QQ_TMPL_NODE_UNQUOTE, SCM_OBJ_NULL);
}

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

typedef struct ScmQQTmplRec ScmQQTmpl;

struct ScmQQTmplRec {
  ScmObjHeader header;
  ScmObj orig;
  ScmObj compiled;
  ScmObj expr;
};

#define SCM_QQTMPL(obj) ((ScmQQTmpl *)(obj))

extern ScmTypeInfo SCM_QQTMPL_TYPE_INFO;

int scm_qqtmpl_initialize(ScmObj qqtmpl, ScmObj tmpl);
ScmObj scm_qqtmpl_new(scm_mem_type_t mtype, ScmObj tmpl);
ScmObj scm_make_qqtmpl_for_unmarshal(void);
size_t scm_qqtmpl_nr_unquoted_expr(ScmObj qqtmpl);
ScmObj scm_qqtmpl_unquoted_expr(ScmObj qqtmpl, size_t n);
ssize_t scm_qqtmpl_push_unquoted_expr(ScmObj qqtmpl, ScmObj expr);
int scm_qqtmpl_compiled(ScmObj qqtmpl, ScmObj compiled);
void scm_qqtmpl_chg_orig_template(ScmObj qqtmpl, ScmObj tmpl);
int scm_qqtmpl_get_contents_for_marshal(ScmObj qq,
                                        scm_csetter_t *tmpl,
                                        scm_csetter_t *compiled,
                                        scm_csetter_t *expr);
int scm_qqtmpl_setup_for_unmarshal(ScmObj qq,
                                   ScmObj tmpl, ScmObj compiled, ScmObj expr);
int scm_qqtmpl_eq(ScmObj qqtmpl1, ScmObj qqtmpl2, bool *rslt);
void scm_qqtmpl_gc_initialize(ScmObj obj);
int scm_qqtmpl_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_qqtmpl_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_QQTMPL_TYPE_INFO);
}

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


/*************************************************************************/
/* Compile/Substitue qq-template                                         */
/*************************************************************************/

ScmObj scm_compile_qq_template(ScmObj tmpl);
ScmObj scm_substitute_qq_template(ScmObj qq, ScmObj values);


#endif /* INCLUDE_COMPILER_H__ */
