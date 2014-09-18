#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

typedef struct ScmCompilerRec ScmCompiler;

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))

#include "scythe/object.h"

struct ScmCompilerRec {
  ScmObjHeader header;
  int label_id;
  ScmObj module;
  ScmObj expr;
};

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

int scm_cmpl_initialize(ScmObj cmpl, ScmObj module);
ScmObj scm_cmpl_new(SCM_MEM_TYPE_T mtype, ScmObj module);
void scm_cmpl_set_module(ScmObj cmpl, ScmObj module);
void scm_cmpl_set_expr(ScmObj cmpl, ScmObj expr);
int scm_cmpl_assign_label_id(ScmObj cmpl);

void scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_cmpl_module(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->module;
}

inline ScmObj
scm_cmpl_expr(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->expr;
}


#endif /* INCLUDE_COMPILER_H__ */
