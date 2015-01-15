#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

typedef struct ScmCompilerRec ScmCompiler;

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))

#include "scythe/object.h"
#include "scythe/fcd_type.h"

struct ScmCompilerRec {
  ScmObjHeader header;
  ScmObj module;
  ScmObj expr;
};

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

int scm_cmpl_initialize(ScmObj cmpl, ScmObj module);
void scm_cmpl_set_module(ScmObj cmpl, ScmObj module);
void scm_cmpl_set_expr(ScmObj cmpl, ScmObj expr);

void scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmObj
scm_cmpl_module(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->module;
}

static inline ScmObj
scm_cmpl_expr(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->expr;
}


#endif /* INCLUDE_COMPILER_H__ */
