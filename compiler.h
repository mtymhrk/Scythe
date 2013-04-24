#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

typedef struct ScmCompilerRec ScmCompiler;

#define SCM_COMPILER(obj) ((ScmCompiler *)(obj))

#include "object.h"

struct ScmCompilerRec {
  ScmObjHeader header;
  int label_id;
  ScmObj module;
};

extern ScmTypeInfo SCM_COMPILER_TYPE_INFO;

int scm_cmpl_initialize(ScmObj cmpl);
ScmObj scm_cmpl_new(SCM_MEM_TYPE_T mtype);
void scm_cmpl_select_module(ScmObj cmpl, ScmObj module);
ScmObj scm_cmpl_compile(ScmObj cmpl, ScmObj exp);

int scm_cmpl_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_cmpl_current_module(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->module;
}


int scm_initialize_module_core_syntax(void);

#ifdef SCM_UNIT_TEST

void scm_cmpl_ut_clear_label_id(void);

#endif


#endif /* INCLUDE_COMPILER_H__ */





