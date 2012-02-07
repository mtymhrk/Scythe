#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmSubrutineRec ScmSubrutine;

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

#include "object.h"
#include "vm.h"

typedef ScmObj (*ScmSubrFunc)(void);

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmObjHeader header;
  ScmSubrFunc subr_func;
};

#define SCM_SUBRUTINE_FUNC(obj) (SCM_SUBRUTINE(obj)->subr_func)
#define SCM_SUBRUTINE_CALL(ret, obj) SCM_SETQ(ret, SCM_SUBRUTINE_FUNC(obj)())

void scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func);
ScmObj scm_subrutine_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmSubrFunc func);
void scm_subrutine_call(ScmObj subr);

void scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem);


#endif /* INCLUDE_PROCEDURE_H__ */
