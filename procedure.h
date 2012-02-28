#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmSubrutineRec ScmSubrutine;

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

#include "object.h"
#include "vm.h"

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmObjHeader header;
  ScmSubrFunc subr_func;
};

void scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func);
ScmObj scm_subrutine_new(SCM_MEM_TYPE_T mtype, ScmSubrFunc func);
ScmObj scm_subrutine_call(ScmObj subr);

void scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem);


#endif /* INCLUDE_PROCEDURE_H__ */
