#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmSubrutineRec ScmSubrutine;

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

#include "object.h"
#include "obuffer.h"
#include "vm.h"

typedef void (*ScmSubrFunc)(void);

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmObjHeader header;
  /* TODO: define signature */
  ScmSubrFunc subr_func;
};

#define SCM_SUBRUTINE_FUNC(obj) (SCM_SUBRUTINE(obj)->subr_func)
#define SCM_SUBRUTINE_CALL(obj) (SCM_SUBRUTINE_FUNC(obj)())

void scm_subrutine_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem);

#endif /* INCLUDE_PROCEDURE_H__ */
