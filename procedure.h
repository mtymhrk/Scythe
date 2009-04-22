#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmPrimProcRec ScmPrimProc;

#include "object.h"
#include "obuffer.h"
#include "vm.h"

typedef void (*PrimProcFunc)(int argc, ScmObj *argv, ScmVM *vm);

extern const ScmTypeInfo SCM_PRIM_PROC_TYPE_INFO;

struct ScmPrimProcRec {
  ScmObjHeader header;
  /* TODO: define signature */
  PrimProcFunc prim_proc;
};

void scm_prim_proc_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_PROCEDURE_H__ */
