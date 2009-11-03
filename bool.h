#ifndef INCLUDE_BOOL_H__
#define INCLUDE_BOOL_H__

#include <stdbool.h>

typedef struct ScmBoolRec ScmBool;

#define SCM_BOOL(obj) ((ScmBool *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_BOOL_TYPE_INFO;

struct ScmBoolRec {
  ScmObjHeader header;
  bool value;
};

#define SCM_BOOL_VALUE(obj) (SCM_BOOL(obj)->value)

void scm_bool_initialize(ScmObj obj, bool value);
void scm_bool_finalize(ScmObj obj);
ScmObj scm_bool_construct(SCM_MEM_ALLOC_TYPE_T mtype, bool value);
ScmObj scm_bool_instance(bool value);
bool scm_bool_value(ScmObj bl);
bool scm_bool_is_bool(ScmObj obj);
void scm_bool_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_BOOL_H__ */
