#ifndef INCLUDE_NIL_H__
#define INCLUDE_NIL_H__

#include <stdbool.h>

typedef struct ScmNilRec ScmNil;

#include "object.h"

struct ScmNilRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_NIL_TYPE_INFO;

void scm_nil_initialize(ScmObj nil);
void scm_nil_finalize(ScmObj nil);
ScmObj scm_nil_construct(SCM_MEM_ALLOC_TYPE_T mtype);
ScmObj scm_nil_instance(void);
bool scm_nil_is_nil(ScmObj obj);
void scm_nil_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /*  INCLUDE_NIL_H__ */
