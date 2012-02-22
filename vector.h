#ifndef INCLUDE_VECTOR_H__
#define INCLUDE_VECTOR_H__

#include <stdbool.h>

typedef struct ScmVectorRec ScmVector;

#define SCM_VECTOR(obj) ((ScmVector *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_VECTOR_TYPE_INFO;

struct ScmVectorRec {
  ScmObjHeader header;
  ScmObj *array;
  size_t length;
};

#define SCM_VECTOR_ARRAY(obj) (SCM_VECTOR(obj)->array)
#define SCM_VECTOR_LENGTH(obj) (SCM_VECTOR(obj)->length)

void scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill);
void scm_vector_finalize(ScmObj vector);
ScmObj scm_vector_new(SCM_CAPI_MEM_TYPE_T mtype, size_t length, ScmObj fill);
size_t scm_vector_length(ScmObj vector);
ScmObj scm_vector_ref(ScmObj vector, size_t index);
ScmObj scm_vector_set(ScmObj vector, size_t index, ScmObj obj);
void scm_vector_fill(ScmObj vector, ScmObj fill);
void scm_vector_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vector_gc_finalize(ScmObj obj);
int scm_vector_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_VECTOR_H__ */
