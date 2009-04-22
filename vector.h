#ifndef INCLUDE_VECTOR_H__
#define INCLUDE_VECTOR_H__

#include <stdbool.h>

typedef struct ScmVectorRec ScmVector;

#define SCM_VECTOR(obj) ((ScmVector *)(obj))

#include "object.h"

extern const ScmTypeInfo SCM_VECTOR_TYPE_INFO;

ScmVector *scm_vector_construct(size_t length);
void scm_vector_destruct(ScmVector *vector);
size_t scm_vector_length(ScmVector *vector);
ScmObj scm_vector_ref(ScmVector *vector, size_t index);
ScmObj scm_vector_set(ScmVector *vector, size_t index, ScmObj obj);
bool scm_vector_is_vector(ScmObj obj);
void scm_vector_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_vector_gc_initialize(ScmObj obj, ScmMem *mem);
void scm_vector_gc_finalize(ScmObj obj);
int scm_vector_gc_ref_iter_begin(ScmObj obj, ScmGCRefItr *itr);
int scm_vector_gc_ref_iter_next(ScmGCRefItr *itr);

#endif /* INCLUDE_VECTOR_H__ */
