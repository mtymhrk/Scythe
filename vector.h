#ifndef INCLUDE_VECTOR_H__
#define INCLUDE_VECTOR_H__

#include <stdbool.h>

typedef struct ScmVectorRec ScmVector;

#define SCM_VECTOR(obj) ((ScmVector *)(obj))

#include "object.h"

ScmVector *scm_vector_construct(size_t length);
void scm_vector_destruct(ScmVector *vector);
size_t scm_vector_length(ScmVector *vector);
ScmObj scm_vector_ref(ScmVector *vector, size_t index);
ScmObj scm_vector_set(ScmVector *vector, size_t index, ScmObj obj);
bool scm_vector_is_vector(ScmObj obj);

#endif /* INCLUDE_VECTOR_H__ */
