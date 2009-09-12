#ifndef INCLUDE_PAIR_H__
#define INCLUDE_PAIR_H__

#include <stdbool.h>

typedef struct ScmPairRec ScmPair;

#define SCM_PAIR(obj) ((ScmPair *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_PAIR_TYPE_INFO;

ScmPair *scm_pair_construct(ScmObj car, ScmObj cdr);
void scm_pair_desturct(ScmPair *pair);
ScmObj scm_pair_car(const ScmPair *pair);
ScmObj scm_pair_cdr(const ScmPair *pair);
bool scm_pair_is_pair(const ScmObj obj);
void scm_pair_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_pair_gc_initialize(ScmObj obj, ScmObj mem);
int scm_pair_gc_accpet(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_PAIR_H__ */
