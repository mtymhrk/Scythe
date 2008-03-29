#ifndef INCLUDE_PAIR_H__
#define INCLUDE_PAIR_H__

#include <stdbool.h>

typedef struct ScmPairRec ScmPair;

#define SCM_PAIR(ojb) ((ScmPair *)(obj))

#include "object.h"

ScmPair *scm_pair_construct(ScmObj car, ScmObj cdr);
ScmObj scm_pair_car(const ScmPair *pair);
ScmObj scm_pair_cdr(const ScmPair *pair);
bool scm_pair_is_pair(const ScmObj obj);

#endif /* INCLUDE_PAIR_H__ */
