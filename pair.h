#ifndef INCLUDE_PAIR_H__
#define INCLUDE_PAIR_H__

#include <stdbool.h>

typedef struct ScmPairRec ScmPair;

#define SCM_PAIR(obj) ((ScmPair *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_PAIR_TYPE_INFO;

struct ScmPairRec {
  ScmObjHeader header;
  ScmObj car;
  ScmObj cdr;
};

#define SCM_PAIR_CAR(pair) (SCM_PAIR(pair)->car)
#define SCM_PAIR_CDR(pair) (SCM_PAIR(pair)->cdr)

void scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr);
ScmObj scm_pair_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj car, ScmObj cdr);
ScmObj scm_pair_car(ScmObj pair);
ScmObj scm_pair_cdr(ScmObj pair);
bool scm_pair_is_pair(const ScmObj obj);
void scm_pair_gc_initialize(ScmObj obj, ScmObj mem);
int scm_pair_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_PAIR_H__ */
