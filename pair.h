#ifndef INCLUDE_PAIR_H__
#define INCLUDE_PAIR_H__

#include <stdbool.h>

typedef struct ScmPairRec ScmPair;

#define SCM_PAIR(obj) ((ScmPair *)(obj))

#include "object.h"
#include "api_enum.h"

extern ScmTypeInfo SCM_PAIR_TYPE_INFO;

struct ScmPairRec {
  ScmObjHeader header;
  ScmObj car;
  ScmObj cdr;
};

#define SCM_PAIR_CAR(pair) (SCM_PAIR(pair)->car)
#define SCM_PAIR_CDR(pair) (SCM_PAIR(pair)->cdr)

int scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr);
ScmObj scm_pair_new(SCM_MEM_TYPE_T mtype, ScmObj car, ScmObj cdr);
int scm_pair_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_pair_gc_initialize(ScmObj obj, ScmObj mem);
int scm_pair_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_pair_car(ScmObj pair)       /* GC OK */
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CAR(pair);
}

inline ScmObj
scm_pair_cdr(ScmObj pair)       /* GC OK */
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CDR(pair);
}

inline int
scm_pair_set_car(ScmObj pair, ScmObj elm)
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  SCM_SLOT_SETQ(ScmPair, pair, car, elm);

  return 0;
}

inline int
scm_pair_set_cdr(ScmObj pair, ScmObj elm)
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  SCM_SLOT_SETQ(ScmPair, pair, cdr, elm);

  return 0;
}


#endif /* INCLUDE_PAIR_H__ */



