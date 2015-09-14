#ifndef INCLUDE_PAIR_H__
#define INCLUDE_PAIR_H__

#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"

typedef struct ScmPairRec ScmPair;

struct ScmPairRec {
  ScmObjHeader header;
  ScmObj car;
  ScmObj cdr;
};

#define SCM_PAIR(obj) ((ScmPair *)(obj))
#define SCM_PAIR_CAR(pair) (SCM_PAIR(pair)->car)
#define SCM_PAIR_CDR(pair) (SCM_PAIR(pair)->cdr)

extern ScmTypeInfo SCM_PAIR_TYPE_INFO;

ScmObj scm_pair_P(ScmObj pair);
int scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr);
ScmObj scm_pair_new(scm_mem_type_t mtype, ScmObj car, ScmObj cdr);
int scm_pair_obj_print(ScmObj obj, ScmObj port, int kind,
                       ScmObjPrintHandler handler);
void scm_pair_gc_initialize(ScmObj obj);
int scm_pair_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_pair_p(ScmObj pair)
{
  return scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO);
}

static inline ScmObj
scm_cons(ScmObj car, ScmObj cdr)
{
  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));
  return scm_pair_new(SCM_MEM_HEAP, car, cdr);
}

static inline ScmObj
scm_car(ScmObj pair)
{
  scm_assert(scm_pair_p(pair));
  return SCM_PAIR_CAR(pair);
}

static inline ScmObj
scm_cdr(ScmObj pair)
{
  scm_assert(scm_pair_p(pair));
  return SCM_PAIR_CDR(pair);
}

static inline void
scm_set_car(ScmObj pair, ScmObj elm)
{
  scm_assert(scm_pair_p(pair));
  scm_assert(scm_obj_not_null_p(elm));
  SCM_SLOT_SETQ(ScmPair, pair, car, elm);
}

static inline void
scm_set_cdr(ScmObj pair, ScmObj elm)
{
  scm_assert(scm_pair_p(pair));
  scm_assert(scm_obj_not_null_p(elm));
  SCM_SLOT_SETQ(ScmPair, pair, cdr, elm);
}


/****************************************************************************/
/*  Lists                                                                   */
/****************************************************************************/

ScmObj scm_cxr(ScmObj pair, const char *dir);
ScmObj scm_list_P(ScmObj lst);
ScmObj scm_make_list(size_t n, ScmObj fill);
ScmObj scm_list_cv(const ScmObj *elm, size_t n);
ScmObj scm_list_cv(const ScmObj *elm, size_t n);
ScmObj scm_list(size_t n, ...);
ssize_t scm_length(ScmObj lst);
ScmObj scm_append_lst(ScmObj lst);
ScmObj scm_append_lst(ScmObj lst);
ScmObj scm_append_cv(const ScmObj *lists, size_t n);
ScmObj scm_append(size_t n, ...);
ScmObj scm_reverse(ScmObj lst);
ScmObj scm_list_tail(ScmObj lst, size_t n);
ScmObj scm_list_ref(ScmObj lst, size_t n);
int scm_list_set(ScmObj lst, size_t n, ScmObj obj);
ScmObj scm_memq(ScmObj obj, ScmObj lst);
ScmObj scm_memv(ScmObj obj, ScmObj lst);
ScmObj scm_member(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_assq(ScmObj obj, ScmObj alist);
ScmObj scm_assv(ScmObj obj, ScmObj alist);
ScmObj scm_assoc(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_list_copy(ScmObj lst);


#endif /* INCLUDE_PAIR_H__ */



