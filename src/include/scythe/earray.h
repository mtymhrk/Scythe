
#ifndef INCLUDE_EARRAY_H__
#define INCLUDE_EARRAY_H__

#include <stdlib.h>
#include <assert.h>

typedef struct EArrayRec EArray;

#include "scythe/object.h"
#include "scythe/impl_utils.h"

#define EARY_MAG 2
#define EARY_DEFAULT_CAP 2

struct EArrayRec {
  size_t cap;
  size_t used;
  size_t rs;
  void *vec;
};

#define EARY_CAPACITY(ary) ((ary)->cap)
#define EARY_SIZE(ary) ((ary)->used)
#define EARY_HEAD(ary) ((ary)->vec)

int eary_init(EArray *ary, size_t rs, size_t ns);
void eary_fin(EArray *ary);
int eary_expand(EArray *ary, size_t ndd);
int eary_contract(EArray *ary);

static inline int
eary_expand_if_necessary(EArray *ary, size_t idx)
{
  if (idx >= ary->cap)
    return eary_expand(ary, idx + 1);
  else
    return 0;
}

static inline void
eary_truncate(EArray *ary)
{
  ary->used = 0;
}

static inline void *
eary_chuck_ary(EArray *ary)
{
  void *a = ary->vec;

  ary->vec = NULL;
  ary->cap = 0;
  ary->used = 0;
  ary->rs = 0;

  return a;
}

static inline void *
eary_idx_to_ptr(EArray *ary, size_t idx)
{
  scm_assert(ary != NULL);
  scm_assert(idx < ary->used);
  return ((char *)(ary->vec)) + (ary->rs * idx);
}

static inline void *
eary_prepare_to_set(EArray *ary, size_t idx)
{
  scm_assert(ary != NULL);

  if (eary_expand_if_necessary(ary, idx) != 0)
    return NULL;

  if (idx >= ary->used)
    ary->used = idx + 1;

  return eary_idx_to_ptr(ary, idx);
}

static inline void *
eary_prepare_to_set_scmobj(EArray *ary, size_t idx)
{
  size_t used;
  void *p;

  scm_assert(ary != NULL);

  used = ary->used;
  p = eary_prepare_to_set(ary, idx);
  if (p == NULL) return NULL;

  for(size_t i = used; i < idx; i++)
    ((ScmObj *)(ary->vec))[i] = SCM_OBJ_NULL;

  return p;
}


#define EARY_GET(ary, typ, idx, val)                    \
  do {                                                  \
    assert((size_t)(idx) < (ary)->used);                \
    (val) = *(typ *)eary_idx_to_ptr(ary, idx);  \
  } while(0)

#define EARY_SET(ary, typ, idx, val, err)                               \
  do {                                                                  \
    void *e_a_r_y__ptr = eary_prepare_to_set(ary, idx);                 \
    if (e_a_r_y__ptr == NULL) {                                         \
      (err) = -1;                                                       \
      break;                                                            \
    }                                                                   \
    *(typ *)e_a_r_y__ptr = (typ)(val);                                  \
    (err) = 0;                                                          \
  } while(0)

#define EARY_SET_SCMOBJ(ary, idx, val, owner, err)                      \
  do {                                                                  \
    void *e_a_r_y__ptr = eary_prepare_to_set_scmobj(ary, idx);          \
    if (e_a_r_y__ptr == NULL) {                                         \
      (err) = -1;                                                       \
      break;                                                            \
    }                                                                   \
    SCM_WB_SETQ(owner, *(ScmObj *)e_a_r_y__ptr, val);                   \
    (err) = 0;                                                          \
  } while(0)

#define EARY_PUSH(ary, typ, val, err)                 \
  do {                                                \
    size_t e_a_r_y__idx_ = (ary)->used;               \
    EARY_SET(ary, typ, e_a_r_y__idx_, val, err);      \
  } while(0)

#define EARY_POP(ary, type, val)                \
  do {                                          \
    EARY_GET(ary, type, (ary)->used - 1, val);  \
    (ary)->used--;                              \
  } while(0)

#define EARY_PUSH_SCMOBJ(ary, val, owner, err)              \
  do {                                                      \
    size_t e_a_r_y__idx_ = (ary)->used;                     \
    EARY_SET_SCMOBJ(ary, e_a_r_y__idx_, val, owner, err);   \
  } while(0)

#define EARY_FOR_EACH(ary, idx, itr)            \
  for(idx = 0, itr = (ary)->vec;                \
      idx < (ary)->used;                        \
      idx++, itr++)

#endif /* INCLUDE_EARRAY_H__ */
