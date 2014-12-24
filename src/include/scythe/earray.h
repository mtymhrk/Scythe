
#ifndef INCLUDE_EARRAY_H__
#define INCLUDE_EARRAY_H__

#include <stdlib.h>
#include <assert.h>

typedef struct EArrayRec EArray;

#include "scythe/object.h"

#define EARY_MAG 2
#define EARY_DEFAULT_CAP 2

struct EArrayRec {
  size_t cap;
  size_t used;
  void *vec;
};

#define EARY_CAPACITY(ary) ((ary)->cap)
#define EARY_SIZE(ary) ((ary)->used)
#define EARY_HEAD(ary) ((ary)->vec)

int eary_init(EArray *ary, size_t rs, size_t ns);
void eary_fin(EArray *ary);
int eary_expand(EArray *ary, size_t rs, size_t ndd);

static inline int
eary_expand_if_necessary(EArray *ary, size_t idx, size_t rs)
{
  if (idx >= ary->cap)
    return eary_expand(ary, rs, idx + 1);
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

  return a;
}

#define EARY_GET(ary, typ, idx, val)            \
  do {                                          \
    assert((size_t)idx < (ary)->used);          \
    (val) = ((typ *)((ary)->vec))[idx];        \
  } while(0)

#define EARY_SET(ary, typ, idx, val, err)                       \
  do {                                                          \
    (err) = -1;                                                 \
    if (eary_expand_if_necessary(ary, idx, sizeof(typ)) != 0)   \
      break;                                                    \
                                                  \
    ((typ *)((ary)->vec))[idx] = (typ)val;        \
    if (idx >= (ary)->used)                       \
      (ary)->used = idx + 1;                      \
    (err) = 0;                                    \
  } while(0)

#define EARY_SET_SCMOBJ(ary, idx, val, owner, err)                       \
  do {                                                                  \
    (err) = -1;                                                         \
    if (eary_expand_if_necessary(ary, idx, sizeof(ScmObj)) != 0)        \
      break;                                                    \
                                                  \
    SCM_WB_SETQ(owner, ((ScmObj *)((ary)->vec))[idx], val); \
    if (idx >= (ary)->used)                       \
      (ary)->used = idx + 1;                      \
    (err) = 0;                                    \
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
