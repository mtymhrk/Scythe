
#ifndef INCLUDE_EARRAY_H__
#define INCLUDE_EARRAY_H__

#include "stdlib.h"

typedef struct EArrayRec EArray;

#include "object.h"

#define EARY_MAG 2

struct EArrayRec {
  size_t cap;
  size_t used;
  void *vec;
};

#define EARY_CAPACITY(ary) ((ary)->cap)
#define EARY_SIZE(ary) ((ary)->used)
#define EARY_HEAD(ary) ((ary)->vec)

inline int
eary_init(EArray *ary, size_t rs, size_t ns)
{
  ary->cap = ns;
  ary->used = 0;
  if (ns == 0) {
    ary->vec = NULL;
  }
  else {
    ary->vec = malloc(rs * ns);
    if (ary->vec == NULL) {
      free(ary);
      return -1;
    }
  }

  return 0;
}

inline void
eary_fin(EArray *ary)
{
  free(ary->vec);
  ary->vec = NULL;
}

inline int
eary_expand(EArray *ary, size_t rs, size_t ndd)
{
  if (ary->cap > SIZE_MAX / EARY_MAG)
    return -1;

  size_t ns = ary->cap * EARY_MAG;
  while (ndd > ns) {
    if (ns > SIZE_MAX / EARY_MAG)
      return -1;
    ns *= EARY_MAG;
  }

  void *p = realloc(ary->vec, rs * ns);
  if (p == NULL)
    return -1;

  ary->vec = p;
  ary->cap = ns;

  return 0;
}

inline int
eary_expand_if_necessary(EArray *ary, size_t idx, size_t rs)
{
  if (idx >= ary->cap)
    return eary_expand(ary, rs, idx + 1);
  else
    return 0;
}


#define EARY_GET(ary, typ, idx, val)            \
  do {                                          \
    assert((size_t)idx < (ary)->used);          \
    (val) = (type *)((ary)->vec)[idx];          \
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

#define EARY_FOR_EACH(ary, typ, idx, itr)   \
  for(size_t idx = 0, typ itr = (ary)->vec; \
      idx < (ary)->used;                    \
      idx++, itr++)

#endif /* INCLUDE_EARRAY_H__ */
