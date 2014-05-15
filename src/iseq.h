#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
#include "api_enum.h"
#include "earray.h"

#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_INDEX_SIZE 32

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  EArray seq;
  EArray index;
};

#define SCM_ISEQ_EARY_SEQ(obj) (&SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_EARY_INDEX(obj) (&SCM_ISEQ(obj)->index)

#define SCM_ISEQ_SEQ_VEC(obj) ((scm_byte_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IDX_VEC(obj) ((size_t *)EARY_HEAD(SCM_ISEQ_EARY_INDEX(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IDX_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_INDEX(obj)))
#define SCM_ISEQ_IDX_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_INDEX(obj)))

int scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
ssize_t scm_iseq_push_ushort(ScmObj iseq, unsigned short val);
unsigned short scm_iseq_get_ushort(ScmObj iseq, size_t idx);
ssize_t scm_iseq_push_uint(ScmObj iseq, unsigned int val);
unsigned int scm_iseq_get_uint(ScmObj iseq, size_t idx);
ssize_t scm_iseq_set_uint(ScmObj iseq, size_t idx, unsigned int val);
ssize_t scm_iseq_push_ulong(ScmObj iseq, unsigned long val);
unsigned long scm_iseq_get_ulong(ScmObj iseq, size_t idx);
ssize_t scm_iseq_set_ulong(ScmObj iseq, size_t idx, unsigned long val);
ssize_t scm_iseq_push_ullong(ScmObj iseq, unsigned long long val);
unsigned long long scm_iseq_get_ullong(ScmObj iseq, size_t idx);
ssize_t scm_iseq_push_obj(ScmObj iseq, ScmObj obj);
ScmObj scm_iseq_get_obj(ScmObj iseq, size_t idx);
ssize_t scm_iseq_set_obj(ScmObj iseq, size_t idx, ScmObj val);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ssize_t
scm_iseq_length(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq);
}

inline scm_byte_t *
scm_iseq_to_ip(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return SCM_ISEQ_SEQ_VEC(iseq);
}

inline ssize_t
scm_iseq_ip_to_idx(ScmObj iseq, scm_byte_t *ip)
{
  ptrdiff_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  idx = ip - SCM_ISEQ_SEQ_VEC(iseq);
  if (idx < 0 || idx >= (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq))
    return -1;
  else
    return (ssize_t)idx;
}

inline unsigned short
scm_iseq_fetch_ushort(scm_byte_t **ip)
{
  unsigned short v;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v = (unsigned int)*(unsigned short *)*ip;
  *ip = *ip + sizeof(unsigned short);

  return v;
}

inline unsigned int
scm_iseq_fetch_uint(scm_byte_t **ip)
{
  unsigned int v;

  v = scm_iseq_fetch_ushort(ip);

#if SIZEOF_INT >= SIZEOF_SHORT * 2
  v |= (unsigned int)scm_iseq_fetch_ushort(ip) << SCM_SHRT_BIT;
#endif

  return v;
}

inline int
scm_iseq_fetch_int(scm_byte_t **ip)
{
  return (int)scm_iseq_fetch_uint(ip);
}

inline unsigned long
scm_iseq_fetch_ulong(scm_byte_t **ip)
{
  unsigned long v;

  v = scm_iseq_fetch_uint(ip);

#if SIZEOF_LONG >= SIZEOF_INT * 2
  v |= (unsigned long)scm_iseq_fetch_uint(ip) << SCM_INT_BIT;
#endif

  return v;
}

inline unsigned long long
scm_iseq_fetch_ullong(scm_byte_t **ip)
{
  unsigned long long v;

  v = scm_iseq_fetch_uint(ip);

#if SIZEOF_LLONG >= SIZEOF_INT * 2
  v |= (unsigned long long)scm_iseq_fetch_uint(ip) << SCM_INT_BIT;
#endif

  return v;
}

inline ScmObj
scm_iseq_fetch_obj(scm_byte_t **ip)
{
#if SIZEOF_SCM_WORD_T == SIZEOF_SHORT
  return SCM_OBJ(scm_iseq_fetch_ushort(ip));
#elsif SIZEOF_SCM_WORD_T == SIZEOF_INT
  return SCM_OBJ(scm_iseq_fetch_uint(ip));
#elsif SIZEOF_SCM_WORD_T == SIZEOF_LONG
  return SCM_OBJ(scm_iseq_fetch_ulong(ip));
#else
  return SCM_OBJ(scm_iseq_fetch_ullong(ip));
#endif
}


#endif /* INCLUDE_ISEQ_H__ */
