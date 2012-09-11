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

#define SCM_ISEQ_SEQ_VEC(obj) ((uint8_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IDX_VEC(obj) ((size_t *)EARY_HEAD(SCM_ISEQ_EARY_INDEX(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IDX_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_INDEX(obj)))
#define SCM_ISEQ_IDX_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_INDEX(obj)))

int scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
ssize_t scm_iseq_push_uint8(ScmObj iseq, uint8_t val);
ssize_t scm_iseq_push_uint32(ScmObj iseq, uint32_t val);
uint32_t scm_iseq_get_uint32(ScmObj iseq, size_t idx);
ssize_t scm_iseq_set_uint32(ScmObj iseq, size_t idx, uint32_t val);
ssize_t scm_iseq_push_uint64(ScmObj iseq, uint64_t val);
uint64_t scm_iseq_get_uint64(ScmObj iseq, size_t idx);
ssize_t scm_iseq_push_obj(ScmObj iseq, ScmObj obj);
ScmObj scm_iseq_get_obj(ScmObj iseq, size_t idx);
ssize_t scm_iseq_set_obj(ScmObj iseq, size_t idx, ScmObj val);
int scm_iseq_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ssize_t
scm_iseq_length(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq);
}

inline uint8_t *
scm_iseq_to_ip(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return SCM_ISEQ_SEQ_VEC(iseq);
}

inline ssize_t
scm_iseq_ip_to_idx(ScmObj iseq, uint8_t *ip)
{
  ssize_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  idx = ip - SCM_ISEQ_SEQ_VEC(iseq);
  if (idx < 0 || idx >= (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq))
    return -1;
  else
    return idx;
}

inline uint32_t
scm_iseq_fetch_uint32(uint8_t **ip)
{
  uint32_t v;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v = ((uint32_t)*(*ip + 3) << 24
       | (uint32_t)*(*ip + 2) << 16
       | (uint32_t)*(*ip + 1) << 8
       | (uint32_t)*(*ip));

  *ip = *ip + 4;

  return v;
}

inline int32_t
scm_iseq_fetch_int32(uint8_t **ip)
{
  int32_t v;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v = (int32_t)((uint32_t)*(*ip + 3) << 24
                | (uint32_t)*(*ip + 2) << 16
                | (uint32_t)*(*ip + 1) << 8
                | (uint32_t)*(*ip));

  *ip = *ip + 4;

  return v;
}

inline uint64_t
scm_iseq_fetch_uint64(uint8_t **ip)
{
  uint64_t v;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v = ((uint64_t)*(*ip + 7) << 56
       | (uint64_t)*(*ip + 6) << 48
       | (uint64_t)*(*ip + 5) << 40
       | (uint64_t)*(*ip + 4) << 32
       | (uint64_t)*(*ip + 3) << 24
       | (uint64_t)*(*ip + 2) << 16
       | (uint64_t)*(*ip + 1) << 8
       | (uint64_t)*(*ip));

  *ip = *ip + 8;

  return v;
}

#endif /* INCLUDE_ISEQ_H__ */
