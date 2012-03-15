#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
#include "api.h"
#include "earray.h"

#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_IMMVS_SIZE 32

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  EArray seq;
  EArray immvs;
};

#define SCM_ISEQ_EARY_SEQ(obj) (&SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_EARY_IMMVS(obj) (&SCM_ISEQ(obj)->immvs)

#define SCM_ISEQ_SEQ(obj) ((uint8_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IMMVAL_VEC(obj) ((ScmObj *)EARY_HEAD(SCM_ISEQ_EARY_IMMVS(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_VEC_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_IMMVS(obj)))
#define SCM_ISEQ_VEC_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_IMMVS(obj)))

int scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ssize_t
scm_iseq_length(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq);
}

inline ssize_t
scm_iseq_nr_immv(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return (ssize_t)SCM_ISEQ_VEC_LENGTH(iseq);
}

inline ScmObj
scm_iseq_get_immval(ScmObj iseq, size_t idx)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  return SCM_ISEQ_IMMVAL_VEC(iseq)[idx];
}

inline ssize_t
scm_iseq_push_immval(ScmObj iseq, ScmObj val)
{
  size_t idx;
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  idx = SCM_ISEQ_VEC_LENGTH(iseq);
  if (idx == SIZE_MAX) return -1; /* [ERR]: iseq: immediate value area over flow */

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), idx, val, iseq, err);

  if(err != 0) return -1;       /* [ERR]: [through] */

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_set_immval(ScmObj iseq, size_t idx, ScmObj val)
{
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx < SCM_ISEQ_VEC_LENGTH(iseq));
  scm_assert(scm_obj_not_null_p(val));

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), (size_t)idx, val, iseq, err);

  if (err != 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_get_uint8(ScmObj iseq, size_t idx, uint8_t *val)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - 1);
  scm_assert(val != NULL);

  *val = SCM_ISEQ_SEQ(iseq)[idx];

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_get_uint32(ScmObj iseq, size_t idx, uint32_t *val)
{
  uint32_t v1, v2, v3, v4;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - 4);

  v1 = SCM_ISEQ_SEQ(iseq)[idx];
  v2 = SCM_ISEQ_SEQ(iseq)[idx + 1];
  v3 = SCM_ISEQ_SEQ(iseq)[idx + 2];
  v4 = SCM_ISEQ_SEQ(iseq)[idx + 3];

  *val = (v4 << 24) | (v3 << 16) | (v2 << 8) | v1;

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_push_uint8(ScmObj iseq, uint8_t val)
{
  int err;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) < SSIZE_MAX - 1);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));
  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx, val, err);
  if (err != 0) return -1;

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_push_uint32(ScmObj iseq, uint32_t val)
{
  int err;
  uint8_t v1, v2, v3, v4;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) < SSIZE_MAX - 1);

  v1 = (uint8_t)(val & 0xff);
  v2 = (uint8_t)((val >> 8) & 0xff);
  v3 = (uint8_t)((val >> 16) & 0xff);
  v4 = (uint8_t)((val >> 24) & 0xff);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx, v1, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 1, v2, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 2, v3, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 3, v4, err);
  if (err != 0) return -1;

  return (ssize_t)idx;
}

inline ssize_t
scm_iseq_set_uint32(ScmObj iseq, size_t idx, uint32_t val)
{
  int err;
  uint8_t v1, v2, v3, v4;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - 4);
  scm_assert(scm_obj_not_null_p(val));

  v1 = (uint8_t)(val & 0xff);
  v2 = (uint8_t)((val >> 8) & 0xff);
  v3 = (uint8_t)((val >> 16) & 0xff);
  v4 = (uint8_t)((val >> 24) & 0xff);

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx, v1, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 1, v2, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 2, v3, err);
  if (err != 0) return -1;

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 3, v4, err);
  if (err != 0) return -1;

  return (ssize_t)idx;
}

#endif /* INCLUDE_ISEQ_H__ */
