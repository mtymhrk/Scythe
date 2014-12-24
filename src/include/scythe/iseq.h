#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/earray.h"

#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_OBJS_SIZE 32
#define SCM_ISEQ_DEFAULT_DSTS_SIZE 32

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  EArray seq;
  EArray objs;
  EArray dsts;
};

#define SCM_ISEQ_EARY_SEQ(obj) (&SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_EARY_OBJS(obj) (&SCM_ISEQ(obj)->objs)
#define SCM_ISEQ_EARY_DSTS(obj) (&SCM_ISEQ(obj)->dsts)

#define SCM_ISEQ_SEQ_VEC(obj) ((scm_byte_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_OBJS_VEC(obj) ((size_t *)EARY_HEAD(SCM_ISEQ_EARY_OBJS(obj)))
#define SCM_ISEQ_DSTS_VEC(obj) ((size_t *)EARY_HEAD(SCM_ISEQ_EARY_DSTS(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_OBJS_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_OBJS(obj)))
#define SCM_ISEQ_OBJS_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_OBJS(obj)))
#define SCM_ISEQ_DSTS_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_DSTS(obj)))
#define SCM_ISEQ_DSTS_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_DSTS(obj)))

int scm_iseq_initialize(ScmObj iseq);
void scm_iseq_finalize(ScmObj obj);
ssize_t scm_iseq_push_inst_noopd(ScmObj iseq, scm_opcode_t op);
ssize_t scm_iseq_push_inst_obj(ScmObj iseq, scm_opcode_t op, ScmObj obj);
ssize_t scm_iseq_push_inst_obj_obj(ScmObj iseq,
                                   scm_opcode_t op, ScmObj obj1, ScmObj obj2);
ssize_t scm_iseq_push_inst_si(ScmObj iseq, scm_opcode_t op, int si);
ssize_t scm_iseq_push_inst_si_si(ScmObj iseq,
                                 scm_opcode_t op, int si1, int si2);
ssize_t scm_iseq_push_inst_si_si_obj(ScmObj iseq,
                                     scm_opcode_t op,
                                     int si1, int si2, ScmObj obj);
ssize_t scm_iseq_push_inst_iof(ScmObj iseq, scm_opcode_t op, int iof);
int scm_iseq_update_opd_iof(ScmObj iseq, size_t idx, int iof);
int scm_iseq_update_opd_obj(ScmObj iseq, size_t idx, ScmObj obj);
int scm_iseq_push_dst(ScmObj iseq, size_t offset);
int scm_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline size_t
scm_iseq_length(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return (size_t)SCM_ISEQ_SEQ_LENGTH(iseq);
}

static inline scm_byte_t *
scm_iseq_to_ip(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  return SCM_ISEQ_SEQ_VEC(iseq);
}

static inline ssize_t
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

static inline size_t
scm_iseq_nr_dst(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  return SCM_ISEQ_DSTS_LENGTH(iseq);
}

static inline ssize_t
scm_iseq_dst(ScmObj iseq, size_t idx)
{
  size_t x;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  if (idx >= SCM_ISEQ_DSTS_LENGTH(iseq))
    return -1;

  EARY_GET(SCM_ISEQ_EARY_DSTS(iseq), size_t, idx, x);
  return (ssize_t)x;
}

static inline const size_t *
scm_iseq_dsts(ScmObj iseq)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  return SCM_ISEQ_DSTS_VEC(iseq);
}

#endif /* INCLUDE_ISEQ_H__ */
