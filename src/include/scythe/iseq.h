#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/earray.h"
#include "scythe/memory.h"

#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_OBJS_SIZE 32
#define SCM_ISEQ_DEFAULT_DSTS_SIZE 32

typedef struct ScmISeqRec ScmISeq;

struct ScmISeqRec {
  ScmObjHeader header;
  EArray seq;
  EArray objs;
};

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))
#define SCM_ISEQ_EARY_SEQ(obj) (&SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_EARY_OBJS(obj) (&SCM_ISEQ(obj)->objs)

#define SCM_ISEQ_SEQ_VEC(obj) ((scm_byte_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_OBJS_VEC(obj) ((size_t *)EARY_HEAD(SCM_ISEQ_EARY_OBJS(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_OBJS_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_OBJS(obj)))
#define SCM_ISEQ_OBJS_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_OBJS(obj)))

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

int scm_iseq_initialize(ScmObj iseq);
void scm_iseq_finalize(ScmObj obj);
ScmObj scm_iseq_new(scm_mem_type_t mtype);
ssize_t scm_iseq_ip_to_offset(ScmObj iseq, scm_byte_t *ip);
ssize_t scm_iseq_push_inst(ScmObj iseq, const void *inst, size_t sz,
                           const size_t *objs, size_t n);
int scm_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt);
void scm_iseq_gc_initialize(ScmObj obj);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_iseq_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_ISEQ_TYPE_INFO) ? true : false);
}

static inline ScmObj
scm_make_iseq(void)
{
  return scm_iseq_new(SCM_MEM_HEAP);
}

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

static inline bool
scm_iseq_ip_in_range_p(ScmObj iseq, const scm_byte_t *ip)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  return (SCM_ISEQ_SEQ_VEC(iseq) <= ip
          && ip < (SCM_ISEQ_SEQ_VEC(iseq) + SCM_ISEQ_SEQ_LENGTH(iseq)));
}


#endif /* INCLUDE_ISEQ_H__ */
