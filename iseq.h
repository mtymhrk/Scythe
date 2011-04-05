#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;
typedef uint32_t scm_iseq_t;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
#include "memory.h"
#include "instractions.h"

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  scm_iseq_t *seq;
  size_t size;
  size_t length;
};

#define SCM_ISEQ_SEQ(obj) (SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_SIZE(obj) (SCM_ISEQ(obj)->size)
#define SCM_ISEQ_LENGTH(obj) (SCM_ISEQ(obj)->length)


#define SCM_ISEQ_DEFAULT_SIZE 32


void scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_ALLOC_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
int scm_iseq_expand_seq(ScmObj iseq, ssize_t needed);
scm_iseq_t *scm_iseq_set_op(ScmObj iseq, scm_iseq_t *sp, SCM_INST_T op);
scm_iseq_t *scm_iseq_set_immval(ScmObj iseq, scm_iseq_t *sp, ScmObj obj);
scm_iseq_t *scm_iseq_set_primval(ScmObj iseq, scm_iseq_t *sp, int val);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline scm_iseq_t *
scm_iseq_set_32(scm_iseq_t *seq, uint32_t val)
{
  *seq = val;
  return seq + sizeof(val)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_32(scm_iseq_t *seq, uint32_t *vp)
{
  *vp = *(uint32_t*)seq;
  return seq + sizeof(*vp)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_set_64(scm_iseq_t *seq, int64_t val)
{
  /* memmove(seq, &val, sizeof(val)); */
  *(int64_t *)seq = val;
  return seq + sizeof(val)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_64(scm_iseq_t *seq, int64_t *vp)
{
  /* memmove(vp, seq, sizeof(*vp)); */
  *vp = *(int64_t *)seq;
  return seq + sizeof(*vp)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_op(scm_iseq_t *seq, SCM_INST_T *po)
{
  return scm_iseq_get_32(seq, (uint32_t *)po);
}

static inline scm_iseq_t *
scm_iseq_get_immval(scm_iseq_t *seq, ScmRef ref)
{
  SCM_REF_SETQ(ref, SCM_REF_OBJ((ScmRef)seq));
  return seq + sizeof(ScmObj)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_primval(scm_iseq_t *seq, int *vp)
{
  return scm_iseq_get_32(seq, (uint32_t *)vp);
}

#endif /* INCLUDE_ISEQ_H__ */
