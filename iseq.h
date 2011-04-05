#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>

typedef struct ScmISeqRec ScmISeq;
typedef int32_t scm_iseq_t;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
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
void scm_iseq_finalize(ScmObj obj);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


static inline scm_iseq_t *
scm_iseq_set_32(scm_iseq_t *seq, int32_t val)
{
  *seq = val;
  return seq + 1;
}

static inline scm_iseq_t *
scm_iseq_get_32(scm_iseq_t *seq, int32_t *vp)
{
  *vp = *seq;
  return seq + 1;
}

static inline scm_iseq_t *
scm_iseq_set_64(scm_iseq_t *seq, int64_t val)
{
  *(int64_t *)seq = val;
  return seq + sizeof(val)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_64(scm_iseq_t *seq, int64_t *vp)
{
  *vp = *(int64_t *)seq;
  return seq + sizeof(*vp)/sizeof(*seq);
}

static inline scm_iseq_t *
scm_iseq_get_op(scm_iseq_t *seq, SCM_INST_T *po)
{
  return scm_iseq_get_32(seq, (int32_t *)po);
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
  return scm_iseq_get_32(seq, vp);
}

#endif /* INCLUDE_ISEQ_H__ */
