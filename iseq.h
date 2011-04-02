#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

typedef struct ScmISeqRec ScmISeq;
typedef int scm_iseq_t;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  scm_iseq_t *seq;
  size_t size;
};

#define SCM_ISEQ_SEQ(obj) (SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_SIZE(obj) (SCM_ISEQ(obj)->size)

#define SCM_ISEQ_DEFAULT_SIZE 32


void scm_iseq_initialize(ScmObj iseq);
void scm_iseq_finalize(ScmObj obj);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_ISEQ_H__ */
