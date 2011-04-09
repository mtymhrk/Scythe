#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
#include "memory.h"
#include "instractions.h"

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  scm_iword_t *seq;
  ScmObj *immval_vec;
  size_t seq_capacity;
  size_t seq_length;
  size_t vec_capacity;
  size_t vec_length;
};

#define SCM_ISEQ_SEQ(obj) (SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_IMMVAL_VEC(obj) (SCM_ISEQ(obj)->immval_vec)
#define SCM_ISEQ_SEQ_CAPACITY(obj) (SCM_ISEQ(obj)->seq_capacity)
#define SCM_ISEQ_SEQ_LENGTH(obj) (SCM_ISEQ(obj)->seq_length)
#define SCM_ISEQ_VEC_CAPACITY(obj) (SCM_ISEQ(obj)->vec_capacity)
#define SCM_ISEQ_VEC_LENGTH(obj) (SCM_ISEQ(obj)->vec_length)


#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_IMMVEC_SIZE 32

void scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_ALLOC_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
int scm_iseq_expand_immval_vec(ScmObj iseq);
int scm_iseq_set_immval(ScmObj iseq, ScmObj val);
int scm_iseq_expand_seq(ScmObj iseq, ssize_t needed);
scm_iword_t *scm_iseq_set_word(ScmObj iseq, scm_iword_t *sp, scm_iword_t word);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline scm_iword_t *
scm_iseq_get_word(scm_iword_t *seq, scm_iword_t *word)
{
  assert(seq != NULL);
  assert(word != NULL);

  *word = *seq;
  return seq + 1;
}

static inline ScmObj
scm_iseq_get_immval(ScmObj iseq, int idx)
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(idx >= 0);

  return SCM_ISEQ_IMMVAL_VEC(iseq)[idx];
}

#endif /* INCLUDE_ISEQ_H__ */
