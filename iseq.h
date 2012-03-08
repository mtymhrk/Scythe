#ifndef INCLUDE_ISEQ_H__
#define INCLUDE_ISEQ_H__

#include <stdint.h>
#include <string.h>

typedef struct ScmISeqRec ScmISeq;

#define SCM_ISEQ(obj) ((ScmISeq *)(obj))

#include "object.h"
#include "api.h"
#include "instractions.h"
#include "earray.h"

#define SCM_ISEQ_DEFAULT_SEQ_SIZE 32
#define SCM_ISEQ_DEFAULT_IMMVS_SIZE 32
#define SCM_ISEQ_IMMVS_MAX SCM_INST_IMMVAL_MAX
#define SCM_ISEQ_LABEL_NAME_MAX 256

/* pseudo-instructions */
enum {
  SCM_ISEQ_PI_LABEL = 0x10000000,  /* define a label */
  SCM_ISEQ_PI_ASM,                 /* make ScmISeq object and set it to VAL register */
};

extern ScmTypeInfo SCM_ISEQ_TYPE_INFO;

struct ScmISeqRec {
  ScmObjHeader header;
  EArray seq;
  EArray immvs;
};

typedef struct {
  char label[SCM_ISEQ_LABEL_NAME_MAX];
  EArray ref;
  size_t idx;
  bool defined_p;
} ScmLabelInfo;

#define SCM_ISEQ_EARY_SEQ(obj) (&SCM_ISEQ(obj)->seq)
#define SCM_ISEQ_EARY_IMMVS(obj) (&SCM_ISEQ(obj)->immvs)

#define SCM_ISEQ_SEQ(obj) ((scm_iword_t *)EARY_HEAD(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_IMMVAL_VEC(obj) ((ScmObj *)EARY_HEAD(SCM_ISEQ_EARY_IMMVS(obj)))
#define SCM_ISEQ_SEQ_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_SEQ_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_SEQ(obj)))
#define SCM_ISEQ_VEC_CAPACITY(obj) (EARY_CAPACITY(SCM_ISEQ_EARY_IMMVS(obj)))
#define SCM_ISEQ_VEC_LENGTH(obj) (EARY_SIZE(SCM_ISEQ_EARY_IMMVS(obj)))

int scm_iseq_initialize(ScmObj iseq);
ScmObj scm_iseq_new(SCM_MEM_TYPE_T mtype);
void scm_iseq_finalize(ScmObj obj);
int scm_iseq_set_immval(ScmObj iseq, ScmObj val);
int scm_iseq_update_immval(ScmObj iseq, int idx, ScmObj val);
int scm_iseq_set_word(ScmObj iseq, size_t index, scm_iword_t word);
ssize_t scm_iseq_push_word(ScmObj iseq, scm_iword_t word);
int scm_iseq_get_word(ScmObj iseq, size_t index, scm_iword_t *word);
ScmObj scm_iseq_list_to_iseq(ScmObj lst);
void scm_iseq_gc_initialize(ScmObj obj, ScmObj mem);
void scm_iseq_gc_finalize(ScmObj obj);
int scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_iseq_get_immval(ScmObj iseq, int idx)
{
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx >= 0);

  return SCM_ISEQ_IMMVAL_VEC(iseq)[idx];
}

#endif /* INCLUDE_ISEQ_H__ */
