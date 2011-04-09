#include <string.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "instractions.h"
#include "iseq.h"

ScmTypeInfo SCM_ISEQ_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmISeq),
  .gc_ini_func         = scm_iseq_gc_initialize,
  .gc_fin_func         = scm_iseq_gc_finalize,
  .gc_accept_func      = scm_iseq_gc_accept,
  .gc_accept_func_weak = NULL
};


void
scm_iseq_initialize(ScmObj iseq) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);

  SCM_ISEQ_SEQ(iseq) =
    scm_memory_allocate(sizeof(scm_iword_t) * SCM_ISEQ_DEFAULT_SEQ_SIZE);
  if (SCM_ISEQ_SEQ(iseq) == NULL)
    ;                           /* TODO: error handling */

  SCM_ISEQ_IMMVAL_VEC(iseq) =
    scm_memory_allocate(sizeof(ScmObj) * SCM_ISEQ_DEFAULT_IMMVEC_SIZE);
  if (SCM_ISEQ_IMMVAL_VEC(iseq) == NULL)
    ;                           /* TODO: error handling */

  SCM_ISEQ_SEQ_CAPACITY(iseq) = SCM_ISEQ_DEFAULT_SEQ_SIZE;
  SCM_ISEQ_SEQ_LENGTH(iseq) = 0;
  SCM_ISEQ_VEC_CAPACITY(iseq) = SCM_ISEQ_DEFAULT_IMMVEC_SIZE;
  SCM_ISEQ_VEC_LENGTH(iseq) = 0;

  /* TODO: fill in by NOOP */
  /* memset(SCM_ISEQ_SEQ(iseq), 0, sizeof(scm_iword_t) * SCM_ISEQ_DEFAULT_SIZE); */
}

ScmObj
scm_iseq_new(SCM_MEM_ALLOC_TYPE_T mtype) /* GC OK */
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_ISEQ_TYPE_INFO, mtype, SCM_REF_MAKE(iseq));

  scm_iseq_initialize(iseq);

  return iseq;
}

void
scm_iseq_finalize(ScmObj obj) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_ISEQ_TYPE_INFO);

  if (SCM_ISEQ_SEQ(obj) != NULL)
    scm_memory_release(SCM_ISEQ_SEQ(obj));
}

int
scm_iseq_expand_immval_vec(ScmObj iseq) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);

  if (SCM_ISEQ_VEC_CAPACITY(iseq) > SSIZE_MAX / 2)
    return -1;

  size_t new_size = SCM_ISEQ_VEC_CAPACITY(iseq) * 2;
  ScmObj *new_vec = scm_memory_allocate(sizeof(ScmObj) * new_size);
  if (new_vec == NULL) return -1;

  SCM_COPY_OBJ_VEC(new_vec, SCM_ISEQ_IMMVAL_VEC(iseq),
                   SCM_ISEQ_VEC_LENGTH(iseq));

  SCM_ISEQ_IMMVAL_VEC(iseq) = new_vec;
  SCM_ISEQ_VEC_CAPACITY(iseq) = new_size;

  return 0;
}

int
scm_iseq_set_immval(ScmObj iseq, ScmObj val) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(val));

  if (SCM_ISEQ_VEC_LENGTH(iseq) >= SCM_ISEQ_VEC_CAPACITY(iseq))
    if (scm_iseq_expand_immval_vec(iseq) < 0)
      return -1;

  int idx = (int)SCM_ISEQ_VEC_LENGTH(iseq);
  SCM_SETQ(SCM_ISEQ_IMMVAL_VEC(iseq)[idx], val);
  SCM_ISEQ_VEC_LENGTH(iseq)++;

  return idx;
}

int
scm_iseq_expand_seq(ScmObj iseq, ssize_t needed) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(needed > 0);

  if (SCM_ISEQ_SEQ_CAPACITY(iseq) > SSIZE_MAX / 2)
    return -1;

  size_t new_size = SCM_ISEQ_VEC_CAPACITY(iseq) * 2;
  while (needed >= (ssize_t)new_size) {
    if (new_size > SSIZE_MAX / 2) return -1;
    new_size *= 2;
  }

  scm_iword_t *new_seq = scm_memory_allocate(new_size);
  if (new_seq == NULL) return -1;

  memcpy(new_seq, SCM_ISEQ_SEQ(iseq),
         sizeof(scm_iword_t) * SCM_ISEQ_SEQ_LENGTH(iseq));

  SCM_ISEQ_SEQ(iseq) = new_seq;
  SCM_ISEQ_SEQ_CAPACITY(iseq) = new_size;

  return 0;
}

scm_iword_t *
scm_iseq_set_word(ScmObj iseq, scm_iword_t *sp, scm_iword_t word) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(sp != NULL);
  assert(sp >= SCM_ISEQ_SEQ(iseq));

  scm_iword_t *p = sp;
  ssize_t idx = sp - SCM_ISEQ_SEQ(iseq);
  if (idx >=  (ssize_t)SCM_ISEQ_SEQ_CAPACITY(iseq)) {
    if (scm_iseq_expand_seq(iseq, idx) < 0)
      return NULL;
    p = SCM_ISEQ_SEQ(iseq) + idx;
  }

  *p++ = word;

  if (p - SCM_ISEQ_SEQ(iseq) > (ssize_t)SCM_ISEQ_SEQ_LENGTH(iseq))
    SCM_ISEQ_SEQ_LENGTH(iseq) = (size_t)(p - SCM_ISEQ_SEQ(iseq));

  return p;
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_ISEQ_TYPE_INFO);

  SCM_ISEQ_SEQ(obj) = NULL;
  SCM_ISEQ_IMMVAL_VEC(obj) = NULL;
}

void
scm_iseq_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_ISEQ_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  for (size_t i = 0; i < SCM_ISEQ_VEC_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_ISEQ_IMMVAL_VEC(obj)[i], mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt))
      return rslt;
  }

  return rslt;
}
