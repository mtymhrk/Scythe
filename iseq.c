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
    scm_memory_allocate(sizeof(scm_iseq_t) * SCM_ISEQ_DEFAULT_SIZE);
  if (SCM_ISEQ_SEQ(iseq) == NULL)
    ;                           /* TODO: error handling */

  SCM_ISEQ_SIZE(iseq) = SCM_ISEQ_DEFAULT_SIZE;

  /* TODO: fill in by NOOP */
  memset(SCM_ISEQ_SEQ(iseq), 0, sizeof(scm_iseq_t) * SCM_ISEQ_DEFAULT_SIZE);
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
scm_iseq_expand_seq(ScmObj iseq, ssize_t needed)
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(needed > 0);

  size_t new_size = SCM_ISEQ_SIZE(iseq) * 2;
  while (needed >= (ssize_t)new_size) {
    new_size *= 2;
    if (new_size > SSIZE_MAX) return -1;
  }

  scm_iseq_t *new_seq = scm_memory_allocate(new_size);
  if (new_seq == NULL) return -1;

  memcpy(new_seq, SCM_ISEQ_SEQ(iseq), SCM_ISEQ_LENGTH(iseq));

  SCM_ISEQ_SEQ(iseq) = new_seq;
  SCM_ISEQ_SIZE(iseq) = new_size;

  return 0;
}

scm_iseq_t *
scm_iseq_write_op(ScmObj iseq, scm_iseq_t *sp, SCM_INST_T op)
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(sp != NULL);
  assert(sp < SCM_ISEQ_SEQ(iseq));

  scm_iseq_t *p = sp;
  ssize_t idx = sp - SCM_ISEQ_SEQ(iseq);
  if (idx >=  (ssize_t)SCM_ISEQ_SIZE(iseq)) {
    if (scm_iseq_expand_seq(iseq, idx) < 0)
      return NULL;
    p = SCM_ISEQ_SEQ(iseq) + idx;
  }

  if (idx > (ssize_t)SCM_ISEQ_LENGTH(iseq))
    SCM_ISEQ_LENGTH(iseq) = (size_t)idx;

  return scm_iseq_set_op(p, op);
}

scm_iseq_t *
scm_iseq_write_immval(ScmObj iseq, scm_iseq_t *sp, ScmObj obj)
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(sp != NULL);
  assert(sp < SCM_ISEQ_SEQ(iseq));

  scm_iseq_t *p = sp;
  ssize_t idx = sp - SCM_ISEQ_SEQ(iseq);
  if (idx >=  (ssize_t)SCM_ISEQ_SIZE(iseq)) {
    if (scm_iseq_expand_seq(iseq, idx) < 0)
      return NULL;
    p = SCM_ISEQ_SEQ(iseq) + idx;
  }

  if (idx > (ssize_t)SCM_ISEQ_LENGTH(iseq))
    SCM_ISEQ_LENGTH(iseq) = (size_t)idx;

  return scm_iseq_set_immval(p, obj);
}

scm_iseq_t *
scm_iseq_write_primval(ScmObj iseq, scm_iseq_t *sp, int val)
{
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);
  assert(sp != NULL);
  assert(sp < SCM_ISEQ_SEQ(iseq));

  scm_iseq_t *p = sp;
  ssize_t idx = sp - SCM_ISEQ_SEQ(iseq);
  if (idx >=  (ssize_t)SCM_ISEQ_SIZE(iseq)) {
    if (scm_iseq_expand_seq(iseq, idx) < 0)
      return NULL;
    p = SCM_ISEQ_SEQ(iseq) + idx;
  }

  if (idx > (ssize_t)SCM_ISEQ_LENGTH(iseq))
    SCM_ISEQ_LENGTH(iseq) = (size_t)idx;

  return scm_iseq_set_primval(p, val);
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_ISEQ_TYPE_INFO);

  SCM_ISEQ_SEQ(obj) = NULL;
}

void
scm_iseq_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_ISEQ_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  if (SCM_ISEQ_SEQ(obj) == NULL)
    return rslt;

  /* TODO: write me */

  return rslt;
}
