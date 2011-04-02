#include <string.h>

#include "object.h"
#include "memory.h"
#include "reference.h"
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
