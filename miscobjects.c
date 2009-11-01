#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "obuffer.h"
#include "miscobjects.h"

static ScmEOF *eof_instance = NULL;

ScmTypeInfo SCM_EOF_TYPE_INFO = {
  scm_eof_pretty_print,      /* pp_func         */
  sizeof(ScmEOF),            /* obj_size        */
  NULL,                      /* gc_ini_func     */
  NULL,                      /* gc_fin_func     */
  NULL,                      /* gc_accept_func */
  false                      /* has_weak_ref    */
};

void
scm_eof_initialize(ScmObj eof)
{
  return;                       /* nothing to do */
}

void
scm_eof_finalize(ScmObj eof)
{
  return;                       /* nothing to do */
}

ScmObj
scm_eof_construct(void)         /* GC OK */
{
  ScmObj eof;

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_EOF_TYPE_INFO, SCM_REF_MAKE(eof));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_EOF_TYPE_INFO, SCM_REF_MAKE(eof)); */
  if (SCM_OBJ_IS_NULL(eof)) return SCM_OBJ_NULL;

  scm_eof_initialize(eof);

  return eof;
}

ScmObj
scm_eof_instance(void)          /* GC OK */
{
  return scm_vm_eof_instance();
}

bool
scm_eof_is_eof(ScmObj obj)      /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_EOF_TYPE_INFO);
}

void
scm_eof_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  assert(obj != NULL); assert(scm_eof_is_eof(obj));
  assert(obuffer != NULL);

  scm_obuffer_concatenate_string(obuffer, "#<eof>");
}

