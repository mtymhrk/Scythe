#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "vm.h"
#include "object.h"
#include "obuffer.h"
#include "nil.h"

#define SCM_NIL(obj) ((ScmNil *)(obj))


ScmTypeInfo SCM_NIL_TYPE_INFO = {
  scm_nil_pretty_print,      /* pp_func              */
  sizeof(ScmNil),            /* obj_size             */
  NULL,                      /* gc_ini_func          */
  NULL,                      /* gc_fin_func          */
  NULL,                      /* gc_accept_func       */
  NULL,                      /* gc_accpet_func_weak  */
};

void
scm_nil_initialize(ScmObj nil)  /* GC OK */
{
  return;                       /* nothing to do */
}

void
scm_nil_finalize(ScmObj nil)    /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_nil_construct(void)         /* GC OK */
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&nil);

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_NIL_TYPE_INFO, SCM_REF_MAKE(nil));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_NIL_TYPE_INFO, SCM_REF_MAKE(nil)); */
  if (SCM_OBJ_IS_NULL(nil)) return SCM_OBJ_NULL;

  scm_nil_initialize(nil);

  return nil;
}

ScmObj
scm_nil_instance(void)          /* GC OK */
{
  return scm_vm_nil_instance();
}

bool
scm_nil_is_nil(ScmObj obj)      /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_NIL_TYPE_INFO);
}

void
scm_nil_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  scm_obuffer_concatenate_string(obuffer, "()");
}

