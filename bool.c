#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "obuffer.h"
#include "bool.h"

ScmTypeInfo SCM_BOOL_TYPE_INFO = {
  scm_bool_pretty_print,      /* pp_func              */
  sizeof(ScmBool),            /* obj_size             */
  NULL,                       /* gc_ini_func          */
  NULL,                       /* gc_fin_func          */
  NULL,                       /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};


void
scm_bool_initialize(ScmObj obj, bool value) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_BOOL_TYPE_INFO);

  SCM_BOOL_VALUE(obj) = value;
}

void
scm_bool_finalize(ScmObj obj)   /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_bool_construct(SCM_MEM_ALLOC_TYPE_T mtype, bool value)  /* GC OK */
{
  ScmObj bl = SCM_OBJ_INIT;;

  SCM_STACK_FRAME_PUSH(&bl);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_BOOL_TYPE_INFO, mtype, SCM_REF_MAKE(bl));
  if (SCM_OBJ_IS_NULL(bl)) return SCM_OBJ_NULL;

  scm_bool_initialize(bl, value);

  return bl;
}

ScmObj
scm_bool_instance(bool value)   /* GC OKsh */
{
  if (value)
    return scm_vm_bool_true_instance();
  else
    return scm_vm_bool_false_instance();
}

bool
scm_bool_value(ScmObj bl)       /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(bl, &SCM_BOOL_TYPE_INFO);

  return SCM_BOOL_VALUE(bl);
}

bool
scm_bool_is_bool(ScmObj obj)    /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_BOOL_TYPE_INFO);
}

void
scm_bool_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmBool *boolv;

  assert(obj != NULL); assert(scm_bool_is_bool(obj));
  assert(obuffer != NULL);

  boolv = SCM_BOOL(obj);

  if (boolv->value)
    scm_obuffer_concatenate_string(obuffer, "#t");
  else
    scm_obuffer_concatenate_string(obuffer, "#f");
}
