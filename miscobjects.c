#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "obuffer.h"
#include "miscobjects.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

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
scm_eof_construct(SCM_MEM_ALLOC_TYPE_T mtype)         /* GC OK */
{
  ScmObj eof;

  /* scm_mem_alloc_root(scm_vm_current_mm(), */
  /*                    &SCM_EOF_TYPE_INFO, SCM_REF_MAKE(eof)); */
  /* TODO: replace above by below */
  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_EOF_TYPE_INFO, mtype, SCM_REF_MAKE(eof));
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



/*******************************************************/
/*  ScmBool                                            */
/*******************************************************/

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



/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

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
scm_nil_construct(SCM_MEM_ALLOC_TYPE_T mtype)         /* GC OK */
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&nil);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_NIL_TYPE_INFO, mtype, SCM_REF_MAKE(nil));
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
