#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "obuffer.h"
#include "integer.h"


ScmTypeInfo SCM_INTEGER_TYPE_INFO = {
  scm_integer_pretty_print,      /* pp_func              */
  sizeof(ScmInteger),            /* obj_size             */
  NULL,                          /* gc_ini_func          */
  NULL,                          /* gc_fin_func          */
  NULL,                          /* gc_accept_func       */
  NULL,                          /* gc_accpet_func_weak  */
};


void
scm_integer_initialize(ScmObj integer, long long value) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(integer, &SCM_INTEGER_TYPE_INFO);

  SCM_INTEGER_VALUE(integer) = value;
}

void
scm_integer_finalize(ScmObj integer) /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_integer_construct(long long value) /* GC OK */
{
  ScmObj integer = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer);

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_INTEGER_TYPE_INFO, SCM_REF_MAKE(integer));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_INTEGER_TYPE_INFO, SCM_REF_MAKE(integer)); */
  if (SCM_OBJ_IS_NULL(integer)) return SCM_OBJ_NULL;

  scm_integer_initialize(integer, value);

  return integer;
}

long long
scm_integer_value(ScmObj integer) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(integer, &SCM_INTEGER_TYPE_INFO);

  return SCM_INTEGER_VALUE(integer);
}

bool
scm_integer_is_integer(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_INTEGER_TYPE_INFO);
}

ScmObj
scm_integer_plus(ScmObj val1, ScmObj val2) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(val1, &SCM_INTEGER_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_construct(SCM_INTEGER_VALUE(val1)
                               + SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_minus(ScmObj val1, ScmObj val2) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(val1, &SCM_INTEGER_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_construct(SCM_INTEGER_VALUE(val1)
                               - SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_multiply(ScmObj val1, ScmObj val2) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(val1, &SCM_INTEGER_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_construct(SCM_INTEGER_VALUE(val1)
                               * SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_divide(ScmObj val1, ScmObj val2) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(val1, &SCM_INTEGER_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_construct(SCM_INTEGER_VALUE(val1)
                               / SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_reminder(ScmObj val1, ScmObj val2) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(val1, &SCM_INTEGER_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_construct(SCM_INTEGER_VALUE(val1)
                               % SCM_INTEGER_VALUE(val2));
}


void
scm_integer_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmInteger *integer;
  char str[21];

  assert(obj != NULL); assert(scm_integer_is_integer(obj));
  assert(obuffer != NULL);

  integer = SCM_INTEGER(obj);

  snprintf(str, sizeof(str), "%lld", integer->value);
  scm_obuffer_concatenate_string(obuffer, str);
}
