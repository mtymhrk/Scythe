#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "integer.h"


ScmTypeInfo SCM_INTEGER_TYPE_INFO = {
  NULL,                          /* pp_func              */
  sizeof(ScmInteger),            /* obj_size             */
  NULL,                          /* gc_ini_func          */
  NULL,                          /* gc_fin_func          */
  NULL,                          /* gc_accept_func       */
  NULL,                          /* gc_accpet_func_weak  */
};


void
scm_integer_initialize(ScmObj integer, long long value) /* GC OK */
{
  scm_assert_obj_type(integer, &SCM_INTEGER_TYPE_INFO);

  SCM_INTEGER_VALUE(integer) = value;
}

void
scm_integer_finalize(ScmObj integer) /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_integer_new(SCM_MEM_ALLOC_TYPE_T mtype, long long value) /* GC OK */
{
  ScmObj integer = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_INTEGER_TYPE_INFO, mtype, SCM_REF_MAKE(integer));
  if (scm_obj_null_p(integer)) return SCM_OBJ_NULL;

  scm_integer_initialize(integer, value);

  return integer;
}

long long
scm_integer_value(ScmObj integer) /* GC OK */
{
  scm_assert_obj_type(integer, &SCM_INTEGER_TYPE_INFO);

  return SCM_INTEGER_VALUE(integer);
}

bool
scm_integer_is_integer(ScmObj obj) /* GC OK */
{
  assert(scm_obj_not_null_p(obj));

  return scm_obj_type_p(obj, &SCM_INTEGER_TYPE_INFO);
}

ScmObj
scm_integer_plus(ScmObj val1, ScmObj val2) /* GC OK */
{
  scm_assert_obj_type(val1, &SCM_INTEGER_TYPE_INFO);
  scm_assert_obj_type(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_new(SCM_MEM_ALLOC_HEAP,
                               SCM_INTEGER_VALUE(val1)
                               + SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_minus(ScmObj val1, ScmObj val2) /* GC OK */
{
  scm_assert_obj_type(val1, &SCM_INTEGER_TYPE_INFO);
  scm_assert_obj_type(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_new(SCM_MEM_ALLOC_HEAP,
                               SCM_INTEGER_VALUE(val1)
                               - SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_multiply(ScmObj val1, ScmObj val2) /* GC OK */
{
  scm_assert_obj_type(val1, &SCM_INTEGER_TYPE_INFO);
  scm_assert_obj_type(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_new(SCM_MEM_ALLOC_HEAP,
                               SCM_INTEGER_VALUE(val1)
                               * SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_divide(ScmObj val1, ScmObj val2) /* GC OK */
{
  scm_assert_obj_type(val1, &SCM_INTEGER_TYPE_INFO);
  scm_assert_obj_type(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_new(SCM_MEM_ALLOC_HEAP,
                               SCM_INTEGER_VALUE(val1)
                               / SCM_INTEGER_VALUE(val2));
}

ScmObj
scm_integer_reminder(ScmObj val1, ScmObj val2) /* GC OK */
{
  scm_assert_obj_type(val1, &SCM_INTEGER_TYPE_INFO);
  scm_assert_obj_type(val2, &SCM_INTEGER_TYPE_INFO);

  return scm_integer_new(SCM_MEM_ALLOC_HEAP,
                               SCM_INTEGER_VALUE(val1)
                               % SCM_INTEGER_VALUE(val2));
}

