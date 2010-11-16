#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "vm.h"
#include "miscobjects.h"
#include "vector.h"
#include "obuffer.h"

ScmTypeInfo SCM_VECTOR_TYPE_INFO = {
  scm_vector_pretty_print,      /* pp_func              */
  sizeof(ScmVector),            /* obj_size             */
  scm_vector_gc_initialize,     /* gc_ini_func          */
  scm_vector_gc_finalize,       /* gc_fin_func          */
  scm_vector_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

void
scm_vector_initialize(ScmObj vector, size_t length) /* GC OK */
{
  size_t i;

  SCM_OBJ_ASSERT_TYPE(vector, &SCM_VECTOR_TYPE_INFO);

  if (length > 0)
    SCM_VECTOR_ARRAY(vector) = scm_memory_allocate(sizeof(ScmObj) * length);
  else
    SCM_VECTOR_ARRAY(vector) = NULL;

  SCM_VECTOR_LENGTH(vector) = length;

  /* initial value of elements of the vector is nil. */
  /* the initial value is not specified in R5RS */
  for (i = 0; i < length; i++)
    SCM_VECTOR_ARRAY(vector)[i] = SCM_OBJ(scm_nil_instance());
}

void
scm_vector_finalize(ScmObj vector) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vector, &SCM_VECTOR_TYPE_INFO);

  if (SCM_VECTOR_ARRAY(vector) != NULL)
    scm_memory_release(SCM_VECTOR_ARRAY(vector));
}

ScmObj
scm_vector_new(SCM_MEM_ALLOC_TYPE_T mtype, size_t length) /* GC OK */
{
  ScmObj vector;

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_VECTOR_TYPE_INFO, mtype, SCM_REF_MAKE(vector));
  if (SCM_OBJ_IS_NULL(vector)) return SCM_OBJ_NULL;

  scm_vector_initialize(vector, length);

  return vector;
}

size_t
scm_vector_length(ScmObj vector) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vector, &SCM_VECTOR_TYPE_INFO);

  return SCM_VECTOR_LENGTH(vector);
}

ScmObj
scm_vector_ref(ScmObj vector, size_t index) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vector, &SCM_VECTOR_TYPE_INFO);
  assert(index < SCM_VECTOR_LENGTH(vector));

  return SCM_VECTOR_ARRAY(vector)[index];
}

ScmObj
scm_vector_set(ScmObj vector, size_t index, ScmObj obj) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vector, &SCM_VECTOR_TYPE_INFO);
  assert(index < SCM_VECTOR_LENGTH(vector));

  return SCM_VECTOR_ARRAY(vector)[index] = obj;
}

bool
scm_vector_is_vector(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_VECTOR_TYPE_INFO);
}

void
scm_vector_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmVector *vector;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VECTOR_TYPE_INFO);
  assert(obuffer != NULL);

  vector = SCM_VECTOR(obj);

  scm_obuffer_concatenate_string(obuffer, "#(");
  if (vector->length > 0) {
    size_t nloop = vector->length - 1;
    size_t i;
    for (i = 0; i < nloop; i++) {
      scm_obj_pretty_print(vector->array[i], obuffer);
      scm_obuffer_concatenate_char(obuffer, ' ');
    }
    scm_obj_pretty_print(vector->array[i], obuffer);
  }
  scm_obuffer_concatenate_char(obuffer, ')');
}

void
scm_vector_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VECTOR_TYPE_INFO);

  SCM_VECTOR_ARRAY(obj) = NULL;
  SCM_VECTOR_LENGTH(obj) = 0;
}

void
scm_vector_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_vector_finalize(obj);
}

int
scm_vector_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  size_t i;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VECTOR_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  for (i = 0; i < SCM_VECTOR_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_VECTOR_ARRAY(obj)[i], mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
  }

  return rslt;
}
