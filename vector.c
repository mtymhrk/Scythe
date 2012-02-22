#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "vector.h"

ScmTypeInfo SCM_VECTOR_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmVector),            /* obj_size             */
  scm_vector_gc_initialize,     /* gc_ini_func          */
  scm_vector_gc_finalize,       /* gc_fin_func          */
  scm_vector_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

void
scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill) /* GC OK */
{
  size_t i;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);

  if (length > 0)
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
  else
    SCM_VECTOR_ARRAY(vector) = NULL;

  SCM_VECTOR_LENGTH(vector) = length;

  for (i = 0; i < length; i++)
    SCM_SLOT_SETQ(ScmVector, vector, array[i], fill);
}

void
scm_vector_finalize(ScmObj vector) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  if (SCM_VECTOR_ARRAY(vector) != NULL)
    scm_capi_free(SCM_VECTOR_ARRAY(vector));
}

ScmObj
scm_vector_new(SCM_CAPI_MEM_TYPE_T mtype,
               size_t length, ScmObj fill) /* GC OK */
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fill, &vector);

  /* Vector の実装として、length を SSIZE_MAX 以下に制限する必要はないが、
     api.c との兼ね合いで制限する */
  scm_assert(length <= SSIZE_MAX);

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  scm_vector_initialize(vector, length, fill);

  return vector;
}

size_t
scm_vector_length(ScmObj vector) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  return SCM_VECTOR_LENGTH(vector);
}

ScmObj
scm_vector_ref(ScmObj vector, size_t index) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(index < SCM_VECTOR_LENGTH(vector));

  return SCM_VECTOR_ARRAY(vector)[index];
}

ScmObj
scm_vector_set(ScmObj vector, size_t index, ScmObj obj) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(index < SCM_VECTOR_LENGTH(vector));
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmVector, vector, array[index], obj);

  return obj;
}

void
scm_vector_fill(ScmObj vector, ScmObj fill)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(fill));

  for (size_t i = 0; i < SCM_VECTOR_LENGTH(vector); i++)
    SCM_SLOT_SETQ(ScmVector, vector, array[i], fill);
}

void
scm_vector_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

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

  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  for (i = 0; i < SCM_VECTOR_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_VECTOR_ARRAY(obj)[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
