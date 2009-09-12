#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "nil.h"
#include "vector.h"
#include "obuffer.h"

struct ScmVectorRec {
  ScmObjHeader header;
  ScmObj *array;
  size_t length;
};

ScmTypeInfo SCM_VECTOR_TYPE_INFO = {
  scm_vector_pretty_print,      /* pp_func              */
  sizeof(ScmVector),            /* obj_size             */
  scm_vector_gc_initialize,     /* gc_ini_func          */
  scm_vector_gc_finalize,       /* gc_fin_func          */
  scm_vector_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

static void
scm_vector_finalize(ScmVector *vector)
{
  assert(vector != NULL);
  scm_memory_release(vector->array);
}

ScmVector *
scm_vector_construct(size_t length)
{
  ScmVector *vector;
  size_t i;

  vector = scm_memory_allocate(sizeof(ScmVector));
  scm_obj_init(SCM_OBJ(vector), &SCM_VECTOR_TYPE_INFO);

  if (length > 0)
    vector->array = scm_memory_allocate(sizeof(ScmObj) * length);
  else
    vector->array = NULL;

  vector->length = length;

  /* initial value of elements of the vector is nil. */
  /* the initial value is not specified in R5RS */
  for (i = 0; i < length; i++)
    vector->array[i] = SCM_OBJ(scm_nil_instance());

  return vector;
}

void
scm_vector_destruct(ScmVector *vector)
{
  assert(vector != NULL);

  scm_vector_finalize(vector);
  scm_memory_release(vector);
}

size_t
scm_vector_length(ScmVector *vector)
{
  assert(vector != NULL);
  return vector->length;
}

ScmObj
scm_vector_ref(ScmVector *vector, size_t index)
{
  assert(vector != NULL);
  assert(index < vector->length);

  return vector->array[index];
}

ScmObj
scm_vector_set(ScmVector *vector, size_t index, ScmObj obj)
{
  assert(vector != NULL);
  assert(index < vector->length);

  return vector->array[index] = obj;
}

bool
scm_vector_is_vector(ScmObj obj)
{
  assert(obj != NULL);

  return SCM_OBJ_IS_TYPE(obj, &SCM_VECTOR_TYPE_INFO);
}

void
scm_vector_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmVector *vector;


  assert(obj != NULL); assert(scm_vector_is_vector(obj));
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
scm_vector_gc_initialize(ScmObj obj, ScmObj mem)
{
  ScmVector *vec;

  assert(obj != NULL);
  assert(mem != NULL);

  vec = SCM_VECTOR(obj);
  vec->length = 0;
}

void
scm_vector_gc_finalize(ScmObj obj)
{
  scm_vector_finalize(SCM_VECTOR(obj));
}

int
scm_vector_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmVector *vec;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  size_t i;

  assert(obj != NULL);
  assert(mem != NULL);
  assert(handler != NULL);

  vec = SCM_VECTOR(obj);
  for (i = 0; i < vec->length; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vec->array[i], mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
  }

  return rslt;
}
