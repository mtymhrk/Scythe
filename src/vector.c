#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "vector.h"

ScmTypeInfo SCM_VECTOR_TYPE_INFO = {
  .name                = "vector",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_vector_obj_print,
  .obj_size            = sizeof(ScmVector),
  .gc_ini_func         = scm_vector_gc_initialize,
  .gc_fin_func         = scm_vector_gc_finalize,
  .gc_accept_func      = scm_vector_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill) /* GC OK */
{
  size_t i;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);

  if (length > 0) {
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1; /* [ERR]: [through] */
  }
  else {
    SCM_VECTOR_ARRAY(vector) = NULL;
  }

  SCM_VECTOR_LENGTH(vector) = length;

  if (scm_obj_null_p(fill))
    fill = SCM_UNDEF_OBJ;

  for (i = 0; i < length; i++)
    SCM_SLOT_SETQ(ScmVector, vector, array[i], fill);

  return 0;
}

int
scm_vector_initialize_ary(ScmObj vector, const ScmObj *elms, size_t length)
{
  size_t i;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length == 0 || (length > 0 && elms != NULL));
  scm_assert(length <= SSIZE_MAX);

  if (length > 0) {
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1; /* [ERR]: [through] */
  }
  else {
    SCM_VECTOR_ARRAY(vector) = NULL;
  }

  SCM_VECTOR_LENGTH(vector) = length;

  for (i = 0; i < length; i++) {
    if (scm_obj_null_p(elms[i])) {
      scm_capi_error("faild to make vector: invalid element", 0);
      return -1;
    }
    SCM_SLOT_SETQ(ScmVector, vector, array[i], elms[i]);
  }

  return 0;
}

int
scm_vector_initialize_lst(ScmObj vector, size_t length, ScmObj lst)
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  size_t i;

  SCM_STACK_FRAME_PUSH(&vector, &lst,
                       &l, &e);

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);
  scm_assert(scm_obj_not_null_p(lst));

  if (length > 0) {
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1; /* [ERR]: [through] */
  }
  else {
    SCM_VECTOR_ARRAY(vector) = NULL;
  }

  SCM_VECTOR_LENGTH(vector) = length;

  for (l = lst, i = 0;
       scm_capi_pair_p(l) && i < length;
       l = scm_api_cdr(l), i++) {
    e = scm_api_car(l);
    if (scm_obj_null_p(e)) return -1;

    SCM_SLOT_SETQ(ScmVector, vector, array[i], e);
  }

  if (scm_obj_null_p(l)) return -1;

  while (i < length)
    SCM_SLOT_SETQ(ScmVector, vector, array[i++], SCM_UNDEF_OBJ);

  return 0;
}

void
scm_vector_finalize(ScmObj vector) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  if (SCM_VECTOR_ARRAY(vector) != NULL)
    scm_capi_free(SCM_VECTOR_ARRAY(vector));
}

ScmObj
scm_vector_new(SCM_MEM_TYPE_T mtype,
               size_t length, ScmObj fill) /* GC OK */
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fill, &vector);

  /* Vector の実装として、length を SSIZE_MAX 以下に制限する必要はないが、
     api.c との兼ね合いで制限する */
  scm_assert(length <= SSIZE_MAX);

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_vector_initialize(vector, length, fill) < 0)
    return SCM_OBJ_NULL; /* [ERR]: [through] */

  return vector;
}

ScmObj
scm_vector_new_from_ary(SCM_MEM_TYPE_T mtype, const ScmObj *elms, size_t length)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vector);

  scm_assert(length == 0 || (length > 0 && elms != NULL));
  scm_assert(length <= SSIZE_MAX);

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_vector_initialize_ary(vector, elms, length) < 0)
    return SCM_OBJ_NULL; /* [ERR]: [through] */

  return vector;
}

ScmObj
scm_vector_new_from_list(SCM_MEM_TYPE_T mtype, size_t length, ScmObj lst)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &vector);

  scm_assert(length <= SSIZE_MAX);
  scm_assert(scm_obj_not_null_p(lst));

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_vector_initialize_lst(vector, length, lst) < 0)
    return SCM_OBJ_NULL; /* [ERR]: [through] */

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

int
scm_vector_set(ScmObj vector, size_t index, ScmObj obj) /* GC OK */
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(index < SCM_VECTOR_LENGTH(vector));
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmVector, vector, array[index], obj);

  return 0;
}

void
scm_vector_fill(ScmObj vector, ScmObj fill)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(fill));

  for (size_t i = 0; i < SCM_VECTOR_LENGTH(vector); i++)
    SCM_SLOT_SETQ(ScmVector, vector, array[i], fill);
}

int
scm_vector_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  size_t idx;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port);

  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

  rslt = scm_capi_write_cstr("#(", SCM_ENC_UTF8, port);
  if (rslt < 0) return -1;

  if (SCM_VECTOR_LENGTH(obj) > 0) {
    for (idx = 0; idx < SCM_VECTOR_LENGTH(obj) - 1; idx++) {
      rslt = scm_obj_call_print_func(SCM_VECTOR_ARRAY(obj)[idx], port, ext_rep);
      if (rslt < 0) return -1;

      rslt = scm_capi_write_cstr(" ", SCM_ENC_UTF8, port);
      if (rslt < 0) return -1;
    }

    rslt = scm_obj_call_print_func(SCM_VECTOR_ARRAY(obj)[idx], port, ext_rep);
    if (rslt < 0) return -1;
  }

  rslt = scm_capi_write_cstr(")", SCM_ENC_UTF8, port);
  if (rslt < 0) return -1;

  return 0;
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
