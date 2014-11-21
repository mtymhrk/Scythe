#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/reference.h"
#include "scythe/api.h"
#include "scythe/vector.h"


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

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

static ssize_t
length_of_vector_maked_with_list(ScmObj lst)
{
  ScmObj l = SCM_OBJ_INIT;
  ssize_t cnt;

  SCM_REFSTK_INIT_REG(&lst,
                      &l);

  if (scm_obj_null_p(lst))
    return 0;

  cnt = 0;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l))
    cnt++;

  if (scm_obj_null_p(l)) return -1;

  return cnt;
}

int
scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill)
{
  size_t i;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);

  if (length > 0) {
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1;
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
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1;
  }
  else {
    SCM_VECTOR_ARRAY(vector) = NULL;
  }

  SCM_VECTOR_LENGTH(vector) = length;

  for (i = 0; i < length; i++) {
    if (scm_obj_null_p(elms[i])) {
      scm_capi_error("failed to make vector: invalid element", 0);
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

  SCM_REFSTK_INIT_REG(&vector, &lst,
                      &l, &e);

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);
  scm_assert(scm_obj_not_null_p(lst));

  if (length > 0) {
    SCM_VECTOR_ARRAY(vector) = scm_capi_malloc(sizeof(ScmObj) * length);
    if (SCM_VECTOR_ARRAY(vector) == NULL) return -1;
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
scm_vector_finalize(ScmObj vector)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  if (SCM_VECTOR_ARRAY(vector) != NULL)
    scm_capi_free(SCM_VECTOR_ARRAY(vector));
}

ScmObj
scm_vector_new(SCM_MEM_TYPE_T mtype,
               size_t length, ScmObj fill)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fill, &vector);

  /* Vector の実装として、length を SSIZE_MAX 以下に制限する必要はないが、
     api.c との兼ね合いで制限する */
  scm_assert(length <= SSIZE_MAX);

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize(vector, length, fill) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

ScmObj
scm_vector_new_from_ary(SCM_MEM_TYPE_T mtype, const ScmObj *elms, size_t length)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vector);

  scm_assert(length == 0 || (length > 0 && elms != NULL));
  scm_assert(length <= SSIZE_MAX);

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize_ary(vector, elms, length) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

ScmObj
scm_vector_new_from_list(SCM_MEM_TYPE_T mtype, ScmObj lst)
{
  ScmObj vector = SCM_OBJ_INIT;
  ssize_t len;

  SCM_REFSTK_INIT_REG(&lst, &vector);

  len = length_of_vector_maked_with_list(lst);
  if (len < 0) return SCM_OBJ_NULL;

  vector = scm_capi_mem_alloc(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize_lst(vector, (size_t)len, lst) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

size_t
scm_vector_length(ScmObj vector)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  return SCM_VECTOR_LENGTH(vector);
}

ScmObj
scm_vector_ref(ScmObj vector, size_t index)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(index < SCM_VECTOR_LENGTH(vector));

  return SCM_VECTOR_ARRAY(vector)[index];
}

int
scm_vector_set(ScmObj vector, size_t index, ScmObj obj)
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
scm_vector_push(ScmObj vector, ScmObj obj)
{
  ScmObj *p;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  if (SCM_VECTOR_LENGTH(vector) >= SSIZE_MAX) {
    scm_capi_error("failed to expand vector: overflow", 0);
    return -1;
  }

  p = scm_capi_realloc(SCM_VECTOR_ARRAY(vector),
                       sizeof(ScmObj) * (SCM_VECTOR_LENGTH(vector) + 1));
  if (p == NULL) return -1;

  SCM_VECTOR_ARRAY(vector) = p;
  SCM_VECTOR_ARRAY(vector)[SCM_VECTOR_LENGTH(vector)] = obj;
  SCM_VECTOR_LENGTH(vector)++;

  return 0;
}

const ScmObj *
scm_vector_content(ScmObj vector)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  return SCM_VECTOR_ARRAY(vector);
}

int
scm_vector_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  size_t idx;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

  rslt = scm_capi_write_cstr("#(", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  if (SCM_VECTOR_LENGTH(obj) > 0) {
    for (idx = 0; idx < SCM_VECTOR_LENGTH(obj) - 1; idx++) {
      rslt = scm_obj_call_print_func(SCM_VECTOR_ARRAY(obj)[idx], port, ext_rep);
      if (rslt < 0) return -1;

      rslt = scm_capi_write_cstr(" ", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }

    rslt = scm_obj_call_print_func(SCM_VECTOR_ARRAY(obj)[idx], port, ext_rep);
    if (rslt < 0) return -1;
  }

  rslt = scm_capi_write_cstr(")", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_vector_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

  SCM_VECTOR_ARRAY(obj) = NULL;
  SCM_VECTOR_LENGTH(obj) = 0;
}

void
scm_vector_gc_finalize(ScmObj obj)
{
  scm_vector_finalize(obj);
}

int
scm_vector_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
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


/*******************************************************************/
/*  ByteVector                                                     */
/*******************************************************************/

ScmTypeInfo SCM_BYTEVECTOR_TYPE_INFO = {
  .name = "bytevector",
  .flags = SCM_TYPE_FLG_MMO,
  .obj_print_func = scm_bytevector_obj_print,
  .obj_size = sizeof(ScmByteVector),
  .gc_ini_func = scm_bytevector_gc_initialize,
  .gc_fin_func = scm_bytevector_gc_finalize,
  .gc_accept_func = NULL,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

int
scm_bytevector_initialize(ScmObj vec, size_t length, int fill)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);
  scm_assert(fill < 256);

  if (length > 0) {
    SCM_BYTEVECTOR_ARRAY(vec) = scm_capi_malloc(length);
    if (SCM_BYTEVECTOR_ARRAY(vec) == NULL) return -1;
  }
  else {
    SCM_BYTEVECTOR_ARRAY(vec) = NULL;
  }

  SCM_BYTEVECTOR_LENGTH(vec) = length;

  if (fill < 0)
    fill = 0;

  for (size_t i = 0; i < length; i++)
    SCM_BYTEVECTOR_ARRAY(vec)[i] = (uint8_t)fill;

  return 0;
}

int
scm_bytevector_initialize_cbytes(ScmObj vec, const void *bytes, size_t length)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert(length == 0 || bytes != NULL);
  scm_assert(length <= SSIZE_MAX);

  if (length > 0) {
    SCM_BYTEVECTOR_ARRAY(vec) = scm_capi_malloc(length);
    if (SCM_BYTEVECTOR_ARRAY(vec) == NULL) return -1;

    memcpy(SCM_BYTEVECTOR_ARRAY(vec), bytes, length);
  }
  else {
    SCM_BYTEVECTOR_ARRAY(vec) = NULL;
  }

  SCM_BYTEVECTOR_LENGTH(vec) = length;

  return 0;
}

void
scm_bytevector_finalize(ScmObj vec)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);

  if (SCM_BYTEVECTOR_ARRAY(vec) == NULL)
    return;

  scm_capi_free(SCM_BYTEVECTOR_ARRAY(vec));
  SCM_BYTEVECTOR_ARRAY(vec) = NULL;
  SCM_BYTEVECTOR_LENGTH(vec) = 0;
}

ScmObj
scm_bytevector_new(SCM_MEM_TYPE_T mtype, size_t length, int fill)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  scm_assert(length <= SSIZE_MAX);
  scm_assert(fill < 256);

  vec = scm_capi_mem_alloc(&SCM_BYTEVECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  if (scm_bytevector_initialize(vec, length, fill) < 0)
    return SCM_OBJ_NULL;

  return vec;
}

ScmObj
scm_bytevector_new_cbyte(SCM_MEM_TYPE_T mtype, const void *bytes, size_t length)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  scm_assert(length == 0 || bytes != NULL);
  scm_assert(length <= SSIZE_MAX);

  vec = scm_capi_mem_alloc(&SCM_BYTEVECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  if (scm_bytevector_initialize_cbytes(vec, bytes, length) < 0)
    return SCM_OBJ_NULL;

  return vec;
}

int
scm_bytevector_cmp(ScmObj v1, ScmObj v2)
{
  size_t len;
  int cmp;

  scm_assert_obj_type(v1, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert_obj_type(v2, &SCM_BYTEVECTOR_TYPE_INFO);

  if (SCM_BYTEVECTOR_LENGTH(v1) == SCM_BYTEVECTOR_LENGTH(v2))
    return memcmp(SCM_BYTEVECTOR_ARRAY(v1), SCM_BYTEVECTOR_ARRAY(v2),
                  SCM_BYTEVECTOR_LENGTH(v1));

  len = ((SCM_BYTEVECTOR_LENGTH(v1) < SCM_BYTEVECTOR_LENGTH(v2)) ?
         SCM_BYTEVECTOR_LENGTH(v1) : SCM_BYTEVECTOR_LENGTH(v2));
  cmp = memcmp(SCM_BYTEVECTOR_ARRAY(v1), SCM_BYTEVECTOR_ARRAY(v2), len);
  if (cmp == 0)
    return ((SCM_BYTEVECTOR_LENGTH(v1) < SCM_BYTEVECTOR_LENGTH(v2)) ? -1 : 1);
  else
    return cmp;
}

int
scm_bytevector_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  int r;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_BYTEVECTOR_TYPE_INFO);

  r = scm_capi_write_cstr("#u8(", SCM_ENC_SRC, port);
  if (r < 0) return -1;

  if (SCM_BYTEVECTOR_LENGTH(obj) > 0) {
    char str[8];
    size_t idx;
    for (idx = 0; idx < SCM_BYTEVECTOR_LENGTH(obj) - 1; idx++) {
      snprintf(str, sizeof(str), "%d ", SCM_BYTEVECTOR_ARRAY(obj)[idx]);

      r = scm_capi_write_cstr(str, SCM_ENC_SRC, port);
      if (r < 0) return -1;
    }

    snprintf(str, sizeof(str), "%d", SCM_BYTEVECTOR_ARRAY(obj)[idx]);
  }

  r = scm_capi_write_cstr(")", SCM_ENC_SRC, port);
  if (r < 0) return -1;

  return 0;
}

void
scm_bytevector_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_BYTEVECTOR_TYPE_INFO);

  SCM_BYTEVECTOR_ARRAY(obj) = NULL;
  SCM_BYTEVECTOR_LENGTH(obj) = 0;
}

void
scm_bytevector_gc_finalize(ScmObj obj)
{
  scm_bytevector_finalize(obj);
}
