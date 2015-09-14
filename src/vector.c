#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/earray.h"
#include "scythe/vm.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/equivalence.h"
#include "scythe/exception.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/string.h"
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
  for (l = lst; scm_pair_p(l); l = scm_cdr(l))
    cnt++;

  if (scm_obj_null_p(l)) return -1;

  return cnt;
}

static  void
vector_norm_star_end(ScmObj vec, ssize_t *start, ssize_t *end)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(start != NULL);
  scm_assert(end != NULL);

  if (*start < 0) *start = 0;
  if (*end < 0) *end = (ssize_t)scm_vector_length(vec);
}

ScmObj
scm_vector_P(ScmObj obj)
{
  return scm_vector_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill)
{
  size_t i;
  int r;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);

  r = eary_init(SCM_VECTOR_EARRAY(vector), sizeof(ScmObj), length);
  if (r < 0) return -1;

  if (scm_obj_null_p(fill))
    fill = SCM_UNDEF_OBJ;

  for (i = 0; i < length; i++)
    EARY_SET_SCMOBJ(SCM_VECTOR_EARRAY(vector), i, fill, vector, r);

  return 0;
}

int
scm_vector_initialize_ary(ScmObj vector, const ScmObj *elms, size_t length)
{
  size_t i;
  int r;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length == 0 || (length > 0 && elms != NULL));
  scm_assert(length <= SSIZE_MAX);

  r = eary_init(SCM_VECTOR_EARRAY(vector), sizeof(ScmObj), length);
  if (r < 0) return -1;

  for (i = 0; i < length; i++) {
    if (scm_obj_null_p(elms[i])) {
      scm_error("failed to make vector: invalid element", 0);
      return -1;
    }
    EARY_SET_SCMOBJ(SCM_VECTOR_EARRAY(vector), i, elms[i], vector, r);
  }

  return 0;
}

int
scm_vector_initialize_lst(ScmObj vector, size_t length, ScmObj lst)
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vector, &lst,
                      &l, &e);

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);
  scm_assert(scm_obj_not_null_p(lst));

  r = eary_init(SCM_VECTOR_EARRAY(vector), sizeof(ScmObj), length);
  if (r < 0) return -1;

  for (l = lst, i = 0; scm_pair_p(l) && i < length; l = scm_cdr(l), i++) {
    e = scm_car(l);
    EARY_SET_SCMOBJ(SCM_VECTOR_EARRAY(vector), i, e, vector, r);
  }

  while (i < length)
    EARY_SET_SCMOBJ(SCM_VECTOR_EARRAY(vector), i, SCM_UNDEF_OBJ, vector, r);

  return 0;
}

void
scm_vector_finalize(ScmObj vector)
{
  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);

  eary_fin(SCM_VECTOR_EARRAY(vector));
}

ScmObj
scm_vector_new(scm_mem_type_t mtype, size_t length, ScmObj fill)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fill, &vector);

  /* Vector の実装として、length を SSIZE_MAX 以下に制限する必要はないが、
     api との兼ね合いで制限する */
  scm_assert(length <= SSIZE_MAX);

  vector = scm_alloc_mem(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize(vector, length, fill) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

ScmObj
scm_vector_new_cv(scm_mem_type_t mtype, const ScmObj *elms, size_t length)
{
  ScmObj vector = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vector);

  scm_assert(length == 0 || elms != NULL);
  scm_assert(length <= SSIZE_MAX);

  vector = scm_alloc_mem(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize_ary(vector, elms, length) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

ScmObj
scm_vector_new_lst(scm_mem_type_t mtype, ScmObj lst)
{
  ScmObj vector = SCM_OBJ_INIT;
  ssize_t len;

  SCM_REFSTK_INIT_REG(&lst, &vector);

  len = length_of_vector_maked_with_list(lst);
  if (len < 0) return SCM_OBJ_NULL;

  vector = scm_alloc_mem(&SCM_VECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vector)) return SCM_OBJ_NULL;

  if (scm_vector_initialize_lst(vector, (size_t)len, lst) < 0)
    return SCM_OBJ_NULL;

  return vector;
}

ScmObj
scm_vector(size_t n, ...)
{
  ScmObj vec = SCM_OBJ_INIT, args[n];
  va_list ap;

  SCM_REFSTK_INIT_REG(&vec);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_vector_cv(args, n);
}

int
scm_vector_push(ScmObj vector, ScmObj obj)
{
  int err;

  scm_assert_obj_type(vector, &SCM_VECTOR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  if (SCM_VECTOR_LENGTH(vector) >= SSIZE_MAX) {
    scm_error("failed to expand vector: overflow", 0);
    return -1;
  }

  EARY_PUSH_SCMOBJ(SCM_VECTOR_EARRAY(vector), obj, vector, err);
  if (err < 0) return -1;

  return 0;
}

ScmObj
scm_vector_to_list(ScmObj vec, ssize_t start, ssize_t end)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  return scm_list_cv(scm_vector_content(vec) + start, (size_t)(end - start));
}

ScmObj
scm_vector_to_string(ScmObj vec, ssize_t start, ssize_t end)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  return scm_string_cv(scm_vector_content(vec) + start, (size_t)(end - start));
}

static ScmObj
string_to_vector_aux(ScmObj str, size_t start, size_t n)
{
  ScmObj elm[n];
  scm_char_t ary[n], *p;
  ScmEncoding *enc;

  for (size_t i = 0; i < n; i++) elm[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&str);
  SCM_REFSTK_REG_ARY(elm, n);

  p = scm_string_to_cchr_ary(str, start, (ssize_t)n, ary);
  if (p == NULL) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 0; i < n; i++) {
    elm[i] = scm_char_new(SCM_MEM_HEAP, ary + i, enc);
    if (scm_obj_null_p(elm[i])) return SCM_OBJ_NULL;
  }

  return scm_vector_cv(elm, n);
}

ScmObj
scm_string_to_vector(ScmObj str, ssize_t start, ssize_t end)
{
  scm_assert(scm_string_p(str));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (start < 0) start = 0;
  if (end < 0) end = (ssize_t)scm_string_length(str);

  return string_to_vector_aux(str, (size_t)start, (size_t)(end - start));
}

ScmObj
scm_vector_copy(ScmObj vec, ssize_t start, ssize_t end)
{
  ScmObj copy = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t n;
  int r;

  SCM_REFSTK_INIT_REG(&vec,
                      &copy);

  scm_assert(scm_vector_p(vec));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  n = (size_t)(end - start);

  copy = scm_vector_new(SCM_MEM_HEAP, n, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(copy)) return SCM_OBJ_NULL;

  for (size_t i = 0; i < n; i++) {
    elm = scm_vector_ref(vec, (size_t)start + i);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    r = scm_vector_set(copy, i, elm);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return copy;
}

static void
vector_copy_i_aux(ScmObj to, size_t at, ScmObj from, size_t pos, size_t len)
{
  ScmObj elm = SCM_OBJ_INIT;

  for (size_t i = 0; i < len; i++) {
    elm = scm_vector_ref(from, pos + i);
    scm_vector_set(to, at + i, elm);
  }
}

static void
vector_copy_i_aux_in_reverse(ScmObj to, size_t at,
                             ScmObj from, size_t pos, size_t len)
{
  ScmObj elm = SCM_OBJ_INIT;

  for (size_t i = len; i > 0; i--) {
    elm = scm_vector_ref(from, pos + i - 1);
    scm_vector_set(to, at + i - 1, elm);
  }
}

int
scm_vector_copy_i(ScmObj to, size_t at, ScmObj from, ssize_t start, ssize_t end)
{
  size_t len, dst_len, from_len;

  scm_assert(scm_vector_p(to));
  scm_assert(at < scm_vector_length(to));
  scm_assert(scm_vector_p(from));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(from));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(from));
  scm_assert(start < 0 || end < 0 || start <= end);

  dst_len = scm_vector_length(to) - at;
  from_len = scm_vector_length(from);

  if (start < 0)
    start = 0;

  if (end > 0) {
    len = (size_t)(end - start);
    if (len > dst_len) {
      scm_error("failed to copy vector: out of range", 0);
      return -1;
    }
  }
  else {
    if (dst_len < from_len - (size_t)start)
      len = dst_len;
    else
      len = from_len - (size_t)start;
  }

  if (scm_eq_p(to, from) && (size_t)start < at)
    vector_copy_i_aux_in_reverse(to, at, from, (size_t)start, len);
  else
    vector_copy_i_aux(to, at, from, (size_t)start, len);

  return 0;
}

ScmObj
scm_vector_append_lst(ScmObj lst)
{
  ScmObj acc = SCM_OBJ_INIT, vec = SCM_OBJ_INIT;
  ScmObj elm = SCM_OBJ_INIT, ls = SCM_OBJ_INIT;
  size_t len, sum, idx;

  SCM_REFSTK_INIT_REG(&lst,
                      &acc, &vec,
                      &elm, &ls);

  if (scm_obj_null_p(lst))
    return scm_vector_new(SCM_MEM_HEAP, 0, SCM_OBJ_NULL);

  sum = 0;
  for (ls = lst; scm_pair_p(ls); ls = scm_cdr(ls)) {
    vec = scm_car(ls);
    if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

    if (!scm_vector_p(vec)) {
      scm_error("failed to append vectors: vector required, but got",
                    1, vec);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(vec);
    if (SSIZE_MAX - sum < len) {
      scm_error("failed to append vectors: too long", 0);
      return SCM_OBJ_NULL;
    }

    sum += len;
  }

  acc = scm_vector_new(SCM_MEM_HEAP, sum, SCM_OBJ_NULL);
  if (scm_obj_null_p(acc)) return SCM_OBJ_NULL;

  idx = 0;
  for (ls = lst; scm_pair_p(ls); ls = scm_cdr(ls)) {
    vec = scm_car(ls);
    if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

    len = scm_vector_length(vec);
    for (size_t i = 0; i < len; i++) {
      elm = scm_vector_ref(vec, i);
      scm_vector_set(acc, idx++, elm);
    }
  }

  return acc;
}

ScmObj
scm_vector_append_cv(ScmObj *ary, size_t n)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t len, sum, idx;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  if (ary == NULL || n == 0)
    return scm_vector_new(SCM_MEM_HEAP, 0, SCM_OBJ_NULL);

  sum = 0;
  for (size_t i = 0; i < n; i++) {
    if (!scm_vector_p(ary[i])) {
      scm_error("failed to append vectors: vector required, but got",
                    1, ary[i]);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(ary[i]);
    if (SSIZE_MAX - sum < len) {
      scm_error("failed to append vectors: too long", 0);
      return SCM_OBJ_NULL;
    }

    sum += len;
  }

  vec = scm_vector_new(SCM_MEM_HEAP, sum, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  idx = 0;
  for (size_t i = 0; i < n; i++) {
    len = scm_vector_length(ary[i]);
    for (size_t j = 0; j < len; j++) {
      elm = scm_vector_ref(ary[i], j);
      scm_vector_set(vec, idx++, elm);
    }
  }

  return vec;
}

ScmObj
scm_vector_append(size_t n, ...)
{
  ScmObj ary[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    ary[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(ary, n);

  return scm_vector_append_cv(ary, n);
}

void
scm_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(scm_obj_not_null_p(fill));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  for (ssize_t i = start; i < end; i++)
    scm_vector_set(vec, (size_t)i, fill);
}

int
scm_vector_obj_print(ScmObj obj, ScmObj port, int kind,
                     ScmObjPrintHandler handler)
{
  size_t idx;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

  rslt = scm_write_cstr("#(", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  if (SCM_VECTOR_LENGTH(obj) > 0) {
    for (idx = 0; idx < SCM_VECTOR_LENGTH(obj) - 1; idx++) {
      rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler,
                                        SCM_VECTOR_ARRAY(obj)[idx], port, kind);
      if (rslt < 0) return -1;

      rslt = scm_write_cstr(" ", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }

    rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler,
                                      SCM_VECTOR_ARRAY(obj)[idx], port, kind);
    if (rslt < 0) return -1;
  }

  rslt = scm_write_cstr(")", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_vector_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);

  eary_init(SCM_VECTOR_EARRAY(obj), 0, 0);
}

void
scm_vector_gc_finalize(ScmObj obj)
{
  scm_vector_finalize(obj);
}

int
scm_vector_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  size_t i;

  scm_assert_obj_type(obj, &SCM_VECTOR_TYPE_INFO);
  scm_assert(handler != NULL);

  for (i = 0; i < SCM_VECTOR_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VECTOR_ARRAY(obj)[i]);
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

ScmObj
scm_bytevector_P(ScmObj obj)
{
  return scm_bytevector_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_bytevector_initialize(ScmObj vec, size_t length, int fill)
{
  int r;

  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert(length <= SSIZE_MAX);
  scm_assert(fill < 256);

  r = eary_init(SCM_BYTEVECTOR_EARRAY(vec), sizeof(uint8_t), length);
  if (r < 0) return -1;

  if (length == 0)
    return 0;

  if (fill < 0)
    fill = 0;

  EARY_SET(SCM_BYTEVECTOR_EARRAY(vec), uint8_t, length - 1, fill, r);
  for (size_t i = 0; i < length - 1; i++)
    SCM_BYTEVECTOR_ARRAY(vec)[i] = (uint8_t)fill;

  return 0;
}

int
scm_bytevector_initialize_cbytes(ScmObj vec, const void *bytes, size_t length)
{
  int r;

  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert(length == 0 || bytes != NULL);
  scm_assert(length <= SSIZE_MAX);

  r = eary_init(SCM_BYTEVECTOR_EARRAY(vec), sizeof(uint8_t), length);
  if (r < 0) return -1;

  if (length == 0)
    return 0;

  EARY_SET(SCM_BYTEVECTOR_EARRAY(vec), uint8_t, length - 1, 0, r);
  memcpy(SCM_BYTEVECTOR_ARRAY(vec), bytes, length);
  return 0;
}

void
scm_bytevector_finalize(ScmObj vec)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);

  eary_fin(SCM_BYTEVECTOR_EARRAY(vec));
}

ScmObj
scm_bytevector_new(scm_mem_type_t mtype, size_t length, int fill)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  scm_assert(length <= SSIZE_MAX);
  scm_assert(fill < 256);

  vec = scm_alloc_mem(&SCM_BYTEVECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  if (scm_bytevector_initialize(vec, length, fill) < 0)
    return SCM_OBJ_NULL;

  return vec;
}

ScmObj
scm_bytevector_new_cbyte(scm_mem_type_t mtype, const void *bytes, size_t length)
{
  ScmObj vec = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vec);

  scm_assert(length == 0 || bytes != NULL);
  scm_assert(length <= SSIZE_MAX);

  vec = scm_alloc_mem(&SCM_BYTEVECTOR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  if (scm_bytevector_initialize_cbytes(vec, bytes, length) < 0)
    return SCM_OBJ_NULL;

  return vec;
}

int
scm_bytevector_push(ScmObj vec, int val)
{
  int err;

  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);
  scm_assert(0 <= val && val <= 255);

  if (SCM_BYTEVECTOR_LENGTH(vec) >= SSIZE_MAX) {
    scm_error("failed to expand bytevector: overflow", 0);
    return -1;
  }

  EARY_PUSH(SCM_BYTEVECTOR_EARRAY(vec), uint8_t, val, err);
  if (err < 0) return -1;

  return 0;
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

void *
scm_bytevector_to_cv(ScmObj vec, void *buf, size_t size)
{
  size_t n;

  scm_assert(scm_bytevector_p(vec));

  n = scm_bytevector_length(vec);
  if (buf == NULL) {
    buf = scm_malloc(n);
    if (buf == NULL) return NULL;
  }
  else if (size < n) {
    n = size;
  }

  memcpy(buf, scm_bytevector_content(vec), n);

  return buf;
}

int
scm_bytevector_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler)
{
  int r;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_BYTEVECTOR_TYPE_INFO);

  r = scm_write_cstr("#u8(", SCM_ENC_SRC, port);
  if (r < 0) return -1;

  if (SCM_BYTEVECTOR_LENGTH(obj) > 0) {
    char str[8];
    size_t idx;
    for (idx = 0; idx < SCM_BYTEVECTOR_LENGTH(obj) - 1; idx++) {
      snprintf(str, sizeof(str), "%d ", SCM_BYTEVECTOR_ARRAY(obj)[idx]);

      r = scm_write_cstr(str, SCM_ENC_SRC, port);
      if (r < 0) return -1;
    }

    snprintf(str, sizeof(str), "%d", SCM_BYTEVECTOR_ARRAY(obj)[idx]);
  }

  r = scm_write_cstr(")", SCM_ENC_SRC, port);
  if (r < 0) return -1;

  return 0;
}

void
scm_bytevector_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_BYTEVECTOR_TYPE_INFO);

  eary_init(SCM_BYTEVECTOR_EARRAY(obj), 0, 0);
}

void
scm_bytevector_gc_finalize(ScmObj obj)
{
  scm_bytevector_finalize(obj);
}
