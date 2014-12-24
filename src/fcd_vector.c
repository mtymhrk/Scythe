#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/vector.h"
#include "scythe/string.h"
#include "scythe/char.h"


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

extern inline bool
scm_fcd_vector_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_VECTOR_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_fcd_vector_P(ScmObj obj)
{
  return scm_fcd_vector_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_make_vector(size_t len, ScmObj fill)
{
  scm_assert(len <= SSIZE_MAX);

  if (scm_obj_null_p(fill))
    return scm_vector_new(SCM_MEM_HEAP, len, SCM_UNDEF_OBJ);
  else
    return scm_vector_new(SCM_MEM_HEAP, len, fill);
}

ScmObj
scm_fcd_vector_lst(ScmObj lst)
{
  return scm_fcd_list_to_vector(lst);
}

ScmObj
scm_fcd_vector_cv(const ScmObj *elm, size_t n)
{
  scm_assert(n <= SIZE_MAX);
  scm_assert(n == 0 || elm != NULL);
  return scm_vector_new_from_ary(SCM_MEM_HEAP, elm, n);
}

ScmObj
scm_fcd_vector(size_t n, ...)
{
  ScmObj vec = SCM_OBJ_INIT, args[n];
  va_list ap;

  SCM_REFSTK_INIT_REG(&vec);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_fcd_vector_cv(args, n);
}

size_t
scm_fcd_vector_length(ScmObj vec)
{
  scm_assert(scm_fcd_vector_p(vec));
  return scm_vector_length(vec);
}

ScmObj
scm_fcd_vector_ref(ScmObj vec, size_t idx)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(idx < scm_vector_length(vec));
  return scm_vector_ref(vec, idx);
}

void
scm_fcd_vector_set_i(ScmObj vec, size_t idx, ScmObj obj)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(idx < scm_vector_length(vec));
  scm_assert(scm_obj_not_null_p(obj));
  scm_vector_set(vec, idx, obj);
}

static inline void
vector_norm_star_end(ScmObj vec, ssize_t *start, ssize_t *end)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(start != NULL);
  scm_assert(end != NULL);

  if (*start < 0) *start = 0;
  if (*end < 0) *end = (ssize_t)scm_vector_length(vec);
}

ScmObj
scm_fcd_vector_to_list(ScmObj vec, ssize_t start, ssize_t end)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  return scm_fcd_list_cv(scm_vector_content(vec) + start,
                         (size_t)(end - start));
}

ScmObj
scm_fcd_list_to_vector(ScmObj lst)
{
  return scm_vector_new_from_list(SCM_MEM_HEAP, lst);
}

ScmObj
scm_fcd_vector_to_string(ScmObj vec, ssize_t start, ssize_t end)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  return scm_fcd_string_cv(scm_vector_content(vec) + start,
                           (size_t)(end - start));
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

  p = scm_string_to_char_ary(str, start, (ssize_t)n, ary);
  if (p == NULL) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 0; i < n; i++) {
    elm[i] = scm_char_new(SCM_MEM_HEAP, ary + i, enc);
    if (scm_obj_null_p(elm[i])) return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_cv(elm, n);
}

ScmObj
scm_fcd_string_to_vector(ScmObj str, ssize_t start, ssize_t end)
{
  scm_assert(scm_fcd_string_p(str));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (start < 0) start = 0;
  if (end < 0) end = (ssize_t)scm_string_length(str);

  return string_to_vector_aux(str, (size_t)start, (size_t)(end - start));
}

ScmObj
scm_fcd_vector_copy(ScmObj vec, ssize_t start, ssize_t end)
{
  ScmObj copy = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t n;
  int r;

  SCM_REFSTK_INIT_REG(&vec,
                      &copy);

  scm_assert(scm_fcd_vector_p(vec));
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
scm_fcd_vector_copy_i(ScmObj to, size_t at,
                      ScmObj from, ssize_t start, ssize_t end)
{
  size_t len, dst_len, from_len;

  scm_assert(scm_fcd_vector_p(to));
  scm_assert(at < scm_vector_length(to));
  scm_assert(scm_fcd_vector_p(from));
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
      scm_fcd_error("failed to copy vector: out of range", 0);
      return -1;
    }
  }
  else {
    if (dst_len < from_len - (size_t)start)
      len = dst_len;
    else
      len = from_len - (size_t)start;
  }

  if (scm_fcd_eq_p(to, from) && (size_t)start < at)
    vector_copy_i_aux_in_reverse(to, at, from, (size_t)start, len);
  else
    vector_copy_i_aux(to, at, from, (size_t)start, len);

  return 0;
}

ScmObj
scm_fcd_vector_append_lst(ScmObj lst)
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
  for (ls = lst; scm_fcd_pair_p(ls); ls = scm_fcd_cdr(ls)) {
    vec = scm_fcd_car(ls);
    if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

    if (!scm_fcd_vector_p(vec)) {
      scm_fcd_error("failed to append vectors: vector required, but got",
                    1, vec);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(vec);
    if (SSIZE_MAX - sum < len) {
      scm_fcd_error("failed to append vectors: too long", 0);
      return SCM_OBJ_NULL;
    }

    sum += len;
  }

  acc = scm_vector_new(SCM_MEM_HEAP, sum, SCM_OBJ_NULL);
  if (scm_obj_null_p(acc)) return SCM_OBJ_NULL;

  idx = 0;
  for (ls = lst; scm_fcd_pair_p(ls); ls = scm_fcd_cdr(ls)) {
    vec = scm_fcd_car(ls);
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
scm_fcd_vector_append_cv(ScmObj *ary, size_t n)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t len, sum, idx;

  SCM_REFSTK_INIT_REG(&vec, &elm);

  if (ary == NULL || n == 0)
    return scm_vector_new(SCM_MEM_HEAP, 0, SCM_OBJ_NULL);

  sum = 0;
  for (size_t i = 0; i < n; i++) {
    if (!scm_fcd_vector_p(ary[i])) {
      scm_fcd_error("failed to append vectors: vector required, but got",
                    1, ary[i]);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(ary[i]);
    if (SSIZE_MAX - sum < len) {
      scm_fcd_error("failed to append vectors: too long", 0);
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
scm_fcd_vector_append(size_t n, ...)
{
  ScmObj ary[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    ary[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(ary, n);

  return scm_fcd_vector_append_cv(ary, n);
}

void
scm_fcd_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(scm_obj_not_null_p(fill));
  scm_assert(start < 0 || (size_t)start < scm_vector_length(vec));
  scm_assert(end < 0 || (size_t)end <= scm_vector_length(vec));
  scm_assert(start < 0 || end < 0 || start <= end);

  vector_norm_star_end(vec, &start, &end);
  for (ssize_t i = start; i < end; i++)
    scm_vector_set(vec, (size_t)i, fill);
}

int
scm_fcd_vector_push(ScmObj vec, ScmObj obj)
{
  scm_assert(scm_fcd_vector_p(vec));
  scm_assert(scm_obj_not_null_p(obj));

  return scm_vector_push(vec, obj);
}


/*******************************************************************/
/*  Bytevectors                                                    */
/*******************************************************************/

extern inline bool
scm_fcd_bytevector_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BYTEVECTOR_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_fcd_bytevector_P(ScmObj obj)
{
  return scm_fcd_bytevector_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_make_bytevector(size_t len, int fill)
{
  scm_assert(len <= SSIZE_MAX);
  scm_assert(fill < 256);
  return scm_bytevector_new(SCM_MEM_HEAP, len, fill);
}

ScmObj
scm_fcd_make_bytevector_from_cv(const void *bytes, size_t length)
{
  scm_assert(bytes != NULL || length == 0);
  scm_assert(length <= SSIZE_MAX);
  return scm_bytevector_new_cbyte(SCM_MEM_HEAP, bytes, length);
}

size_t
scm_fcd_bytevector_length(ScmObj vec)
{
  scm_assert(scm_fcd_bytevector_p(vec));
  return scm_bytevector_length(vec);
}

void
scm_fcd_bytevector_u8_set_i(ScmObj vec, size_t idx, int val)
{
  scm_assert(scm_fcd_bytevector_p(vec));
  scm_assert(idx < scm_bytevector_length(vec));
  scm_assert(0 <= val && val < 256);
  scm_bytevector_u8_set(vec, idx, val);
}

void *
scm_fcd_bytevector_to_cv(ScmObj vec, void *buf, size_t size)
{
  size_t n;

  scm_assert(scm_fcd_bytevector_p(vec));

  n = scm_bytevector_length(vec);
  if (buf == NULL) {
    buf = scm_fcd_malloc(n);
    if (buf == NULL) return NULL;
  }
  else if (size < n) {
    n = size;
  }

  memcpy(buf, scm_bytevector_content(vec), n);

  return buf;
}
