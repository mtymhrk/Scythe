#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "encoding.h"
#include "string.h"

#define SCM_STRING_BLOCK_SIZE  64
#define CAPACITY(str) (SCM_STRING_CAPACITY(str)                 \
                       - (size_t)((SCM_STRING_HEAD(str)         \
                                   - SCM_STRING_BUFFER(str))))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - SCM_STRING_BYTESIZE(str))

ScmTypeInfo SCM_STRING_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmString),
  .gc_ini_func         = scm_string_gc_initialize,
  .gc_fin_func         = scm_string_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};

static ssize_t
scm_string_check_bytes(void *str, size_t size,
                       const ScmEncVirtualFunc *vf) /* GC OK */
{
  ScmStrItr iter;
  ssize_t len;

  if (str == NULL || vf == NULL || size > SSIZE_MAX) return -1;

  iter = scm_str_itr_begin((void *)str, size, vf->char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  len = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    len++;
    scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return -1;
  }

  return len;
}

static ssize_t
scm_string_copy_bytes_with_check(void *dst, const void *src, size_t size,
                                 const ScmEncVirtualFunc *vf) /* GC OK */
{
  ssize_t len;

  if (dst == NULL || src == NULL || vf == NULL || size > SSIZE_MAX)
    return -1;

  memcpy(dst, src, size);
  len = scm_string_check_bytes(dst, size, vf);
  if (len < 0) return -1;

  return len;
}

static ScmObj
scm_string_copy_and_expand(ScmObj src, size_t size) /* GC OK */
{
  ScmObj str = SCM_OBJ_INIT;;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);
  scm_assert(size <= SSIZE_MAX);

  str = scm_string_new(SCM_MEM_HEAP,
                       NULL, size, SCM_STRING_ENC(src));
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  SCM_STRING_BYTESIZE(str) =
    (size < SCM_STRING_BYTESIZE(src)) ? size : SCM_STRING_BYTESIZE(src);
  len =
    scm_string_copy_bytes_with_check(SCM_STRING_BUFFER(str),
                                     SCM_STRING_HEAD(src),
                                     /* XXX: SCM_STRING_BYTESIZE(str) ? */
                                     SCM_STRING_BYTESIZE(src),
                                     SCM_ENCODING_VFUNC(SCM_STRING_ENC(src)));
  if (len < 0) {
    /* scm_string_end(str); */
    return SCM_OBJ_NULL;
  }

  SCM_STRING_LENGTH(str) = (size_t)len;

  return str;
}

static void
scm_string_replace_contents(ScmObj target, ScmObj src) /* GC OK */
{
  scm_assert_obj_type(target, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  if (scm_obj_null_p(target) || scm_obj_null_p(src)) return;

  if (*SCM_STRING_REF_CNT(target) > 1) {
    SCM_STRING_DEC_REF_CNT(target);
  }
  else {
    scm_capi_free(SCM_STRING_BUFFER(target));
    scm_capi_free(SCM_STRING_REF_CNT(target));
  }

  *SCM_STRING(target) = *SCM_STRING(src);
  SCM_STRING_INC_REF_CNT(target);
}

static void
scm_string_finalize(ScmObj str) /* GC OK */
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  if (SCM_STRING_REF_CNT(str) != NULL && *SCM_STRING_REF_CNT(str) > 1)
    SCM_STRING_DEC_REF_CNT(str);
  else {
    scm_capi_free(SCM_STRING_BUFFER(str));
    scm_capi_free(SCM_STRING_REF_CNT(str));
  }

  /* push() 関数や append() 関数内の tmp はこれらの関数から直接 finalize() を call  */
  /* され、さらに GC 時にも gc_finalize() から call されるため、2 度 call されても問  */
  /* 題ないようにする必要がある                                                  */
  SCM_STRING_BUFFER(str) = NULL;
  SCM_STRING_REF_CNT(str) = NULL;
}

void
scm_string_initialize(ScmObj str,
                      const void *src, size_t size, SCM_ENC_T enc) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&str);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(size <= SSIZE_MAX);

  SCM_STRING_BUFFER(str) = NULL;
  SCM_STRING_REF_CNT(str) = NULL;
  SCM_STRING_ENC(str) = enc;

  for (SCM_STRING_CAPACITY(str) = SCM_STRING_BLOCK_SIZE;
       SCM_STRING_CAPACITY(str) < size;
       SCM_STRING_CAPACITY(str) *= 2)
    ;

  SCM_STRING_BUFFER(str) = scm_capi_malloc(SCM_STRING_CAPACITY(str));
  SCM_STRING_HEAD(str) = SCM_STRING_BUFFER(str);
  if (SCM_STRING_BUFFER(str) == NULL)
    ;                           /* TODO: error handling */

  SCM_STRING_REF_CNT(str) =
    scm_capi_malloc(sizeof(*SCM_STRING_REF_CNT(str)));
  if (SCM_STRING_REF_CNT(str) == NULL)
    ;                           /* TODO: error handling */

  *SCM_STRING_REF_CNT(str) = 1;

  if (src != NULL) {
    ssize_t len = scm_string_copy_bytes_with_check(SCM_STRING_BUFFER(str),
                                                   src, size,
                                                   SCM_ENCODING_VFUNC(enc));
    if (len < 0)
      ;                  /* TODO: error handling: invalid byte sequence */

    SCM_STRING_LENGTH(str) = (size_t)len;
    SCM_STRING_BYTESIZE(str) = size;
  }
  else {
    SCM_STRING_LENGTH(str) = 0;
    SCM_STRING_BYTESIZE(str) = 0;
  }
}

ScmObj
scm_string_new(SCM_MEM_TYPE_T mtype, const void *src, size_t size, SCM_ENC_T enc) /* GC OK */
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);
  scm_assert(size <= SSIZE_MAX);

  scm_assert(/*0 <= enc && */enc < SCM_ENC_NR_ENC);

  str = scm_capi_mem_alloc(&SCM_STRING_TYPE_INFO, mtype);

  scm_string_initialize(str, src, size, enc);

  return str;
}

ScmObj
scm_string_copy(ScmObj src)     /* GC OK */
{
  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  return scm_string_new(SCM_MEM_HEAP,
                        SCM_STRING_HEAD(src),
                        SCM_STRING_BYTESIZE(src),
                        SCM_STRING_ENC(src));
}

ScmObj
scm_string_dup(ScmObj src)      /* GC OK */
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&src, &str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  str = scm_capi_mem_alloc_heap(&SCM_STRING_TYPE_INFO);

  SCM_STRING_BUFFER(str) = SCM_STRING_BUFFER(src);
  SCM_STRING_HEAD(str) = SCM_STRING_HEAD(src);
  SCM_STRING_CAPACITY(str) = SCM_STRING_CAPACITY(src);
  SCM_STRING_BYTESIZE(str) = SCM_STRING_BYTESIZE(src);
  SCM_STRING_LENGTH(str) = SCM_STRING_LENGTH(src);
  SCM_STRING_REF_CNT(str) = SCM_STRING_REF_CNT(src);
  SCM_STRING_ENC(str) = SCM_STRING_ENC(src);

  SCM_STRING_INC_REF_CNT(str);

  return str;
}

size_t
scm_string_length(ScmObj str)   /* GC OK */
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return SCM_STRING_LENGTH(str);
}

size_t
scm_string_bytesize(ScmObj str) /* GC OK */
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return SCM_STRING_BYTESIZE(str);
}

bool
scm_string_is_equal(ScmObj str1, ScmObj str2) /* GC OK */
{
  scm_assert_obj_type(str1, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(str2, &SCM_STRING_TYPE_INFO);

  if (SCM_STRING_LENGTH(str1) != SCM_STRING_LENGTH(str2)) return false;
  if (SCM_STRING_BYTESIZE(str1) != SCM_STRING_BYTESIZE(str2)) return false;
  if (SCM_STRING_HEAD(str1) == SCM_STRING_HEAD(str2)) return true;

  return (memcmp(SCM_STRING_HEAD(str1),
                 SCM_STRING_HEAD(str2),
                 SCM_STRING_BYTESIZE(str1))
          == 0);
}

ScmObj
scm_string_substr(ScmObj str, size_t pos, size_t len) /* GC OK */
{
  ScmObj substr = SCM_OBJ_INIT;
  ScmStrItr head, tail;
  ScmStrItr (*index2iter)(void *p, size_t size, size_t idx);

  SCM_STACK_FRAME_PUSH(&str, &substr);
  scm_assert(pos <= SSIZE_MAX);
  scm_assert(len <= SSIZE_MAX);

  if (pos + len > SCM_STRING_LENGTH(str)) return SCM_OBJ_NULL;

  index2iter = SCM_ENCODING_VFUNC_INDEX2ITER(SCM_STRING_ENC(str));
  head = index2iter(SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos);
  tail = index2iter(SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos + len);

  if (SCM_STR_ITR_IS_ERR(&head) || SCM_STR_ITR_IS_ERR(&tail))
    return SCM_OBJ_NULL;

  substr = scm_string_dup(str);
  SCM_STRING_HEAD(substr) = (uint8_t *)SCM_STR_ITR_PTR(&head);
  SCM_STRING_LENGTH(substr) = len;
  SCM_STRING_BYTESIZE(substr)
    = (size_t)((uint8_t *)SCM_STR_ITR_PTR(&tail)
               - (uint8_t *)SCM_STR_ITR_PTR(&head));

  return substr;
}

ScmObj
scm_string_push(ScmObj str, const scm_char_t c) /* GC OK */
{
  ScmObj tmp = SCM_OBJ_INIT;
  int (*char_width)(const void *p, size_t size);
  ssize_t width;

  SCM_STACK_FRAME_PUSH(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(SCM_STRING_ENC(str));
  width = char_width(&c, sizeof(c));
  if (width < 0) return SCM_OBJ_NULL;

  if ((*SCM_STRING_REF_CNT(str) > 1) || ROOM_FOR_APPEND(str) < (size_t)width) {
    tmp = scm_string_copy_and_expand(str,
                                     SCM_STRING_BYTESIZE(str) + (size_t)width);
    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str), &c, (size_t)width);
  SCM_STRING_LENGTH(str) += 1;
  SCM_STRING_BYTESIZE(str) += (size_t)width;

  return str;
}

ScmObj
scm_string_append(ScmObj str, ScmObj append) /* GC OK */
{
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(append, &SCM_STRING_TYPE_INFO);

  if ((*SCM_STRING_REF_CNT(str) > 1) ||
      ROOM_FOR_APPEND(str) < SCM_STRING_BYTESIZE(append)) {
    tmp = scm_string_copy_and_expand(str,
                                     SCM_STRING_BYTESIZE(str)
                                     + SCM_STRING_BYTESIZE(append));
    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str),
         SCM_STRING_HEAD(append), SCM_STRING_BYTESIZE(append));
  SCM_STRING_LENGTH(str) += SCM_STRING_LENGTH(append);
  SCM_STRING_BYTESIZE(str) += SCM_STRING_BYTESIZE(append);

  return str;
}

scm_char_t
scm_string_ref(ScmObj str, size_t pos) /* GC OK */
{
  ScmStrItr iter;
  scm_char_t c;
  ScmStrItr (*index2iter)(void *p, size_t size, size_t idx);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);

  c = SCM_CHR_ZERO;
  if (pos >= SCM_STRING_LENGTH(str)) return c;

  index2iter = SCM_ENCODING_VFUNC_INDEX2ITER(SCM_STRING_ENC(str));
  iter = index2iter(SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos);
  if (SCM_STR_ITR_IS_ERR(&iter)) return c;
  if (SCM_STR_ITR_IS_END(&iter)) return c;

  memcpy(&c, SCM_STR_ITR_PTR(&iter), (size_t)SCM_STR_ITR_WIDTH(&iter));

  return c;
}

ScmObj
scm_string_set(ScmObj str, size_t pos, const scm_char_t c) /* GC OK */
{
  ScmObj front = SCM_OBJ_NULL, rear = SCM_OBJ_NULL, tmp = SCM_OBJ_NULL;
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, size_t idx);
  ScmStrItr iter;
  int cw, iw;

  SCM_STACK_FRAME_PUSH(&str, &front, &rear, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);

  if (pos >= SCM_STRING_LENGTH(str)) return SCM_OBJ_NULL;

  index2iter = SCM_ENCODING_VFUNC_INDEX2ITER(SCM_STRING_ENC(str));
  char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(SCM_STRING_ENC(str));

  iter = index2iter(SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos);
  if (SCM_STR_ITR_IS_ERR(&iter)) return SCM_OBJ_NULL;

  cw = char_width(&c, sizeof(c));
  if (cw < 0) return SCM_OBJ_NULL;

  iw = SCM_STR_ITR_WIDTH(&iter);
  if (iw < 0) return SCM_OBJ_NULL;

  if (*SCM_STRING_REF_CNT(str) == 1
      && (iw > cw || ROOM_FOR_APPEND(str) >= (size_t)(cw - iw))) {
    size_t rest = (size_t)SCM_STR_ITR_REST(&iter);
    size_t offset = (size_t)SCM_STR_ITR_OFFSET(&iter, SCM_STRING_HEAD(str));

    if (cw != iw) {
      memmove(SCM_STRING_HEAD(str) + offset + cw,
              SCM_STRING_HEAD(str) + offset + iw,
              rest - (size_t)iw);
      if (iw > cw)
        SCM_STRING_BYTESIZE(str) -= (size_t)(iw - cw);
      else
        SCM_STRING_BYTESIZE(str) += (size_t)(cw - iw);
    }
    memcpy(SCM_STRING_HEAD(str) + offset, &c, (size_t)cw);

    return str;
  }
  else if (cw == iw) {
    size_t offset = SCM_STR_ITR_OFFSET(&iter, SCM_STRING_HEAD(str));
    tmp = scm_string_copy(str);

    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    memcpy(SCM_STRING_HEAD(tmp) + offset, &c, (size_t)cw);
    scm_string_replace_contents(str, tmp);

    return str;
  }
  else {
    front = scm_string_substr(str, 0, pos);
    rear = scm_string_substr(str, pos + 1, SCM_STRING_LENGTH(str) - pos - 1);

    if (scm_obj_null_p(front) || scm_obj_null_p(rear))
      return SCM_OBJ_NULL;

    tmp = scm_string_copy_and_expand(front,
                                     SCM_STRING_BYTESIZE(front)
                                     + (size_t)cw + SCM_STRING_BYTESIZE(rear));
    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    if (scm_obj_null_p(scm_string_push(tmp, c))) return SCM_OBJ_NULL;
    if (scm_obj_null_p(scm_string_append(tmp, rear))) return SCM_OBJ_NULL;

    scm_string_replace_contents(str, tmp);

    return str;
  }
}

/* TODO: optimize */
ScmObj
scm_string_fill(ScmObj str, size_t pos, size_t len, scm_char_t c) /* GC OK */
{
  int (*char_width)(const void *p, size_t size);
  ssize_t filledsize;
  size_t i;
  ScmObj front = SCM_OBJ_INIT, rear = SCM_OBJ_INIT, tmp = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &front, &rear, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);
  scm_assert(len <= SSIZE_MAX);

  if (pos > SCM_STRING_LENGTH(str)) return SCM_OBJ_NULL;

  char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(SCM_STRING_ENC(str));

  filledsize = char_width(&c, sizeof(c)) * (ssize_t)len;
  if (filledsize < 0) return SCM_OBJ_NULL;

  front = scm_string_substr(str, 0, pos);
  if (scm_obj_null_p(front)) return SCM_OBJ_NULL;

  if (pos + len < SCM_STRING_LENGTH(str)) {
    rear = scm_string_substr(str, pos + len,
                             SCM_STRING_LENGTH(str) - pos - len);
    if (scm_obj_null_p(rear)) return SCM_OBJ_NULL;
  }

  tmp = scm_string_copy_and_expand(front,
                                   SCM_STRING_BYTESIZE(front)
                                   + len
                                   + (scm_obj_null_p(rear) ?
                                      0 : SCM_STRING_BYTESIZE(rear)));
  if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;

  for (i = 0; i < len; i++)
    if (scm_obj_null_p(scm_string_push(tmp, c)))
      return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(rear))
    if (scm_obj_null_p(scm_string_append(tmp, rear)))
      return SCM_OBJ_NULL;

  scm_string_replace_contents(str, tmp);

  return str;
}

ssize_t
scm_string_find_chr(ScmObj str, scm_char_t c) /* GC OK */
{
  int (*char_width)(const void *p, size_t size);
  ScmStrItr iter;
  int cw, pos;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(SCM_STRING_ENC(str));
  cw = char_width(&c, sizeof(c));
  if (cw < 0) return -1;

  iter = scm_str_itr_begin(SCM_STRING_HEAD(str),
                           SCM_STRING_BYTESIZE(str), char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  pos = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    int w = SCM_STR_ITR_WIDTH(&iter);
    if (w < 0) return -1;
    if ((cw == w) && (memcmp(&c, SCM_STR_ITR_PTR(&iter), (size_t)cw) == 0))
      return pos;

    scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

    pos++;
  }

  return -1;
}

ssize_t
scm_string_match(ScmObj str, ScmObj pat) /* GC OK */
{
  int (*char_width)(const void *p, size_t size);
  ScmStrItr iter_str_ext, iter_pat;
  int pos;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(pat, &SCM_STRING_TYPE_INFO);

  pos = 0;

  if (SCM_STRING_ENC(str) != SCM_STRING_ENC(pat))
    return -1;

  char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(SCM_STRING_ENC(str));

  iter_str_ext = scm_str_itr_begin(SCM_STRING_HEAD(str),
                                   SCM_STRING_BYTESIZE(str), char_width);
  if (SCM_STR_ITR_IS_ERR(&iter_str_ext)) return -1;

  while (!SCM_STR_ITR_IS_END(&iter_str_ext)) {
    ScmStrItr iter_str_inn;

    SCM_STR_ITR_COPY(&iter_str_ext, &iter_str_inn);

    iter_pat = scm_str_itr_begin(SCM_STRING_HEAD(pat),
                                 SCM_STRING_BYTESIZE(pat), char_width);
    if (SCM_STR_ITR_IS_ERR(&iter_pat)) return -1;

    if (SCM_STR_ITR_REST(&iter_str_ext) < SCM_STR_ITR_REST(&iter_pat))
      return -1;

    while (!SCM_STR_ITR_IS_END(&iter_str_inn)
           && !SCM_STR_ITR_IS_END(&iter_pat)) {

      if (SCM_STR_ITR_WIDTH(&iter_str_inn) != SCM_STR_ITR_WIDTH(&iter_pat))
        break;

      if (memcmp(SCM_STR_ITR_PTR(&iter_str_inn), SCM_STR_ITR_PTR(&iter_pat),
                 (size_t)SCM_STR_ITR_WIDTH(&iter_str_inn)) != 0)
        break;

      scm_str_itr_next(&iter_str_inn);
      if (SCM_STR_ITR_IS_ERR(&iter_str_inn)) return -1;

      scm_str_itr_next(&iter_pat);
      if (SCM_STR_ITR_IS_ERR(&iter_pat)) return -1;
    }

    if (SCM_STR_ITR_IS_END(&iter_pat))
      return pos;

    scm_str_itr_next(&iter_str_ext);
    if (SCM_STR_ITR_IS_ERR(&iter_str_ext)) return -1;;

    pos++;
  }

  return -1;
}

ssize_t
scm_string_dump(ScmObj str, void *buf, size_t size) /* GC OK */
{
  size_t len;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  len = (size < SCM_STRING_BYTESIZE(str)) ? size : SCM_STRING_BYTESIZE(str);
  memcpy(buf, SCM_STRING_HEAD(str), len); // XXX

  return (ssize_t)len;
}

SCM_ENC_T
scm_string_encoding(ScmObj str) /* GC OK */
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  return SCM_STRING_ENC(str);
}

void *
scm_string_content(ScmObj str)  /* GC OK */
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  return SCM_STRING_HEAD(str);
}

void
scm_string_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);

  SCM_STRING_BUFFER(obj) = NULL;
  SCM_STRING_REF_CNT(obj) = NULL;
}

void
scm_string_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_string_finalize(obj);
}

size_t
scm_string_hash_value(ScmObj str) /* GC OK */
{
  size_t hash;
  unsigned int i;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  hash = 0;
  for (i = 0; i < SCM_STRING_BYTESIZE(str); i++)
    hash = (hash << 5) - hash + (unsigned char)SCM_STRING_BYTE_AT(str, i);

  return hash;
}
