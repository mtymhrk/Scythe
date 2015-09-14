#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/vm.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/exception.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/vector.h"
#include "scythe/string.h"

#define SCM_STRING_BLOCK_SIZE  64
#define CAPACITY(str) (SCM_STRING_CAPACITY(str)                 \
                       - (size_t)((SCM_STRING_HEAD(str)         \
                                   - SCM_STRING_BUFFER(str))))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - SCM_STRING_BYTESIZE(str))

ScmTypeInfo SCM_STRING_TYPE_INFO = {
  .name                = "string",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_string_obj_print,
  .obj_size            = sizeof(ScmString),
  .gc_ini_func         = scm_string_gc_initialize,
  .gc_fin_func         = scm_string_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static ssize_t
check_bytes(void *str, size_t size, ScmEncoding *enc)
{
  ScmStrItr iter;
  ssize_t len;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);

  scm_str_itr_begin((void *)str, size, enc, &iter);
  if (scm_str_itr_err_p(&iter)) return -1;

  len = 0;
  while (!scm_str_itr_end_p(&iter)) {
    len++;
    scm_str_itr_next(&iter);
    if (scm_str_itr_err_p(&iter)) return -1;
  }

  return len;
}

static ssize_t
copy_bytes_with_check(void *dst, const void *src, size_t size, ScmEncoding *enc)
{
  ssize_t len;

  scm_assert(dst != NULL);
  scm_assert(src != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);

  memcpy(dst, src, size);
  len = check_bytes(dst, size, enc);
  if (len < 0) return -1;

  return len;
}

static ScmObj
copy_and_expand(ScmObj src, size_t size)
{
  ScmObj str = SCM_OBJ_INIT;;
  ssize_t len;

  SCM_REFSTK_INIT_REG(&str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);
  scm_assert(size <= SSIZE_MAX);

  str = scm_string_new(SCM_MEM_HEAP, NULL, size, SCM_STRING_ENC(src));
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  SCM_STRING_BYTESIZE(str) =
    (size < SCM_STRING_BYTESIZE(src)) ? size : SCM_STRING_BYTESIZE(src);
  len =
    copy_bytes_with_check(SCM_STRING_BUFFER(str),
                          SCM_STRING_HEAD(src),
                          /* XXX: SCM_STRING_BYTESIZE(str) ? */
                          SCM_STRING_BYTESIZE(src),
                          SCM_STRING_ENC(src));
  scm_assert(len >= 0);

  SCM_STRING_LENGTH(str) = (size_t)len;

  return str;
}

static void
replace_contents(ScmObj target, ScmObj src)
{
  scm_assert_obj_type(target, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  if (scm_obj_null_p(target) || scm_obj_null_p(src)) return;

  if (*SCM_STRING_REF_CNT(target) > 1) {
    SCM_STRING_DEC_REF_CNT(target);
  }
  else {
    scm_free(SCM_STRING_BUFFER(target));
    scm_free(SCM_STRING_REF_CNT(target));
  }

  *SCM_STRING(target) = *SCM_STRING(src);
  SCM_STRING_INC_REF_CNT(target);
}

static ScmObj
clone_string(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return scm_string_new(SCM_MEM_HEAP,
                        SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str),
                        SCM_STRING_ENC(str));
}

static int
push_cchr(ScmObj str, const scm_char_t *c)
{
  ScmObj tmp = SCM_OBJ_INIT;
  ssize_t width;

  SCM_REFSTK_INIT_REG(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  width = scm_enc_char_width(SCM_STRING_ENC(str), c, sizeof(*c));
  if (width < 0) {
    scm_error("failed to push a character into string: "
                  "invalid byte sequence", 0);
    return -1;
  }

  if ((*SCM_STRING_REF_CNT(str) > 1) || ROOM_FOR_APPEND(str) < (size_t)width) {
    if (SCM_STRING_BYTESIZE(str) > SSIZE_MAX - (size_t)width) {
      scm_error("failed to push a character into string: string too long",
                    0);
      return -1;
    }
    tmp = copy_and_expand(str, SCM_STRING_BYTESIZE(str) + (size_t)width);
    if (scm_obj_null_p(tmp)) return -1;
    replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str), c, (size_t)width);
  SCM_STRING_LENGTH(str) += 1;
  SCM_STRING_BYTESIZE(str) += (size_t)width;

  return 0;
}

static int
set_cchr(ScmObj str, size_t pos, const scm_char_t *c)
{
  ScmObj front = SCM_OBJ_NULL, rear = SCM_OBJ_NULL, tmp = SCM_OBJ_NULL;
  ScmStrItr iter;
  int cw, iw;

  SCM_REFSTK_INIT_REG(&str, &front, &rear, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);

  if (pos >= SCM_STRING_LENGTH(str)) {
    scm_error("can not update a character in string: "
                   "argument out of range", 0);
    return -1;
  }

  scm_enc_index2itr(SCM_STRING_ENC(str),
                    SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos, &iter);
  scm_assert(!scm_str_itr_err_p(&iter));

  cw = scm_enc_char_width(SCM_STRING_ENC(str), c, sizeof(*c));
  if (cw < 0) {
    scm_error("can not update a character in string: "
                   "invalid byte sequence", 0);
    return -1;
  }

  iw = scm_str_itr_width(&iter);
  scm_assert(iw >= 0);

  if (*SCM_STRING_REF_CNT(str) == 1
      && (iw > cw || ROOM_FOR_APPEND(str) >= (size_t)(cw - iw))) {
    size_t rest = (size_t)scm_str_itr_rest(&iter);
    size_t offset = (size_t)scm_str_itr_offset(&iter, SCM_STRING_HEAD(str));

    if (cw != iw) {
      memmove(SCM_STRING_HEAD(str) + offset + cw,
              SCM_STRING_HEAD(str) + offset + iw,
              rest - (size_t)iw);
      if (iw > cw)
        SCM_STRING_BYTESIZE(str) -= (size_t)(iw - cw);
      else
        SCM_STRING_BYTESIZE(str) += (size_t)(cw - iw);
    }
    memcpy(SCM_STRING_HEAD(str) + offset, c, (size_t)cw);

    return 0;
  }
  else if (cw == iw) {
    size_t offset = scm_str_itr_offset(&iter, SCM_STRING_HEAD(str));
    tmp = clone_string(str);

    if (scm_obj_null_p(tmp)) return -1;
    memcpy(SCM_STRING_HEAD(tmp) + offset, c, (size_t)cw);
    replace_contents(str, tmp);

    return 0;
  }
  else {
    if (cw > iw &&
        SCM_STRING_BYTESIZE(str) > SSIZE_MAX - (size_t)(cw - iw)) {
      scm_error("failed to set a character into string: string too long",
                    0);
      return -1;
    }

    front = scm_string_substr(str, 0, pos);
    rear = scm_string_substr(str, pos + 1, SCM_STRING_LENGTH(str) - pos - 1);

    if (scm_obj_null_p(front) || scm_obj_null_p(rear))
      return -1;

    tmp = copy_and_expand(front,
                          SCM_STRING_BYTESIZE(front)
                          + (size_t)cw + SCM_STRING_BYTESIZE(rear));
    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    if (push_cchr(tmp, c) < 0) return -1;
    if (scm_string_push_string(tmp, rear) < 0) return -1;

    replace_contents(str, tmp);
    return 0;
  }
}

enum { DOWNCASE, UPCASE };

static ScmObj
change_case(ScmObj str, int dir)
{
  ScmObj s = SCM_OBJ_INIT;
  ScmStrItr itr;

  SCM_REFSTK_INIT_REG(&str,
                      &s);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(dir == DOWNCASE || dir == UPCASE);

  s = scm_string_new(SCM_MEM_HEAP, NULL, 0, SCM_STRING_ENC(str));
  if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

  scm_str_itr_begin(SCM_STRING_HEAD(str),
                    SCM_STRING_BYTESIZE(str), SCM_STRING_ENC(str), &itr);
  if (scm_str_itr_err_p(&itr)) {
    /*  TODO: wirte me */
    /* scm_capi_error("", 0); */
    return SCM_OBJ_NULL;
  }

  while (!scm_str_itr_end_p(&itr)) {
    scm_char_t chr;
    ssize_t w;
    int r;

    if (dir == DOWNCASE)
      w = scm_enc_downcase(SCM_STRING_ENC(str), scm_str_itr_ptr(&itr),
                           (size_t)scm_str_itr_rest(&itr), &chr);
    else
      w = scm_enc_upcase(SCM_STRING_ENC(str), scm_str_itr_ptr(&itr),
                         (size_t)scm_str_itr_rest(&itr), &chr);
    if (w < 0) {
      /*  TODO: wirte me */
      /* scm_capi_error("", 0); */
      return SCM_OBJ_NULL;
    }

    r = push_cchr(s, &chr);
    if (r < 0) return SCM_OBJ_NULL;

    scm_str_itr_next(&itr);
    if (scm_str_itr_err_p(&itr)) {
      /*  TODO: wirte me */
      /* scm_capi_error("", 0); */
      return SCM_OBJ_NULL;
    }
  }

  return s;
}

static char *
change_encoding(const char *src, size_t ssz, const char *from, const char *to,
                size_t *dsz)
{
  ScmEncCnv cnv;
  char *buf;
  size_t cap, sum;

  scm_assert(src != NULL);
  scm_assert(from != NULL);
  scm_assert(to != NULL);

  if (ssz == 0) {
    if (dsz != NULL) *dsz = 0;
    return scm_malloc(1);
  }

  scm_enc_cnv_init(&cnv, from, to, src, ssz);
  if (scm_enc_cnv_err_p(&cnv)) {
    scm_error("failed to convert encoding: invalid encoding name", 0);
    return NULL;
  }

  cap = ssz;
  buf = scm_malloc(cap);
  if (buf == NULL) goto err;

  sum = 0;
  while (!scm_enc_cnv_end_p(&cnv)) {
    size_t s = scm_enc_cnv_convert(&cnv, buf + sum, cap - sum, true);
    if (scm_enc_cnv_err_p(&cnv)) {
      if (scm_enc_cnv_illegal_p(&cnv))
        scm_error("fiald to convert encoding: illegal multibyte sequence", 0);
      else if (scm_enc_cnv_incomplete_p(&cnv))
        scm_error("fiald to convert encoding: "
                      "incomplete multibyte sequence", 0);
      else
        scm_error("fiald to convert encoding: unknown error has occurred", 0);

      goto err;
    }
    else if (scm_enc_cnv_insufficient_buf_p(&cnv)) {
      char *p;
      if (cap == SIZE_MAX) {
        scm_error("failed to convert encoding: too big string", 0);
        goto err;
      }

      cap =  (SIZE_MAX / 2 < cap) ? SIZE_MAX : cap * 2;
      p = scm_realloc(buf, cap);
      if (p != NULL) goto err;

      buf = p;
    }

    sum += s;
  }

  scm_enc_cnv_fin(&cnv);
  if (dsz != NULL) *dsz = sum;
  return buf;

 err:
  if (buf != NULL) scm_free(buf);
  scm_enc_cnv_fin(&cnv);
  return NULL;
}

static int
write_ext_rep_inner(ScmObj str, ScmObj port, size_t len)
{
  scm_char_t ary[len], *p;
  ScmEncoding *enc;
  int rslt, c;

  SCM_REFSTK_INIT_REG(&str, &port);

  p = scm_string_to_cchr_ary(str,0, (ssize_t)len, ary);
  if (p == NULL) return -1;

  enc = SCM_STRING_ENC(str);

  rslt = scm_write_cstr("\"", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  for (size_t i = 0; i < len; i++) {
    if (!scm_enc_ascii_p(enc, ary[i].bytes, sizeof(ary[i]))) {
      rslt = scm_write_cchr(ary[i], enc, port);
      if (rslt < 0) return -1;

      continue;
    }

    c = scm_enc_cnv_to_ascii(enc, &ary[i]);
    if (0x20 <= c && c <= 0x7f) {
      rslt = scm_write_cchr(ary[i], enc, port);
      if (rslt < 0) return -1;

      continue;
    }

    switch (c) {
    case '"':
      rslt = scm_write_cstr("\\\"", SCM_ENC_SRC, port);
      break;
    case '\\':
      rslt = scm_write_cstr("\\\\", SCM_ENC_SRC, port);
      break;
    case '\a':
      rslt = scm_write_cstr("\\a", SCM_ENC_SRC, port);
      break;
    case '\b':
      rslt = scm_write_cstr("\\b", SCM_ENC_SRC, port);
      break;
    case '\t':
      rslt = scm_write_cstr("\\t", SCM_ENC_SRC, port);
      break;
    case '\n':
      rslt = scm_write_cstr("\\n", SCM_ENC_SRC, port);
      break;
    case '\r':
      rslt = scm_write_cstr("\\r", SCM_ENC_SRC, port);
      break;
    default:
      rslt = scm_string_inline_hex_escape(ary[i], enc, port);
      break;
    }
    if (rslt < 0) return -1;
  }

  rslt = scm_write_cstr("\"", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

static int
write_ext_rep(ScmObj obj, ScmObj port)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);
  return write_ext_rep_inner(obj, port, SCM_STRING_LENGTH(obj));
}

ScmObj
scm_string_P(ScmObj obj)
{
  return scm_string_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_string_initialize(ScmObj str,
                      const void *src, size_t size, ScmEncoding *enc)
{
  SCM_REFSTK_INIT_REG(&str);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);

  SCM_STRING_BUFFER(str) = NULL;
  SCM_STRING_REF_CNT(str) = NULL;
  SCM_STRING_ENC(str) = enc;

  for (SCM_STRING_CAPACITY(str) = SCM_STRING_BLOCK_SIZE;
       SCM_STRING_CAPACITY(str) < size;
       SCM_STRING_CAPACITY(str) *= 2)
    ;

  SCM_STRING_BUFFER(str) = scm_malloc(SCM_STRING_CAPACITY(str));
  SCM_STRING_HEAD(str) = SCM_STRING_BUFFER(str);
  if (SCM_STRING_BUFFER(str) == NULL) return -1;

  SCM_STRING_REF_CNT(str) =
    scm_malloc(sizeof(*SCM_STRING_REF_CNT(str)));
  if (SCM_STRING_REF_CNT(str) == NULL) return -1;

  *SCM_STRING_REF_CNT(str) = 1;

  if (src != NULL) {
    ssize_t len = copy_bytes_with_check(SCM_STRING_BUFFER(str), src, size, enc);
    if (len < 0) {
      scm_error("can not make string object: invalid byte sequence", 0);
      return -1;
    }

    SCM_STRING_LENGTH(str) = (size_t)len;
    SCM_STRING_BYTESIZE(str) = size;
  }
  else {
    SCM_STRING_LENGTH(str) = 0;
    SCM_STRING_BYTESIZE(str) = 0;
  }

  return 0;
}

void
scm_string_finalize(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  if (SCM_STRING_REF_CNT(str) != NULL && *SCM_STRING_REF_CNT(str) > 1)
    SCM_STRING_DEC_REF_CNT(str);
  else {
    scm_free(SCM_STRING_BUFFER(str));
    scm_free(SCM_STRING_REF_CNT(str));
  }

  /* push() 関数や append() 関数内の tmp はこれらの関数から直接 finalize()
   * を call され、さらに GC 時にも gc_finalize() から call されるため、2 度
   * call されても問題ないようにする必要がある
   */
  SCM_STRING_BUFFER(str) = NULL;
  SCM_STRING_REF_CNT(str) = NULL;
}

ScmObj
scm_string_new(scm_mem_type_t mtype,
               const void *src, size_t size, ScmEncoding *enc)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str);
  scm_assert(size <= SSIZE_MAX);

  if (enc == NULL)
    enc = scm_system_encoding();

  str = scm_alloc_mem(&SCM_STRING_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  if (scm_string_initialize(str, src, size, enc) < 0)
    return SCM_OBJ_NULL;

  return str;
}

ScmObj
scm_make_string_from_cstr(const char *str, ScmEncoding *enc)
{
  if (str == NULL) {
    return scm_string_new(SCM_MEM_HEAP, "", 0, enc);
  }
  else {
    size_t sz = strlen(str);
    if (sz > SSIZE_MAX) {
      scm_error("failed to make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_HEAP, str, sz, enc);
  }
}

ScmObj
scm_make_string_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  if (data == NULL) {
    return scm_string_new(SCM_MEM_HEAP, "", 0, enc);
  }
  else {
    if (size > SSIZE_MAX) {
      scm_error("failed to make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_HEAP, data, size, enc);
  }
}

ScmObj
scm_make_string_from_external(const void *data, size_t size, const char *enc)
{
  ScmObj obj = SCM_OBJ_INIT;
  char encname[256];
  char *buf;
  size_t sz;

  if (data == NULL)
    return scm_make_string_from_bin(data, size, NULL);

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(encname, sizeof(encname));
    scm_assert(r > 0);
    enc = encname;
  }

  if (scm_enc_find_enc(enc) == scm_system_encoding())
    return scm_make_string_from_bin(data, size, NULL);

  buf = change_encoding(data, size, enc, scm_enc_name(scm_system_encoding()),
                        &sz);
  if (buf == NULL) return SCM_OBJ_NULL;

  obj = scm_make_string_from_bin(buf, sz, NULL);
  scm_free(buf);
  return obj;
}

ScmObj
scm_string_cv(const ScmObj *chr, size_t n)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&str);

  if (chr == NULL || n == 0)
    return scm_string_new(SCM_MEM_HEAP, NULL, 0, NULL);

  enc = 0;
  for (size_t i = 0; i < n; i++) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    if (!scm_char_p(chr[i])) {
      scm_error("failed to make string: required character, but got",
                    1, chr[i]);
      return SCM_OBJ_NULL;
    }

    if (i == 0) {
      enc = scm_char_encoding(chr[i]);
      str = scm_string_new(SCM_MEM_HEAP, NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr[i]);

    if (e != enc) {
      scm_error("failed to make string string: encoding mismatch",
                    1, chr[i]);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr[i]);

    r = push_cchr(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

ScmObj
scm_string(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_string_cv(args, n);
}

ScmObj
scm_string_dup(ScmObj src)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&src, &str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  str = scm_alloc_heap(&SCM_STRING_TYPE_INFO, 0);

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

bool
scm_string_equal_p(ScmObj str1, ScmObj str2)
{
  scm_assert_obj_type(str1, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(str2, &SCM_STRING_TYPE_INFO);

  if (SCM_STRING_ENC(str1) != SCM_STRING_ENC(str2)) return false;
  if (SCM_STRING_LENGTH(str1) != SCM_STRING_LENGTH(str2)) return false;
  if (SCM_STRING_BYTESIZE(str1) != SCM_STRING_BYTESIZE(str2)) return false;
  if (SCM_STRING_HEAD(str1) == SCM_STRING_HEAD(str2)) return true;

  return (memcmp(SCM_STRING_HEAD(str1),
                 SCM_STRING_HEAD(str2),
                 SCM_STRING_BYTESIZE(str1))
          == 0);
}

int
scm_string_cmp(ScmObj s1, ScmObj s2, int *rslt)
{
  int r;

  scm_assert_obj_type(s1, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(s2, &SCM_STRING_TYPE_INFO);
  scm_assert(SCM_STRING_ENC(s1) == SCM_STRING_ENC(s2));

  r = 0;
  for (size_t i = 0;
       i < SCM_STRING_BYTESIZE(s1) && SCM_STRING_BYTESIZE(s2);
       i++) {
    if (SCM_STRING_BYTE_AT(s1, i) < SCM_STRING_BYTE_AT(s2, i)) {
      r = -1;
      break;
    }
    else if (SCM_STRING_BYTE_AT(s1, i) > SCM_STRING_BYTE_AT(s2, i)) {
      r = 1;
      break;
    }
  }

  if (r == 0) {
    if (SCM_STRING_BYTESIZE(s1) < SCM_STRING_BYTESIZE(s2))
      r = -1;
    else if (SCM_STRING_BYTESIZE(s1) > SCM_STRING_BYTESIZE(s2))
      r = 1;
  }

  if (rslt != NULL) *rslt = r;

  return 0;
}

static int
compare_aux(ScmObj s1, ScmObj s2, bool (*filter)(int cmp), bool *rslt)
{
  int err, cmp;

  scm_assert(scm_string_p(s1));
  scm_assert(scm_string_p(s2));

  if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_error("failed to compare strings: encoding mismatch", 2, s1, s2);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = filter(cmp);

  return 0;
}

static bool
filter_for_eq(int cmp)
{
  return (cmp == 0) ? true : false;
}

int
scm_string_eq(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_eq, rslt);
}

ScmObj
scm_string_eq_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_string_eq(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static int
cmp_fold(ScmObj lst, int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt), bool *rslt)

{
  ScmObj str = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_pair_p(l); l = scm_cdr(l)) {
    str = scm_car(l);
    if (!scm_string_p(str)) {
      scm_error("failed to compare strings: string required, but got",
                    1, str);
      return -1;
    }

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, str, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = str;
  }

  *rslt = true;

  return 0;
}

ScmObj
scm_string_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_error("string=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  r = cmp_fold(lst, scm_string_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_lt(int cmp)
{
  return (cmp == -1) ? true : false;
}

int
scm_string_lt(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_lt, rslt);
}

ScmObj
scm_string_lt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_string_lt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_string_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = cmp_fold(lst, scm_string_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_gt(int cmp)
{
  return (cmp == 1) ? true : false;
}

int
scm_string_gt(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_gt, rslt);
}

ScmObj
scm_string_gt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_string_gt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_string_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = cmp_fold(lst, scm_string_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_le(int cmp)
{
  return(cmp == -1 || cmp == 0) ? true : false;
}

int
scm_string_le(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_le, rslt);
}

ScmObj
scm_string_le_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_string_le(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_string_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = cmp_fold(lst, scm_string_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_ge(int cmp)
{
  return (cmp == 0 || cmp == 1) ? true : false;
}

int
scm_string_ge(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_ge, rslt);
}

ScmObj
scm_string_ge_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_string_ge(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_string_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = cmp_fold(lst, scm_string_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_string_encode(ScmObj str, ScmEncoding *enc)
{
  ScmObj out = SCM_OBJ_INIT;
  char *buf;
  size_t sz;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(enc != NULL);

  SCM_REFSTK_INIT_REG(&str,
                      &out);

  if (SCM_STRING_ENC(str) == enc)
    return scm_string_dup(str);

  buf = change_encoding((char *)SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str),
                        scm_enc_name(SCM_STRING_ENC(str)), scm_enc_name(enc),
                        &sz);
  if (buf == NULL) return SCM_OBJ_NULL;

  out = scm_string_new(SCM_MEM_HEAP, buf, sz, enc);
  scm_free(buf);
  return out;
}

ScmObj
scm_string_convert(ScmObj str, const char *enc)
{
  ScmObj out = SCM_OBJ_INIT;
  char *buf;
  size_t sz;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(enc != NULL);

  SCM_REFSTK_INIT_REG(&str,
                      &out);

  buf = change_encoding((char *)SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str),
                        scm_enc_name(SCM_STRING_ENC(str)), enc,
                        &sz);
  if (buf == NULL) return SCM_OBJ_NULL;

  out = scm_make_bytevector_from_cv(buf, sz);
  scm_free(buf);
  return out;
}

ScmObj
scm_string_substr(ScmObj str, size_t pos, size_t len)
{
  ScmObj substr = SCM_OBJ_INIT;
  ScmStrItr head, tail;

  SCM_REFSTK_INIT_REG(&str, &substr);
  scm_assert(pos <= SSIZE_MAX);
  scm_assert(len <= SSIZE_MAX);

  if (pos >= SCM_STRING_LENGTH(str)
      || (ssize_t)pos > (ssize_t)SCM_STRING_LENGTH(str) - (ssize_t)len) {
    /* TODO: change error message */
    scm_error("can not make substring: argument is out of range", 0);
    return SCM_OBJ_NULL;
  }

  scm_enc_index2itr(SCM_STRING_ENC(str), SCM_STRING_HEAD(str),
                    SCM_STRING_BYTESIZE(str), pos, &head);
  scm_enc_index2itr(SCM_STRING_ENC(str), SCM_STRING_HEAD(str),
                    SCM_STRING_BYTESIZE(str), pos + len, &tail);

  scm_assert(!scm_str_itr_err_p(&head) && !scm_str_itr_err_p(&tail));

  substr = scm_string_dup(str);
  SCM_STRING_HEAD(substr) = SCM_STR_ITR_PTR(&head, SCM_STRING_HEAD(str));
  SCM_STRING_LENGTH(substr) = len;
  SCM_STRING_BYTESIZE(substr)
    = (size_t)((const uint8_t *)scm_str_itr_ptr(&tail)
               - (const uint8_t *)scm_str_itr_ptr(&head));

  return substr;
}

int
scm_string_push_cchr(ScmObj str, scm_char_t chr, ScmEncoding *enc)
{
  ScmEncoding *s_enc;

  scm_assert(scm_string_p(str));

  s_enc = scm_string_encoding(str);
  if (s_enc != enc) {
    scm_error("failed to append a character to string: encoding mismatch",
                  0);
    return -1;
  }

  return push_cchr(str, &chr);
}

int
scm_string_push_string(ScmObj str, ScmObj append)
{
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(append, &SCM_STRING_TYPE_INFO);
  scm_assert(SCM_STRING_ENC(str) == SCM_STRING_ENC(append));

  if ((*SCM_STRING_REF_CNT(str) > 1) ||
      ROOM_FOR_APPEND(str) < SCM_STRING_BYTESIZE(append)) {
    tmp = copy_and_expand(str,
                          SCM_STRING_BYTESIZE(str)
                          + SCM_STRING_BYTESIZE(append));
    if (scm_obj_null_p(tmp)) return -1;
    replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str),
         SCM_STRING_HEAD(append), SCM_STRING_BYTESIZE(append));
  SCM_STRING_LENGTH(str) += SCM_STRING_LENGTH(append);
  SCM_STRING_BYTESIZE(str) += SCM_STRING_BYTESIZE(append);

  return 0;
}

int
scm_string_ref_cchr(ScmObj str, size_t pos, scm_char_t *chr)
{
  ScmStrItr iter;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos < scm_string_length(str));
  scm_assert(chr != NULL);

  scm_enc_index2itr(SCM_STRING_ENC(str),
                    SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos, &iter);

  scm_assert(!scm_str_itr_err_p(&iter));
  scm_assert(!scm_str_itr_end_p(&iter));

  memset(chr->bytes, 0, sizeof(*chr));
  memcpy(chr->bytes, scm_str_itr_ptr(&iter), (size_t)scm_str_itr_width(&iter));

  return 0;
}

ScmObj
scm_string_ref_char(ScmObj str, size_t pos)
{
  scm_char_t c;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&str);

  scm_assert(scm_string_p(str));
  scm_assert(pos < scm_string_length(str));

  enc = scm_string_encoding(str);
  scm_string_ref_cchr(str, pos, &c);
  return scm_make_char(&c, enc);
}

int
scm_string_set_cchr(ScmObj str, size_t pos, scm_char_t chr, ScmEncoding *enc)
{
  ScmEncoding *s_enc;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos < scm_string_length(str));

  s_enc = scm_string_encoding(str);
  if (s_enc != enc) {
    scm_error("failed to store a character to string: encoding mismatch",
                  0);
    return -1;
  }

  return set_cchr(str, pos, &chr);
}

int
scm_string_set_char(ScmObj str, size_t pos, ScmObj chr)
{
  scm_assert(scm_string_p(str));
  scm_assert(pos < scm_string_length(str));
  scm_assert(scm_char_p(chr));

  return scm_string_set_cchr(str, pos,
                             scm_char_value(chr), scm_char_encoding(chr));
}

ScmObj
scm_string_downcase(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return change_case(str, DOWNCASE);
}

ScmObj
scm_string_upcase(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return change_case(str, UPCASE);
}

ScmObj
scm_string_append_lst(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, l = SCM_OBJ_INIT, s = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &l, &s);

  if (scm_obj_null_p(lst))
    return scm_make_string_from_bin(NULL, 0, scm_system_encoding());

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_pair_p(l); l = scm_cdr(l)) {
    ScmEncoding *e;
    int r;

    s = scm_car(l);
    if (!scm_string_p(s)) {
      scm_error("failed to append strings: string required, but got", 1, s);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      str = scm_string_dup(s);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
      enc = scm_string_encoding(s);
    }
    else {
      e = scm_string_encoding(s);
      if (enc != e) {
        scm_error("failed to append strings: encoding mismatch", 2, str, s);
        return SCM_OBJ_NULL;
      }

      r = scm_string_push_string(str, s);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(str))
    return scm_make_string_from_bin(NULL, 0, scm_system_encoding());
  else
    return str;
}

ScmObj
scm_string_append_cv(ScmObj *ary, size_t n)
{
  ScmObj str = SCM_OBJ_NULL;
  ScmEncoding *enc, *e;

  SCM_REFSTK_INIT_REG(&str);

  if (ary == NULL || n == 0)
    return scm_make_string_from_bin(NULL, 0, scm_system_encoding());

  /* str = scm_string_copy(ary[0], -1, -1); */
  str = scm_string_dup(ary[0]);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 1; i < n; i++) {
    int r;

    if (!scm_string_p(ary[i])) {
      scm_error("failed to append strings: sring required, but got",
                    1, ary[i]);
      return SCM_OBJ_NULL;
    }

    e = scm_string_encoding(ary[i]);
    if (enc != e) {
      scm_error("failed to append strings: encoding mismatch",
                    2, str, ary[i]);
      return SCM_OBJ_NULL;
    }

    r = scm_string_push_string(str, ary[i]);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

ScmObj
scm_string_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_string_append_cv(args, n);
}

ScmObj
scm_string_copy(ScmObj str, ssize_t start, ssize_t end)
{
  scm_assert(scm_string_p(str));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  return scm_string_substr(str, (size_t)start, (size_t)(end - start));
}

static int
string_copy_i_aux(ScmObj to, size_t at, ScmObj from, size_t pos, size_t len)
{
  scm_char_t ary[len], *p;

  SCM_REFSTK_INIT_REG(&to, &from);

  p = scm_string_to_cchr_ary(from, pos, (ssize_t)len, ary);
  if (p == NULL) return -1;

  for (size_t i = 0; i < len; i++) {
    int r = set_cchr(to, at + i, ary + i);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_string_copy_i(ScmObj to, size_t at, ScmObj from, ssize_t start, ssize_t end)
{
  size_t to_len, from_len, sub_len, len;

  SCM_REFSTK_INIT_REG(&to, &from);

  scm_assert(scm_string_p(to));
  scm_assert(at < scm_string_length(to));
  scm_assert(scm_string_p(from));
  scm_assert(start < 0 || (size_t)start < scm_string_length(from));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(from));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (scm_string_encoding(to) != scm_string_encoding(from)) {
    scm_error("failed to copy string: encoding mismatch", 1, to, from);
    return -1;
  }

  to_len = scm_string_length(to);
  from_len = scm_string_length(from);
  sub_len = to_len - at;

  if (start < 0)
    start = 0;

  if (end > 0) {
    len = (size_t)(end - start);
    if (len > sub_len) {
      scm_error("failed to copy string: out of range", 0);
      return -1;
    }
  }
  else {
    if (sub_len < from_len - (size_t)start)
      len = sub_len;
    else
      len = from_len - (size_t)start;
  }

  return string_copy_i_aux(to, at, from, (size_t)start, len);
}

int
scm_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end)
{
  scm_char_t c;

  scm_assert(scm_string_p(str));
  scm_assert(scm_char_p(fill));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (scm_string_encoding(str) != scm_char_encoding(fill)) {
    scm_error("failed to fill string with character: encoding mismatch",
                  2, str, fill);
    return -1;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  c = scm_char_value(fill);

  for (ssize_t i = start; i < end; i++) {
    int r = set_cchr(str, (size_t)i, &c);
    if (r < 0) return -1;
  }

  return 0;
}

scm_char_t *
scm_string_to_cchr_ary(ScmObj str, size_t pos, ssize_t len, scm_char_t *ary)
{
  ScmStrItr iter;
  size_t l, i;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  if (pos >= SCM_STRING_LENGTH(str)) {
    scm_error("failed to make array of characters: invalid argument", 0);
    return NULL;
  }

  if (len == 0)
    return ary;

  l = (SCM_STRING_LENGTH(str) - pos);
  if (len > 0 && (size_t)len < l)
    l = (size_t)len;

  if (ary == NULL) {
    ary = scm_malloc(sizeof(scm_char_t) * l);
    if (ary == NULL) return NULL;
  }

  scm_enc_index2itr(SCM_STRING_ENC(str),
                    SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos, &iter);

  if (scm_str_itr_err_p(&iter)) return NULL;

  i = 0;
  while (i < l && !scm_str_itr_end_p(&iter)) {
    memcpy(ary[i++].bytes, scm_str_itr_ptr(&iter),
           (size_t)scm_str_itr_width(&iter));
    scm_str_itr_next(&iter);
    scm_assert(!scm_str_itr_err_p(&iter));
  }

  return ary;
}

static ScmObj
string_to_list_aux(ScmObj str, size_t pos, size_t len)
{
  scm_char_t c_ary[len], *p;
  ScmObj o_ary[len];
  ScmEncoding *enc;

  for (size_t i = 0; i < len; i++) o_ary[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&str);
  SCM_REFSTK_REG_ARY(o_ary, len);

  p = scm_string_to_cchr_ary(str, pos, (ssize_t)len, c_ary);
  if (p == NULL) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 0; i < len; i++) {
    o_ary[i] = scm_char_new(SCM_MEM_HEAP, c_ary + i, enc);

    if (scm_obj_null_p(o_ary[i])) return SCM_OBJ_NULL;
  }

  return scm_list_cv(o_ary, len);
}

ScmObj
scm_string_to_list(ScmObj str, ssize_t start, ssize_t end)
{
  size_t len;

  scm_assert(scm_string_p(str));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  len = scm_string_length(str);
  scm_assert(len <= SSIZE_MAX);

  if (start < 0) start = 0;
  if (end < 0)  end = (ssize_t)len;

  return string_to_list_aux(str, (size_t)start, (size_t)(end - start));
}

ScmObj
scm_list_to_string(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, l = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &chr, &l);

  if (scm_obj_null_p(lst))
    return scm_make_string_from_bin(NULL, 0, scm_system_encoding());

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_pair_p(l); l = scm_cdr(l)) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    chr = scm_car(l);
    if (!scm_char_p(chr)) {
      scm_error("failed to convert from list to string: "
                    "character require, but got", 1, chr);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      enc = scm_char_encoding(chr);
      str = scm_make_string_from_bin(NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr);

    if (e != enc) {
      scm_error("failed to convert from list to string: encoding mismatch",
                    2, str, chr);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr);

    r = push_cchr(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(str))
    return scm_make_string_from_bin(NULL, 0, scm_system_encoding());
  else
    return str;
}

char *
scm_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  size_t n;

  scm_assert(scm_string_p(str));

  n = scm_string_bytesize(str);
  scm_assert(n <= SSIZE_MAX);

  if (cstr == NULL) {
    size = n + 1;
    cstr = scm_malloc(size);
    if (cstr == NULL) return SCM_OBJ_NULL;
  }
  else if (size == 0) {
    return cstr;
  }
  else if (size - 1 < n) {
    n = size - 1;
  }

  memcpy(cstr, scm_string_content(str), n);
  cstr[n] = '\0';

  return cstr;
}

ssize_t
scm_string_to_path_cstr(ScmObj str, char *cstr, size_t sz)
{
  ScmObj o = SCM_OBJ_INIT;
  char encname[256];
  ssize_t r;
  size_t s;
  void *p;

  SCM_REFSTK_INIT_REG(&str,
                      &o);

  scm_assert(scm_string_p(str));
  scm_assert(cstr != NULL);

  r = scm_enc_locale_to_enc_name(encname, sizeof(encname));
  scm_assert(r > 0);
  if (scm_enc_find_enc(encname) == scm_string_encoding(str)) {
    s = scm_string_bytesize(str);
    if (s > sz - 1) {
      scm_error("too long path name", 1, str);
      return -1;
    }

    p = scm_string_to_cstr(str, cstr, sz);
    if (p == NULL) return -1;
  }
  else {
    o = scm_string_convert(str, encname);
    if (scm_obj_null_p(o)) return -1;

    s = scm_bytevector_length(o);
    if (s > sz - 1) {
      scm_error("too long path name", 1, str);
      return -1;
    }

    p = scm_bytevector_to_cv(o, cstr, sz - 1);
    if (p == NULL) return -1;
    cstr[s] = '\0';
  }

  return (ssize_t)(s + 1);
}

int
scm_string_obj_print(ScmObj obj, ScmObj port, int kind,
                     ScmObjPrintHandler handler)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);

  if (kind == SCM_OBJ_PRINT_DISPLAY) {
    int r = scm_write_string(obj, port, -1, -1);
    if (r < 0) return -1;
  }
  else {
    int r = write_ext_rep(obj, port);
    if (r < 0) return -1;
  }

  return 0;
}

void
scm_string_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);

  SCM_STRING_BUFFER(obj) = NULL;
  SCM_STRING_REF_CNT(obj) = NULL;
}

void
scm_string_gc_finalize(ScmObj obj)
{
  scm_string_finalize(obj);
}

size_t
scm_string_hash_value(ScmObj str)
{
  size_t hash;
  unsigned int i;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  hash = 0;
  for (i = 0; i < SCM_STRING_BYTESIZE(str); i++)
    hash = (hash << 5) - hash + (unsigned char)SCM_STRING_BYTE_AT(str, i);

  return hash;
}

int
scm_string_inline_hex_escape(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  char cstr[32];
  long long scalar = scm_enc_cnv_to_scalar(enc, chr.bytes, sizeof(chr));
  int width;

  scm_assert(scalar >= 0);

  if (scalar <= 0xff)        width = 2;
  else if (scalar <= 0xffff) width = 4;
  else                       width = 8;

  snprintf(cstr, sizeof(cstr), "\\x%0*llx;", width, scalar);
  return scm_write_cstr(cstr, SCM_ENC_SRC, port);
}
