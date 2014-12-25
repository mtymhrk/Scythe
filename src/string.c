#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <limits.h>
#include <iconv.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/encoding.h"
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
scm_string_check_bytes(void *str, size_t size, ScmEncoding *enc)
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
scm_string_copy_bytes_with_check(void *dst, const void *src,
                                 size_t size, ScmEncoding *enc)
{
  ssize_t len;

  scm_assert(dst != NULL);
  scm_assert(src != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);

  memcpy(dst, src, size);
  len = scm_string_check_bytes(dst, size, enc);
  if (len < 0) return -1;

  return len;
}

static ScmObj
scm_string_copy_and_expand(ScmObj src, size_t size)
{
  ScmObj str = SCM_OBJ_INIT;;
  ssize_t len;

  SCM_REFSTK_INIT_REG(&str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);
  scm_assert(size <= SSIZE_MAX);

  str = scm_fcd_string_new(SCM_MEM_HEAP,
                           NULL, size, SCM_STRING_ENC(src));
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  SCM_STRING_BYTESIZE(str) =
    (size < SCM_STRING_BYTESIZE(src)) ? size : SCM_STRING_BYTESIZE(src);
  len =
    scm_string_copy_bytes_with_check(SCM_STRING_BUFFER(str),
                                     SCM_STRING_HEAD(src),
                                     /* XXX: SCM_STRING_BYTESIZE(str) ? */
                                     SCM_STRING_BYTESIZE(src),
                                     SCM_STRING_ENC(src));
  scm_assert(len >= 0);

  SCM_STRING_LENGTH(str) = (size_t)len;

  return str;
}

static void
scm_string_replace_contents(ScmObj target, ScmObj src)
{
  scm_assert_obj_type(target, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  if (scm_obj_null_p(target) || scm_obj_null_p(src)) return;

  if (*SCM_STRING_REF_CNT(target) > 1) {
    SCM_STRING_DEC_REF_CNT(target);
  }
  else {
    scm_fcd_free(SCM_STRING_BUFFER(target));
    scm_fcd_free(SCM_STRING_REF_CNT(target));
  }

  *SCM_STRING(target) = *SCM_STRING(src);
  SCM_STRING_INC_REF_CNT(target);
}

enum { DOWNCASE, UPCASE };

static ScmObj
scm_string_change_case(ScmObj str, int dir)
{
  ScmObj s = SCM_OBJ_INIT;
  ScmStrItr itr;

  SCM_REFSTK_INIT_REG(&str,
                      &s);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(dir == DOWNCASE || dir == UPCASE);

  s = scm_fcd_string_new(SCM_MEM_HEAP, NULL, 0, SCM_STRING_ENC(str));
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

    r = scm_string_push(s, &chr);
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

static ScmObj
scm_string_change_encoding(ScmObj src, ScmEncoding *to)
{
  ScmObj str = SCM_OBJ_INIT;
  char *in;
  char *out;
  iconv_t cd;
  size_t ins, outs, rslt;

  SCM_REFSTK_INIT_REG(&src,
                      &str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);
  scm_assert(to != NULL);

  str = scm_fcd_string_new(SCM_MEM_HEAP, NULL, 0, to);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  cd = iconv_open(scm_enc_name(to),
                  scm_enc_name(SCM_STRING_ENC(src)));
  if (cd == (iconv_t)-1) {
    scm_fcd_error("failed to call 'iconv_open'", 0);
    return SCM_OBJ_NULL;
  }

  in = (char *)SCM_STRING_HEAD(src);
  ins = SCM_STRING_BYTESIZE(src);
  out = (char *)SCM_STRING_HEAD(str);
  outs = SCM_STRING_CAPACITY(str);

  do {
    rslt = iconv(cd, &in, &ins, &out, &outs);
    if (rslt == (size_t)-1) {
      if (errno == EILSEQ) {
        scm_fcd_error("fiald to call 'iconv': illegal multibyte sequence", 0);
        goto err;
      }
      else if (errno == EINVAL) {
        scm_fcd_error("failed to call 'iconv': imcomplete multibyte sequence", 0);
        goto err;
      }
      else if (errno == E2BIG) {
        size_t cap = SCM_STRING_CAPACITY(str);
        uint8_t *p;
        ssize_t s;

        if (cap == SIZE_MAX) {
          scm_fcd_error("failed to encode string: too big string", 0);
          goto err;
        }

        cap =  (SIZE_MAX / 2 < cap) ? SIZE_MAX : cap * 2;
        p = scm_fcd_realloc(SCM_STRING_BUFFER(str), cap);
        if (p != NULL) goto err;

        s = out - (char *)SCM_STRING_HEAD(str);

        SCM_STRING_BUFFER(str) = p;
        SCM_STRING_HEAD(str) = p;
        SCM_STRING_CAPACITY(str) = cap;

        out = (char *)p + s;
        outs = cap - (size_t)s;
      }
      else {
        scm_fcd_error("failed to call 'iconv': unknown error has occurred", 0);
        goto err;
      }
    }
  } while (ins > 0);

  SCM_STRING_BYTESIZE(str) = (size_t)((uint8_t *)out - SCM_STRING_HEAD(str));
  SCM_STRING_LENGTH(str) = SCM_STRING_LENGTH(src);

  iconv_close(cd);
  return str;

 err:
  iconv_close(cd);
  return SCM_OBJ_NULL;
}

static int
scm_string_write_ext_rep_inner(ScmObj str, ScmObj port, size_t len)
{
  scm_char_t ary[len], *p;
  ScmEncoding *enc;
  int rslt, c;

  SCM_REFSTK_INIT_REG(&str, &port);

  p = scm_string_to_char_ary(str,0, (ssize_t)len, ary);
  if (p == NULL) return -1;

  enc = SCM_STRING_ENC(str);

  rslt = scm_fcd_write_cstr("\"", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  for (size_t i = 0; i < len; i++) {
    if (!scm_enc_ascii_p(enc, ary[i].bytes, sizeof(ary[i]))) {
      rslt = scm_fcd_write_cchr(ary[i], enc, port);
      if (rslt < 0) return -1;

      continue;
    }

    c = scm_enc_cnv_to_ascii(enc, &ary[i]);
    if (0x20 <= c && c <= 0x7f) {
      rslt = scm_fcd_write_cchr(ary[i], enc, port);
      if (rslt < 0) return -1;

      continue;
    }

    switch (c) {
    case '"':
      rslt = scm_fcd_write_cstr("\\\"", SCM_ENC_SRC, port);
      break;
    case '\\':
      rslt = scm_fcd_write_cstr("\\\\", SCM_ENC_SRC, port);
      break;
    case '\a':
      rslt = scm_fcd_write_cstr("\\a", SCM_ENC_SRC, port);
      break;
    case '\b':
      rslt = scm_fcd_write_cstr("\\b", SCM_ENC_SRC, port);
      break;
    case '\t':
      rslt = scm_fcd_write_cstr("\\t", SCM_ENC_SRC, port);
      break;
    case '\n':
      rslt = scm_fcd_write_cstr("\\n", SCM_ENC_SRC, port);
      break;
    case '\r':
      rslt = scm_fcd_write_cstr("\\r", SCM_ENC_SRC, port);
      break;
    default:
      rslt = scm_string_inline_hex_escape(ary[i], enc, port);
      break;
    }
    if (rslt < 0) return -1;
  }

  rslt = scm_fcd_write_cstr("\"", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_string_write_ext_rep(ScmObj obj, ScmObj port)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);

  return scm_string_write_ext_rep_inner(obj, port, SCM_STRING_LENGTH(obj));
}

static void
scm_string_finalize(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  if (SCM_STRING_REF_CNT(str) != NULL && *SCM_STRING_REF_CNT(str) > 1)
    SCM_STRING_DEC_REF_CNT(str);
  else {
    scm_fcd_free(SCM_STRING_BUFFER(str));
    scm_fcd_free(SCM_STRING_REF_CNT(str));
  }

  /* push() 関数や append() 関数内の tmp はこれらの関数から直接 finalize()
   * を call され、さらに GC 時にも gc_finalize() から call されるため、2 度
   * call されても問題ないようにする必要がある
   */
  SCM_STRING_BUFFER(str) = NULL;
  SCM_STRING_REF_CNT(str) = NULL;
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

  SCM_STRING_BUFFER(str) = scm_fcd_malloc(SCM_STRING_CAPACITY(str));
  SCM_STRING_HEAD(str) = SCM_STRING_BUFFER(str);
  if (SCM_STRING_BUFFER(str) == NULL) return -1;

  SCM_STRING_REF_CNT(str) =
    scm_fcd_malloc(sizeof(*SCM_STRING_REF_CNT(str)));
  if (SCM_STRING_REF_CNT(str) == NULL) return -1;

  *SCM_STRING_REF_CNT(str) = 1;

  if (src != NULL) {
    ssize_t len = scm_string_copy_bytes_with_check(SCM_STRING_BUFFER(str),
                                                   src, size, enc);
    if (len < 0) {
      scm_fcd_error("can not make string object: invalid byte sequence", 0);
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

ScmObj
scm_string_copy(ScmObj src)
{
  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  return scm_fcd_string_new(SCM_MEM_HEAP,
                            SCM_STRING_HEAD(src),
                            SCM_STRING_BYTESIZE(src),
                            SCM_STRING_ENC(src));
}

ScmObj
scm_string_dup(ScmObj src)
{
  ScmObj str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&src, &str);

  scm_assert_obj_type(src, &SCM_STRING_TYPE_INFO);

  str = scm_fcd_mem_alloc_heap(&SCM_STRING_TYPE_INFO, 0);

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
scm_string_length(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return SCM_STRING_LENGTH(str);
}

size_t
scm_string_bytesize(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return SCM_STRING_BYTESIZE(str);
}

bool
scm_string_is_equal(ScmObj str1, ScmObj str2)
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

ScmObj
scm_string_encode(ScmObj str, ScmEncoding *enc)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(enc != NULL);

  SCM_REFSTK_INIT_REG(&str);

  if (SCM_STRING_ENC(str) == enc)
    return scm_string_dup(str);

  return scm_string_change_encoding(str, enc);
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
    scm_fcd_error("can not make substring: argument is out of range", 0);
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
scm_string_push(ScmObj str, const scm_char_t *c)
{
  ScmObj tmp = SCM_OBJ_INIT;
  ssize_t width;

  SCM_REFSTK_INIT_REG(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  width = scm_enc_char_width(SCM_STRING_ENC(str), c, sizeof(*c));
  if (width < 0) {
    scm_fcd_error("failed to push a character into string: "
                  "invalid byte sequence", 0);
    return -1;
  }

  if ((*SCM_STRING_REF_CNT(str) > 1) || ROOM_FOR_APPEND(str) < (size_t)width) {
    if (SCM_STRING_BYTESIZE(str) > SSIZE_MAX - (size_t)width) {
      scm_fcd_error("failed to push a character into string: string too long",
                    0);
      return -1;
    }
    tmp = scm_string_copy_and_expand(str,
                                     SCM_STRING_BYTESIZE(str) + (size_t)width);
    if (scm_obj_null_p(tmp)) return -1;
    scm_string_replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str), c, (size_t)width);
  SCM_STRING_LENGTH(str) += 1;
  SCM_STRING_BYTESIZE(str) += (size_t)width;

  return 0;
}

int
scm_string_append(ScmObj str, ScmObj append)
{
  ScmObj tmp = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert_obj_type(append, &SCM_STRING_TYPE_INFO);
  scm_assert(SCM_STRING_ENC(str) == SCM_STRING_ENC(append));

  if ((*SCM_STRING_REF_CNT(str) > 1) ||
      ROOM_FOR_APPEND(str) < SCM_STRING_BYTESIZE(append)) {
    tmp = scm_string_copy_and_expand(str,
                                     SCM_STRING_BYTESIZE(str)
                                     + SCM_STRING_BYTESIZE(append));
    if (scm_obj_null_p(tmp)) return -1;
    scm_string_replace_contents(str, tmp);
    scm_string_finalize(tmp);
  }

  memcpy(SCM_STRING_HEAD(str) + SCM_STRING_BYTESIZE(str),
         SCM_STRING_HEAD(append), SCM_STRING_BYTESIZE(append));
  SCM_STRING_LENGTH(str) += SCM_STRING_LENGTH(append);
  SCM_STRING_BYTESIZE(str) += SCM_STRING_BYTESIZE(append);

  return 0;
}

int
scm_string_ref(ScmObj str, size_t pos, scm_char_t *chr)
{
  ScmStrItr iter;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);
  scm_assert(chr != NULL);

  if (pos >= SCM_STRING_LENGTH(str)) {
    scm_fcd_error("can not get a character in string: "
                   "argument is out of range", 0);
    return -1;
  }

  scm_enc_index2itr(SCM_STRING_ENC(str),
                    SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos, &iter);

  scm_assert(!scm_str_itr_err_p(&iter));
  scm_assert(!scm_str_itr_end_p(&iter));

  memset(chr->bytes, 0, sizeof(*chr));
  memcpy(chr->bytes, scm_str_itr_ptr(&iter), (size_t)scm_str_itr_width(&iter));

  return 0;
}

int
scm_string_set(ScmObj str, size_t pos, const scm_char_t *c)
{
  ScmObj front = SCM_OBJ_NULL, rear = SCM_OBJ_NULL, tmp = SCM_OBJ_NULL;
  ScmStrItr iter;
  int cw, iw;

  SCM_REFSTK_INIT_REG(&str, &front, &rear, &tmp);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(pos <= SSIZE_MAX);

  if (pos >= SCM_STRING_LENGTH(str)) {
    scm_fcd_error("can not update a character in string: "
                   "argument out of range", 0);
    return -1;
  }

  scm_enc_index2itr(SCM_STRING_ENC(str),
                    SCM_STRING_HEAD(str), SCM_STRING_BYTESIZE(str), pos, &iter);
  scm_assert(!scm_str_itr_err_p(&iter));

  cw = scm_enc_char_width(SCM_STRING_ENC(str), c, sizeof(*c));
  if (cw < 0) {
    scm_fcd_error("can not update a character in string: "
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
    tmp = scm_string_copy(str);

    if (scm_obj_null_p(tmp)) return -1;
    memcpy(SCM_STRING_HEAD(tmp) + offset, c, (size_t)cw);
    scm_string_replace_contents(str, tmp);

    return 0;
  }
  else {
    front = scm_string_substr(str, 0, pos);
    rear = scm_string_substr(str, pos + 1, SCM_STRING_LENGTH(str) - pos - 1);

    if (scm_obj_null_p(front) || scm_obj_null_p(rear))
      return -1;

    tmp = scm_string_copy_and_expand(front,
                                     SCM_STRING_BYTESIZE(front)
                                     + (size_t)cw + SCM_STRING_BYTESIZE(rear));
    if (scm_obj_null_p(tmp)) return SCM_OBJ_NULL;
    if (scm_string_push(tmp, c) < 0) return -1;
    if (scm_string_append(tmp, rear) < 0) return -1;

    scm_string_replace_contents(str, tmp);

    return 0;
  }
}

ScmObj
scm_string_downcase(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return scm_string_change_case(str, DOWNCASE);
}

ScmObj
scm_string_upcase(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  return scm_string_change_case(str, UPCASE);
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

ssize_t
scm_string_dump(ScmObj str, void *buf, size_t size)
{
  size_t len;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  len = (size < SCM_STRING_BYTESIZE(str)) ? size : SCM_STRING_BYTESIZE(str);
  memcpy(buf, SCM_STRING_HEAD(str), len); // XXX

  return (ssize_t)len;
}

ScmEncoding *
scm_string_encoding(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  return SCM_STRING_ENC(str);
}

void *
scm_string_content(ScmObj str)
{
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  return SCM_STRING_HEAD(str);
}

scm_char_t *
scm_string_to_char_ary(ScmObj str, size_t pos, ssize_t len, scm_char_t *ary)
{
  ScmStrItr iter;
  size_t l, i;

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  if (pos >= SCM_STRING_LENGTH(str)) {
    scm_fcd_error("failed to make array of characters: invalid argument", 0);
    return NULL;
  }

  if (len == 0)
    return ary;

  l = (SCM_STRING_LENGTH(str) - pos);
  if (len > 0 && (size_t)len < l)
    l = (size_t)len;

  if (ary == NULL) {
    ary = scm_fcd_malloc(sizeof(scm_char_t) * l);
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

int
scm_string_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_STRING_TYPE_INFO);

  if (ext_rep) {
    int r = scm_string_write_ext_rep(obj, port);
    if (r < 0) return -1;
  }
  else {
    int r = scm_fcd_write_string(obj, port, -1, -1);
    if (r < 0) return -1;
  }

  return 0;
}

void
scm_string_gc_initialize(ScmObj obj, ScmObj mem)
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
  return scm_fcd_write_cstr(cstr, SCM_ENC_SRC, port);
}
