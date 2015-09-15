#include <sys/types.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/bedrock.h"
#include "scythe/refstk.h"
#include "scythe/string.h"
#include "scythe/exception.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/string.h"

static int
format_mod(ScmObj port, scm_char_t chr, ScmEncoding *enc, ScmObj obj)
{
  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(enc != NULL);

  if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 'a')) {
    int r;

    if (scm_obj_null_p(obj)) {
      scm_error("format: too few arguments", 0);
      return -1;
    }

    r = scm_display(obj, port);
    if (r < 0) return -1;

    return 1;
  }
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 's')) {
    int r;

    if (scm_obj_null_p(obj)) {
      scm_error("format: too few arugmnets", 0);
      return -1;
    }

    r = scm_write(obj, port);
    if (r < 0) return -1;

    return 1;
  }
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '%')) {
    int r = scm_newline(port);
    if (r < 0) return -1;

    return 0;
  }
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '~')) {
    int r = scm_write_cchr(chr, enc, port);
    if (r < 0) return -1;

    return 0;
  }
  else {
    scm_error("format: unknown specifier", 0);
    return -1;
  }
}

static int
format_aux(ScmObj port, bool *escaped,
           scm_char_t chr, ScmEncoding *enc, ScmObj obj)
{
  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(escaped != NULL);
  scm_assert(enc != NULL);

  if (*escaped) {
    int r = format_mod(port, chr, enc, obj);
    *escaped = false;
    return r;
  }
  else {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '~')) {
      *escaped = true;
      return 0;
    }
    else {
      return scm_write_cchr(chr, enc, port);
    }
  }
}

static int
pformat_lst_aux(ScmObj port, ScmObj fmt, size_t len, ScmObj lst)
{
  ScmObj o = SCM_OBJ_INIT;
  scm_char_t chr[len], *p;
  ScmEncoding *enc;
  bool escaped;

  SCM_REFSTK_INIT_REG(&port, &fmt, &lst,
                      &o);

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(fmt));

  p = scm_string_to_cchr_ary(fmt, 0, (ssize_t)len, chr);
  if (p == NULL) return -1;

  enc = scm_string_encoding(fmt);
  if (enc == NULL) return -1;

  escaped = false;

  o = SCM_OBJ_NULL;
  if (scm_pair_p(lst)) {
    o = scm_car(lst);
    lst = scm_cdr(lst);
  }

  for (size_t chr_idx = 0; chr_idx < len; chr_idx++) {
    int r = format_aux(port, &escaped, chr[chr_idx], enc, o);
    if (r < 0) return -1;

    if (r > 0) {
      o = SCM_OBJ_NULL;
      if (scm_pair_p(lst)) {
        o = scm_car(lst);
        lst = scm_cdr(lst);
      }
    }
  }

  return 0;
}

int
scm_pformat_lst(ScmObj port, ScmObj fmt, ScmObj lst)
{
  size_t len;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(fmt));

  len = scm_string_length(fmt);
  return pformat_lst_aux(port, fmt, len, lst);
}

ScmObj
scm_format_lst(ScmObj fmt, ScmObj lst)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmEncoding *fenc, *senc;
  int r;

  SCM_REFSTK_INIT_REG(&fmt, &lst,
                      &port, &str);

  scm_assert(scm_string_p(fmt));

  port = scm_open_output_string();
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  r = scm_pformat_lst(port, fmt, lst);
  if (r < 0) return SCM_OBJ_NULL;

  str = scm_get_output_string(port);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  fenc = scm_string_encoding(fmt);
  if (fenc == NULL) return SCM_OBJ_NULL;

  senc = scm_string_encoding(str);
  if (senc == NULL) return SCM_OBJ_NULL;

  return ((fenc == senc) ? str : scm_string_encode(str, fenc));
}

static int
pformat_cv_aux(ScmObj port,
               ScmObj fmt, size_t len, ScmObj *obj, size_t n)
{
  ScmObj o = SCM_OBJ_INIT;
  scm_char_t chr[len], *p;
  ScmEncoding *enc;
  bool escaped;
  size_t obj_idx;

  SCM_REFSTK_INIT_REG(&port, &fmt,
                      &o);

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(fmt));
  scm_assert(obj != NULL);

  p = scm_string_to_cchr_ary(fmt, 0, (ssize_t)len, chr);
  if (p == NULL) return -1;

  enc = scm_string_encoding(fmt);
  if (enc == NULL) return -1;

  escaped = false;
  obj_idx = 0;
  o = (n == 0) ? SCM_OBJ_NULL : obj[obj_idx++];

  for (size_t chr_idx = 0; chr_idx < len; chr_idx++) {
    int r = format_aux(port, &escaped, chr[chr_idx], enc, o);
    if (r < 0) return -1;

    if (r > 0) {
      o = SCM_OBJ_NULL;
      if (obj_idx < n) {
        if (scm_obj_null_p(obj[obj_idx])) {
          scm_error("format: invalid argument", 0);
          return -1;
        }
        o = obj[obj_idx++];
      }
    }
  }

  return 0;
}

int
scm_pformat_cv(ScmObj port, ScmObj fmt, ScmObj *obj, size_t n)
{
  size_t len;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(fmt));
  scm_assert(n == 0 || obj != NULL);

  len = scm_string_length(fmt);
  return pformat_cv_aux(port, fmt, len, obj, n);
}

ScmObj
scm_format_cv(ScmObj fmt, ScmObj *obj, size_t n)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmEncoding *fenc, *senc;
  int r;

  SCM_REFSTK_INIT_REG(&fmt,
                      &port, &str);

  scm_assert(scm_string_p(fmt));
  scm_assert(n == 0 || obj != NULL);

  port = scm_open_output_string();
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  r = scm_pformat_cv(port, fmt, obj, n);
  if (r < 0) return SCM_OBJ_NULL;

  str = scm_get_output_string(port);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  fenc = scm_string_encoding(fmt);
  if (fenc == NULL) return SCM_OBJ_NULL;

  senc = scm_string_encoding(str);
  if (senc == NULL) return SCM_OBJ_NULL;

  return ((fenc == senc) ? str : scm_string_encode(str, fenc));
}

static ScmObj
pformat_aux_inner(ScmObj port, ScmObj fmt, const char *cfmt,
                  va_list arg, size_t n)
{
  ScmObj obj[n];

  SCM_REFSTK_INIT_REG(&port, &fmt);

  for (size_t i = 0; i < n; i++)
    obj[i] = va_arg(arg, ScmObj);

  SCM_REFSTK_REG_ARY(obj, n);

  if (scm_obj_null_p(fmt)) {
    fmt = scm_make_string_from_cstr(cfmt, SCM_ENC_SRC);
    if (scm_obj_null_p(fmt)) return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    int r = scm_pformat_cv(port, fmt, obj, n);
    return (r < 0) ? SCM_OBJ_NULL : SCM_UNDEF_OBJ;
  }
  else {
    return scm_format_cv(fmt, obj, n);
  }
}

static ScmObj
pformat_aux(ScmObj port, ScmObj fmt, const char *cfmt, va_list arg)
{
  ScmObj obj = SCM_OBJ_INIT, ret = SCM_OBJ_INIT;
  va_list copy;
  size_t n;

  va_copy(copy, arg);
  n = 0;
  for (obj = va_arg(arg, ScmObj);
       scm_obj_not_null_p(obj);
       obj = va_arg(arg, ScmObj))
    n++;

  ret = pformat_aux_inner(port, fmt, cfmt, copy, n);
  va_end(copy);

  return ret;
}

int
scm_pformat_va(ScmObj port, ScmObj fmt, va_list arg)
{
  ScmObj r = SCM_OBJ_INIT;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(fmt));

  r = pformat_aux(port, fmt, NULL, arg);
  return (scm_obj_null_p(r) ? -1 : 0);
}

int
scm_pformat(ScmObj port, ScmObj fmt, ...)
{
  va_list arg;
  int ret;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));
  scm_assert(scm_string_p(port));

  va_start(arg, fmt);
  ret = scm_pformat_va(port, fmt, arg);
  va_end(arg);

  return ret;
}

ScmObj
scm_format_va(ScmObj fmt, va_list arg)
{
  scm_assert(scm_string_p(fmt));
  return pformat_aux(SCM_OBJ_NULL, fmt, NULL, arg);
}

ScmObj
scm_format(ScmObj fmt, ...)
{
  ScmObj ret = SCM_OBJ_INIT;
  va_list arg;

  scm_assert(scm_string_p(fmt));

  va_start(arg, fmt);
  ret = scm_format_va(fmt, arg);
  va_end(arg);

  return ret;
}

int
scm_pformat_cstr_va(ScmObj port, const char *fmt, va_list arg)
{
  ScmObj r = SCM_OBJ_INIT;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));

  r = pformat_aux(port, SCM_OBJ_NULL, fmt, arg);
  return (scm_obj_null_p(r) ? -1 : 0);
}

int
scm_pformat_cstr(ScmObj port, const char *fmt, ...)
{
  va_list arg;
  int r;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_textual_port_p(port));

  va_start(arg, fmt);
  r = scm_pformat_cstr_va(port, fmt, arg);
  va_end(arg);

  return r;
}

ScmObj
scm_format_cstr_va(const char *fmt, va_list arg)
{
  return pformat_aux(SCM_OBJ_NULL, SCM_OBJ_NULL, fmt, arg);
}

ScmObj
scm_format_cstr(const char *fmt, ...)
{
  ScmObj ret = SCM_OBJ_INIT;
  va_list arg;

  va_start(arg, fmt);
  ret = scm_format_cstr_va(fmt, arg);
  va_end(arg);

  return ret;
}
