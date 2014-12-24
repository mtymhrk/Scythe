#include <stdbool.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/string.h"
#include "scythe/char.h"

extern inline bool
scm_fcd_string_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_STRING_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_fcd_string_P(ScmObj obj)
{
  return scm_fcd_string_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_make_string_from_cstr(const char *str, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_fcd_system_encoding();

  if (str == NULL) {
    return scm_string_new(SCM_MEM_HEAP, "", 0, enc);
  }
  else {
    size_t sz = strlen(str);
    if (sz > SSIZE_MAX) {
      scm_fcd_error("failed to make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_HEAP, str, sz, enc);
  }
}


ScmObj
scm_fcd_make_string_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_fcd_system_encoding();

  if (data == NULL) {
    return scm_string_new(SCM_MEM_HEAP, "", 0, enc);
  }
  else {
    if (size > SSIZE_MAX) {
      scm_fcd_error("failed to make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_HEAP, data, size, enc);
  }
}

ScmObj
scm_fcd_string_lst(ScmObj lst)
{
  return scm_fcd_list_to_string(lst);
}

ScmObj
scm_fcd_string_cv(const ScmObj *chr, size_t n)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&str);

  if (chr == NULL || n == 0)
    return scm_string_new(SCM_MEM_HEAP, NULL, 0, scm_fcd_system_encoding());

  enc = 0;
  for (size_t i = 0; i < n; i++) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    if (!scm_fcd_char_p(chr[i])) {
      scm_fcd_error("failed to make string: required character, but got",
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
      scm_fcd_error("failed to make string string: encoding mismatch",
                    1, chr[i]);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr[i]);

    r = scm_string_push(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

size_t
scm_fcd_string(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_fcd_string_cv(args, n);
}

size_t
scm_fcd_string_length(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_string_length(str);
}

size_t
scm_fcd_string_bytesize(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_string_bytesize(str);
}

ScmObj
scm_fcd_string_ref(ScmObj str, size_t pos)
{
  scm_char_t c;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&str);

  scm_assert(scm_fcd_string_p(str));
  scm_assert(pos < scm_string_length(str));

  enc = scm_string_encoding(str);
  scm_string_ref(str, pos, &c);
  return scm_fcd_make_char(&c, enc);
}

int
scm_fcd_string_set_i(ScmObj str, size_t pos, ScmObj chr)
{
  scm_char_t c;

  scm_assert(scm_fcd_string_p(str));
  scm_assert(pos < scm_string_length(str));
  scm_assert(scm_fcd_char_p(chr));

  if (scm_string_encoding(str) != scm_char_encoding(chr)) {
    scm_fcd_error("failed to store a character in string: encoding mismatch",
                  2, str, chr);
    return -1;
  }

  c = scm_char_value(chr);
  return scm_string_set(str, pos, &c);
}

static inline int
compare_aux(ScmObj s1, ScmObj s2, bool (*filter)(int cmp), bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_string_p(s1));
  scm_assert(scm_fcd_string_p(s2));

  if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_fcd_error("failed to compare strings: encoding mismatch", 2, s1, s2);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = filter(cmp);

  return 0;
}

static inline bool
filter_for_eq(int cmp)
{
  return (cmp == 0) ? true : false;
}

int
scm_fcd_string_eq(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_eq, rslt);
}

ScmObj
scm_fcd_string_eq_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_string_eq(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static int
string_cmp_fold(ScmObj lst, int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt),
                bool *rslt)

{
  ScmObj str = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    str = scm_fcd_car(l);
    if (!scm_fcd_string_p(str)) {
      scm_fcd_error("failed to compare strings: string required, but got",
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
scm_fcd_string_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_fcd_error("string=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  r = string_cmp_fold(lst, scm_fcd_string_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_lt(int cmp)
{
  return (cmp == -1) ? true : false;
}

int
scm_fcd_string_lt(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_lt, rslt);
}

ScmObj
scm_fcd_string_lt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_string_lt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_string_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = string_cmp_fold(lst, scm_fcd_string_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_gt(int cmp)
{
  return (cmp == 1) ? true : false;
}

int
scm_fcd_string_gt(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_gt, rslt);
}

ScmObj
scm_fcd_string_gt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_string_gt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_string_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = string_cmp_fold(lst, scm_fcd_string_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_le(int cmp)
{
  return(cmp == -1 || cmp == 0) ? true : false;
}

int
scm_fcd_string_le(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_le, rslt);
}

ScmObj
scm_fcd_string_le_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_string_le(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_string_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = string_cmp_fold(lst, scm_fcd_string_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static inline bool
filter_for_ge(int cmp)
{
  return (cmp == 0 || cmp == 1) ? true : false;
}

int
scm_fcd_string_ge(ScmObj s1, ScmObj s2, bool *rslt)
{
  return compare_aux(s1, s2, filter_for_ge, rslt);
}

ScmObj
scm_fcd_string_ge_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_string_ge(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_string_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = string_cmp_fold(lst, scm_fcd_string_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_string_upcase(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_string_upcase(str);
}

ScmObj
scm_fcd_string_downcase(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_string_downcase(str);
}

ScmObj
scm_fcd_substring(ScmObj str, size_t start, size_t end)
{
  scm_assert(scm_fcd_string_p(str));
  scm_assert(start <= SSIZE_MAX);
  scm_assert(end <= SSIZE_MAX);
  scm_assert(start <= end);
  scm_assert(end <= scm_string_length(str));
  return scm_string_substr(str, start, end - start);
}

ScmObj
scm_fcd_string_append_lst(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, l = SCM_OBJ_INIT, s = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &l, &s);

  if (scm_obj_null_p(lst))
    return scm_fcd_make_string_from_bin(NULL, 0, scm_fcd_system_encoding());

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    ScmEncoding *e;
    int r;

    s = scm_fcd_car(l);
    if (!scm_fcd_string_p(s)) {
      scm_fcd_error("failed to append strings: string required, but got", 1, s);
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
        scm_fcd_error("failed to append strings: encoding mismatch", 2, str, s);
        return SCM_OBJ_NULL;
      }

      r = scm_string_append(str, s);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(str))
    return scm_fcd_make_string_from_bin(NULL, 0, scm_fcd_system_encoding());
  else
    return str;
}

ScmObj
scm_fcd_string_append_cv(ScmObj *ary, size_t n)
{
  ScmObj str = SCM_OBJ_NULL;
  ScmEncoding *enc, *e;

  SCM_REFSTK_INIT_REG(&str);

  if (ary == NULL || n == 0)
    return scm_fcd_make_string_from_bin(NULL, 0, scm_fcd_system_encoding());

  str = scm_fcd_string_copy(ary[0], -1, -1);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 1; i < n; i++) {
    int r;

    if (!scm_fcd_string_p(ary[i])) {
      scm_fcd_error("failed to append strings: sring required, but got",
                    1, ary[i]);
      return SCM_OBJ_NULL;
    }

    e = scm_string_encoding(ary[i]);
    if (enc != e) {
      scm_fcd_error("failed to append strings: encoding mismatch",
                    2, str, ary[i]);
      return SCM_OBJ_NULL;
    }

    r = scm_string_append(str, ary[i]);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

ScmObj
scm_fcd_string_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_fcd_string_append_cv(args, n);
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

  p = scm_string_to_char_ary(str, pos, (ssize_t)len, c_ary);
  if (p == NULL) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 0; i < len; i++) {
    o_ary[i] = scm_char_new(SCM_MEM_HEAP, c_ary + i, enc);

    if (scm_obj_null_p(o_ary[i])) return SCM_OBJ_NULL;
  }

  return scm_fcd_list_cv(o_ary, len);
}

ScmObj
scm_fcd_string_to_list(ScmObj str, ssize_t start, ssize_t end)
{
  size_t len;

  scm_assert(scm_fcd_string_p(str));
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
scm_fcd_list_to_string(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, l = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&lst,
                      &str, &chr, &l);

  if (scm_obj_null_p(lst))
    return scm_fcd_make_string_from_bin(NULL, 0, scm_fcd_system_encoding());

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    chr = scm_fcd_car(l);
    if (!scm_fcd_char_p(chr)) {
      scm_fcd_error("failed to convert from list to string: "
                    "character require, but got", 1, chr);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      enc = scm_char_encoding(chr);
      str = scm_fcd_make_string_from_bin(NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr);

    if (e != enc) {
      scm_fcd_error("failed to convert from list to string: encoding mismatch",
                    2, str, chr);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr);

    r = scm_string_push(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(str))
    return scm_fcd_make_string_from_bin(NULL, 0, scm_fcd_system_encoding());
  else
    return str;
}

ScmObj
scm_fcd_string_copy(ScmObj str, ssize_t start, ssize_t end)
{
  scm_assert(scm_fcd_string_p(str));
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

  p = scm_string_to_char_ary(from, pos, (ssize_t)len, ary);
  if (p == NULL) return -1;

  for (size_t i = 0; i < len; i++) {
    int r = scm_string_set(to, at + i, ary + i);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_fcd_string_copy_i(ScmObj to, size_t at,
                      ScmObj from, ssize_t start, ssize_t end)
{
  size_t to_len, from_len, sub_len, len;

  SCM_REFSTK_INIT_REG(&to, &from);

  scm_assert(scm_fcd_string_p(to));
  scm_assert(at < scm_string_length(to));
  scm_assert(scm_fcd_string_p(from));
  scm_assert(start < 0 || (size_t)start < scm_string_length(from));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(from));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (scm_string_encoding(to) != scm_string_encoding(from)) {
    scm_fcd_error("failed to copy string: encoding mismatch", 1, to, from);
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
      scm_fcd_error("failed to copy string: out of range", 0);
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
scm_fcd_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end)
{
  scm_char_t c;

  scm_assert(scm_fcd_string_p(str));
  scm_assert(scm_fcd_char_p(fill));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (scm_string_encoding(str) != scm_char_encoding(fill)) {
    scm_fcd_error("failed to fill string with character: encoding mismatch",
                  2, str, fill);
    return -1;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  c = scm_char_value(fill);

  for (ssize_t i = start; i < end; i++) {
    int r = scm_string_set(str, (size_t)i, &c);
    if (r < 0) return -1;
  }

  return 0;
}

/* TODO: string_encoding, string_to_cstr, string_push はインタフェースの見直し
 *      が必要
 */

ScmEncoding *
scm_fcd_string_encoding(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_string_encoding(str);
}

char *
scm_fcd_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  size_t n;

  scm_assert(scm_fcd_string_p(str));

  n = scm_string_bytesize(str);
  scm_assert(n <= SSIZE_MAX);

  if (cstr == NULL) {
    size = n + 1;
    cstr = scm_fcd_malloc(size);
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

int
scm_fcd_string_push(ScmObj str, scm_char_t chr, ScmEncoding *enc)
{
  ScmEncoding *s_enc;

  scm_assert(scm_fcd_string_p(str));

  s_enc = scm_string_encoding(str);
  if (s_enc != enc) {
    scm_fcd_error("failed to append a character to string: encoding mismatch",
                  0);
    return -1;
  }

  return scm_string_push(str, &chr);
}
