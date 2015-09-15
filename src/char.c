#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/bedrock.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/number.h"
#include "scythe/exception.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  .name                = "char",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_char_obj_print,
  .obj_size            = sizeof(ScmChar),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};


static int
scm_char_write_ext_rep(ScmObj obj, ScmObj port)
{
  ScmEncoding *enc;
  scm_char_t chr;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  enc = SCM_CHAR_ENC(obj);
  chr = SCM_CHAR_VALUE(obj);

  if (scm_enc_printable_p(enc, chr.bytes, sizeof(chr))) {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), ' ')) {
      rslt = scm_write_cstr("#\\space", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else {
      rslt = scm_write_cstr("#\\", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = scm_write_char(obj, port);
      if (rslt < 0) return -1;
    }
  }
  else {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\a')) {
      rslt = scm_write_cstr("#\\alarm", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\b')) {
      rslt = scm_write_cstr("#\\backspaace", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x7f)) {
      rslt = scm_write_cstr("#\\delete", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x1b)) {
      rslt = scm_write_cstr("#\\escape", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\n')) {
      rslt = scm_write_cstr("#\\newline", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\0')) {
      rslt = scm_write_cstr("#\\null", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\r')) {
      rslt = scm_write_cstr("#\\return", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\t')) {
      rslt = scm_write_cstr("#\\tab", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else {
      char cstr[32];
      long long scalar = scm_enc_cnv_to_scalar(enc, chr.bytes, sizeof(chr));
      if (scalar < 0) return -1;
      snprintf(cstr, sizeof(cstr), "#\\x%llx", scalar);
      scm_write_cstr(cstr, SCM_ENC_SRC, port);
    }
  }

  return 0;
}

static ssize_t
scm_char_change_encode(scm_char_t *chr, size_t width,
                       const char *from, const char *to,
                       scm_char_t *out)
{
  ScmEncCnv cnv;
  size_t w;

  scm_assert(chr != NULL);
  scm_assert(from != NULL);
  scm_assert(to != NULL);
  scm_assert(out != NULL);

  scm_enc_cnv_init(&cnv, from, to, (char *)chr->bytes, width);
  if (scm_enc_cnv_err_p(&cnv)) {
    scm_error("failed to convert encoding: invalid encoding name", 0);
    return -1;
  }

  w = scm_enc_cnv_convert(&cnv, out->bytes, sizeof(*out), false);
  if (scm_enc_cnv_err_p(&cnv)) {
    if (scm_enc_cnv_illegal_p(&cnv))
      scm_error("failed to cconvert encoding: "
                    "illegal multibyte sequence", 0);
    else if (scm_enc_cnv_incomplete_p(&cnv))
      scm_error("failed to convert encoding: "
                    "incomplete  multibyte sequence", 0);
    else
      scm_error("failed to convert encoding: "
                    "unknown error has occurred", 0);

    goto err;
  }
  else if (scm_enc_cnv_insufficient_buf_p(&cnv)) {
    scm_error("failed to convert encoding: too big multibyte sequence", 0);
    goto err;
  }

  scm_enc_cnv_fin(&cnv);
  return (ssize_t)w;

 err:
  scm_enc_cnv_fin(&cnv);
  return -1;
}


ScmObj
scm_char_P(ScmObj obj)
{
  return scm_char_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_char_initialize(ScmObj chr, const scm_char_t *value, ScmEncoding *enc)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(value != NULL);
  scm_assert(enc != NULL);

  if (!scm_enc_valid_char_p(enc, value)) {
    scm_error("can not make character object: invalid byte sequence", 0);
    return -1;
  }

  SCM_CHAR_VALUE(chr) = *value;
  SCM_CHAR_ENC(chr) = enc;

  return 0;
}

void
scm_char_finalize(ScmObj chr)
{
  return;                       /* nothing to do */
}

ScmObj
scm_char_new(scm_mem_type_t mtype, const scm_char_t *value, ScmEncoding *enc)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  scm_assert(value != NULL);
  scm_assert(enc != NULL);

  chr = scm_alloc_mem(&SCM_CHAR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  if (scm_char_initialize(chr, value, enc) < 0)
    return SCM_OBJ_NULL;

  return chr;
}

ScmObj
scm_make_char(const scm_char_t *chr, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_system_encoding();

  if (!scm_enc_valid_char_p(enc, chr)) {
    scm_error("failed to make character object: invalid sequence", 0);
    return SCM_OBJ_NULL;
  }

  return scm_char_new(SCM_MEM_HEAP, chr, enc);
}

scm_char_t
scm_char_value(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_VALUE(chr);
}

long long
scm_char_scalar(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr),
                               SCM_CHAR_VALUE(chr).bytes,
                               sizeof(scm_char_t));
}

ScmEncoding *
scm_char_encoding(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  return SCM_CHAR_ENC(chr);
}

ScmObj
scm_char_encode(ScmObj chr, ScmEncoding *enc)
{
  scm_char_t c;
  ssize_t w;

  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(enc != NULL);

  if (SCM_CHAR(chr)->enc == enc)
    return scm_char_new(SCM_MEM_HEAP, &SCM_CHAR(chr)->value, enc);

  w = scm_enc_char_width(scm_char_encoding(chr), c.bytes, sizeof(c));
  scm_assert(w > 0);
  w = scm_char_change_encode(&SCM_CHAR(chr)->value, (size_t)w,
                             scm_enc_name(scm_char_encoding(chr)),
                             scm_enc_name(enc),
                             &c);
  if (w < 0) return SCM_OBJ_NULL;

  return scm_char_new(SCM_MEM_HEAP, &c, enc);
}

int
scm_char_cmp(ScmObj chr1, ScmObj chr2, int *rslt)
{
  long long v1, v2;

  scm_assert_obj_type(chr1, &SCM_CHAR_TYPE_INFO);
  scm_assert_obj_type(chr2, &SCM_CHAR_TYPE_INFO);
  scm_assert(SCM_CHAR_ENC(chr1) == SCM_CHAR_ENC(chr2));

  v1 = scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr1),
                             SCM_CHAR_VALUE(chr1).bytes, sizeof(scm_char_t));
  if (v1 < 0) {
    scm_error("can not get scalar value of character", 0);
    return -1;
  }

  v2 = scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr2),
                             SCM_CHAR_VALUE(chr2).bytes, sizeof(scm_char_t));
  if (v2 < 0) {
    scm_error("can not get scalar value of character", 0);
    return -1;
  }

  if (rslt != NULL) {
    if (v1 < v2)
      *rslt = -1;
    else if (v1 > v2)
      *rslt = 1;
    else
      *rslt = 0;
  }

  return 0;
}

static int
char_cmp_fold(ScmObj lst, int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt),
              bool *rslt)

{
  ScmObj chr = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &chr, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_pair_p(l); l = scm_cdr(l)) {
    chr = scm_car(l);
    if (!scm_char_p(chr)) {
      scm_error("failed to compare chracters: chracter required, but got",
                    1, chr);
      return -1;
    }

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, chr, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = chr;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

int
scm_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_char_p(chr1));
  scm_assert(scm_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_error("failed to compare characters: encoding mismatch",
                  2, chr1, chr2);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_char_eq_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_char_eq(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_char_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_char_p(chr1));
  scm_assert(scm_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_error("failed to compare characters: encoding mismatch",
                  2, chr1, chr2);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1) ? true : false;

  return 0;
}

ScmObj
scm_char_lt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_char_lt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_char_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_char_p(chr1));
  scm_assert(scm_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_error("failed to compare characters: encoding mismatch",
                  2, chr1, chr2);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_char_gt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_char_gt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_char_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_char_le(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_char_p(chr1));
  scm_assert(scm_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_error("failed to compare characters: encoding mismatch",
                  2, chr1, chr2);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1 || cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_char_le_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_char_le(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_char_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_char_p(chr1));
  scm_assert(scm_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_error("failed to compare characters: encoding mismatch",
                  2, chr1, chr2);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0 || cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_char_ge_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_char_ge(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_char_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_char_to_integer(ScmObj chr)
{
  long long scalar;

  scm_assert(scm_char_p(chr));

  scalar = scm_char_scalar(chr);

  /* TODO: エンコーディングが Unicode のものではない場合、scalar に 0x110000
   * を足す
   */

  if (scalar > SCM_SWORD_MAX) {
    scm_error("failed to convert from character to integer: overflow", 0);
    return SCM_OBJ_NULL;
  }

  return scm_make_number_from_sword((scm_sword_t)scalar);
}

ScmObj
scm_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  scm_sword_t scalar;
  scm_char_t c;
  ssize_t s;
  int r;

  scm_assert(scm_num_integer_p(num));

  if (enc == NULL)
    enc = scm_system_encoding();

  r = scm_integer_to_sword(num, &scalar);
  if (r < 0) return SCM_OBJ_NULL;

  /* TODO: エンコーディングが Unicode のものではない場合、scalar から 0x110000
   * を引く
   */

  s = scm_enc_cnv_from_scalar(enc, scalar, &c);
  /* scalar 値に相当する文字が無い場合、#f を返す。r7rs-draft-9 未定義 */
  if (s < 0) return SCM_FALSE_OBJ;

  return scm_char_new(SCM_MEM_HEAP, &c, enc);
}

ssize_t
scm_char_to_cchr(ScmObj chr, scm_char_t *cp)
{
  scm_char_t c;

  scm_assert(scm_char_p(chr));

  c = scm_char_value(chr);
  if (cp != NULL) *cp = c;
  return scm_enc_char_width(scm_char_encoding(chr), c.bytes, sizeof(c));
}

int
scm_char_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  if (kind == SCM_OBJ_PRINT_DISPLAY)
    rslt = scm_write_char(obj, port);
  else
    rslt = scm_char_write_ext_rep(obj, port);

  if (rslt < 0) return -1;

  return 0;
}
