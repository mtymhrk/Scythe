#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/char.h"

extern inline bool
scm_fcd_char_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_CHAR_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_fcd_char_P(ScmObj obj)
{
  return scm_fcd_char_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_new(SCM_MEM_TYPE_T mtype,
                 const scm_char_t *value, ScmEncoding *enc)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr);

  scm_assert(value != NULL);
  scm_assert(enc != NULL);

  chr = scm_fcd_mem_alloc(&SCM_CHAR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  if (scm_char_initialize(chr, value, enc) < 0)
    return SCM_OBJ_NULL;

  return chr;
}

ScmObj
scm_fcd_make_char(const scm_char_t *chr, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_fcd_system_encoding();

  if (!scm_enc_valid_char_p(enc, chr)) {
    scm_fcd_error("failed to make character object: invalid sequence", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_new(SCM_MEM_HEAP, chr, enc);
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
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    chr = scm_fcd_car(l);
    if (!scm_fcd_char_p(chr)) {
      scm_fcd_error("failed to compare chracters: chracter required, but got",
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
scm_fcd_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_char_p(chr1));
  scm_assert(scm_fcd_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_fcd_error("failed to compare characters: encoding mismatch",
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
scm_fcd_char_eq_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_char_eq(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_fcd_char_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_char_p(chr1));
  scm_assert(scm_fcd_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_fcd_error("failed to compare characters: encoding mismatch",
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
scm_fcd_char_lt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_char_lt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_fcd_char_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_char_p(chr1));
  scm_assert(scm_fcd_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_fcd_error("failed to compare characters: encoding mismatch",
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
scm_fcd_char_gt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_char_gt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_fcd_char_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_char_le(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_char_p(chr1));
  scm_assert(scm_fcd_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_fcd_error("failed to compare characters: encoding mismatch",
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
scm_fcd_char_le_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_char_le(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_fcd_char_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  scm_assert(scm_fcd_char_p(chr1));
  scm_assert(scm_fcd_char_p(chr2));

  if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_fcd_error("failed to compare characters: encoding mismatch",
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
scm_fcd_char_ge_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_char_ge(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = char_cmp_fold(lst, scm_fcd_char_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_char_to_integer(ScmObj chr)
{
  long long scalar;

  scm_assert(scm_fcd_char_p(chr));

  scalar = scm_char_scalar(chr);

  /* TODO: エンコーディングが Unicode のものではない場合、scalar に 0x110000
   * を足す
   */

  if (scalar > SCM_SWORD_MAX) {
    scm_fcd_error("failed to convert from character to integer: overflow", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_number_from_sword((scm_sword_t)scalar);
}

ScmObj
scm_fcd_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  scm_sword_t scalar;
  scm_char_t c;
  ssize_t s;
  int r;

  scm_assert(scm_fcd_integer_p(num));

  if (enc == NULL)
    enc = scm_fcd_system_encoding();

  r = scm_fcd_integer_to_sword(num, &scalar);
  if (r < 0) return SCM_OBJ_NULL;

  /* TODO: エンコーディングが Unicode のものではない場合、scalar から 0x110000
   * を引く
   */

  s = scm_enc_cnv_from_scalar(enc, scalar, &c);
  /* scalar 値に相当する文字が無い場合、#f を返す。r7rs-draft-9 未定義 */
  if (s < 0) return SCM_FALSE_OBJ;

  return scm_fcd_char_new(SCM_MEM_HEAP, &c, enc);
}


/* TODO: char_to_cchr、char_encoding はインタフェースの見直しが必要
 */

ssize_t
scm_fcd_char_to_cchr(ScmObj chr, scm_char_t *cp)
{
  scm_char_t c;

  scm_assert(scm_fcd_char_p(chr));

  c = scm_char_value(chr);
  if (cp != NULL) *cp = c;
  return scm_enc_char_width(scm_char_encoding(chr), c.bytes, sizeof(c));
}

ScmEncoding *
scm_fcd_char_encoding(ScmObj chr)
{
  scm_assert(scm_fcd_char_p(chr));
  return scm_char_encoding(chr);
}

