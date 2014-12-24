#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/number_common.h"
#include "scythe/fixnum.h"
#include "scythe/bignum.h"
#include "scythe/number_parser.h"

extern inline bool
scm_fcd_fixnum_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_FIXNUM_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_fixnum_P(ScmObj obj)
{
  return scm_fcd_fixnum_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_bignum_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BIGNUM_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_bignum_P(ScmObj obj)
{
  return scm_fcd_bignum_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_number_p(ScmObj obj)
{
  return scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_NUM);
}

extern inline ScmObj
scm_fcd_number_P(ScmObj obj)
{
  return scm_fcd_number_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_complex_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, complex_p);
}

extern inline ScmObj
scm_fcd_complex_P(ScmObj obj)
{
  return scm_fcd_complex_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_real_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, real_p);
}

extern inline ScmObj
scm_fcd_real_P(ScmObj obj)
{
  return scm_fcd_real_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_rational_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, rational_p);
}

extern inline ScmObj
scm_fcd_rational_P(ScmObj obj)
{
  return scm_fcd_rational_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_integer_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, integer_p);
}

extern inline ScmObj
scm_fcd_integer_P(ScmObj obj)
{
  return scm_fcd_integer_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_exact_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, exact_p);
}

extern inline ScmObj
scm_fcd_exact_P(ScmObj obj)
{
  return scm_fcd_exact_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_inexact_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, inexact_p);
}

extern inline ScmObj
scm_fcd_inexact_P(ScmObj obj)
{
  return scm_fcd_inexact_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_exact_integer_p(ScmObj obj)
{
  if (scm_fcd_integer_p(obj) && scm_fcd_exact_p(obj))
    return true;
  else
    return false;
}

extern inline ScmObj
scm_fcd_exact_integer_P(ScmObj obj)
{
  return scm_fcd_exact_integer_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_finite_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, finite_p);
}

extern inline ScmObj
scm_fcd_finite_P(ScmObj obj)
{
  return scm_fcd_finite_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_infinite_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, infinite_p);
}

extern inline ScmObj
scm_fcd_infinite_P(ScmObj obj)
{
  return scm_fcd_infinite_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_nan_p(ScmObj obj)
{
  if (!scm_fcd_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, nan_p);
}

extern inline ScmObj
scm_fcd_nan_P(ScmObj obj)
{
  return scm_fcd_nan_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_make_number_from_literal(const void *literal, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_fcd_system_encoding();

  return scm_num_make_from_literal(literal, enc);
}

ScmObj
scm_fcd_make_number_from_sword(scm_sword_t num)
{
  if (num < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < num)
    return scm_bignum_new_from_sword(SCM_MEM_HEAP, num);
  else
    return scm_fixnum_new(num);
}

ScmObj
scm_fcd_make_number_from_size_t(size_t num)
{
  if (num > SCM_FIXNUM_MAX)
    return scm_bignum_new_from_uword(SCM_MEM_HEAP, num);
  else
    return scm_fixnum_new((scm_sword_t)num);
}

static int
num_cmp_fold(ScmObj lst, int (*cmp)(ScmObj n1, ScmObj n2, bool *rslt),
             bool *rslt)

{
  ScmObj num = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &num, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    num = scm_fcd_car(l);

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, num, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = num;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

int
scm_fcd_num_eq(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_fcd_num_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = num_cmp_fold(lst, scm_fcd_num_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_num_eq_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_num_eq(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_num_lt(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1) ? true : false;

  return 0;
}

ScmObj
scm_fcd_num_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = num_cmp_fold(lst, scm_fcd_num_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_num_lt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_num_lt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_num_gt(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_fcd_num_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = num_cmp_fold(lst, scm_fcd_num_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_num_gt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  rslt = scm_fcd_num_gt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_num_le(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1 || cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_fcd_num_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = num_cmp_fold(lst, scm_fcd_num_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_num_le_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_num_le(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_fcd_num_ge(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0 || cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_fcd_num_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = num_cmp_fold(lst, scm_fcd_num_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_num_ge_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_fcd_num_ge(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_zero_p(ScmObj num)
{
  if (!scm_fcd_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, zero_p);
}

ScmObj
scm_fcd_zero_P(ScmObj num)
{
  scm_assert(scm_fcd_number_P(num));
  return scm_fcd_zero_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_positive_p(ScmObj num)
{
  if (!scm_fcd_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, positive_p);
}

ScmObj
scm_fcd_positive_P(ScmObj num)
{
  scm_assert(scm_fcd_number_p(num));
  return scm_fcd_positive_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_negative_p(ScmObj num)
{
  if (!scm_fcd_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, negative_p);
}

ScmObj
scm_fcd_negative_P(ScmObj num)
{
  scm_assert(scm_fcd_number_p(num));
  return scm_fcd_negative_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_odd_p(ScmObj num)
{
  if (!scm_fcd_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, odd_p);
}

ScmObj
scm_fcd_odd_P(ScmObj num)
{
  scm_assert(scm_fcd_number_p(num));
  return scm_fcd_odd_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_even_p(ScmObj num)
{
  if (!scm_fcd_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, even_p);
}

ScmObj
scm_fcd_even_P(ScmObj num)
{
  scm_assert(scm_fcd_number_p(num));
  return scm_fcd_even_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static ScmObj
num_bop_fold(ScmObj lst, ScmObj (*func)(ScmObj n1, ScmObj n2))
{
  ScmObj rslt = SCM_OBJ_INIT, num = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &rslt, &num, &l);

  scm_assert(scm_fcd_pair_p(lst));
  scm_assert(func != NULL);

  rslt = scm_fcd_car(lst);
  if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;

  for (l = scm_fcd_cdr(lst); scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    num = scm_fcd_car(l);
    if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

    rslt = func(rslt, num);
    if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return rslt;
}

ScmObj
scm_fcd_max(ScmObj n1, ScmObj n2)
{
  bool n1_ie, n2_ie;
  int cmp, err;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  n1_ie = SCM_NUM_CALL_FUNC(n1, inexact_p);
  n2_ie = SCM_NUM_CALL_FUNC(n2, inexact_p);

  if (n1_ie && !n2_ie) {
    n2 = scm_fcd_inexact(n2);
    scm_assert(scm_obj_not_null_p(n2));
  }
  else if (!n1_ie && n2_ie) {
    n1 = scm_fcd_inexact(n1);
    scm_assert(scm_obj_not_null_p(n1));
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return SCM_OBJ_NULL;

  return (cmp == 0 || cmp == 1) ? n1 : n2;
}

ScmObj
scm_fcd_max_lst(ScmObj lst)
{
  scm_assert(scm_fcd_pair_p(lst));
  return num_bop_fold(lst, scm_fcd_max);
}

ScmObj
scm_fcd_min(ScmObj n1, ScmObj n2)
{
  bool n1_ie, n2_ie;
  int cmp, err;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  scm_assert(scm_fcd_number_p(n1));
  scm_assert(scm_fcd_number_p(n2));

  n1_ie = SCM_NUM_CALL_FUNC(n1, inexact_p);
  n2_ie = SCM_NUM_CALL_FUNC(n2, inexact_p);

  if (n1_ie && !n2_ie) {
    n2 = scm_fcd_inexact(n2);
    scm_assert(scm_obj_not_null_p(n2));
  }
  else if (!n1_ie && n2_ie) {
    n1 = scm_fcd_inexact(n1);
    scm_assert(scm_obj_not_null_p(n1));
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return SCM_OBJ_NULL;

  return (cmp == 0 || cmp == -1) ? n1 : n2;
}

ScmObj
scm_fcd_min_lst(ScmObj lst)
{
  scm_assert(scm_fcd_pair_p(lst));
  return num_bop_fold(lst, scm_fcd_min);
}

ScmObj
scm_fcd_plus(ScmObj x, ScmObj y)
{
  scm_assert(scm_fcd_number_p(x));
  scm_assert(scm_fcd_number_p(y));
  return SCM_NUM_CALL_FUNC(x, plus, y);
}

ScmObj
scm_fcd_plus_lst(ScmObj lst)
{
  SCM_REFSTK_INIT_REG(&lst);

  scm_assert(scm_fcd_nil_p(lst) || scm_fcd_pair_p(lst));

  if (scm_fcd_nil_p(lst))
    return SCM_FIXNUM_ZERO;
  else
    return num_bop_fold(lst, scm_fcd_plus);
}

ScmObj
scm_fcd_mul(ScmObj x, ScmObj y)
{
  scm_assert(scm_fcd_number_p(x));
  scm_assert(scm_fcd_number_p(y));
  return SCM_NUM_CALL_FUNC(x, mul, y);
}

ScmObj
scm_fcd_mul_lst(ScmObj lst)
{
  SCM_REFSTK_INIT_REG(&lst);

  scm_assert(scm_fcd_nil_p(lst) || scm_fcd_pair_p(lst));

  if (scm_fcd_nil_p(lst))
    return SCM_FIXNUM_ONE;
  else
    return num_bop_fold(lst, scm_fcd_mul);
}

ScmObj
scm_fcd_minus(ScmObj x, ScmObj y)
{
  scm_assert(scm_fcd_number_p(x));
  scm_assert(scm_fcd_number_p(y));
  return SCM_NUM_CALL_FUNC(x, minus, y);
}

ScmObj
scm_fcd_minus_lst(ScmObj lst)
{
  ScmObj a = SCM_OBJ_INIT, d = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &a, &d);

  scm_assert(scm_fcd_pair_p(lst));

  a = scm_fcd_car(lst);
  if (scm_obj_null_p(a)) return SCM_OBJ_NULL;

  d = scm_fcd_cdr(lst);
  if (scm_obj_null_p(d)) return SCM_OBJ_NULL;

  if (!scm_fcd_pair_p(d))
    return SCM_NUM_CALL_FUNC(a, invert_sign);
  else
    return num_bop_fold(lst, scm_fcd_minus);
}

ScmObj
scm_fcd_abs(ScmObj num)
{
  scm_assert(scm_fcd_number_p(num));
  if (SCM_NUM_CALL_FUNC(num, positive_p))
    return SCM_NUM_CALL_FUNC(num, copy);
  else
    return SCM_NUM_CALL_FUNC(num, invert_sign);
}

int
scm_fcd_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  scm_assert(scm_fcd_integer_p(x));
  scm_assert(scm_fcd_integer_p(y));
  return SCM_NUM_CALL_FUNC(x, floor_div, y, q, r);
}

ScmObj
scm_fcd_floor_quo(ScmObj x, ScmObj y)
{
  ScmObj q = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&x, &y, &q);

  scm_assert(scm_fcd_integer_p(x));
  scm_assert(scm_fcd_integer_p(y));

  rslt = SCM_NUM_CALL_FUNC(x, floor_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_fcd_floor_rem(ScmObj x, ScmObj y)
{
  ScmObj r = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&x, &y, &r);

  scm_assert(scm_fcd_integer_p(x));
  scm_assert(scm_fcd_integer_p(y));

  rslt = SCM_NUM_CALL_FUNC(x, floor_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}

int
scm_fcd_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  scm_assert(scm_fcd_integer_p(x));
  scm_assert(scm_fcd_integer_p(y));
  return SCM_NUM_CALL_FUNC(x, truncate_div, y, q, r);
}

ScmObj
scm_fcd_truncate_quo(ScmObj x, ScmObj y)
{
  ScmObj q = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&x, &y, &q);

  scm_assert(scm_fcd_number_p(x));
  scm_assert(scm_fcd_number_p(y));

  rslt = SCM_NUM_CALL_FUNC(x, truncate_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_fcd_truncate_rem(ScmObj x, ScmObj y)
{
  ScmObj r = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&x, &y, &r);

  scm_assert(scm_fcd_number_p(x));
  scm_assert(scm_fcd_number_p(y));

  rslt = SCM_NUM_CALL_FUNC(x, truncate_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}

ScmObj
scm_fcd_exact(ScmObj num)
{
  /* 仮実装 */
  scm_assert(scm_fcd_number_p(num));
  return num;
}

ScmObj
scm_fcd_inexact(ScmObj num)
{
  /* 仮実装 */
  scm_assert(scm_fcd_number_p(num));
  return num;
}

int
scm_fcd_integer_to_sword(ScmObj num, scm_sword_t *w)
{
  scm_assert(scm_fcd_integer_p(num));

  if (scm_fcd_fixnum_p(num)) {
    *w = scm_fixnum_value(num);
  }
  else if (scm_fcd_bignum_p(num)) {
    int r = scm_bignum_to_sword(num, w);
    if (r < 0) {
      scm_fcd_error("failed to convert number to scm_sword_t: overflow", 1, num);
      return -1;
    }
  }

  return 0;
}

int
scm_fcd_integer_to_size_t(ScmObj num, size_t *s)
{
  scm_assert(scm_fcd_integer_p(num));

  if (scm_fcd_fixnum_p(num)) {
    scm_sword_t w = scm_fixnum_value(num);
    if (w < 0) {
      scm_fcd_error("failed to convert number to size_t: overflow", 1, num);
      return -1;
    }
    *s = (size_t)w;
  }
  else if (scm_fcd_bignum_p(num)) {
    int r = scm_bignum_to_size_t(num, s);
    if (r < 0) {
      scm_fcd_error("failed to convert number to size_t: overflow", 1, num);
      return -1;
    }
  }

  return 0;
}
