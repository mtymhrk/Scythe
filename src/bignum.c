#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "earray.h"
#include "impl_utils.h"
#include "number_common.h"
#include "fixnum.h"
#include "bignum.h"

ScmNumFunc SCM_BIGNUM_FUNC = {
  .coerce       = scm_bignum_coerce,
  .copy         = scm_bignum_copy,
  .complex_p    = scm_bignum_complex_p,
  .real_p       = scm_bignum_real_p,
  .rational_p   = scm_bignum_rational_p,
  .integer_p    = scm_bignum_integer_p,
  .exact_p      = scm_bignum_exact_p,
  .inexact_p    = scm_bignum_inexact_p,
  .finite_p     = scm_bignum_finite_p,
  .infinite_p   = scm_bignum_infinite_p,
  .nan_p        = scm_bignum_nan_p,
  .cmp          = scm_bignum_cmp,
  .zero_p       = scm_bignum_zero_p,
  .positive_p   = scm_bignum_positive_p,
  .negative_p   = scm_bignum_negative_p,
  .odd_p        = scm_bignum_odd_p,
  .even_p       = scm_bignum_even_p,
  .invert_sign  = scm_bignum_invert_sign,
  .plus         = scm_bignum_plus,
  .minus        = scm_bignum_minus,
  .mul          = scm_bignum_mul,
  .floor_div    = scm_bignum_floor_div,
  .ceiling_div  = scm_bignum_ceiling_div,
  .truncate_div = scm_bignum_truncate_div,
};

ScmTypeInfo SCM_BIGNUM_TYPE_INFO = {
  .name                = "bignum",
  .flags               = SCM_TYPE_FLG_MMO | SCM_TYPE_FLG_NUM,
  .pp_func             = scm_bignum_pretty_print,
  .obj_size            = sizeof(ScmBignum),
  .gc_ini_func         = scm_bignum_gc_initialize,
  .gc_fin_func         = scm_bignum_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = &SCM_BIGNUM_FUNC,
};

static inline scm_bignum_c_t
scm_bignum_extract_2d(ScmObj bn, size_t d)
{
  scm_bignum_c_t h, l;

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  if (SCM_BIGNUM(bn)->nr_digits <= d)
    return 0;

  if (SCM_BIGNUM(bn)->nr_digits <= d + 1) {
    h = 0;
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, d, l);
  }
  else {
    h = 0;
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, d + 1, h);
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, d, l);
  }

  return h * SCM_BIGNUM_BASE + l;
}

static inline scm_bignum_c_t
scm_bignum_extract_1d(ScmObj bn, size_t d)
{
  scm_bignum_c_t v;

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  if (SCM_BIGNUM(bn)->nr_digits <= d)
    return 0;

  EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, d, v);

  return v;
}

static inline scm_bignum_c_t
scm_bignum_msd(ScmObj bn)
{
  scm_bignum_c_t v;

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t,
           SCM_BIGNUM(bn)->nr_digits - 1, v);

  return v;
}

static int
scm_bignum_base_conv(scm_bignum_d_t *digits, size_t len, scm_bignum_c_t fbase,
                     EArray *ary, scm_bignum_c_t tbase)
{
  scm_bignum_c_t d, val, c;
  int err;

  eary_truncate(ary);

  if (fbase == tbase) {
    size_t i;
    for (i = len; i > 0 && digits[i - 1] == 0; i--)
      ;
    for (; i > 0; i--) {
      EARY_SET(ary, scm_bignum_d_t, i - 1, digits[i - 1], err);
      if (err < 0) return -1;
    }
  }
  else {
    for (size_t i = len; i > 0; i--) {
      c = 0;
      for (size_t j = 0; j < EARY_SIZE(ary) || c > 0; j++) {
        if (j < EARY_SIZE(ary))
          EARY_GET(ary, scm_bignum_d_t, j, val);
        else
          val = 0;
        d = (val * fbase + c) % tbase;
        c = (val * fbase + c) / tbase;
        EARY_SET(ary, scm_bignum_d_t, j, d, err);
        if (err < 0) return -1;
      }

      c = (scm_bignum_c_t)digits[i - 1];

      for (size_t j = 0; c > 0; j++) {
        if (j < EARY_SIZE(ary))
          EARY_GET(ary, scm_bignum_d_t, j, val);
        else
          val = 0;
        d = (val + c) % tbase;
        c = (val + c) / tbase;
        EARY_SET(ary, scm_bignum_d_t, j, d, err);
        if (err < 0) return -1;
      }
    }
  }

  if (EARY_SIZE(ary) == 0) {
    EARY_SET(ary, scm_bignum_d_t, 0, 0, err);
    if (err < 0) return -1;
  }

  return 0;
}

static int
scm_bignum_adder(const scm_bignum_d_t *aug, size_t aug_d, bool aug_c,
                 const scm_bignum_d_t *add, size_t add_d, bool add_c,
                 scm_bignum_d_t *rslt, size_t *rslt_d, char *sign)
{
  const scm_bignum_c_t mask = SCM_BIGNUM_BASE - 1;

  scm_bignum_c_t v, v1, v2, c, c1, c2;
  size_t len;

  v = 0;
  c = 0;
  len = 1;
  c1 = 1;
  c2 = 1;
  for (size_t i = 0; i < *rslt_d; i++) {
    v1 = (i < aug_d) ? aug[i] : 0;
    v2 = (i < add_d) ? add[i] : 0;

    if (aug_c) {
      v1 = (~v1 & mask) + c1;
      c1 = (v1 & ~mask) ? 1 : 0;
      v1 = v1 & mask;
    }

    if (add_c) {
      v2 = (~v2 & mask) + c2;
      c2 = (v2 & ~mask) ? 1 : 0;
      v2 = v2 & mask;
    }

    v = v1 + v2 + c;
    c = (v & ~mask) ? 1 : 0;
    v &= mask;

    rslt[i] = (scm_bignum_d_t)v;
    if (v > 0) len = i + 1;
  }

  *sign = (v & ~(mask >> 1)) ? '-' : '+';
  if (*sign == '-') {
    c = 1;
    for (size_t i = 0; i < *rslt_d; i++) {
      v = (scm_uword_t)rslt[i];
      v = (~v & mask) + c;
      c = (v & ~mask) ? 1 : 0;
      v = v & mask;
      rslt[i] = (scm_bignum_d_t)v;
      if (v > 0) len = i + 1;
    }
  }

  *rslt_d = len;

  return 0;
}

static int
scm_bignum_nadd_1d(ScmObj bignum, scm_bignum_sc_t addend)
{
  scm_bignum_d_t add[1];
  size_t place;
  char sign;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(-((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1) <= addend);
  scm_assert(addend <= ((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1));

  if (addend >= 0)
    add[0] = (scm_bignum_d_t)addend;
  else
    add[0] = (scm_bignum_d_t)-addend;

  /* 加算結果が桁上りする場合にそなえて記憶領域を確保するために 0 を設定する */
  place = SCM_BIGNUM(bignum)->nr_digits;
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, place, 0, err);
  if (err < 0) return -1;
  place++;

  scm_bignum_adder(EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                   SCM_BIGNUM(bignum)->nr_digits,
                   (SCM_BIGNUM(bignum)->sign == '-') ? true : false,
                   add,
                   1,
                   (addend < 0) ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(bignum)->digits), &place, &sign);

  SCM_BIGNUM(bignum)->nr_digits = place;
  SCM_BIGNUM(bignum)->sign = sign;

  return 0;
}

static int
scm_bignum_nmul_1d(ScmObj bignum, scm_bignum_sc_t multiplier)
{
  scm_bignum_c_t mul, v, v1, c, c_tmp;
  size_t len;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(-((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1) <= multiplier);
  scm_assert(multiplier <= ((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1));

  if (multiplier >= 0)
    mul = (scm_bignum_c_t)multiplier;
  else
    mul = (scm_bignum_c_t)-multiplier;

  len = 1;
  c = 0;
  for (size_t i = 0; i < SCM_BIGNUM(bignum)->nr_digits || c > 0; i++) {
    if (i < SCM_BIGNUM(bignum)->nr_digits)
      EARY_GET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i, v1);
    else
      v1 = 0;

    v = v1 * mul;
    c_tmp = v / SCM_BIGNUM_BASE;
    v = v % SCM_BIGNUM_BASE;
    v += c;
    c = c_tmp + v / SCM_BIGNUM_BASE;
    v = v % SCM_BIGNUM_BASE;

    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i, v, err);
    if (err < 0) return -1;
    if (v > 0) len = i + 1;
  }

  if (multiplier < 0)
    SCM_BIGNUM(bignum)->sign = (SCM_BIGNUM(bignum)->sign == '+') ? '-' : '+';

  SCM_BIGNUM(bignum)->nr_digits = len;

  return 0;
}

static int
scm_bignum_replace_mul_1d(ScmObj result,
                          ScmObj multiplicand, scm_bignum_sc_t multiplier)
{
  scm_bignum_c_t mul, v, v1, c, c_tmp;
  size_t len;
  int err;

  scm_assert_obj_type(result, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(multiplicand, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(-((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1) <= multiplier);
  scm_assert(multiplier <= ((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1));

  if (multiplier >= 0)
    mul = (scm_bignum_c_t)multiplier;
  else
    mul = (scm_bignum_c_t)-multiplier;

  eary_truncate(&SCM_BIGNUM(result)->digits);

  len = 1;
  c = 0;
  for (size_t i = 0; i < SCM_BIGNUM(multiplicand)->nr_digits || c > 0; i++) {
    if (i < SCM_BIGNUM(multiplicand)->nr_digits)
      EARY_GET(&SCM_BIGNUM(multiplicand)->digits, scm_bignum_d_t, i, v1);
    else
      v1 = 0;

    v = v1 * mul;
    c_tmp = v / SCM_BIGNUM_BASE;
    v = v % SCM_BIGNUM_BASE;
    v += c;
    c = c_tmp + v / SCM_BIGNUM_BASE;
    v = v % SCM_BIGNUM_BASE;

    EARY_SET(&SCM_BIGNUM(result)->digits, scm_bignum_d_t, i, v, err);
    if (err < 0) return -1;
    if (v > 0) len = i + 1;
  }

  if (multiplier >= 0)
    SCM_BIGNUM(result)->sign = SCM_BIGNUM(multiplicand)->sign;
  else
    SCM_BIGNUM(result)->sign =
      (SCM_BIGNUM(multiplicand)->sign == '+') ? '-' : '+';

  SCM_BIGNUM(result)->nr_digits = len;

  return 0;
}


static int
scm_bignum_ndiv_1d(ScmObj bignum, scm_bignum_sc_t divisor,
                   scm_bignum_sc_t *rem)
{
  scm_bignum_c_t div, v, v1, r;
  size_t len;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(-((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1) <= divisor);
  scm_assert(divisor <= ((scm_bignum_sc_t)SCM_BIGNUM_BASE - 1));

  if (divisor >= 0)
    div = (scm_bignum_c_t)divisor;
  else
    div = (scm_bignum_c_t)-divisor;

  if (div == 0) {
    scm_capi_error("division by zero", 0);
    return -1;
  }

  r = 0;
  len = 1;
  for (size_t i = SCM_BIGNUM(bignum)->nr_digits; i > 0; i--) {
    EARY_GET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i - 1, v1);

    v = (r * SCM_BIGNUM_BASE + v1) / div;
    r = (r * SCM_BIGNUM_BASE + v1) % div;

    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i - 1, v, err);
    if (err < 0) return -1;
    if (len == 1 && v > 0) len = i;
  }

  if (rem != NULL)
    *rem = ((SCM_BIGNUM(bignum)->sign == '+') ?
            (scm_bignum_sc_t)r : -(scm_bignum_sc_t)r);

  if (divisor < 0)
    SCM_BIGNUM(bignum)->sign = (SCM_BIGNUM(bignum)->sign == '+') ? '-' : '+';

  SCM_BIGNUM(bignum)->nr_digits = len;

  return 0;
}

static int
scm_bignum_nlshift(ScmObj bignum, size_t n)
{
  scm_bignum_d_t *head;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  if (n == 0) return 0;

  if (SCM_BIGNUM(bignum)->nr_digits == 1) {
    scm_bignum_c_t v;
    EARY_GET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 0, v);
    if (v == 0) return 0;
  }

  if (SCM_BIGNUM(bignum)->nr_digits + n < n) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return -1;
  }

  /* shift 結果を格納できるようあらかじめ配列を拡張するために 0 を設定する */
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t,
           SCM_BIGNUM(bignum)->nr_digits + n - 1, 0, err);
  if (err < 0) return -1;

  head = EARY_HEAD(&SCM_BIGNUM(bignum)->digits);
  if (n >= SCM_BIGNUM(bignum)->nr_digits)
    memcpy(head + n, head,
           sizeof(scm_bignum_d_t) * SCM_BIGNUM(bignum)->nr_digits);
  else
    memmove(head + n, head,
            sizeof(scm_bignum_d_t) * SCM_BIGNUM(bignum)->nr_digits);

  memset(head, 0, sizeof(scm_bignum_d_t) * n);

  SCM_BIGNUM(bignum)->nr_digits += n;

  return 0;
}

static int
scm_bignum_nrshift(ScmObj bignum, size_t n)
{
  scm_bignum_d_t *head;
  size_t len;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  if (n == 0) return 0;

  if (n >= SCM_BIGNUM(bignum)->nr_digits) {
    SCM_BIGNUM(bignum)->sign = '+';
    SCM_BIGNUM(bignum)->nr_digits = 1;
    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 0, 0, err);
    if (err < 0) return -1;
    return 0;
  }

  len = SCM_BIGNUM(bignum)->nr_digits - n;
  head = EARY_HEAD(&SCM_BIGNUM(bignum)->digits);
  if (len <= n)
    memcpy(head, head + n, sizeof(scm_bignum_d_t) * len);
  else
    memmove(head, head + n, sizeof(scm_bignum_d_t) * len);

  SCM_BIGNUM(bignum)->nr_digits = len;

  return 0;
}

static int
scm_bignum_ninc(ScmObj bignum, ScmObj inc)
{
  size_t place;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(inc, &SCM_BIGNUM_TYPE_INFO);

  place = SCM_BIGNUM(bignum)->nr_digits;
  if (place < SCM_BIGNUM(inc)->nr_digits)
    place = SCM_BIGNUM(inc)->nr_digits;

  if (place >= SIZE_MAX) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return -1;
  }

  /* 加算結果が桁上りする場合にそなえて記憶領域を確保するために 0 を設定する */
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, place, 0, err);
  if (err < 0) return -1;
  place++;

  scm_bignum_adder(EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                   SCM_BIGNUM(bignum)->nr_digits,
                   (SCM_BIGNUM(bignum)->sign == '-') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(inc)->digits),
                   SCM_BIGNUM(inc)->nr_digits,
                   (SCM_BIGNUM(inc)->sign == '-') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                   &place,
                   &SCM_BIGNUM(bignum)->sign);

  SCM_BIGNUM(bignum)->nr_digits = place;

  return 0;
}

static int
scm_bignum_ndec(ScmObj bignum, ScmObj dec)
{
  size_t place;
  int err;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(dec, &SCM_BIGNUM_TYPE_INFO);

  place = SCM_BIGNUM(bignum)->nr_digits;
  if (place < SCM_BIGNUM(dec)->nr_digits)
    place = SCM_BIGNUM(dec)->nr_digits;

  if (place >= SIZE_MAX) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return -1;
  }

  /* 加算結果が桁上りする場合にそなえて記憶領域を確保するために 0 を設定する */
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, place, 0, err);
  if (err < 0) return -1;
  place++;

  scm_bignum_adder(EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                   SCM_BIGNUM(bignum)->nr_digits,
                   (SCM_BIGNUM(bignum)->sign == '-') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(dec)->digits),
                   SCM_BIGNUM(dec)->nr_digits,
                   (SCM_BIGNUM(dec)->sign == '+') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                   &place,
                   &SCM_BIGNUM(bignum)->sign);

  SCM_BIGNUM(bignum)->nr_digits = place;

  return 0;
}

static int
scm_bignum_abs_cmp(ScmObj bn1, ScmObj bn2)
{
  scm_bignum_c_t v1, v2;

  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  if (SCM_BIGNUM(bn1)->nr_digits > SCM_BIGNUM(bn2)->nr_digits)
    return 1;
  else if (SCM_BIGNUM(bn1)->nr_digits < SCM_BIGNUM(bn2)->nr_digits)
    return -1;

  for (size_t i = SCM_BIGNUM(bn1)->nr_digits; i > 0; i--) {
    EARY_GET(&SCM_BIGNUM(bn1)->digits, scm_bignum_d_t, i - 1, v1);
    EARY_GET(&SCM_BIGNUM(bn2)->digits, scm_bignum_d_t, i - 1, v2);

    if (v1 > v2) return 1;
    else if (v1 < v2) return -1;
  }

  return 0;
}

static ScmObj
scm_bignum_2_fixnum_if_possible(ScmObj bignum)
{
  scm_bignum_sc_t v, num, abs_max;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  if (SCM_BIGNUM(bignum)->sign == '+')
    abs_max = SCM_FIXNUM_MAX;
  else
    abs_max = -SCM_FIXNUM_MIN;

  num = 0;
  for (size_t i = SCM_BIGNUM(bignum)->nr_digits; i > 0; i--) {
    EARY_GET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i - 1, v);
    if (num > (abs_max - v) / (scm_bignum_sc_t)SCM_BIGNUM_BASE)
      return bignum;

    num = num * (scm_bignum_sc_t)SCM_BIGNUM_BASE + v;
  }

  if (SCM_BIGNUM(bignum)->sign == '+')
    return scm_fixnum_new(num);
  else
    return scm_fixnum_new(-num);
}

static int
scm_bignum_quo_rem(ScmObj bn1, ScmObj bn2,
                   scm_csetter_t *quo, scm_csetter_t *rem)
{
  ScmObj qu = SCM_OBJ_INIT, re = SCM_OBJ_INIT;
  ScmObj a = SCM_OBJ_INIT, m = SCM_OBJ_INIT, c = SCM_OBJ_INIT;
  ScmObj mq = SCM_OBJ_INIT;
  int cmp;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &qu, &re, &a, &m, &c, &mq);

  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  if (scm_bignum_zero_p(bn2)) {
    scm_capi_error("division by zero", 0);
    return -1;
  }

  if (scm_bignum_zero_p(bn1)) {
    qu = re = bn1;
    goto ret;
  }

  cmp = scm_bignum_abs_cmp(bn1, bn2);
  if (cmp == -1) {
    if (quo != NULL) {
      qu = scm_bignum_new_from_sword(SCM_MEM_HEAP, 0);
      if (scm_obj_null_p(qu)) return -1;
    }
    re = bn1;
  }
  else if (cmp == 0) {
    if (quo != NULL) {
      if (SCM_BIGNUM(bn1)->sign == SCM_BIGNUM(bn2)->sign)
        qu = scm_bignum_new_from_sword(SCM_MEM_HEAP, 1);
      else
        qu = scm_bignum_new_from_sword(SCM_MEM_HEAP, -1);
      if (scm_obj_null_p(qu)) return -1;
    }
    if (rem != NULL) {
      re = scm_bignum_new_from_sword(SCM_MEM_HEAP, 0);
      if (scm_obj_null_p(re)) return -1;
    }
  }
  else {
    scm_bignum_c_t q, d;
    size_t b, msd;
    int rslt;

    d = SCM_BIGNUM_BASE / (scm_bignum_msd(bn2) + 1);

    a = scm_bignum_copy(bn1);
    if (scm_obj_null_p(a)) return -1;
    SCM_BIGNUM(a)->sign = '+';

    rslt = scm_bignum_nmul_1d(a, (scm_bignum_sc_t)d);
    if (rslt < 0) return -1;

    m = scm_bignum_copy(bn2);
    if (scm_obj_null_p(m)) return -1;
    SCM_BIGNUM(m)->sign = '+';

    rslt = scm_bignum_nmul_1d(m, (scm_bignum_sc_t)d);
    if (rslt < 0) return -1;

    c = scm_bignum_new_from_sword(SCM_MEM_HEAP, 0);
    if (scm_obj_null_p(c)) return -1;

    b = SCM_BIGNUM(a)->nr_digits - SCM_BIGNUM(m)->nr_digits;
    rslt = scm_bignum_nlshift(m, b);
    if (rslt < 0) return -1;

    mq = scm_bignum_copy(m);
    if (scm_obj_null_p(mq)) return -1;

    msd = SCM_BIGNUM(m)->nr_digits - 1;

    if (scm_bignum_abs_cmp(a, m) >= 0) {
      rslt = scm_bignum_ndec(a, m);
      if (rslt < 0) return -1;

      rslt = scm_bignum_nadd_1d(c, 1);
      if (rslt < 0) return -1;
    }

    for (size_t i = 0; i < b; i++) {
      msd--;

      rslt = scm_bignum_nrshift(m, 1);
      if (rslt < 0) return -1;

      rslt = scm_bignum_nlshift(c, 1);
      if (rslt < 0) return -1;

      q = scm_bignum_extract_2d(a, msd) / scm_bignum_extract_1d(m, msd);
      if (q > 0) {
        if (q >= SCM_BIGNUM_BASE) q = SCM_BIGNUM_BASE - 1;

        rslt = scm_bignum_replace_mul_1d(mq, m, (scm_bignum_sc_t)q);
        if (rslt < 0) return -1;

        rslt = scm_bignum_ndec(a, mq);
        if (rslt < 0) return -1;

        while (SCM_BIGNUM(a)->sign == '-') {
          rslt = scm_bignum_ninc(a, m);
          if (rslt < 0) return -1;
          q--;
        }

        rslt = scm_bignum_nadd_1d(c, (scm_bignum_sc_t)q);
        if (rslt < 0) return -1;
      }
    }

    if (quo != NULL) {
      if (SCM_BIGNUM(bn2)->sign == '+')
        SCM_BIGNUM(c)->sign = SCM_BIGNUM(bn1)->sign;
      else
        SCM_BIGNUM(c)->sign = SCM_BIGNUM(bn1)->sign == '+' ? '-' : '+';

      qu = c;
    }

    if (rem != NULL) {
      rslt = scm_bignum_ndiv_1d(a, (scm_bignum_sc_t)d, NULL);
      if (rslt < 0) return -1;
      SCM_BIGNUM(a)->sign = SCM_BIGNUM(bn1)->sign;

      re = a;
    }
  }

 ret:

  if (quo != NULL) scm_csetter_setq(quo, qu);
  if (rem != NULL) scm_csetter_setq(rem, re);

  return 0;
}

int
scm_bignum_initialize_ary(ScmObj bignum,
                          char sign, scm_bignum_d_t *digits, size_t len,
                          scm_bignum_c_t base)
{
  int rslt;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(sign == '+' || sign == '-');

  SCM_BIGNUM(bignum)->sign = sign;
  rslt = eary_init(&SCM_BIGNUM(bignum)->digits, sizeof(scm_bignum_d_t), len);
  if (rslt != 0) return -1;     /* [ERR]: [through] */

  rslt = scm_bignum_base_conv(digits, len, base,
                              &SCM_BIGNUM(bignum)->digits, SCM_BIGNUM_BASE);
  if (rslt < 0)  return -1;        /* [ERR]: [through] */

  SCM_BIGNUM(bignum)->nr_digits = EARY_SIZE(&SCM_BIGNUM(bignum)->digits);

  return 0;
}

int
scm_bignum_initialize_sword(ScmObj bignum, scm_sword_t val)
{
  scm_uword_t uval, dg;
  int err, rslt;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  rslt = eary_init(&SCM_BIGNUM(bignum)->digits, sizeof(scm_bignum_d_t), 2);
  if (rslt != 0) return -1;     /* [ERR]: [through] */

  if (val >= 0) {
    SCM_BIGNUM(bignum)->sign = '+';
    uval = (scm_uword_t)val;
  }
  else {
    SCM_BIGNUM(bignum)->sign = '-';
    uval = (scm_uword_t)-val;
  }

  dg = uval % SCM_BIGNUM_BASE;
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 0, dg, err);
  if (err < 0) return -1;

  dg = uval / SCM_BIGNUM_BASE;
  if (dg > 0) {
    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 1, dg, err);
    if (err < 0) return -1;
    SCM_BIGNUM(bignum)->nr_digits = 2;
  }
  else {
    SCM_BIGNUM(bignum)->nr_digits = 1;
  }

  return 0;
}

ScmObj
scm_bignum_new_from_ary(SCM_MEM_TYPE_T mtype, char sign,
                        scm_bignum_d_t *digits, size_t len, scm_bignum_c_t base)
{
  ScmObj bn = SCM_OBJ_INIT;

  bn = scm_capi_mem_alloc(&SCM_BIGNUM_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_bignum_initialize_ary(bn, sign, digits, len, base) < 0)
    return SCM_OBJ_NULL;  /* [ERR]: [through] */

  return bn;
}

ScmObj
scm_bignum_new_from_sword(SCM_MEM_TYPE_T mtype, scm_sword_t val)
{
  ScmObj bn = SCM_OBJ_INIT;

  bn = scm_capi_mem_alloc(&SCM_BIGNUM_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_bignum_initialize_sword(bn, val) < 0)
    return SCM_OBJ_NULL;  /* [ERR]: [through] */

  return bn;
}

ScmObj
scm_bignum_new_from_fixnum(SCM_MEM_TYPE_T mtype, ScmObj fn)
{
  ScmObj bn = SCM_OBJ_INIT;
  scm_sword_t sword;

  scm_assert(scm_capi_fixnum_p(fn));

  sword = scm_fixnum_value(fn);
  bn = scm_capi_mem_alloc(&SCM_BIGNUM_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_bignum_initialize_sword(bn, sword) < 0)
    return SCM_OBJ_NULL;  /* [ERR]: [through] */

  return bn;
}

ScmObj
scm_bignum_copy(ScmObj bignum)
{
  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  return scm_bignum_new_from_ary(SCM_MEM_HEAP,
                                 SCM_BIGNUM(bignum)->sign,
                                 EARY_HEAD(&SCM_BIGNUM(bignum)->digits),
                                 SCM_BIGNUM(bignum)->nr_digits,
                                 SCM_BIGNUM_BASE);
}

int
scm_bignum_to_sword(ScmObj bn, scm_sword_t *w)
{
  scm_uword_t abs_max, num, n;

  scm_assert(scm_capi_bignum_p(bn));
  scm_assert(w != NULL);

  if (SCM_BIGNUM(bn)->sign == '+')
    abs_max = SCM_SWORD_MAX;
  else
    abs_max = -(scm_uword_t)SCM_SWORD_MIN;

  num = 0;
  for (size_t i = SCM_BIGNUM(bn)->nr_digits; i > 0; i--) {
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, i, n);
    if (num > (abs_max - n) / SCM_BIGNUM_BASE) return -1;
    num = num * SCM_BIGNUM_BASE + n;
  }

  if (SCM_BIGNUM(bn)->sign == '+')
    *w = (scm_sword_t)num;
  else
    *w = (scm_sword_t)-num;

  return 0;
}

int
scm_bignum_to_size_t(ScmObj bn, size_t *s)
{
  size_t num, n;

  scm_assert(scm_capi_bignum_p(bn));
  scm_assert(s != NULL);

  if (SCM_BIGNUM(bn)->sign == '-') return -1;

  num = 0;
  for (size_t i = SCM_BIGNUM(bn)->nr_digits; i > 0; i--) {
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, i, n);
    if (num > (SIZE_MAX - n) / SCM_BIGNUM_BASE) return -1;
    num = num * SCM_BIGNUM_BASE + n;
  }

  *s = num;

  return 0;
}

void
scm_bignum_finalize(ScmObj bignum)
{
  eary_fin(&SCM_BIGNUM(bignum)->digits);
}

bool
scm_bignum_complex_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_real_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_rational_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_integer_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_exact_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_inexact_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return false;
}

bool
scm_bignum_finite_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return true;
}

bool
scm_bignum_infinite_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return false;
}

bool
scm_bignum_nan_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return false;
}

int
scm_bignum_cmp(ScmObj bn, ScmObj num, int *cmp)
{
  SCM_STACK_FRAME_PUSH(&bn, &num);

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(num));

  if (scm_capi_fixnum_p(num)) {
    num = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, num);
    if (scm_obj_null_p(num)) return -1;
  }
  else if (!scm_capi_bignum_p(num)) {
    bn = SCM_NUM_CALL_FUNC(num, coerce, bn);
    if (scm_obj_null_p(bn)) return -1;

    return SCM_NUM_CALL_FUNC(bn, cmp, num, cmp);
  }

  if (cmp != NULL) {
    if (SCM_BIGNUM(bn)->sign == '-' && SCM_BIGNUM(num)->sign == '+') {
      *cmp = -1;
    }
    else if (SCM_BIGNUM(bn)->sign == '+' && SCM_BIGNUM(num)->sign == '-') {
      *cmp = 1;
    }
    else {
      *cmp = scm_bignum_abs_cmp(bn, num);
      if (SCM_BIGNUM(bn)->sign == '-') *cmp = -*cmp;
    }
  }

  return 0;
}

bool
scm_bignum_zero_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  if (SCM_BIGNUM(bn)->nr_digits == 1) {
    scm_bignum_c_t v;
    EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, 0, v);
    if (v == 0) return true;
  }

  return false;
}

bool
scm_bignum_positive_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return (SCM_BIGNUM(bn)->sign == '+') ? true : false;
}

bool
scm_bignum_negative_p(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  return (SCM_BIGNUM(bn)->sign == '-') ? true : false;
}

bool
scm_bignum_odd_p(ScmObj bn)
{
  scm_bignum_c_t v;

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, 0, v);

  return ((v & 0x01) == 1) ? true : false;
}

bool
scm_bignum_even_p(ScmObj bn)
{
  scm_bignum_c_t v;

  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  EARY_GET(&SCM_BIGNUM(bn)->digits, scm_bignum_d_t, 0, v);

  return ((v & 0x01) == 0) ? true : false;
}

ScmObj
scm_bignum_invert_sign(ScmObj bn)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);

  bn = scm_bignum_copy(bn);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL;

  SCM_BIGNUM(bn)->sign = (SCM_BIGNUM(bn)->sign == '+') ? '-' : '+';

  return bn;
}

ScmObj
scm_bignum_plus(ScmObj aug, ScmObj add)
{
  size_t place;

  SCM_STACK_FRAME_PUSH(&aug, &add);

  scm_assert_obj_type(aug, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(add));

  if (scm_capi_fixnum_p(add)) {
    add = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, add);
    if (scm_obj_null_p(add)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else if (!scm_capi_bignum_p(add)) {
    aug = SCM_NUM_CALL_FUNC(add, coerce, aug);
    if (scm_obj_null_p(aug)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(aug, plus, add);
  }

  place = SCM_BIGNUM(aug)->nr_digits;
  if (place < SCM_BIGNUM(add)->nr_digits)
    place = SCM_BIGNUM(add)->nr_digits;
  place++;

  if (place == 0) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return SCM_OBJ_NULL;
  }

  scm_bignum_d_t ary[place];
  char sign;

  scm_bignum_adder(EARY_HEAD(&SCM_BIGNUM(aug)->digits),
                   SCM_BIGNUM(aug)->nr_digits,
                   (SCM_BIGNUM(aug)->sign == '-') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(add)->digits),
                   SCM_BIGNUM(add)->nr_digits,
                   (SCM_BIGNUM(add)->sign == '-') ? true : false,
                   ary, &place, &sign);

  return scm_num_make_int_from_ary(sign, ary, place, SCM_BIGNUM_BASE);
}

ScmObj
scm_bignum_minus(ScmObj min, ScmObj sub)
{
  size_t place;

  SCM_STACK_FRAME_PUSH(&min, &sub);

  scm_assert_obj_type(min, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(sub));

  if (scm_capi_fixnum_p(sub)) {
    sub = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, sub);
    if (scm_obj_null_p(sub)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else if (!scm_capi_bignum_p(sub)) {
    min = SCM_NUM_CALL_FUNC(sub, coerce, min);
    if (scm_obj_null_p(min)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(min, minus, sub);
  }

  place = SCM_BIGNUM(min)->nr_digits;
  if (place < SCM_BIGNUM(sub)->nr_digits)
    place = SCM_BIGNUM(sub)->nr_digits;
  place++;

  if (place == 0) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return SCM_OBJ_NULL;
  }

  scm_bignum_d_t ary[place];
  char sign;

  scm_bignum_adder(EARY_HEAD(&SCM_BIGNUM(min)->digits),
                   SCM_BIGNUM(min)->nr_digits,
                   (SCM_BIGNUM(min)->sign == '-') ? true : false,
                   EARY_HEAD(&SCM_BIGNUM(sub)->digits),
                   SCM_BIGNUM(sub)->nr_digits,
                   (SCM_BIGNUM(sub)->sign == '+') ? true : false,
                   ary, &place, &sign);

  return scm_num_make_int_from_ary(sign, ary, place, SCM_BIGNUM_BASE);
}

ScmObj
scm_bignum_mul(ScmObj mud, ScmObj mur)
{
  char sign;
  size_t place;

  SCM_STACK_FRAME_PUSH(&mud, &mur);

  scm_assert_obj_type(mud, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(mur));

  if (scm_capi_fixnum_p(mur)) {
    mur = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, mur);
    if (scm_obj_null_p(mur)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else if (!scm_capi_bignum_p(mur)) {
    mud = SCM_NUM_CALL_FUNC(mur, coerce, mud);
    if (scm_obj_null_p(mud)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(mud, mul, mur);
  }

  place = SCM_BIGNUM(mud)->nr_digits + SCM_BIGNUM(mur)->nr_digits;
  if (place < SCM_BIGNUM(mud)->nr_digits
      || place < SCM_BIGNUM(mur)->nr_digits) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return SCM_OBJ_NULL;
  }

  scm_bignum_c_t v, v1, v2, c, c_tmp;
  scm_bignum_d_t ary[place];
  size_t len;

  memset(ary, 0, sizeof(ary));
  len = 1;
  for (size_t i = 0; i < SCM_BIGNUM(mur)->nr_digits; i++) {
    EARY_GET(&SCM_BIGNUM(mur)->digits, scm_bignum_d_t, i, v2);
    c = 0;
    for (size_t j = 0; j < SCM_BIGNUM(mud)->nr_digits || c > 0; j++) {
      if (j < SCM_BIGNUM(mud)->nr_digits)
        EARY_GET(&SCM_BIGNUM(mud)->digits, scm_bignum_d_t, j, v1);
      else
        v1 = 0;

      v = v1 * v2;
      c_tmp = v / SCM_BIGNUM_BASE;
      v = v % SCM_BIGNUM_BASE;
      v += ary[i + j] + c;
      c = c_tmp + v / SCM_BIGNUM_BASE;
      v = v % SCM_BIGNUM_BASE;
      ary[i + j] = (scm_bignum_d_t)v;
      if (v > 0) len = i + j + 1;
    }
  }

  if (SCM_BIGNUM(mud)->sign == SCM_BIGNUM(mur)->sign)
    sign = '+';
  else
    sign = '-';

  return scm_num_make_int_from_ary(sign, ary, len, SCM_BIGNUM_BASE);
}

int
scm_bignum_floor_div(ScmObj dvd, ScmObj dvr,
                     scm_csetter_t *quo, scm_csetter_t *rem)
{
  ScmObj qu = SCM_OBJ_INIT, re = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvd, &dvr, &qu, &re);

  scm_assert_obj_type(dvd, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (scm_capi_fixnum_p(dvr)) {
    dvr = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, dvr);
    if (scm_obj_null_p(dvr)) return -1;
  }
  else if (!scm_capi_bignum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(dvd, floor_div, dvd, quo, rem);
  }

  rslt = scm_bignum_quo_rem(dvd, dvr, SCM_CSETTER_L(qu), SCM_CSETTER_L(re));
  if (rslt < 0) return -1;

  if (!scm_bignum_zero_p(re)
      && (SCM_BIGNUM(dvd)->sign != SCM_BIGNUM(dvr)->sign)) {
    if (quo != NULL) {
      rslt = scm_bignum_nadd_1d(qu, -1);
      if (rslt < 0) return -1;
    }

    if (rem != NULL) {
      rslt = scm_bignum_ninc(re, dvr);
      if (rslt < 0) return -1;
    }
  }

  if (quo != NULL) {
    qu = scm_bignum_2_fixnum_if_possible(qu);
    scm_csetter_setq(quo, qu);
  }

  if (rem != NULL) {
    re = scm_bignum_2_fixnum_if_possible(re);
    scm_csetter_setq(rem, re);
  }

  return 0;
}

int
scm_bignum_ceiling_div(ScmObj dvd, ScmObj dvr,
                       scm_csetter_t *quo, scm_csetter_t *rem)
{
  ScmObj qu = SCM_OBJ_INIT, re = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvd, &dvr, &qu, &re);

  scm_assert_obj_type(dvd, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (scm_capi_fixnum_p(dvr)) {
    dvr = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, dvr);
    if (scm_obj_null_p(dvr)) return -1;
  }
  else if (!scm_capi_bignum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(dvd, ceiling_div, dvd, quo, rem);
  }

  rslt = scm_bignum_quo_rem(dvd, dvr, SCM_CSETTER_L(qu), SCM_CSETTER_L(re));
  if (rslt < 0) return -1;

  if (!scm_bignum_zero_p(re)
      && (SCM_BIGNUM(dvd)->sign == SCM_BIGNUM(dvr)->sign)) {
    if (quo != NULL) {
      rslt = scm_bignum_nadd_1d(qu, 1);
      if (rslt < 0) return -1;
    }

    if (rem != NULL) {
      rslt = scm_bignum_ndec(re, dvr);
      if (rslt < 0) return -1;
    }
  }

  if (quo != NULL) {
    qu = scm_bignum_2_fixnum_if_possible(qu);
    scm_csetter_setq(quo, qu);
  }

  if (rem != NULL) {
    re = scm_bignum_2_fixnum_if_possible(re);
    scm_csetter_setq(rem, re);
  }

  return 0;
}

int
scm_bignum_truncate_div(ScmObj dvd, ScmObj dvr,
                        scm_csetter_t *quo, scm_csetter_t *rem)
{
  ScmObj qu = SCM_OBJ_INIT, re = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvd, &dvr, &qu, &re);

  scm_assert_obj_type(dvd, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (scm_capi_fixnum_p(dvr)) {
    dvr = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, dvr);
    if (scm_obj_null_p(dvr)) return -1;
  }
  else if (!scm_capi_bignum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(dvd, truncate_div, dvd, quo, rem);
  }

  rslt = scm_bignum_quo_rem(dvd, dvr,
                            (quo != NULL) ? SCM_CSETTER_L(qu) : NULL,
                            (rem != NULL) ? SCM_CSETTER_L(re) : NULL);
  if (rslt < 0) return -1;

  if (quo != NULL) {
    qu = scm_bignum_2_fixnum_if_possible(qu);
    scm_csetter_setq(quo, qu);
  }

  if (rem != NULL) {
    re = scm_bignum_2_fixnum_if_possible(re);
    scm_csetter_setq(rem, re);
  }

  return 0;
}

ScmObj
scm_bignum_coerce(ScmObj bn, ScmObj num)
{
  scm_assert_obj_type(bn, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(num));

  if (scm_capi_fixnum_p(num)) {
    return scm_bignum_new_from_fixnum(SCM_MEM_HEAP, num);
  }
  else {
    scm_capi_error("undefined arithmetic operation pattern", 2, bn, num);
    return SCM_OBJ_NULL;
  }
}


int
scm_bignum_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  scm_bignum_c_t base;
  scm_bignum_d_t val;
  EArray ary;
  int rslt, place, width;
  char str[32];

  SCM_STACK_FRAME_PUSH(&obj, &port);

  scm_assert_obj_type(obj, &SCM_BIGNUM_TYPE_INFO);

  rslt = scm_num_calc_base_and_place_for_ary_of_digits(10, &base, &place);
  if (rslt < 0) return -1;

  rslt = eary_init(&ary, sizeof(scm_bignum_d_t), SCM_BIGNUM(obj)->nr_digits);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  rslt = scm_bignum_base_conv(EARY_HEAD(&SCM_BIGNUM(obj)->digits),
                            EARY_SIZE(&SCM_BIGNUM(obj)->digits),
                            SCM_BIGNUM_BASE,
                            &ary, base);
  if (rslt < 0) goto err;       /* [ERR]: [through] */

  if (SCM_BIGNUM(obj)->sign == '-') {
    rslt = scm_capi_write_cstr("-", SCM_ENC_ASCII, port);
    if (rslt < 0) goto err;    /* [ERR]: [through] */
  }

  width = 0;
  for (size_t i = EARY_SIZE(&ary); i > 0; i--) {
    EARY_GET(&ary, scm_bignum_d_t, i - 1, val);
    snprintf(str, sizeof(str), "%0*u", width, val);
    rslt = scm_capi_write_cstr(str, SCM_ENC_ASCII, port);
    if (rslt < 0) goto err;    /* [ERR]: [through] */
    width = place;
  }

  eary_fin(&ary);
  return 0;

 err:
  eary_fin(&ary);
  return -1;
}

void
scm_bignum_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_BIGNUM_TYPE_INFO);

  eary_init(&SCM_BIGNUM(obj)->digits, 0, 0);
}

void
scm_bignum_gc_finalize(ScmObj obj)
{
  scm_bignum_finalize(obj);
}
