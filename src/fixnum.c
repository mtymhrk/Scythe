#include "object.h"
#include "reference.h"
#include "api.h"
#include "impl_utils.h"
#include "number_common.h"
#include "bignum.h"
#include "fixnum.h"

ScmNumFunc SCM_FIXNUM_FUNC = {
  .coerce       = scm_fixnum_coerce,
  .copy         = scm_fixnum_copy,
  .complex_p    = scm_fixnum_complex_p,
  .real_p       = scm_fixnum_real_p,
  .rational_p   = scm_fixnum_rational_p,
  .integer_p    = scm_fixnum_integer_p,
  .exact_p      = scm_fixnum_exact_p,
  .inexact_p    = scm_fixnum_inexact_p,
  .finite_p     = scm_fixnum_finite_p,
  .infinite_p   = scm_fixnum_infinite_p,
  .nan_p        = scm_fixnum_nan_p,
  .cmp          = scm_fixnum_cmp,
  .zero_p       = scm_fixnum_zero_p,
  .positive_p   = scm_fixnum_positive_p,
  .negative_p   = scm_fixnum_negative_p,
  .odd_p        = scm_fixnum_odd_p,
  .even_p       = scm_fixnum_even_p,
  .invert_sign  = scm_fixnum_invert_sign,
  .plus         = scm_fixnum_plus,
  .minus        = scm_fixnum_minus,
  .mul          = scm_fixnum_mul,
  .floor_div    = scm_fixnum_floor_div,
  .ceiling_div  = scm_fixnum_ceiling_div,
  .truncate_div = scm_fixnum_truncate_div,
};

ScmTypeInfo SCM_FIXNUM_TYPE_INFO = {
  .name                = "fixnum",
  .flags               = SCM_TYPE_FLG_NUM,
  .obj_print_func      = scm_fixnum_obj_print,
  .obj_size            = 0,
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = &SCM_FIXNUM_FUNC,
};

static inline int
scm_fixnum_multi(scm_sword_t v1, scm_sword_t v2, scm_sword_t *v)
{
  const scm_sword_t max = (scm_sword_t)1 << ((SCM_FIXNUM_BITS - 1)/2);
  const scm_sword_t min = -max;

  if ((v1 < min || max <= v1) && (v2 < min || max <= v2))
    return -1;
  else if (v1 == min && v2 == min)
    return -1;

  *v = v1 * v2;

  if (*v < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < *v)
    return -1;
  else if (*v / v1 != v2)
    return -1;

  return 0;
}

static int
scm_fixnum_quo_rem(scm_sword_t dvd, scm_sword_t dvr,
                   scm_sword_t *quo, scm_sword_t *rem)
{
  scm_sword_t x, y, q, r;
  char x_s, y_s;

  if (dvr == 0) {
    scm_capi_error("divided by zero", 0);
    return -1;
  }

  if (dvd == 0) {
    if (quo != NULL) *quo = 0;
    if (rem != NULL) *rem = dvr;
    return 0;
  }

  if (dvd >= 0) {
    x = dvd; x_s = '+';
  }
  else {
    x = -dvd; x_s = '-';
  }

  if (dvr >= 0) {
    y = dvr; y_s = '+';
  }
  else {
    y = -dvr; y_s = '-';
  }

  if (quo != NULL) {
    q = x / y;
    *quo = (x_s == y_s) ? q : -q;
  }

  if (rem != NULL) {
    r = x % y;
    *rem = (x_s == '+') ? r : -r;
  }

  return 0;
}

ScmObj
scm_fixnum_copy(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return fn;
}

bool
scm_fixnum_complex_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_real_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_rational_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_integer_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_exact_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_inexact_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return false;
}

bool
scm_fixnum_finite_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return true;
}

bool
scm_fixnum_infinite_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return false;
}

bool
scm_fixnum_nan_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return false;
}

int
scm_fixnum_cmp(ScmObj fn, ScmObj num, int *cmp)
{
  scm_sword_t v1, v2;

  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(num));

  if (!scm_capi_fixnum_p(num)) {
    SCM_STACK_FRAME_PUSH(&fn, &num);

    fn = SCM_NUM_CALL_FUNC(num, coerce, fn);
    if (scm_obj_null_p(fn)) return -1;

    return SCM_NUM_CALL_FUNC(fn, cmp, num, cmp);
  }

  if (cmp != NULL) {
    v1 = scm_fixnum_value(fn);
    v2 = scm_fixnum_value(num);

    if (v1 < v2)
      *cmp = -1;
    else if (v1 == v2)
      *cmp =  0;
    else
      *cmp = 1;
  }

  return 0;
}

bool
scm_fixnum_zero_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return (fn == SCM_FIXNUM_ZERO) ? true : false;
}

bool
scm_fixnum_positive_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return (scm_fixnum_value(fn) >= 0) ? true : false;
}

bool
scm_fixnum_negative_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return (scm_fixnum_value(fn) < 0) ? true : false;
}

bool
scm_fixnum_odd_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return ((fn & (0x01u << SCM_FIXNUM_SHIFT_BIT)) != 0) ? true : false;
}

bool
scm_fixnum_even_p(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return ((fn & (0x01u << SCM_FIXNUM_SHIFT_BIT)) == 0) ? true : false;
}

ScmObj
scm_fixnum_invert_sign(ScmObj fn)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);

  return scm_capi_make_number_from_sword(-scm_fixnum_value(fn));
}

ScmObj
scm_fixnum_plus(ScmObj aug, ScmObj add)
{
  SCM_STACK_FRAME_PUSH(&add);

  scm_assert_obj_type(aug, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(add));

  if (scm_capi_fixnum_p(add)) {
    scm_sword_t v = scm_fixnum_value(aug) + scm_fixnum_value(add);

    if (v < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < v)
      return scm_bignum_new_from_sword(SCM_MEM_HEAP, v);
    else
      return scm_fixnum_new(v);
  }
  else {
    aug = SCM_NUM_CALL_FUNC(add, coerce, aug);
    if (scm_obj_null_p(aug)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(aug, plus, add);
  }
}

ScmObj
scm_fixnum_minus(ScmObj min, ScmObj sub)
{
  SCM_STACK_FRAME_PUSH(&sub);

  scm_assert_obj_type(min, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(sub));

  if (scm_capi_fixnum_p(sub)) {
    scm_sword_t v = scm_fixnum_value(min) - scm_fixnum_value(sub);

    if (v < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < v)
      return scm_bignum_new_from_sword(SCM_MEM_HEAP, v);
    else
      return scm_fixnum_new(v);
  }
  else {
    min = SCM_NUM_CALL_FUNC(sub, coerce, min);
    if (scm_obj_null_p(min)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(min, minus, sub);
  }
}

ScmObj
scm_fixnum_mul(ScmObj mud, ScmObj mur)
{
  scm_sword_t v, v1, v2;

  SCM_STACK_FRAME_PUSH(&mur);

  scm_assert_obj_type(mud, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(mur));

  v1 = scm_fixnum_value(mud);
  if (v1 == 0) return mud;

  if (scm_capi_fixnum_p(mur)) {
    v2 = scm_fixnum_value(mur);

    if (scm_fixnum_multi(v1, v2, &v) == -1) {
      mud = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, mud);
      if (scm_obj_null_p(mud)) return SCM_OBJ_NULL;

      mur = scm_bignum_new_from_fixnum(SCM_MEM_HEAP, mur);
      if (scm_obj_null_p(mud)) return SCM_OBJ_NULL;

      return scm_bignum_mul(mud, mur);
    }

    return scm_fixnum_new(v);
  }
  else {
    mud = SCM_NUM_CALL_FUNC(mur, coerce, mud);
    if (scm_obj_null_p(mud)) return SCM_OBJ_NULL;

    return SCM_NUM_CALL_FUNC(mud, mul, mur);
  }
}

int
scm_fixnum_floor_div(ScmObj dvd, ScmObj dvr,
                     scm_csetter_t *quo, scm_csetter_t *rem)
{
  scm_sword_t x, y, q, r;
  char x_s, y_s;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvr);

  scm_assert_obj_type(dvd, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (!scm_capi_fixnum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return -1;

    return SCM_NUM_CALL_FUNC(dvd, floor_div, dvr, quo, rem);
  }

  x = scm_fixnum_value(dvd);
  y = scm_fixnum_value(dvr);

  x_s = (x >= 0) ? '+' : '-';
  y_s = (y >= 0) ? '+' : '-';

  rslt = scm_fixnum_quo_rem(x, y, &q, &r);
  if (rslt < 0) return -1;

  if (r != 0 && x_s != y_s) {
    q--;
    r += y;
  }

  if (quo != NULL)
    scm_csetter_setq(quo, scm_fixnum_new(q));

  if (rem != NULL)
    scm_csetter_setq(rem, scm_fixnum_new(r));

  return 0;
}

int
scm_fixnum_ceiling_div(ScmObj dvd, ScmObj dvr,
                       scm_csetter_t *quo, scm_csetter_t *rem)
{
  scm_sword_t x, y, q, r;
  char x_s, y_s;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvr);

  scm_assert_obj_type(dvd, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (!scm_capi_fixnum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return -1;

    return SCM_NUM_CALL_FUNC(dvd, ceiling_div, dvr, quo, rem);
  }

  x = scm_fixnum_value(dvd);
  y = scm_fixnum_value(dvr);

  x_s = (x >= 0) ? '+' : '-';
  y_s = (y >= 0) ? '+' : '-';

  rslt = scm_fixnum_quo_rem(x, y, &q, &r);
  if (rslt < 0) return -1;

  if (r != 0 && x_s == y_s) {
    q++;
    r -= y;
  }

  if (quo != NULL)
    scm_csetter_setq(quo, scm_fixnum_new(q));

  if (rem != NULL)
    scm_csetter_setq(rem, scm_fixnum_new(r));

  return 0;
}

int
scm_fixnum_truncate_div(ScmObj dvd, ScmObj dvr,
                        scm_csetter_t *quo, scm_csetter_t *rem)
{
  scm_sword_t x, y, q, r;
  int rslt;

  SCM_STACK_FRAME_PUSH(&dvr);

  scm_assert_obj_type(dvd, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_capi_number_p(dvr));

  if (!scm_capi_fixnum_p(dvr)) {
    dvd = SCM_NUM_CALL_FUNC(dvr, coerce, dvd);
    if (scm_obj_null_p(dvd)) return -1;

    return SCM_NUM_CALL_FUNC(dvd, truncate_div, dvr, quo, rem);
  }

  x = scm_fixnum_value(dvd);
  y = scm_fixnum_value(dvr);

  rslt = scm_fixnum_quo_rem(x, y, &q, &r);
  if (rslt < 0) return -1;

  if (quo != NULL)
    scm_csetter_setq(quo, scm_fixnum_new(q));

  if (rem != NULL)
    scm_csetter_setq(rem, scm_fixnum_new(r));

  return 0;
}


ScmObj
scm_fixnum_coerce(ScmObj fn, ScmObj num)
{
  scm_assert_obj_type(fn, &SCM_FIXNUM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(num));

  scm_capi_error("undefined arithmetic operation pattern", 2, fn, num);
  return SCM_OBJ_NULL;
}

int
scm_fixnum_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  char cstr[32];
  int rslt;

  scm_assert_obj_type(obj, &SCM_FIXNUM_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "%lld",
           (long long)SCM_RSHIFT_ARITH((scm_sword_t)obj, SCM_FIXNUM_SHIFT_BIT));

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_UTF8, port);
  if (rslt < 0) return -1;

  return 0;
}
