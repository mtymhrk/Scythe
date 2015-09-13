#ifndef INCLUDE_NUMBER_COMMON_H__
#define INCLUDE_NUMBER_COMMON_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"

typedef struct ScmNumFuncRec ScmNumFunc;

struct ScmNumFuncRec {
  ScmObj (*coerce)(ScmObj obj, ScmObj num);
  ScmObj (*copy)(ScmObj num);
  bool (*complex_p)(ScmObj obj);
  bool (*real_p)(ScmObj obj);
  bool (*rational_p)(ScmObj obj);
  bool (*integer_p)(ScmObj obj);
  bool (*exact_p)(ScmObj obj);
  bool (*inexact_p)(ScmObj obj);
  bool (*finite_p)(ScmObj obj);
  bool (*infinite_p)(ScmObj obj);
  bool (*nan_p)(ScmObj obj);
  int (*cmp)(ScmObj x, ScmObj y, int *cmp);
  bool (*zero_p)(ScmObj num);
  bool (*positive_p)(ScmObj num);
  bool (*negative_p)(ScmObj num);
  bool (*odd_p)(ScmObj num);
  bool (*even_p)(ScmObj num);
  ScmObj (*invert_sign)(ScmObj num);
  ScmObj (*plus)(ScmObj aug, ScmObj add);
  ScmObj (*minus)(ScmObj min, ScmObj sub);
  ScmObj (*mul)(ScmObj mud, ScmObj mur);
  int (*floor_div)(ScmObj dvd, ScmObj dvr,
                   scm_csetter_t *quo, scm_csetter_t *rem);
  int (*ceiling_div)(ScmObj dvd, ScmObj dvr,
                     scm_csetter_t *quo, scm_csetter_t *rem);
  int (*truncate_div)(ScmObj dvd, ScmObj dvr,
                      scm_csetter_t *quo, scm_csetter_t *rem);
};

#define SCM_NUM_CALL_FUNC(obj, func, ...) \
  ((ScmNumFunc *)scm_obj_type_extra(obj))->func(obj, ## __VA_ARGS__)

ScmObj scm_number_P(ScmObj obj);
ScmObj scm_num_complex_P(ScmObj obj);
ScmObj scm_num_real_P(ScmObj obj);
ScmObj scm_num_rational_P(ScmObj obj);
ScmObj scm_num_integer_P(ScmObj obj);
ScmObj scm_num_exact_P(ScmObj obj);
ScmObj scm_num_inexact_P(ScmObj obj);
ScmObj scm_num_exact_integer_P(ScmObj obj);
ScmObj scm_num_finite_P(ScmObj obj);
ScmObj scm_num_infinite_P(ScmObj obj);
ScmObj scm_num_nan_P(ScmObj obj);
ScmObj scm_make_number_from_literal(const void *literal, ScmEncoding *enc);
ScmObj scm_make_number_from_sword(scm_sword_t num);
ScmObj scm_make_number_from_size_t(size_t num);
int scm_num_eq(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_num_eq_P_lst(ScmObj lst);
ScmObj scm_num_eq_P(ScmObj n1, ScmObj n2);
int scm_num_lt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_num_lt_P_lst(ScmObj lst);
ScmObj scm_num_lt_P(ScmObj n1, ScmObj n2);
int scm_num_gt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_num_gt_P_lst(ScmObj lst);
ScmObj scm_num_gt_P(ScmObj n1, ScmObj n2);
int scm_num_le(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_num_le_P_lst(ScmObj lst);
ScmObj scm_num_le_P(ScmObj n1, ScmObj n2);
int scm_num_ge(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_num_ge_P_lst(ScmObj lst);
ScmObj scm_num_ge_P(ScmObj n1, ScmObj n2);
ScmObj scm_num_zero_P(ScmObj num);
ScmObj scm_num_positive_P(ScmObj num);
ScmObj scm_num_negative_P(ScmObj num);
ScmObj scm_num_odd_P(ScmObj num);
ScmObj scm_num_even_P(ScmObj num);
ScmObj scm_num_max(ScmObj n1, ScmObj n2);
ScmObj scm_num_max_lst(ScmObj lst);
ScmObj scm_num_min(ScmObj n1, ScmObj n2);
ScmObj scm_num_min_lst(ScmObj lst);
ScmObj scm_num_plus_lst(ScmObj lst);
ScmObj scm_num_mul_lst(ScmObj lst);
ScmObj scm_num_minus_lst(ScmObj lst);
ScmObj scm_num_abs(ScmObj num);
ScmObj scm_num_floor_quo(ScmObj x, ScmObj y);
ScmObj scm_num_floor_rem(ScmObj x, ScmObj y);
ScmObj scm_num_truncate_quo(ScmObj x, ScmObj y);
ScmObj scm_num_truncate_rem(ScmObj x, ScmObj y);
ScmObj scm_num_exact(ScmObj num);
ScmObj scm_num_inexact(ScmObj num);
int scm_integer_to_sword(ScmObj num, scm_sword_t *w);
int scm_integer_to_size_t(ScmObj num, size_t *s);


static inline bool
scm_number_p(ScmObj obj)
{
  return scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_NUM);
}

static inline bool
scm_num_complex_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, complex_p);
}

static inline bool
scm_num_real_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, real_p);
}

static inline bool
scm_num_rational_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, rational_p);
}

static inline bool
scm_num_integer_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, integer_p);
}

static inline bool
scm_num_exact_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, exact_p);
}

static inline bool
scm_num_inexact_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, inexact_p);
}

static inline bool
scm_num_exact_integer_p(ScmObj obj)
{
  if (scm_num_integer_p(obj) && scm_num_exact_p(obj))
    return true;
  else
    return false;
}

static inline bool
scm_num_finite_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, finite_p);
}

static inline bool
scm_num_infinite_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, infinite_p);
}

static inline bool
scm_num_nan_p(ScmObj obj)
{
  if (!scm_number_p(obj)) return false;
  return SCM_NUM_CALL_FUNC(obj, nan_p);
}

static inline bool
scm_num_zero_p(ScmObj num)
{
  if (!scm_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, zero_p);
}

static inline bool
scm_num_positive_p(ScmObj num)
{
  if (!scm_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, positive_p);
}

static inline bool
scm_num_negative_p(ScmObj num)
{
  if (!scm_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, negative_p);
}

static inline bool
scm_num_odd_p(ScmObj num)
{
  if (!scm_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, odd_p);
}

static inline bool
scm_num_even_p(ScmObj num)
{
  if (!scm_number_p(num)) return false;
  return SCM_NUM_CALL_FUNC(num, even_p);
}

static inline ScmObj
scm_num_plus(ScmObj x, ScmObj y)
{
  scm_assert(scm_number_p(x));
  scm_assert(scm_number_p(y));
  return SCM_NUM_CALL_FUNC(x, plus, y);
}

static inline ScmObj
scm_num_mul(ScmObj x, ScmObj y)
{
  scm_assert(scm_number_p(x));
  scm_assert(scm_number_p(y));
  return SCM_NUM_CALL_FUNC(x, mul, y);
}

static inline ScmObj
scm_num_minus(ScmObj x, ScmObj y)
{
  scm_assert(scm_number_p(x));
  scm_assert(scm_number_p(y));
  return SCM_NUM_CALL_FUNC(x, minus, y);
}

static inline int
scm_num_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  scm_assert(scm_num_integer_p(x));
  scm_assert(scm_num_integer_p(y));
  return SCM_NUM_CALL_FUNC(x, floor_div, y, q, r);
}

static inline int
scm_num_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  scm_assert(scm_num_integer_p(x));
  scm_assert(scm_num_integer_p(y));
  return SCM_NUM_CALL_FUNC(x, truncate_div, y, q, r);
}


#endif  /* INCLUDE_NUMBER_COMMON_H__ */
