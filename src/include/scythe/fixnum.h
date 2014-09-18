#ifndef INCLUDE_FIXNUM_H__
#define INCLUDE_FIXNUM_H__

#include "scythe/object.h"
#include "scythe/api_type.h"
#include "scythe/impl_utils.h"

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

#define SCM_FIXNUM_SHIFT_BIT 1
#define SCM_FIXNUM_MAX (SCM_SWORD_MAX >> SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_MIN (SCM_RSHIFT_ARITH(SCM_SWORD_MIN, SCM_FIXNUM_SHIFT_BIT))
#define SCM_FIXNUM_BITS (sizeof(scm_sword_t) * CHAR_BIT - SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_ZERO ((0 << SCM_FIXNUM_SHIFT_BIT) + 1)
#define SCM_FIXNUM_ONE ((1 << SCM_FIXNUM_SHIFT_BIT) + 1)

inline ScmObj
scm_fixnum_new(scm_sword_t num)
{
  scm_assert(num >= SCM_FIXNUM_MIN);
  scm_assert(num <= SCM_FIXNUM_MAX);

  num <<= SCM_FIXNUM_SHIFT_BIT;

  return SCM_OBJ(num + 1);
}

inline scm_sword_t
scm_fixnum_value(ScmObj num)
{
  scm_assert_obj_type(num, &SCM_FIXNUM_TYPE_INFO);

  return SCM_RSHIFT_ARITH((scm_sword_t)num, SCM_FIXNUM_SHIFT_BIT);
}

ScmObj scm_fixnum_copy(ScmObj fn);
bool scm_fixnum_complex_p(ScmObj fn);
bool scm_fixnum_real_p(ScmObj fn);
bool scm_fixnum_rational_p(ScmObj fn);
bool scm_fixnum_integer_p(ScmObj fn);
bool scm_fixnum_exact_p(ScmObj fn);
bool scm_fixnum_inexact_p(ScmObj fn);
bool scm_fixnum_finite_p(ScmObj fn);
bool scm_fixnum_infinite_p(ScmObj fn);
bool scm_fixnum_nan_p(ScmObj fn);
int scm_fixnum_cmp(ScmObj fn, ScmObj num, int *cmp);
bool scm_fixnum_zero_p(ScmObj num);
bool scm_fixnum_positive_p(ScmObj fn);
bool scm_fixnum_negative_p(ScmObj fn);
bool scm_fixnum_odd_p(ScmObj fn);
bool scm_fixnum_even_p(ScmObj fn);
ScmObj scm_fixnum_invert_sign(ScmObj fn);
ScmObj scm_fixnum_plus(ScmObj aug, ScmObj add);
ScmObj scm_fixnum_minus(ScmObj min, ScmObj sub);
ScmObj scm_fixnum_mul(ScmObj mud, ScmObj mur);
int scm_fixnum_floor_div(ScmObj dvd, ScmObj dvr,
                         scm_csetter_t *quo, scm_csetter_t *rem);
int scm_fixnum_ceiling_div(ScmObj dvd, ScmObj dvr,
                           scm_csetter_t *quo, scm_csetter_t *rem);
int scm_fixnum_truncate_div(ScmObj dvd, ScmObj dvr,
                            scm_csetter_t *quo, scm_csetter_t *rem);

ScmObj scm_fixnum_coerce(ScmObj fn, ScmObj num);

int scm_fixnum_obj_print(ScmObj obj, ScmObj port, bool ext_rep);


#endif  /*  INCLUDE_FIXNUM_H__ */
