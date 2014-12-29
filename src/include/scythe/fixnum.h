#ifndef INCLUDE_FIXNUM_H__
#define INCLUDE_FIXNUM_H__

#include "scythe/object.h"
#include "scythe/impl_utils.h"

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

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

int scm_fixnum_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);


#endif  /*  INCLUDE_FIXNUM_H__ */
