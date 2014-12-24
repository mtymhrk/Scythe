#ifndef INCLUDE_BIGNUM_H__
#define INCLUDE_BIGNUM_H__

#include <stddef.h>
#include <limits.h>

typedef struct ScmBignumRec ScmBignum;

#define SCM_BIGNUM(obj) ((ScmBignum *)(obj))

#include "scythe/object.h"
#include "scythe/fcd_type.h"
#include "scythe/earray.h"
#include "scythe/impl_utils.h"

struct ScmBignumRec {
  ScmObjHeader header;
  char sign;
  size_t nr_digits;
  EArray digits;
};

extern ScmTypeInfo SCM_BIGNUM_TYPE_INFO;

int scm_bignum_calc_base_and_place_for_ary_of_digits(int radix,
                                                     scm_bignum_c_t *base,
                                                     int *place);
ScmObj scm_bignum_make_int_from_ary(char sign,
                                    scm_bignum_d_t *ary, size_t size,
                                    scm_bignum_c_t base);

int scm_bignum_initialize_ary(ScmObj bignum,
                              char sign, scm_bignum_d_t *digits, size_t len,
                              scm_bignum_c_t base);
int scm_bignum_initialize_uword(ScmObj bignum, scm_uword_t val);
int scm_bignum_initialize_sword(ScmObj bignum, scm_sword_t val);
ScmObj scm_bignum_copy(ScmObj bignum);
int scm_bignum_to_sword(ScmObj bn, scm_sword_t *w);
int scm_bignum_to_size_t(ScmObj bn, size_t *s);
bool scm_bignum_complex_p(ScmObj bn);
bool scm_bignum_real_p(ScmObj bn);
bool scm_bignum_rational_p(ScmObj bn);
bool scm_bignum_integer_p(ScmObj bn);
bool scm_bignum_exact_p(ScmObj bn);
bool scm_bignum_inexact_p(ScmObj bn);
bool scm_bignum_finite_p(ScmObj bn);
bool scm_bignum_infinite_p(ScmObj bn);
bool scm_bignum_nan_p(ScmObj bn);
int scm_bignum_cmp(ScmObj bn, ScmObj num, int *cmp);
bool scm_bignum_zero_p(ScmObj bn);
bool scm_bignum_positive_p(ScmObj bn);
bool scm_bignum_negative_p(ScmObj bn);
bool scm_bignum_odd_p(ScmObj bn);
bool scm_bignum_even_p(ScmObj bn);
ScmObj scm_bignum_invert_sign(ScmObj bn);
ScmObj scm_bignum_plus(ScmObj aug, ScmObj add);
ScmObj scm_bignum_minus(ScmObj min, ScmObj sub);
ScmObj scm_bignum_mul(ScmObj mud, ScmObj mur);
int scm_bignum_floor_div(ScmObj dvd, ScmObj dvr,
                         scm_csetter_t *quo, scm_csetter_t *rem);
int scm_bignum_ceiling_div(ScmObj dvd, ScmObj dvr,
                           scm_csetter_t *quo, scm_csetter_t *rem);
int scm_bignum_truncate_div(ScmObj dvd, ScmObj dvr,
                            scm_csetter_t *quo, scm_csetter_t *rem);

ScmObj scm_bignum_coerce(ScmObj bn, ScmObj num);

int scm_bignum_obj_print(ScmObj obj, ScmObj port, bool ext_rep);
void scm_bignum_gc_initialize(ScmObj obj, ScmObj mem);
void scm_bignum_gc_finalize(ScmObj obj);


#endif  /* INCLUDE_BIGNUM_H__ */
