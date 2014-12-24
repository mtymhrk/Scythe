#ifndef INCLUDE_FCD_NUMBER_H__
#define INCLUDE_FCD_NUMBER_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd_type.h"

bool scm_fcd_fixnum_p(ScmObj obj);
ScmObj scm_fcd_fixnum_P(ScmObj obj);
ScmObj scm_fcd_fixnum_new(scm_sword_t num);
bool scm_fcd_bignum_p(ScmObj obj);
ScmObj scm_fcd_bignum_P(ScmObj obj);
ScmObj scm_fcd_bignum_new_cv(SCM_MEM_TYPE_T mtype, char sign,
                             scm_bignum_d_t *digits, size_t len,
                             scm_bignum_c_t base);
ScmObj scm_fcd_bignum_new_sword(SCM_MEM_TYPE_T mtype, scm_sword_t val);
ScmObj scm_fcd_bignum_new_uword(SCM_MEM_TYPE_T mtype, scm_uword_t val);
ScmObj scm_fcd_bignum_new_fixnum(SCM_MEM_TYPE_T mtype, ScmObj fn);
bool scm_fcd_number_p(ScmObj obj);
ScmObj scm_fcd_number_P(ScmObj obj);
bool scm_fcd_complex_p(ScmObj obj);
ScmObj scm_fcd_complex_P(ScmObj obj);
bool scm_fcd_real_p(ScmObj obj);
ScmObj scm_fcd_real_P(ScmObj obj);
bool scm_fcd_rational_p(ScmObj obj);
ScmObj scm_fcd_rational_P(ScmObj obj);
bool scm_fcd_integer_p(ScmObj obj);
ScmObj scm_fcd_integer_P(ScmObj obj);
bool scm_fcd_exact_p(ScmObj obj);
ScmObj scm_fcd_exact_P(ScmObj obj);
bool scm_fcd_inexact_p(ScmObj obj);
ScmObj scm_fcd_inexact_P(ScmObj obj);
bool scm_fcd_exact_integer_p(ScmObj obj);
ScmObj scm_fcd_exact_integer_P(ScmObj obj);
bool scm_fcd_finite_p(ScmObj obj);
ScmObj scm_fcd_finite_P(ScmObj obj);
bool scm_fcd_infinite_p(ScmObj obj);
ScmObj scm_fcd_infinite_P(ScmObj obj);
bool scm_fcd_nan_p(ScmObj obj);
ScmObj scm_fcd_nan_P(ScmObj obj);
ScmObj scm_fcd_make_number_from_literal(const void *literal, ScmEncoding *enc);
ScmObj scm_fcd_make_number_from_sword(scm_sword_t num);
ScmObj scm_fcd_make_number_from_size_t(size_t num);
int scm_fcd_num_eq(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_fcd_num_eq_P_lst(ScmObj lst);
ScmObj scm_fcd_num_eq_P(ScmObj n1, ScmObj n2);
int scm_fcd_num_lt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_fcd_num_lt_P_lst(ScmObj lst);
ScmObj scm_fcd_num_lt_P(ScmObj n1, ScmObj n2);
int scm_fcd_num_gt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_fcd_num_gt_P_lst(ScmObj lst);
ScmObj scm_fcd_num_gt_P(ScmObj n1, ScmObj n2);
int scm_fcd_num_le(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_fcd_num_le_P_lst(ScmObj lst);
ScmObj scm_fcd_num_le_P(ScmObj n1, ScmObj n2);
int scm_fcd_num_ge(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_fcd_num_ge_P_lst(ScmObj lst);
ScmObj scm_fcd_num_ge_P(ScmObj n1, ScmObj n2);
bool scm_fcd_zero_p(ScmObj num);
ScmObj scm_fcd_zero_P(ScmObj num);
bool scm_fcd_positive_p(ScmObj num);
ScmObj scm_fcd_positive_P(ScmObj num);
bool scm_fcd_negative_p(ScmObj num);
ScmObj scm_fcd_negative_P(ScmObj num);
bool scm_fcd_odd_p(ScmObj num);
ScmObj scm_fcd_odd_P(ScmObj num);
bool scm_fcd_even_p(ScmObj num);
ScmObj scm_fcd_even_P(ScmObj num);
ScmObj scm_fcd_max(ScmObj n1, ScmObj n2);
ScmObj scm_fcd_max_lst(ScmObj lst);
ScmObj scm_fcd_min(ScmObj n1, ScmObj n2);
ScmObj scm_fcd_min_lst(ScmObj lst);
ScmObj scm_fcd_plus(ScmObj x, ScmObj y);
ScmObj scm_fcd_plus_lst(ScmObj lst);
ScmObj scm_fcd_mul(ScmObj x, ScmObj y);
ScmObj scm_fcd_mul_lst(ScmObj lst);
ScmObj scm_fcd_minus(ScmObj x, ScmObj y);
ScmObj scm_fcd_minus_lst(ScmObj lst);
ScmObj scm_fcd_abs(ScmObj num);
int scm_fcd_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_fcd_floor_quo(ScmObj x, ScmObj y);
ScmObj scm_fcd_floor_rem(ScmObj x, ScmObj y);
int scm_fcd_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_fcd_truncate_quo(ScmObj x, ScmObj y);
ScmObj scm_fcd_truncate_rem(ScmObj x, ScmObj y);
ScmObj scm_fcd_exact(ScmObj num);
ScmObj scm_fcd_inexact(ScmObj num);
int scm_fcd_integer_to_sword(ScmObj num, scm_sword_t *w);
int scm_fcd_integer_to_size_t(ScmObj num, size_t *s);

#endif  /* INCLUDE_FCD_NUMBER_H__ */
