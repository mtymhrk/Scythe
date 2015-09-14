#ifndef INCLUDE_BIGNUM_H__
#define INCLUDE_BIGNUM_H__

#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/earray.h"
#include "scythe/impl_utils.h"
#include "scythe/memory.h"

#if SIZEOF_LONG > SIZEOF_INT

typedef unsigned int scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UINT_MAX + 1)

#elif SIZEOF_LLONG > SIZEOF_INT

typedef unsigned int scm_bignum_d_t;
typedef unsigned long long scm_bignum_c_t;
typedef long long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UINT_MAX + 1)

#elif SIZEOF_LONG > SIZEOF_SHORT

typedef unsigned short scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)USHORT_MAX + 1)

#else

typedef unsigned char scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UCHAR_MAX + 1)

#endif

typedef struct ScmBignumRec ScmBignum;

struct ScmBignumRec {
  ScmObjHeader header;
  char sign;
  size_t nr_digits;
  EArray digits;
};

#define SCM_BIGNUM(obj) ((ScmBignum *)(obj))

extern ScmTypeInfo SCM_BIGNUM_TYPE_INFO;

int scm_bignum_calc_base_and_place_for_ary_of_digits(int radix,
                                                     scm_bignum_c_t *base,
                                                     int *place);

ScmObj scm_bignum_P(ScmObj obj);
ScmObj scm_bignum_make_int_from_ary(char sign,
                                    scm_bignum_d_t *ary, size_t size,
                                    scm_bignum_c_t base);

int scm_bignum_initialize_ary(ScmObj bignum,
                              char sign, scm_bignum_d_t *digits, size_t len,
                              scm_bignum_c_t base);
int scm_bignum_initialize_uword(ScmObj bignum, scm_uword_t val);
int scm_bignum_initialize_sword(ScmObj bignum, scm_sword_t val);
void scm_bignum_finalize(ScmObj bignum);
ScmObj scm_bignum_new_cv(scm_mem_type_t mtype, char sign,
                         scm_bignum_d_t *digits, size_t len,
                         scm_bignum_c_t base);
ScmObj scm_bignum_new_sword(scm_mem_type_t mtype, scm_sword_t val);
ScmObj scm_bignum_new_uword(scm_mem_type_t mtype, scm_uword_t val);
ScmObj scm_bignum_new_fixnum(scm_mem_type_t mtype, ScmObj fn);
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

int scm_bignum_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_bignum_gc_initialize(ScmObj obj);
void scm_bignum_gc_finalize(ScmObj obj);


static inline bool
scm_bignum_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BIGNUM_TYPE_INFO);
}


#endif  /* INCLUDE_BIGNUM_H__ */
