#ifndef INCLUDE_NUMERIC_H__
#define INCLUDE_NUMERIC_H__

typedef struct ScmBignumRec ScmBignum;
typedef struct ScmNumVFuncRec ScmNumVFunc;

#define SCM_BIGNUM(obj) ((ScmBignum *)(obj))

#include "object.h"
#include "api_enum.h"
#include "earray.h"
#include "impl_utils.h"


/***************************************************************************/
/*  Common                                                                 */
/***************************************************************************/

struct ScmNumVFuncRec {
  ScmObj (*coerce)(ScmObj obj, ScmObj num);
  bool (*integer_p)(ScmObj obj);
  int (*cmp)(ScmObj x, ScmObj y, int *cmp);
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


#define SCM_NUM_CALL_VFUNC(obj, func, ...) \
  ((ScmNumVFunc *)scm_obj_type_extra(obj))->func(obj, ## __VA_ARGS__)


/***************************************************************************/
/*  Fixnum                                                                 */
/***************************************************************************/

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

#define SCM_FIXNUM_SHIFT_BIT 1
#define SCM_FIXNUM_MAX (SCM_SWORD_MAX >> SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_MIN (SCM_RSHIFT_ARITH(SCM_SWORD_MIN, SCM_FIXNUM_SHIFT_BIT))
#define SCM_FIXNUM_BITS (sizeof(scm_sword_t) * CHAR_BIT - SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_ZERO ((0 << SCM_FIXNUM_SHIFT_BIT) + 1)

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

  return scm_rshift_arith_sword((scm_sword_t)num, SCM_FIXNUM_SHIFT_BIT);
}

inline bool
scm_fixnum_zero_p(ScmObj num)
{
  return (num == SCM_FIXNUM_ZERO);
}

bool scm_fixnum_integer_p(ScmObj fn);
int scm_fixnum_cmp(ScmObj fn, ScmObj num, int *cmp);
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

int scm_fixnum_pretty_print(ScmObj obj, ScmObj port, bool write_p);


/***************************************************************************/
/*  Bignum                                                                 */
/***************************************************************************/


#if ULONG_MAX > UINT_MAX * UINT_MAX

typedef unsigned int scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UINT_MAX + 1)

#elif ULLONG_MAX > UINT_MAX * UINT_MAX

typedef unsigned int scm_bignum_d_t;
typedef unsigned long long scm_bignum_c_t;
typedef long long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE (UINT_MAX + 1);

#elif ULONG_MAX > USHORT_MAX

typedef unsigned short scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE (USHORT_MAX + 1);

#else

typedef unsigned short scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE;

#endif

struct ScmBignumRec {
  ScmObjHeader header;
  char sign;
  size_t nr_digits;
  EArray digits;
};

extern ScmTypeInfo SCM_BIGNUM_TYPE_INFO;


void scm_bignum_finalize_ary(ScmObj bignum);
int scm_bignum_initialize_sword(ScmObj bignum, scm_sword_t val);
ScmObj scm_bignum_new_from_ary(SCM_MEM_TYPE_T mtype, char sign,
                               scm_bignum_d_t *digits, size_t len,
                               scm_bignum_c_t base);
ScmObj scm_bignum_new_from_sword(SCM_MEM_TYPE_T mtype, scm_sword_t val);
ScmObj scm_bignum_new_from_fixnum(SCM_MEM_TYPE_T mtype, ScmObj fn);
ScmObj scm_bignum_copy(ScmObj bignum);
int scm_bignum_to_sword(ScmObj bn, scm_sword_t *w);
int scm_bignum_to_size_t(ScmObj bn, size_t *s);
bool scm_bignum_integer_p(ScmObj bn);
int scm_bignum_cmp(ScmObj bn, ScmObj num, int *cmp);
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

int scm_bignum_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_bignum_gc_initialize(ScmObj obj, ScmObj mem);
void scm_bignum_gc_finalize(ScmObj obj);


/***************************************************************************/
/*  Parser for Literal                                                     */
/***************************************************************************/

ScmObj scm_num_make_from_literal(const char *literal, size_t size);

#endif /* INCLUDE_NUMERIC_H__ */
