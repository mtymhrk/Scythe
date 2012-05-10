#ifndef INCLUDE_NUMERIC_H__
#define INCLUDE_NUMERIC_H__

typedef struct ScmBignumRec ScmBignum;

#define SCM_BIGNUM(obj) ((ScmBignum *)(obj))

#include "object.h"
#include "earray.h"
#include "impl_utils.h"

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

ScmObj scm_fixnum_plus(ScmObj fn1, ScmObj fn2);
ScmObj scm_fixnum_minus(ScmObj fn1, ScmObj fn2);
ScmObj scm_fixnum_mul(ScmObj fn1, ScmObj fn2);

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
ScmObj scm_bignum_plus(ScmObj bn1, ScmObj bn2);
ScmObj scm_bignum_minus(ScmObj bn1, ScmObj bn2);
ScmObj scm_bignum_mul(ScmObj bn1, ScmObj bn2);
int scm_bignum_div(ScmObj bn1, ScmObj bn2,
                   scm_csetter_t *quo, scm_csetter_t *rem);

int scm_bignum_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_bignum_gc_initialize(ScmObj obj, ScmObj mem);
void scm_bignum_gc_finalize(ScmObj obj);


/***************************************************************************/
/*  Parser for Literal                                                     */
/***************************************************************************/

ScmObj scm_num_make_from_literal(const char *literal, size_t size);

#endif /* INCLUDE_NUMERIC_H__ */
