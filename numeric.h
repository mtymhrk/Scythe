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
#define SCM_FIXNUM_BITS (sizeof(scm_sword_t) * 8 - 2)

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

int scm_fixnum_pretty_print(ScmObj obj, ScmObj port, bool write_p);


/***************************************************************************/
/*  Bignum                                                                 */
/***************************************************************************/

#if 0

#define SCM_BIGNUM_BASE ((UINT_MAX >> 1) + 1);

enum { SCM_BIGNUM_SIGN_PLUS = 1, SCM_BIGNUM_SIGN_MINUS = -1 };

typedef unsigned int scm_bignum_d_t;

struct ScmBignumRec {
  ScmObjHeader header;
  int sign;
  size_t nr_digit;
  EArray digits;
};

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

void scm_bignum_initialize(ScmObj bignum, int sign,
                           size_t nr_digit, const scm_bignum_d_t *digits);
void scm_bignum_finalize(ScmObj bignum);

void scm_bignum_gc_initialize(ScmObj obj, ScmObj mem);
void scm_bignum_gc_finalize(ScmObj obj);

#endif


/***************************************************************************/
/*  Parser for Literal                                                     */
/***************************************************************************/

ScmObj scm_num_make_from_literal(const char *literal, size_t size);

#endif /* INCLUDE_NUMERIC_H__ */
