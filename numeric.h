#ifndef INCLUDE_NUMERIC_H__
#define INCLUDE_NUMERIC_H__

#include "object.h"
#include "impl_utils.h"

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

#define SCM_FIXNUM_SHIFT_BIT 1
#define SCM_FIXNUM_MAX (SCM_SWORD_MAX >> SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_MIN (SCM_RSHIFT_ARITH(SCM_SWORD_MIN, SCM_FIXNUM_SHIFT_BIT))

static inline ScmObj
scm_fixnum_new(scm_sword_t num)
{
  scm_assert(num >= SCM_FIXNUM_MIN);
  scm_assert(num <= SCM_FIXNUM_MAX);

  num <<= SCM_FIXNUM_SHIFT_BIT;

  return SCM_OBJ(num + 1);
}

static inline scm_sword_t
scm_fixnum_value(ScmObj num)
{
  scm_assert_obj_type(num, &SCM_FIXNUM_TYPE_INFO);

  return scm_rshift_arith_sword((scm_sword_t)num, SCM_FIXNUM_SHIFT_BIT);
}

#endif /* INCLUDE_NUMERIC_H__ */
