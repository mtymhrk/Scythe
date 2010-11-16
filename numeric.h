#ifndef INCLUDE_NUMERIC_H__
#define INCLUDE_NUMERIC_H__

#include "object.h"
#include "obuffer.h"

extern ScmTypeInfo SCM_FIXNUM_TYPE_INFO;

#define SCM_FIXNUM_SHIFT_BIT 1
#define SCM_FIXNUM_MAX (SCM_SWORD_MAX >> SCM_FIXNUM_SHIFT_BIT)
#define SCM_FIXNUM_MIN (SCM_RSHIFT_ARITH(SCM_SWORD_MIN, SCM_FIXNUM_SHIFT_BIT))

static inline ScmObj
scm_fixnum_new(scm_sword_t num)
{
  assert(num >= SCM_FIXNUM_MIN);
  assert(num <= SCM_FIXNUM_MAX);

  num <<= SCM_FIXNUM_SHIFT_BIT;

  return SCM_OBJ(num + 1);
}

static inline scm_sword_t
scm_fixnum_value(ScmObj num)
{
  SCM_OBJ_ASSERT_TYPE(num, &SCM_FIXNUM_TYPE_INFO);

  return SCM_RSHIFT_ARITH((scm_sword_t)num, SCM_FIXNUM_SHIFT_BIT);
}

void scm_fixnum_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_NUMERIC_H__ */
