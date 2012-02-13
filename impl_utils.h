#ifndef INCLUDE_IMPL_UTILS_H__
#define INCLUDE_IMPL_UTILS_H__

#include <limits.h>
#include <assert.h>
#include <stdlib.h>

#if (((~0) >> 1) == ~0) /* 符号付き整数の右シフトが算術シフトか */
  #define SCM_RSHIFT_ARITH(x, y) ((x) >> (y))
#else
  #define SCM_RSHIFT_ARITH(x, y) (((x) < 0) ? ~((~(x)) >> y) : (x) >> (y))
#endif

inline scm_sword_t
scm_rshift_arith_sword(scm_sword_t x, unsigned int y)
{
  assert(y < (sizeof(scm_sword_t) * CHAR_BIT));
#if (((~0) >> 1) == ~0)
  return x >> y;
#else
  return (x < 0) ? ~((~x) >> y) : x >> y;
#endif
}


#define SCM_CONCAT_SYMBOL_2__(x, y) x##y
#define SCM_CONCAT_SYMBOL__(x, y) SCM_CONCAT_SYMBOL_2__(x, y)


#endif /* INCLUDE_IMPL_UTILS_H__ */
