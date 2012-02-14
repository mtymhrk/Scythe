#ifndef INCLUDE_IMPL_UTILS_H__
#define INCLUDE_IMPL_UTILS_H__

#include <limits.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>

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


#define SCM_SYSCALL(rslt, expr)                 \
  do {                                            \
    (rslt) = (expr);                            \
  } while (rslt < 0 && errno == EINTR)


#ifdef SCM_UNIT_TEST
#  define scm_local_func
#  define scm_local_inline extern inline
#else
#  define scm_local_func static
#  define scm_local_inline static inline
#endif

#endif /* INCLUDE_IMPL_UTILS_H__ */
