#ifndef INCLUDE_IMPL_UTILS_H__
#define INCLUDE_IMPL_UTILS_H__

#include <limits.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>

#include <execinfo.h>

#define SCM_SHRT_BIT (sizeof(short) * CHAR_BIT)
#define SCM_INT_BIT  (sizeof(int) * CHAR_BIT)

#if (((~0) >> 1) == ~0) /* 符号付き整数の右シフトが算術シフトか */
  #define SCM_RSHIFT_ARITH(x, y) ((x) >> (y))
#else
  #define SCM_RSHIFT_ARITH(x, y) (((x) < 0) ? ~((~(x)) >> y) : (x) >> (y))
#endif


#define SCM_CONCAT_SYMBOL_2__(x, y) x##y
#define SCM_CONCAT_SYMBOL__(x, y) SCM_CONCAT_SYMBOL_2__(x, y)


#define SCM_SYSCALL(rslt, expr)                 \
  do {                                            \
    (rslt) = (expr);                            \
  } while (rslt < 0 && errno == EINTR)


#define SCM_ALIGNOF(type) offsetof(struct { char a; type b; }, b)

#ifdef SCM_UNIT_TEST
#  define scm_local_func
#  define scm_local_inline extern inline
#else
#  define scm_local_func static
#  define scm_local_inline static inline
#endif

#include <stdio.h>

#define scm_debug(...)                                           \
  do {                                                           \
    fprintf(stderr, "%s:%u:%s: ", __FILE__, __LINE__, __func__); \
    fprintf(stderr, __VA_ARGS__);                                \
    fputc('\n', stderr);                                         \
    fflush(stderr);                                              \
  } while (0)

inline void
scm_print_backtrace(int fd)
{
  void *trace[256];
  int n = backtrace(trace, sizeof(trace) / sizeof(trace[0]));
  backtrace_symbols_fd(trace, n, fd);
}

inline unsigned long
scm_pow_ul(unsigned long x, unsigned long y)
{
  unsigned long z = 1;
  for (unsigned long i = 0; i < y; i++)
    z *= x;
  return z;
}

inline unsigned long
scm_log_ul(unsigned long b, unsigned long x)
{
  unsigned long i, z = x;
  for (i = 0; z > b; i++)
    z /= b;
  return i;
}

#endif /* INCLUDE_IMPL_UTILS_H__ */
