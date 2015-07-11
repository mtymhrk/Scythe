#ifndef INCLUDE_IMPL_UTILS_H__
#define INCLUDE_IMPL_UTILS_H__

#include <limits.h>
#include <stdlib.h>
#include <errno.h>

#include "scythe/config.h"

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

#include <stdio.h>

extern int scm_debug_print_flag;

#define scm_debug(...)                                                  \
  do {                                                                  \
    if (scm_debug_print_flag) {                                         \
      fprintf(stderr, "%s:%u:%s: ", __FILE__, __LINE__, __func__);      \
      fprintf(stderr, __VA_ARGS__);                                     \
      fputc('\n', stderr);                                              \
      fflush(stderr);                                                   \
    }                                                                   \
  } while (0)

#ifdef HAVE_EXECINFO_H

#include <execinfo.h>

static inline void
scm_print_backtrace(int fd)
{
  void *trace[256];
  int n = backtrace(trace, sizeof(trace) / sizeof(trace[0]));
  backtrace_symbols_fd(trace, n, fd);
}

#else  /* !HAVE_EXECINFO_H */

#include <unistd.h>

static inline void
scm_print_backtrace(int fd)
{
  static const char msg[] =
    "failed to print the backtrace: backtrace() is not provided\n";
  write(fd, msg, sizeof(msg) - 1);
}

#endif  /* HAVE_EXECINFO_H */

void scm_assert_fail(const char *assertion,
                     const char *file, unsigned int line, const char *function)
  __attribute__((noreturn));

#ifdef SCM_DEBUG

#define scm_assert(expr)                                        \
  ((expr)                                                       \
   ? (void)(0)                                                  \
   : scm_assert_fail(#expr, __FILE__, __LINE__, __func__))

#else  /* !SCM_DEBUG */

#define scm_assert(...)

#endif  /* SCM_DEBUG */

#endif /* INCLUDE_IMPL_UTILS_H__ */
