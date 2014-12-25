#include <stdlib.h>

#include "scythe/impl_utils.h"


void
scm_assert_fail(const char *assertion,
                const char *file, unsigned int line, const char *function)
{
  fprintf(stderr, "%s:%u: %s: Assertion `%s' failed.\n",
          file, line, function, assertion);
  fprintf(stderr, "=== Backtrace ====================\n");
  fflush(stderr);
  scm_print_backtrace(2);
  fprintf(stderr, "==================================\n");
  abort();
}
