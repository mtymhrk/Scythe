#include <stdio.h>

#include "scythe/api.h"

int
main(int argc, char **argv)
{
  ScmEvaluator *ev;
  int rslt;

  if (argc < 2) {
    fprintf(stderr, "%s: too few arguments\n", argv[0]);
    fprintf(stderr, "Usage: %s <file>\n", argv[0]);
    return -1;
  }

  ev = scm_capi_evaluator();
  if (ev == NULL) return -1;

  rslt = scm_capi_compile_file(argv[1], ev);

  scm_capi_evaluator_end(ev);

  return (rslt < 0) ? -1 : 0;
}
