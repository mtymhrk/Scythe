#include "api.h"

int
main(int argc, char **argv)
{
  ScmEvaluator *ev;
  int rslt;

  ev = scm_capi_evaluator();
  if (ev == NULL) return -1;

  rslt = scm_capi_run_repl(ev);
  if (rslt < 0) {
    scm_capi_evaluator_end(ev);
    return -1;
  }

  scm_capi_evaluator_end(ev);

  return 0;
}
