#include <stdio.h>

#include "scythe/api.h"

int
main(int argc, char **argv)
{
  ScmScythe *scy;
  int r, retval;

  if (argc < 2) {
    fprintf(stderr, "%s: too few arguments\n", argv[0]);
    fprintf(stderr, "Usage: %s <file>\n", argv[0]);
    return -1;
  }

  retval = -1;

  scy = scm_capi_scythe_new();
  if (scy == NULL) return -1;

  r = scm_capi_scythe_bootup(scy);
  if (r < 0) goto end;

  r = scm_capi_scythe_load_core(scy);
  if (r < 0) goto end;

  r = scm_capi_scythe_compile_file(scy, argv[1]);
  if (r < 0) goto end;

  retval = 0;

 end:
  scm_capi_scythe_end(scy);

  return retval;
}
