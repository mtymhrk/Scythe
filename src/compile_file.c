#include <unistd.h>
#include <stdio.h>

#include "scythe/api.h"

static const char *command_name = NULL;
static const char *input_file = NULL;
static const char *output_file = "marshal.out";

static void
usage()
{
  fprintf(stderr, "Usage: %s [-o <file>] [--] <file>\n", command_name);
}

static int
parse_arguments(int argc, char **argv)
{
  int c;

  command_name = argv[0];

  while ((c = getopt(argc, argv, "+:o:")) != -1) {
    switch (c) {
    case 'o':
      output_file = optarg;
      break;
    case ':':
      fprintf(stderr, "%s: option requires an argument  -%c\n",
              command_name, optopt);
      goto err_usage;
      break;
    case '?':
      fprintf(stderr, "%s: invalid option  -%c\n", command_name, optopt);
      goto err_usage;
      break;
    default:
      return -1;
      break;
    }
  }

  if (optind >= argc) {
    fprintf(stderr, "%s: too few arguments\n", command_name);
    goto err_usage;
  }
  else if (optind + 1 < argc) {
    fprintf(stderr, "%s: too many arguments\n", command_name);
    goto err_usage;
  }

  input_file = argv[optind];
  return 0;

 err_usage:
  usage();
  return -1;
}


int
compile_file(int argc, char **argv)
{
  ScmScythe *scy;
  int r, retval;

  r = parse_arguments(argc, argv);
  if (r < 0) return -1;

  retval = -1;

  r = scm_capi_scythe_init();
  if (r < 0) return -1;

  scy = scm_capi_scythe_new();
  if (scy == NULL) return -1;

  r = scm_capi_scythe_default_setup(scy);
  if (r < 0) goto end;

  r = scm_capi_scythe_bootup(scy);
  if (r < 0) goto end;

  r = scm_capi_scythe_load_core(scy);
  if (r < 0) goto end;

  r = scm_capi_scythe_compile_file(scy, input_file, output_file);
  if (r < 0) goto end;

  retval = 0;

 end:
  scm_capi_scythe_end(scy);

  return retval;
}
