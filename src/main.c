#include <unistd.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "scythe/api.h"

static const char *command_name = NULL;
static const char *opt_expr = NULL;
static int scheme_argc = 0;
static char **scheme_argv = NULL;
static bool interactive_flag = false;


static void
message(const char *fmt, ...)
{
  va_list arg;

  va_start(arg, fmt);
  vfprintf(stderr, fmt, arg);
  va_end(arg);
  fputc('\n', stderr);
}

static void
emessage(const char *fmt, ...)
{
  va_list arg;

  fprintf(stderr, "%s: ", command_name);
  va_start(arg, fmt);
  vfprintf(stderr, fmt, arg);
  va_end(arg);
  fputc('\n', stderr);
}

static void
usage()
{
  message("Usage: %s [options] [--] [file] [arumgnets]", command_name);
  message("Options:\n"
          "  -e <expr>   Evaluate Scheme expression <expr>. Omit [file].\n"
          "  -i          Interactive mode.\n"
          "  -I <path>   Add <path> to the load path list.");
}

static int
parse_arguments(int argc, char **argv, ScmScythe *scy)
{
  int c, r;

  opterr = 0;

  command_name = argv[0];

  while ((c = getopt(argc, argv, "+:e:iI:")) != -1) {
    switch (c) {
    case 'e':
      opt_expr = optarg;
      break;
    case 'i':
      interactive_flag = true;
      break;
    case 'I':
      r = scm_capi_scythe_add_load_path(scy, optarg);
      if (r < 0) return -1;
      break;
    case ':':
      emessage("option requires an argument  -%c", (char)optopt);
      usage();
      return -1;
      break;
    case '?':
      emessage("invalid option  -%c", (char)optopt);
      usage();
      return -1;
      break;
    default:
      return -1;
      break;
    }
  }

  if (optind >= argc)
    return 0;

  if (strcmp("--", argv[optind]) == 0)
    optind++;

  scheme_argc = argc - optind;
  if (scheme_argc > 0)
    scheme_argv = argv + optind;

  return 0;
}

static int
execute_opt_expr(ScmScythe *scy)
{
  int r;

  r = scm_capi_scythe_exec_cstr(scy, opt_expr);
  if (r < 0) return -1;

  return 0;
}

static int
execute_file(ScmScythe *scy)
{
  int r;

  r = scm_capi_scythe_exec_file(scy, scheme_argv[0]);
  if (r < 0) return -1;

  return 0;
}

static int
execute_repl(ScmScythe *scy)
{
  int r;

  r = scm_capi_scythe_run_repl(scy);
  if (r < 0) return -1;

  return 0;
}

static int
setup_load_path(ScmScythe *scy)
{
  int r;

  r = scm_capi_scythe_add_load_path(scy, SCYTHE_LIB_DIR);
  if (r < 0) return -1;

  return 0;
}

static ScmScythe *
make_scythe(void)
{
  ScmScythe *scy;
  int r;

  r = scm_capi_scythe_init();
  if (r < 0) return NULL;

  scy = scm_capi_scythe_new();
  if (scy == NULL) return NULL;

  r = setup_load_path(scy);
  if (r < 0) goto err;

  return scy;

 err:
  scm_capi_scythe_end(scy);
  return NULL;
}

static int
setup_scythe(ScmScythe *scy)
{
  int r;

  r = scm_capi_scythe_bootup(scy);
  if (r < 0) return -1;

  r = scm_capi_scythe_load_core(scy);
  if (r < 0) return -1;

  return 0;
}

int
main(int argc, char **argv)
{
  ScmScythe *scy;
  int r, retval;

  retval = -1;

  scy = make_scythe();
  if (scy == NULL) return -1;

  r = parse_arguments(argc, argv, scy);
  if (r < 0) goto end;

  r = setup_scythe(scy);
  if (r < 0) goto end;

  if (opt_expr != NULL)
    retval = execute_opt_expr(scy);
  else if (scheme_argc > 0)
    retval = execute_file(scy);
  else if (interactive_flag)
    retval = execute_repl(scy);
  else {
    /* TODO: 標準入力から S 式を読み取って実行する */
    retval = -1;
  }

 end:
  scm_capi_scythe_end(scy);
  return retval;
}
