#include <unistd.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "api.h"

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
          "  -i          Interactive mode.");
}

static int
parse_arguments(int argc, char **argv)
{
  int c;

  opterr = 0;

  command_name = argv[0];

  while ((c = getopt(argc, argv, "+:e:i")) != -1) {
    switch (c) {
    case 'e':
      opt_expr = optarg;
      break;
    case 'i':
      interactive_flag = true;
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
execute_opt_expr(void)
{
  ScmEvaluator *ev;
  int rslt;

  ev = scm_capi_evaluator();
  if (ev == NULL) return -1;

  rslt = scm_capi_exec_cstr(opt_expr, ev);

  scm_capi_evaluator_end(ev);

  return (rslt < 0) ? -1 : 0;
}

static int
execute_file(void)
{
  ScmEvaluator *ev;
  int rslt;

  ev = scm_capi_evaluator();
  if (ev == NULL) return -1;

  rslt = scm_capi_exec_file(scheme_argv[0], ev);

  scm_capi_evaluator_end(ev);

  return (rslt < 0) ? -1 : 0;
}

static int
execute_repl(void)
{
  ScmEvaluator *ev;
  int rslt;

  ev = scm_capi_evaluator();
  if (ev == NULL) return -1;

  rslt = scm_capi_run_repl(ev);

  scm_capi_evaluator_end(ev);

  return (rslt < 0) ? -1 : 0;
}

int
main(int argc, char **argv)
{
  int rslt;

  rslt = parse_arguments(argc, argv);
  if (rslt < 0) return -1;

  if (opt_expr != NULL)
    return execute_opt_expr();
  else if (scheme_argc > 0)
    return execute_file();
  else if (interactive_flag)
    return execute_repl();
  else {
    /* TODO: 標準入力から S 式を読み取って実行する */
    return -1;
  }
}
