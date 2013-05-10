#include <stdint.h>
#include <stdarg.h>

#include "object.h"
#include "api.h"
#include "compiler.h"
#include "core_subr.h"
#include "impl_utils.h"

static ScmObj
scm_make_module_name(int n, SCM_ENC_T enc, ...)
{
  ScmObj str[n];
  ScmObj name = SCM_OBJ_INIT;
  const char *args[n];
  va_list ap;

  SCM_STACK_FRAME_PUSH(&name);

  scm_assert(n > 0);

  for (int i = 0; i < n; i++) {
    str[i] = SCM_OBJ_INIT;
    SCM_STACK_PUSH(&str[i]);
  }

  va_start(ap, enc);
  for (int i = 0; i < n; i++)
    args[i] = va_arg(ap, const char *);
  va_end(ap);

  for (int i = 0; i < n; i++) {
    str[i] = scm_capi_make_symbol_from_cstr(args[i], enc);
    if (scm_obj_null_p(str[i])) return SCM_OBJ_NULL;
  }

  name = scm_api_nil();

  for (int i = n; i > 0; i--) {
    name = scm_api_cons(str[i - 1], name);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
  }

  return name;
}

static int
scm_load_module_scheme_base_syntax(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod);

  name = scm_make_module_name(3, SCM_ENC_ASCII, "scheme", "base", "syntax");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  rslt = scm_cmpl_define_syntax(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_define_scheme_base_subr(ScmObj module)
{
  static struct {
    const char *name; int arity; unsigned int flag; ScmSubrFunc func;
  } const data[] = {
    { "null?", SCM_SUBR_ARITY_NULL_P, SCM_SUBR_FLAG_NULL_P, scm_subr_func_null_P },
    { "cons", SCM_SUBR_ARITY_CONS, SCM_SUBR_FLAG_CONS, scm_subr_func_cons },
    { "car", SCM_SUBR_ARITY_CAR, SCM_SUBR_FLAG_CAR, scm_subr_func_car },
    { "cdr", SCM_SUBR_ARITY_CDR, SCM_SUBR_FLAG_CDR, scm_subr_func_cdr },
    { "read", SCM_SUBR_ARITY_READ, SCM_SUBR_FLAG_READ, scm_subr_func_read },
    { "write", SCM_SUBR_ARITY_WRITE, SCM_SUBR_FLAG_WRITE, scm_subr_func_write },
    { "display", SCM_SUBR_ARITY_DISPLAY, SCM_SUBR_FLAG_DISPLAY, scm_subr_func_display },
    { "newline", SCM_SUBR_ARITY_NEWLINE, SCM_SUBR_FLAG_NEWLINE, scm_subr_func_newline },
    { "flush-output-port", SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT, SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT, scm_subr_func_flush_output_port },
    { "call/cc", SCM_SUBR_ARITY_CALLCC, SCM_SUBR_FLAG_CALLCC, scm_subr_func_callcc },
    { "values", SCM_SUBR_ARITY_VALUES, SCM_SUBR_FLAG_VALUES, scm_subr_func_values },
    { "eval-asm", SCM_SUBR_ARITY_EVAL_ASM, SCM_SUBR_FLAG_EVAL_ASM, scm_subr_func_eval_asm },
    { "eval", SCM_SUBR_ARITY_EVAL, SCM_SUBR_FLAG_EVAL, scm_subr_func_eval },
    { "exit", SCM_SUBR_ARITY_EXIT, SCM_SUBR_FLAG_EXIT, scm_subr_func_exit },
  };

  ScmObj sym  = SCM_OBJ_INIT;
  ScmObj subr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &subr);

  for (size_t i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
    sym = scm_capi_make_symbol_from_cstr(data[i].name, SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    subr = scm_capi_make_subrutine(data[i].func, data[i].arity, data[i].flag,
                                   module);
    if (scm_obj_null_p(subr)) return -1;

    rslt = scm_capi_define_global_var(module, sym, subr, true);
    if (rslt < 0) return -1;                   /* [ERR]: [through] */
  }

  subr = scm_capi_make_subrutine(scm_subr_func_default_exception_handler,
                                 SCM_SUBR_ARITY_DEFAULT_EXCEPTION_HANDLER,
                                 SCM_SUBR_FLAG_DEFAULT_EXCEPTION_HANDLER,
                                 module);
  if (scm_obj_null_p(subr)) return -1;

  scm_capi_push_exception_handler(subr);

  return 0;
}

static int
scm_define_scheme_base_clsr(ScmObj module)
{
  const char *syms[] = { "call-with-values" };
  int arities[] = { 2 };
  const char *codes[] = { scm_clsr_code_call_with_values };
  ScmObj sym  = SCM_OBJ_INIT, code = SCM_OBJ_INIT, clsr = SCM_OBJ_INIT;
  ScmObj port = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &code, &clsr,
                       &port);

  for (size_t i = 0; i < sizeof(syms)/sizeof(syms[0]); i++) {
    port = scm_capi_open_input_string_from_cstr(codes[i], SCM_ENC_ASCII);
    if (scm_obj_null_p(port)) return -1;

    code = scm_api_read(port);
    if (scm_obj_null_p(code)) return -1;

    code = scm_api_assemble(code);
    if (scm_obj_null_p(code)) return -1;

    sym = scm_capi_make_symbol_from_cstr(syms[i], SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    clsr = scm_capi_make_closure(code, SCM_OBJ_NULL, arities[i]);
    if (scm_obj_null_p(code)) return -1;

    rslt = scm_capi_define_global_var(module, sym, clsr, true);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_load_module_scheme_base(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod, &imp);

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "base");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  /*
   * load (scheme base syntax) module and import it
   */

  rslt = scm_load_module_scheme_base_syntax();
  if (rslt < 0) return -1;

  name = scm_make_module_name(3, SCM_ENC_ASCII, "scheme", "base", "syntax");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;

  /*
   * define global variables
   */

  rslt = scm_define_scheme_base_subr(mod);
  if (rslt < 0) return -1;

  rslt = scm_define_scheme_base_clsr(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_main(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod, &imp);

  name = scm_make_module_name(1, SCM_ENC_ASCII, "main");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  /*
   * load (scheme base) module and import it
   */

  rslt = scm_load_module_scheme_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "base");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_load_core_modules(void)
{
  return scm_load_module_main();
}
