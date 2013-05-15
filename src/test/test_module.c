#include <cutter.h>

#include "object.h"
#include "api.h"

static ScmEvaluator *ev;

static ScmObj undef;
static ScmObj module;
static ScmObj name;
static ScmObj gloc;
static ScmObj symbol;
static ScmObj syntax;

ScmObj
dummy_syntax_handler(ScmObj cmpl, ScmObj exp, ScmObj env,
                     ScmObj next, int arity,
                     bool tail_p, bool toplevel_p,
                     ssize_t *rdepth)
{
  return SCM_OBJ_NULL;
}

void
make_syntax(const char *k)
{
  ScmObj key = SCM_OBJ_INIT;

  syntax = undef;

  key = scm_capi_make_symbol_from_cstr(k, SCM_ENC_ASCII);
  syntax = scm_capi_make_syntax(key, dummy_syntax_handler);
}

void
find_module(const char *n)
{
  module = name = undef;

  name = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  name = scm_capi_list(1, name);
  cut_assert_equal_int(0, scm_capi_find_module(name, SCM_CSETTER_L(module)));
}

void
make_module(const char *n)
{
  module = name = undef;

  name = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  name = scm_capi_list(1, name);
  module = scm_api_make_module(name);
}

void
import_module(const char *n)
{
  ScmObj mod = SCM_OBJ_INIT, nam = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mod, &nam);

  nam = name;
  mod = module;

  find_module(n);

  scm_capi_import(mod, module);

  name = nam;
  module = mod;
}

void
make_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  gloc = scm_capi_make_gloc(module, symbol);
}

void
find_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  scm_capi_find_gloc(module, symbol, SCM_CSETTER_L(gloc));
}

void
cut_setup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);

  undef = SCM_UNDEF_OBJ;
  assert(scm_obj_not_null_p(undef));

  module = name = gloc = symbol = syntax = undef;
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(module));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(name));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(gloc));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(symbol));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(syntax));
}

void
cut_teardown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_make_module(void)
{
  make_module("test");

  cut_assert_true(scm_capi_module_p(module));
  cut_assert_true(scm_capi_true_p(scm_api_equal_P(name,
                                                  scm_api_module_name(module))));
}

void
test_make_module__already_exist(void)
{
  make_module("main");

  cut_assert_true(scm_obj_null_p(module));
}

void
test_import(void)
{
  ScmObj main_mod = SCM_OBJ_INIT;

  find_module("main");

  main_mod = module;

  module = SCM_OBJ_NULL;

  make_module("test");

  cut_assert_equal_int(0, scm_capi_import(module, main_mod));
}

void
test_find_module(void)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mod);

  make_module("test");

  mod = module;

  find_module("test");

  cut_assert_true(scm_capi_eq_p(mod, module));
}

void
test_find_module__not_exist(void)
{
  find_module("test");

  cut_assert_true(scm_obj_null_p(module));
}

void
test_make_gloc(void)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val);

  make_module("test");
  make_gloc("var");

  cut_assert_true(scm_capi_gloc_p(gloc));
  cut_assert_equal_int(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  cut_assert_true(scm_capi_eq_p(symbol, sym));
  cut_assert_equal_int(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(val)));
  cut_assert_true(scm_obj_null_p(val));
}

void
test_make_gloc__already_exist(void)
{
  make_module("test");
  make_gloc("var");
  make_gloc("var");

  cut_assert_true(scm_obj_null_p(gloc));
}

void
test_find_gloc(void)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  make_module("test");
  make_gloc("var");
  find_gloc("var");

  cut_assert_true(scm_capi_gloc_p(gloc));
  cut_assert_equal_int(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  cut_assert_true(scm_capi_eq_p(symbol, sym));
}

void
test_find_gloc__not_exist(void)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  make_module("test");
  find_gloc("var");

  cut_assert_true(scm_obj_null_p(gloc));
}

void
test_gloc_bind(void)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&expected, &actual);

  make_module("test");
  make_gloc("var");

  expected = SCM_EOF_OBJ;

  cut_assert_equal_int(0, scm_capi_gloc_bind(gloc, expected));
  cut_assert_equal_int(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(expected, actual));
}

void
test_define_global_var(void)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = SCM_EOF_OBJ;

  make_module("test");

  cut_assert_equal_int(0, scm_capi_define_global_var(module, sym, val, false));
  cut_assert_equal_int(0, scm_capi_global_var_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(val, actual));
}

void
test_define_global_var__already_bound(void)
{
  ScmObj sym = SCM_OBJ_INIT, val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val1, &val2, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val1 = SCM_EOF_OBJ;
  val2 = SCM_FALSE_OBJ;

  make_module("test");

  cut_assert_equal_int(0, scm_capi_define_global_var(module, sym, val1, false));

  cut_assert_equal_int(0, scm_capi_define_global_var(module, sym, val2, false));
  cut_assert_equal_int(0, scm_capi_global_var_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(val2, actual));
}

void
test_global_var_ref__unbound(void)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  cut_assert_equal_int(0, scm_capi_global_var_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_obj_null_p(actual));
}

void
test_global_var_ref__refer_exported_symbol_of_imported_module(void)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = SCM_EOF_OBJ;

  make_module("imp");
  make_module("test");
  import_module("imp");
  find_module("imp");

  cut_assert_equal_int(0, scm_capi_define_global_var(module, sym, val, true));

  find_module("test");

  cut_assert_equal_int(0, scm_capi_global_var_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(val, actual));
}

void
test_global_var_ref__refer_unexported_symbol_of_imported_module(void)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = SCM_EOF_OBJ;

  make_module("imp");
  make_module("test");
  import_module("imp");
  find_module("imp");

  cut_assert_equal_int(0, scm_capi_define_global_var(module, sym, val, false));

  find_module("test");

  cut_assert_equal_int(0, scm_capi_global_var_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_obj_null_p(actual));
}

void
test_define_global_syx(void)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("test");

  cut_assert_equal_int(0, scm_capi_define_global_syx(module, sym, syx, false));
  cut_assert_equal_int(0, scm_capi_global_syx_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(syx, actual));
}

void
test_define_global_syx__already_bound(void)
{
  ScmObj sym = SCM_OBJ_INIT, syx1 = SCM_OBJ_INIT, syx2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx1, &syx2, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx1 = syntax;

  make_syntax("bar");
  syx2 = syntax;

  make_module("test");

  cut_assert_equal_int(0, scm_capi_define_global_syx(module, sym, syx1, false));

  cut_assert_equal_int(0, scm_capi_define_global_syx(module, sym, syx2, false));
  cut_assert_equal_int(0, scm_capi_global_syx_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(syx2, actual));
}

void
test_global_syx_ref__unbound(void)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  cut_assert_equal_int(0, scm_capi_global_syx_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_obj_null_p(actual));
}

void
test_global_syx_ref__refer_exported_symbol_of_imported_module(void)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp");
  make_module("test");
  import_module("imp");
  find_module("imp");

  cut_assert_equal_int(0, scm_capi_define_global_syx(module, sym, syx, true));

  find_module("test");

  cut_assert_equal_int(0, scm_capi_global_syx_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_capi_eq_p(syx, actual));
}

void
test_global_syx_ref__refer_unexported_symbol_of_imported_module(void)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp");
  make_module("test");
  import_module("imp");
  find_module("imp");

  cut_assert_equal_int(0, scm_capi_define_global_syx(module, sym, syx, false));

  find_module("test");

  cut_assert_equal_int(0, scm_capi_global_syx_ref(module,
                                                  sym, SCM_CSETTER_L(actual)));
  cut_assert_true(scm_obj_null_p(actual));
}
