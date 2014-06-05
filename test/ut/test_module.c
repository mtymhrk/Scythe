#include "test.h"

#include "object.h"
#include "api.h"

TEST_GROUP(module);

static ScmEvaluator *ev;

static ScmObj undef;
static ScmObj module;
static ScmObj name;
static ScmObj gloc;
static ScmObj symbol;
static ScmObj syntax;

TEST_SETUP(module)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_evaluator_load_core(ev);

  undef = scm_api_undef();
  assert(scm_obj_not_null_p(undef));

  module = name = gloc = symbol = syntax = undef;
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(module));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(name));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(gloc));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(symbol));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(syntax));
}

TEST_TEAR_DOWN(module)
{
  scm_capi_evaluator_end(ev);
}

static ScmObj
dummy_syntax_handler(ScmObj cmpl, ScmObj exp, ScmObj env,
                     ScmObj next, int arity,
                     bool tail_p, bool toplevel_p,
                     ssize_t *rdepth)
{
  return SCM_OBJ_NULL;
}

static void
make_syntax(const char *k)
{
  ScmObj key = SCM_OBJ_INIT;

  syntax = undef;

  key = scm_capi_make_symbol_from_cstr(k, SCM_ENC_ASCII);
  syntax = scm_capi_make_syntax(key, dummy_syntax_handler);
}

static void
find_module(const char *n)
{
  module = name = undef;

  name = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  name = scm_capi_list(1, name);
  TEST_ASSERT_EQUAL_INT(0, scm_capi_find_module(name, SCM_CSETTER_L(module)));
}

static void
make_module(const char *n)
{
  module = name = undef;

  name = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  name = scm_capi_list(1, name);
  module = scm_api_make_module(name);
}

static void
import_module(const char *n, bool res)
{
  ScmObj mod = SCM_OBJ_INIT, nam = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mod, &nam);

  nam = name;
  mod = module;

  find_module(n);

  scm_capi_import(mod, module, res);

  name = nam;
  module = mod;
}

static void
make_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  gloc = scm_capi_make_gloc(module, symbol);
}

static void
find_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  scm_capi_find_gloc(module, symbol, SCM_CSETTER_L(gloc));
}

TEST(module, make_module)
{
  make_module("test");

  TEST_ASSERT_TRUE(scm_capi_module_p(module));
  TEST_ASSERT_TRUE(scm_capi_true_p(scm_api_equal_P(name,
                                                   scm_api_module_name(module))));
}

TEST(module, make_module__already_exist)
{
  make_module("main");

  TEST_ASSERT_SCM_NULL(module);
}

TEST(module, import)
{
  ScmObj main_mod = SCM_OBJ_INIT;

  find_module("main");

  main_mod = module;

  module = SCM_OBJ_NULL;

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_import(module, main_mod, false));
}

TEST(module, find_module)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mod);

  make_module("test");

  mod = module;

  find_module("test");

  TEST_ASSERT_SCM_EQ(mod, module);
}

TEST(module, find_module__not_exist)
{
  find_module("test");

  TEST_ASSERT_SCM_NULL(module);
}

TEST(module, make_gloc)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val);

  make_module("test");
  make_gloc("var");

  TEST_ASSERT_TRUE(scm_capi_gloc_p(gloc));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  TEST_ASSERT_SCM_EQ(symbol, sym);
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(val)));
  TEST_ASSERT_SCM_NULL(val);
}

TEST(module, make_gloc__already_exist)
{
  make_module("test");
  make_gloc("var");
  make_gloc("var");

  TEST_ASSERT_SCM_NULL(gloc);
}

TEST(module, find_gloc)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  make_module("test");
  make_gloc("var");
  find_gloc("var");

  TEST_ASSERT_TRUE(scm_capi_gloc_p(gloc));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  TEST_ASSERT_SCM_EQ(symbol, sym);
}

TEST(module, find_gloc__not_exist)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym);

  make_module("test");
  find_gloc("var");

  TEST_ASSERT_SCM_NULL(gloc);
}

TEST(module, gloc_bind)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&expected, &actual);

  make_module("test");
  make_gloc("var");

  expected = scm_api_eof();

  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_bind(gloc, expected));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(module, define_global_var)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(val, actual);
}

TEST(module, define_global_var__already_bound)
{
  ScmObj sym = SCM_OBJ_INIT, val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val1, &val2, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val1 = scm_api_eof();
  val2 = scm_api_false();

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module,
                                                      sym, val1, false));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module,
                                                      sym, val2, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(val2, actual);
}

TEST(module, global_var_ref__unbound)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(module, global_var_ref__refer_exported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("imp");
  make_module("test");
  import_module("imp", false);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(val, actual);
}

TEST(module, global_var_ref__refer_unexported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("imp");
  make_module("test");
  import_module("imp", false);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, false));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(module, global_var_ref__refer_exported_symbol_of_imported_module__restrictive)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("imp");
  make_module("test");
  import_module("imp", true);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(val, actual);
}

TEST(module, global_var_ref__refer_exported_symbol_of_imported_module__restrictive__2)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("imp-a");
  make_module("imp-b");
  import_module("imp-a", true);
  make_module("test");
  import_module("imp-b", false);
  find_module("imp-a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(module, define_global_syx)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(syx, actual);
}

TEST(module, define_global_syx__already_bound)
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

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx1, false));

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx2, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(syx2, actual);
}

TEST(module, global_syx_ref__unbound)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(module, global_syx_ref__refer_exported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp");
  make_module("test");
  import_module("imp", false);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(syx, actual);
}

TEST(module, global_syx_ref__refer_unexported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp");
  make_module("test");
  import_module("imp", false);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, false));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(module, global_syx_ref__refer_exported_symbol_of_imported_module__restrictive)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp");
  make_module("test");
  import_module("imp", true);
  find_module("imp");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(syx, actual);
}

TEST(module, global_syx_ref__refer_exported_symbol_of_imported_module__restrictive_2)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("imp-a");
  make_module("imp-b");
  import_module("imp-a", true);
  make_module("test");
  import_module("imp-b", false);
  find_module("imp-a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, true));

  find_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}
