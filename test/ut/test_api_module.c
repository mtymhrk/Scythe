#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_module);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

static ScmObj undef;
static ScmObj module;
static ScmObj name;
static ScmObj gloc;
static ScmObj symbol;
static ScmObj syntax;

TEST_SETUP(api_module)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_evaluator_load_core(ev);
  scm_fcd_ref_stack_save(&rsi);

  undef = scm_api_undef();
  assert(scm_obj_not_null_p(undef));

  module = name = gloc = symbol = syntax = undef;
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(module));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(name));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(gloc));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(symbol));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(syntax));
}

TEST_TEAR_DOWN(api_module)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static void
make_syntax(const char *k)
{
  ScmObj key = SCM_OBJ_INIT;

  syntax = undef;

  key = scm_capi_make_symbol_from_cstr(k, SCM_ENC_ASCII);
  syntax = scm_api_make_syntax(key, SCM_NIL_OBJ);
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

  SCM_REFSTK_INIT_REG(&mod, &nam);

  nam = name;
  mod = module;

  find_module(n);

  scm_capi_import(mod, module, res);

  name = nam;
  module = mod;
}

static void
get_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  gloc = scm_api_get_gloc(module, symbol);
}

static void
find_gloc(const char *n)
{
  gloc = symbol = undef;

  symbol = scm_capi_make_symbol_from_cstr(n, SCM_ENC_ASCII);
  scm_capi_find_gloc(module, symbol, SCM_CSETTER_L(gloc));
}

TEST(api_module, make_module)
{
  make_module("test");

  TEST_ASSERT_TRUE(scm_capi_module_p(module));
  TEST_ASSERT_TRUE(scm_capi_true_p(scm_api_equal_P(name,
                                                   scm_api_module_name(module))));
}

TEST(api_module, make_module__already_exist)
{
  make_module("main");

  TEST_ASSERT_SCM_NULL(module);
}

TEST(api_module, import)
{
  ScmObj main_mod = SCM_OBJ_INIT;

  find_module("main");

  main_mod = module;

  module = SCM_OBJ_NULL;

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_import(module, main_mod, false));
}

TEST(api_module, find_module)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mod);

  make_module("test");

  mod = module;

  find_module("test");

  TEST_ASSERT_SCM_EQ(mod, module);
}

TEST(api_module, find_module__not_exist)
{
  find_module("test");

  TEST_ASSERT_SCM_NULL(module);
}

TEST(api_module, make_gloc)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val);

  make_module("test");
  get_gloc("var");

  TEST_ASSERT_TRUE(scm_capi_gloc_p(gloc));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  TEST_ASSERT_SCM_EQ(symbol, sym);
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(val)));
  TEST_ASSERT_SCM_NULL(val);
}

TEST(api_module, make_gloc__already_exist)
{
  ScmObj prev = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&prev);

  make_module("test");
  get_gloc("var");
  prev = gloc;
  get_gloc("var");

  TEST_ASSERT_SCM_EQ(prev, gloc);
}

TEST(api_module, find_gloc)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  make_module("test");
  get_gloc("var");
  find_gloc("var");

  TEST_ASSERT_TRUE(scm_capi_gloc_p(gloc));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym)));
  TEST_ASSERT_SCM_EQ(symbol, sym);
}

TEST(api_module, find_gloc__not_exist)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym);

  make_module("test");
  find_gloc("var");

  TEST_ASSERT_SCM_NULL(gloc);
}

TEST(api_module, gloc_bind)
{
  ScmObj expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &actual);

  make_module("test");
  get_gloc("var");

  expected = scm_api_eof();

  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_bind(gloc, expected));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_gloc_value(gloc, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(expected, actual);
}

TEST(api_module, define_global_var)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);
  val = scm_api_eof();

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_var(module, sym, val, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(val, actual);
}

TEST(api_module, define_global_var__already_bound)
{
  ScmObj sym = SCM_OBJ_INIT, val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val1, &val2, &actual);

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

TEST(api_module, global_var_ref__unbound)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_var_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(api_module, global_var_ref__refer_exported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val, &actual);

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

TEST(api_module, global_var_ref__refer_unexported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val, &actual);

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

TEST(api_module, global_var_ref__refer_exported_symbol_of_imported_module__restrictive)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val, &actual);

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

TEST(api_module, global_var_ref__refer_exported_symbol_of_imported_module__restrictive__2)
{
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &val, &actual);

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

TEST(api_module, define_global_syx)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_syntax("foo");
  syx = syntax;

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_define_global_syx(module, sym, syx, false));
  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_EQ(syx, actual);
}

TEST(api_module, define_global_syx__already_bound)
{
  ScmObj sym = SCM_OBJ_INIT, syx1 = SCM_OBJ_INIT, syx2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx1, &syx2, &actual);

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

TEST(api_module, global_syx_ref__unbound)
{
  ScmObj sym = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &actual);

  sym = scm_capi_make_symbol_from_cstr("var", SCM_ENC_ASCII);

  make_module("test");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_global_syx_ref(module,
                                                   sym, SCM_CSETTER_L(actual)));
  TEST_ASSERT_SCM_NULL(actual);
}

TEST(api_module, global_syx_ref__refer_exported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx, &actual);

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

TEST(api_module, global_syx_ref__refer_unexported_symbol_of_imported_module)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx, &actual);

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

TEST(api_module, global_syx_ref__refer_exported_symbol_of_imported_module__restrictive)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx, &actual);

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

TEST(api_module, global_syx_ref__refer_exported_symbol_of_imported_module__restrictive_2)
{
  ScmObj sym = SCM_OBJ_INIT, syx = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym, &syx, &actual);

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
