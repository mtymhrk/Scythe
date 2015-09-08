#include "scythe/fcd.h"

#include "test.h"

ScmObj
ut_read_cstr(const char *str)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_fcd_open_input_string_cstr(str, SCM_ENC_NAME_SRC);
  return scm_fcd_read(port);
}

ScmObj
ut_get_proc(const char *name, const char * const *module, size_t n)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, mod_name = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&sym, &mod, &mod_name,
                      &proc, &o);

  mod_name = SCM_NIL_OBJ;
  for (size_t i = n; i > 0; i--) {
    o = scm_fcd_make_symbol_from_cstr(module[i - 1], SCM_ENC_SRC);
    if (scm_obj_null_p(o)) return SCM_OBJ_NULL;

    mod_name = scm_fcd_cons(o, mod_name);
    if (scm_obj_null_p(mod_name)) return SCM_OBJ_NULL;
  }

  sym = scm_fcd_make_symbol_from_cstr(name, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  r = scm_fcd_find_module(mod_name, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_fcd_error("failed to find module", 1, mod_name);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_global_var_ref(mod, sym, SCM_CSETTER_L(proc));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(proc)) {
    scm_fcd_error("unbund variable", 1, sym);
    return SCM_OBJ_NULL;
  }

  return proc;
}

ScmScythe *
ut_scythe_setup(bool load)
{
  ScmScythe *scy;
  int r;

  scy = scm_fcd_scythe_new();
  if (scy == NULL) return NULL;

  r = scm_fcd_scythe_bootup(scy);
  if (r < 0) return NULL;

  if (load) {
    r = scm_fcd_scythe_load_core(scy);
    if (r < 0) return NULL;
  }

  scm_fcd_scythe_enable(scy);

  return scy;
}

void
ut_scythe_tear_down(ScmScythe *scy)
{
  scm_fcd_scythe_disable(scy);
  scm_fcd_scythe_shutdown(scy);
  scm_fcd_scythe_end(scy);
}

ScmObj
ut_compile(ScmObj exp)
{
  ScmObj compile = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&exp,
                      &compile, &args, &val);

  r = scm_fcd_cached_global_var_ref(SCM_CACHED_GV_COMPILE,
                                    SCM_CSETTER_L(compile));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(compile)) {
    scm_fcd_error("unbound variable: compile", 0);
    return SCM_OBJ_NULL;
  }

  args = scm_fcd_make_compiler(SCM_OBJ_NULL);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  args = scm_fcd_list(2, exp, args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  val = scm_fcd_vm_apply(scm_fcd_current_vm(), compile, args);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_fcd_vector_ref(val, 0);
}

ScmObj
ut_precompile(ScmObj exp)
{
  ScmObj precompile = SCM_OBJ_INIT, args = SCM_OBJ_NULL, val = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&exp,
                      &precompile, &args, &val);

  precompile = ut_get_proc("precompile",
                           (const char *[]){"scythe", "internal", "compile"},
                           3);
  if(scm_obj_null_p(precompile)) return SCM_OBJ_NULL;

  args = scm_fcd_make_compiler(SCM_OBJ_NULL);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  args = scm_fcd_list(2, exp, args);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  val = scm_fcd_vm_apply(scm_fcd_current_vm(), precompile, args);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_fcd_vector_ref(val, 0);
}

ScmObj
ut_eval(ScmObj exp)
{
  ScmObj eval = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&exp,
                      &eval, &args, &val);

  r = scm_fcd_cached_global_var_ref(SCM_CACHED_GV_EVAL, SCM_CSETTER_L(eval));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(eval)) {
    scm_fcd_error("unbound variable: eval", 0);
    return SCM_OBJ_NULL;
  }

  args = scm_fcd_cons(exp, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return SCM_OBJ_NULL;

  val = scm_fcd_vm_apply(scm_fcd_current_vm(), eval, args);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_fcd_vector_ref(val, 0);
}

void
ut_disposal_unhandled_exc(void)
{
  scm_fcd_vm_disposal_unhandled_exc(scm_fcd_current_vm());
}
