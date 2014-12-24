#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/module.h"
#include "scythe/vm.h"

extern inline bool
scm_fcd_gloc_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_GLOC_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_gloc_value(ScmObj gloc)
{
  scm_assert(scm_fcd_gloc_p(gloc));
  return scm_gloc_value(gloc);
}

extern inline ScmObj
scm_fcd_gloc_symbol(ScmObj gloc)
{
  scm_assert(scm_fcd_gloc_p(gloc));
  return scm_gloc_symbol(gloc);
}

void
scm_fcd_gloc_bind(ScmObj gloc, ScmObj val)
{
  scm_assert(scm_fcd_gloc_p(gloc));
  scm_assert(scm_obj_not_null_p(val));
  scm_gloc_bind(gloc, val);
}

extern inline bool
scm_fcd_module_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MODULE_TYPE_INFO);
}

extern inline bool
scm_fcd_module_name_p(ScmObj obj)
{
  return (scm_fcd_symbol_p(obj) || scm_fcd_pair_p(obj));

}

extern inline bool
scm_fcd_module_specifier_p(ScmObj obj)
{
  return (scm_fcd_module_p(obj)
          || scm_fcd_symbol_p(obj) || scm_fcd_pair_p(obj));
}

ScmObj
scm_fcd_make_module(ScmObj name)
{
  ScmObj mod = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&name,
                      &mod);

  scm_assert(scm_fcd_module_name_p(name));
  rslt = scm_moduletree_find(scm_bedrock_modtree(scm_fcd_current_br()),
                             name, SCM_CSETTER_L(mod));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(mod)) {
    scm_fcd_error("failed to make a module: already exist", 1, name);
    return SCM_OBJ_NULL;
  }

  return scm_moduletree_module(scm_bedrock_modtree(scm_fcd_current_br()),
                               name);
}

int
scm_fcd_find_module(ScmObj name, scm_csetter_t *mod)
{
  scm_assert(scm_fcd_module_name_p(name));
  return scm_moduletree_find(scm_bedrock_modtree(scm_fcd_current_br()),
                             name, mod);
}

ScmObj
scm_fcd_module_name(ScmObj module)
{
  scm_assert(scm_fcd_module_p(module));
  return scm_module_name(module);
}

int
scm_fcd_import(ScmObj module, ScmObj imported, bool restrictive)
{
  ScmObj imp = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&module, &imported,
                      &imp);

  scm_assert(scm_fcd_module_p(module));
  scm_assert(scm_fcd_module_specifier_p(imported));

  if (!scm_fcd_module_p(imported)) {
    int r = scm_fcd_find_module(imported, SCM_CSETTER_L(imp));
    if (r < 0) return -1;

    if (scm_obj_null_p(imp)) {
      scm_fcd_error("failed to import a module: not found", 1, imported);
      return -1;
    }

    imported = imp;
  }

  return scm_module_import(module, imported, restrictive);
}

ScmObj
scm_fcd_get_gloc(ScmObj module, ScmObj sym)
{
  scm_assert(scm_fcd_module_p(module));
  scm_assert(scm_fcd_symbol_p(sym));
  return scm_module_gloc_eval(module, sym);
}

int
scm_fcd_find_gloc(ScmObj module, ScmObj sym, scm_csetter_t *gloc)
{
  scm_assert(scm_fcd_module_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  if (gloc != NULL)
    return scm_module_find_sym_eval(module, sym, gloc);
  else
    return 0;
}

int
scm_fcd_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&module, &sym, &val,
                      &mod);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val));

  if (!scm_fcd_module_p(module)) {
    int r = scm_fcd_find_module(module, SCM_CSETTER_L(mod));
    if (r < 0) return -1;

    if (scm_obj_null_p(mod)) {
      scm_fcd_error("failed to define global variable: no such a module",
                     1, module);
      return -1;
    }

    module = mod;
  }

  return scm_module_define_eval(module, sym, val, export);
}

int
scm_fcd_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&module, &sym, &syx);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));
  scm_assert(scm_fcd_syntax_p(syx));

  if (!scm_fcd_module_p(module)) {
    int r = scm_fcd_find_module(module, SCM_CSETTER_L(mod));
    if (r < 0) return -1;

    if (scm_obj_null_p(mod)) {
      scm_fcd_error("failed to define global syntax: no such a module",
                     1, module);
      return -1;
    }

    module = mod;
  }

  return scm_module_define_cmpl(module, sym, syx, export);
}

int
scm_fcd_global_var_ref(ScmObj module, ScmObj sym, scm_csetter_t *val)
{
  ScmObj mod = SCM_OBJ_INIT, gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &mod, &gloc, &v);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  if (!scm_fcd_module_p(module)) {
    int r = scm_fcd_find_module(module, SCM_CSETTER_L(mod));
    if (r < 0) return -1;

    if (scm_obj_null_p(mod)) {
      scm_fcd_error("failed to get a global variable value: "
                    "no such a module", 1, module);
      return -1;
    }

    module = mod;
  }

  rslt = scm_module_find_sym_eval(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;


  if (scm_obj_not_null_p(gloc))
    v = scm_gloc_value(gloc);
  else
    v = SCM_OBJ_NULL;

  if (val != NULL)
    scm_csetter_setq(val, v);

  return 0;
}

int
scm_fcd_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  ScmObj mod = SCM_OBJ_INIT, gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &mod, &gloc, &v);


  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  if (!scm_fcd_module_p(module)) {
    int r = scm_fcd_find_module(module, SCM_CSETTER_L(mod));
    if (r < 0) return -1;

    if (scm_obj_null_p(mod)) {
      scm_fcd_error("failed to get a global syntax: no such a module",
                    1, module);
      return -1;
    }

    module = mod;
  }

  rslt = scm_module_find_sym_cmpl(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc))
    v = scm_gloc_value(gloc);
  else
    v = SCM_OBJ_NULL;

  if (syx != NULL)
    scm_csetter_setq(syx, v);

  return 0;
}
