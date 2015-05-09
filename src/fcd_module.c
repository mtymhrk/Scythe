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

ScmObj
scm_fcd_gloc_new(SCM_MEM_TYPE_T mtype, ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&gloc, &sym);

  gloc = scm_fcd_mem_alloc(&SCM_GLOC_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  if (scm_gloc_initialize(gloc, sym, SCM_UNINIT_OBJ) < 0)
    return SCM_OBJ_NULL;

  return gloc;
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
  scm_assert(scm_obj_not_null_p(val) && !scm_fcd_landmine_object_p(val));
  scm_gloc_bind(gloc, val);
}

extern inline bool
scm_fcd_module_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MODULE_TYPE_INFO);
}

ScmObj
scm_fcd_module_P(ScmObj obj)
{
  return (scm_fcd_module_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
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
scm_fcd_module_new(SCM_MEM_TYPE_T mtype, ScmObj name)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &mod);

  scm_assert(scm_fcd_pair_p(name));

  mod = scm_fcd_mem_alloc(&SCM_MODULE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  if (scm_module_initialize(mod, name) < 0)
    return SCM_OBJ_NULL;

  return mod;
}

ScmObj
scm_fcd_moduletree_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj tree = SCM_OBJ_INIT;

  tree = scm_fcd_mem_alloc(&SCM_MODULETREE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tree)) return SCM_OBJ_NULL;

  if (scm_moduletree_initialize(tree) < 0)
    return SCM_OBJ_NULL;

  return tree;
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

static ScmObj
get_module(ScmObj spec)
{
  ScmObj mod = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&spec,
                      &mod);

  scm_assert(scm_fcd_module_specifier_p(spec));

  if (scm_fcd_module_p(spec))
    return spec;

  r = scm_fcd_find_module(spec, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_fcd_error("failed to find loaded module: no such a module", 1, spec);
    return SCM_OBJ_NULL;
  }

  return mod;
}

int
scm_fcd_import(ScmObj module, ScmObj imported, bool restrictive)
{
  SCM_REFSTK_INIT_REG(&module, &imported);

  scm_assert(scm_fcd_module_p(module));
  scm_assert(scm_fcd_module_specifier_p(imported));

  imported = get_module(imported);
  if (scm_obj_null_p(imported)) return -1;

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
  SCM_REFSTK_INIT_REG(&module, &sym, &val);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_define_eval(module, sym, val, export);
}

int
scm_fcd_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  SCM_REFSTK_INIT_REG(&module, &sym, &syx);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));
  scm_assert(scm_fcd_syntax_p(syx) || scm_fcd_macro_p(syx));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_define_cmpl(module, sym, syx, export);
}

int
scm_fcd_global_var_ref(ScmObj module, ScmObj sym, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &gloc, &v);

  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  rslt = scm_module_find_sym_eval(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_value(gloc);
    if (scm_fcd_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else {
    v = SCM_OBJ_NULL;
  }

  if (val != NULL)
    scm_csetter_setq(val, v);

  return 0;
}

int
scm_fcd_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &gloc, &v);


  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  rslt = scm_module_find_sym_cmpl(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_value(gloc);
    if (scm_fcd_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else
    v = SCM_OBJ_NULL;

  if (syx != NULL)
    scm_csetter_setq(syx, v);

  return 0;
}

int
scm_fcd_export_global_var(ScmObj module, ScmObj sym)
{
  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_export_eval(module, sym);
}

int
scm_fcd_export_global_syx(ScmObj module, ScmObj sym)
{
  scm_assert(scm_fcd_module_specifier_p(module));
  scm_assert(scm_fcd_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_export_cmpl(module, sym);
}

