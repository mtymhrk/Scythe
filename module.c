#include <limits.h>

#include "object.h"
#include "impl_utils.h"
#include "api.h"
#include "chashtbl.h"
#include "module.h"


/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

ScmTypeInfo SCM_GLOC_TYPE_INFO = {
  .name                = "gloc",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_gloc_pretty_print,
  .obj_size            = sizeof(ScmGLoc),
  .gc_ini_func         = scm_gloc_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_gloc_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val) /* GC OK */
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));

  SCM_SLOT_SETQ(ScmGLoc, gloc, sym, sym);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
  SCM_GLOC(gloc)->exported = false;

  return 0;
}

ScmObj
scm_gloc_new(SCM_MEM_TYPE_T mtype, ScmObj sym) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &sym);

  gloc = scm_capi_mem_alloc(&SCM_GLOC_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_gloc_initialize(gloc, sym, SCM_OBJ_NULL) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return gloc;
}

int
scm_gloc_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_GLOC_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<gloc %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return 0;
}

void
scm_gloc_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_GLOC_TYPE_INFO);

  SCM_GLOC(obj)->sym = SCM_OBJ_NULL;
  SCM_GLOC(obj)->val = SCM_OBJ_NULL;
}

int
scm_gloc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_GLOC_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->sym, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->val, mem);
}


/****************************************************************************/
/*  Module                                                                  */
/****************************************************************************/

ScmTypeInfo SCM_MODULE_TYPE_INFO = {
  .name                = "module",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_module_pretty_print,
  .obj_size            = sizeof(ScmModule),
  .gc_ini_func         = scm_module_gc_initialize,
  .gc_fin_func         = scm_module_gc_finalize,
  .gc_accept_func      = scm_module_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

#define SCM_MODULE_GLOCTBL_SIZE 256

static size_t
scm_module_hash_func(ScmCHashTblKey key)
{
  return scm_capi_symbol_hash_value(SCM_OBJ(key));
}

static bool
scm_module_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_capi_eq_p(SCM_OBJ(key1), SCM_OBJ(key2));
}

scm_local_func int
scm_module_expand_imports_if_needed(ScmObj mod, unsigned int add)
{
  size_t need, cap;
  ScmObj *new;

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  if (SIZE_MAX - SCM_MODULE(mod)->nr_imp < add) {
    scm_capi_error("import failure: number of imported modules is overflow", 0);
    return -1;
  }

  need = SCM_MODULE(mod)->nr_imp + add;
  if (need <= SCM_MODULE(mod)->imp_capacity)
    return 0;

  cap = SCM_MODULE(mod)->imp_capacity;
  for (cap = ((cap == 0) ? 8 : cap); cap < need; cap *= 2) {
    if (SIZE_MAX / 2 < cap) {
      cap = SIZE_MAX;
      break;
    }
  }

  new = scm_capi_realloc(SCM_MODULE(mod)->imports, cap);
  if (new == NULL) return -1;

  SCM_MODULE(mod)->imports = new;
  SCM_MODULE(mod)->imp_capacity = cap;

  return 0;
}

enum { SCM_MODULE_EVAL, SCM_MODULE_CMPL };

scm_local_func int
scm_module_search_gloc(ScmObj mod, ScmObj sym, int type, scm_csetter_t *setter)
{
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &sym);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);

  if (type == SCM_MODULE_EVAL)
    rslt = scm_chash_tbl_get(SCM_MODULE(mod)->eval_gloctbl,
                             sym, (ScmCHashTblVal *)setter, &found);
  else
    rslt = scm_chash_tbl_get(SCM_MODULE(mod)->cmpl_gloctbl,
                             sym, (ScmCHashTblVal *)setter, &found);
  if (rslt != 0) return -1;

  if (!found) scm_csetter_setq(setter, SCM_OBJ_NULL);

  return 0;
}

scm_local_func ScmObj
scm_module_gloc(ScmObj mod, ScmObj sym, int type)
{
  ScmObj gloc = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &sym,
                       &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);

  if (type == SCM_MODULE_EVAL)
    rslt = scm_chash_tbl_get(SCM_MODULE(mod)->eval_gloctbl, sym,
                             (ScmCHashTblVal *)SCM_CSETTER_L(gloc), &found);
  else
    rslt = scm_chash_tbl_get(SCM_MODULE(mod)->cmpl_gloctbl, sym,
                             (ScmCHashTblVal *)SCM_CSETTER_L(gloc), &found);

  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (found) return gloc;

  gloc = scm_gloc_new(SCM_MEM_HEAP, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (type == SCM_MODULE_EVAL)
    rslt = scm_chash_tbl_insert(SCM_MODULE(mod)->eval_gloctbl, sym, gloc);
  else
    rslt = scm_chash_tbl_insert(SCM_MODULE(mod)->cmpl_gloctbl, sym, gloc);

  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return gloc;
}


scm_local_func int
scm_module_define(ScmObj mod, ScmObj sym, ScmObj val, bool export, int type)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&mod, &sym, &val,
                       &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);

  gloc = scm_module_gloc(mod, sym, type);
  if (scm_obj_null_p(gloc)) return -1;

  scm_gloc_bind(gloc, val);

  if (export)
    scm_gloc_export(gloc);

  return 0;
}

scm_local_func int
scm_module_export(ScmObj mod, ScmObj sym, int type)
{
  ScmObj gloc = SCM_OBJ_INIT, undef = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &sym,
                       &gloc, &undef);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);

  rslt = scm_module_search_gloc(mod, sym, type, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(gloc)) {
    undef = scm_api_undef();

    rslt = scm_module_define(mod, sym, undef, true, type);
    if (rslt < 0) return -1;
  }
  else {
    scm_gloc_export(gloc);
  }

  return 0;
}

scm_local_func int
scm_module_find_exported_sym(ScmObj mod,
                             ScmObj sym, int type, scm_csetter_t *setter)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &sym,
                       &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);
  scm_assert(setter != NULL);

  rslt = scm_module_search_gloc(mod, sym, type, setter);
  if (rslt < 0) return -1;

  gloc = scm_csetter_val(setter);
  if (scm_obj_not_null_p(gloc) && scm_gloc_exported_p(gloc))
    return 0;

  scm_csetter_setq(setter, SCM_OBJ_NULL);

  for (size_t i = 0; i < SCM_MODULE(mod)->nr_imp; i++) {
    rslt = scm_module_find_exported_sym(SCM_MODULE(mod)->imports[i],
                                        sym, type, setter);
    if (rslt != 0) return -1;

    if (scm_obj_not_null_p(scm_csetter_val(setter)))
      return 0;
  }

  return 0;
}

scm_local_func int
scm_module_find_sym(ScmObj mod, ScmObj sym, int type, scm_csetter_t *setter)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &sym);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(type == SCM_MODULE_EVAL || type == SCM_MODULE_CMPL);
  scm_assert(setter != NULL);

  rslt = scm_module_search_gloc(mod, sym, type, setter);
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(scm_csetter_val(setter)))
    return 0;

  for (size_t i = 0; i < SCM_MODULE(mod)->nr_imp; i++) {
    rslt = scm_module_find_exported_sym(SCM_MODULE(mod)->imports[i],
                                        sym, type, setter);
    if (rslt != 0) return -1;

    if (scm_obj_not_null_p(scm_csetter_val(setter)))
      return 0;
  }

  return 0;
}

int
scm_module_initialize(ScmObj mod, ScmObj name)
{
  SCM_STACK_FRAME_PUSH(&mod, &name);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(name));

  SCM_SLOT_SETQ(ScmModule, mod, name, name);

  SCM_MODULE(mod)->eval_gloctbl =
    scm_chash_tbl_new(mod, SCM_MODULE_GLOCTBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                      scm_module_hash_func, scm_module_cmp_func);
  if (SCM_MODULE(mod)->eval_gloctbl == NULL) return -1;

  SCM_MODULE(mod)->cmpl_gloctbl =
    scm_chash_tbl_new(mod, SCM_MODULE_GLOCTBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                      scm_module_hash_func, scm_module_cmp_func);
  if (SCM_MODULE(mod)->cmpl_gloctbl == NULL) {
    scm_chash_tbl_end(SCM_MODULE(mod)->eval_gloctbl);
    SCM_MODULE(mod)->eval_gloctbl = NULL;
    return -1;
  }

  SCM_MODULE(mod)->imports = NULL;
  SCM_MODULE(mod)->imp_capacity = 0;
  SCM_MODULE(mod)->nr_imp = 0;

  return 0;
}

void
scm_module_finalize(ScmObj mod)
{
  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  if (SCM_MODULE(mod)->eval_gloctbl != NULL) {
    scm_chash_tbl_end(SCM_MODULE(mod)->eval_gloctbl);
    SCM_MODULE(mod)->eval_gloctbl = NULL;
  }

  if (SCM_MODULE(mod)->cmpl_gloctbl != NULL) {
    scm_chash_tbl_end(SCM_MODULE(mod)->cmpl_gloctbl);
    SCM_MODULE(mod)->cmpl_gloctbl = NULL;
  }
}

ScmObj
scm_module_new(SCM_MEM_TYPE_T mtype, ScmObj name)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&name,
                       &mod);

  scm_assert(scm_capi_symbol_p(name));

  mod = scm_capi_mem_alloc(&SCM_MODULE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  if (scm_module_initialize(mod, name) < 0)
    return SCM_OBJ_NULL;

  return mod;
}

int
scm_module_import(ScmObj mod, ScmObj imp)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&mod, &imp);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert_obj_type(imp, &SCM_MODULE_TYPE_INFO);

  rslt = scm_module_expand_imports_if_needed(mod, 1);
  if (rslt < 0) return -1;

  SCM_MODULE(mod)->imports[SCM_MODULE(mod)->nr_imp++] = imp;

  return 0;
}

int
scm_module_define_eval(ScmObj mod, ScmObj sym, ScmObj val, bool export)
{
  return scm_module_define(mod, sym, val, export, SCM_MODULE_EVAL);
}

int
scm_module_define_cmpl(ScmObj mod, ScmObj sym, ScmObj val, bool export)
{
  return scm_module_define(mod, sym, val, export, SCM_MODULE_CMPL);
}

int
scm_module_export_eval(ScmObj mod, ScmObj sym)
{
  return scm_module_export(mod, sym, SCM_MODULE_EVAL);
}

int
scm_module_export_cmpl(ScmObj mod, ScmObj sym)
{
  return scm_module_export(mod, sym, SCM_MODULE_CMPL);
}

ScmObj
scm_module_gloc_eval(ScmObj mod, ScmObj sym)
{
  return scm_module_gloc(mod, sym, SCM_MODULE_EVAL);
}

ScmObj
scm_module_gloc_cmpl(ScmObj mod, ScmObj sym)
{
  return scm_module_gloc(mod, sym, SCM_MODULE_CMPL);
}

int
scm_module_find_sym_eval(ScmObj mod, ScmObj sym, scm_csetter_t *setter)
{
  return scm_module_find_sym(mod, sym, SCM_MODULE_EVAL, setter);
}

int
scm_module_find_sym_cmpl(ScmObj mod, ScmObj sym, scm_csetter_t *setter)
{
  return scm_module_find_sym(mod, sym, SCM_MODULE_CMPL, setter);
}

int
scm_module_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj ro = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port,
                       &ro);

  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);

  rslt = scm_capi_write_cstr("#<module ", SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  ro = scm_api_write_string(SCM_MODULE(obj)->name, port);
  if (scm_obj_null_p(ro)) return -1;

  rslt = scm_capi_write_cstr(">", SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_module_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);

  SCM_MODULE(obj)->name = SCM_OBJ_NULL;
  SCM_MODULE(obj)->nr_imp = 0;
  SCM_MODULE(obj)->eval_gloctbl = NULL;
  SCM_MODULE(obj)->cmpl_gloctbl = NULL;
}

void
scm_module_gc_finalize(ScmObj obj)
{
  scm_module_finalize(obj);
}

int
scm_module_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_MODULE(obj)->name, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < SCM_MODULE(obj)->nr_imp; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_MODULE(obj)->imports[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  if (SCM_MODULE(obj)->eval_gloctbl != NULL) {
    rslt = scm_chash_tbl_gc_accept(SCM_MODULE(obj)->eval_gloctbl,
                                   obj, mem, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  if (SCM_MODULE(obj)->cmpl_gloctbl != NULL) {
    rslt = scm_chash_tbl_gc_accept(SCM_MODULE(obj)->cmpl_gloctbl,
                                   obj, mem, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}


/****************************************************************************/
/*  ModuleTbl                                                               */
/****************************************************************************/

ScmTypeInfo SCM_MODULETBL_TYPE_INFO = {
  .name = "moduletbl",
  .flags = SCM_TYPE_FLG_MMO,
  .pp_func = NULL,
  .obj_size = sizeof(ScmModuleTbl),
  .gc_ini_func = scm_moduletbl_gc_initialize,
  .gc_fin_func = scm_moduletbl_gc_finalize,
  .gc_accept_func = scm_moduletbl_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

#define SCM_MODULETBL_SIZE 256

static size_t
scm_moduletbl_hash_func(ScmCHashTblKey key)
{
  return scm_capi_symbol_hash_value(SCM_OBJ(key));
}

static bool
scm_moduletbl_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_capi_eq_p(SCM_OBJ(key1), SCM_OBJ(key2));
}

int
scm_moduletbl_initialize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_MODULETBL_TYPE_INFO);

  SCM_MODULETBL(tbl)->tbl =
    scm_chash_tbl_new(tbl, SCM_MODULETBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                      scm_moduletbl_hash_func, scm_moduletbl_cmp_func);
  if (SCM_MODULETBL(tbl)->tbl == NULL) return -1;

  return 0;
}

void
scm_moduletbl_finalize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_MODULETBL_TYPE_INFO);

  if (SCM_MODULETBL(tbl)->tbl != NULL) {
    scm_chash_tbl_end(SCM_MODULETBL(tbl)->tbl);
    SCM_MODULETBL(tbl)->tbl = NULL;
  }
}

ScmObj
scm_moduletbl_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj tbl;

  SCM_STACK_FRAME_PUSH(&tbl);

  tbl = scm_capi_mem_alloc(&SCM_MODULETBL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL;

  if (scm_moduletbl_initialize(tbl) < 0)
    return SCM_OBJ_NULL;

  return tbl;
}

ScmObj
scm_moduletbl_module(ScmObj tbl, ScmObj name)
{
  ScmObj mod;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&tbl,
                       &mod);

  scm_assert_obj_type(tbl, &SCM_MODULETBL_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(name));

  rslt = scm_chash_tbl_get(SCM_MODULETBL(tbl)->tbl, name,
                           (ScmCHashTblVal *)SCM_CSETTER_L(mod), &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return mod;

  mod = scm_module_new(SCM_MEM_HEAP, name);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_MODULETBL(tbl)->tbl, name, mod);
  if (rslt != 0) return SCM_OBJ_NULL;

  return mod;
}

int
scm_moduletbl_find(ScmObj tbl, ScmObj name, scm_csetter_t *mod)
{
  bool found;
  int rslt;

  scm_assert_obj_type(tbl, &SCM_MODULETBL_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(name));
  scm_assert(mod != NULL);

  rslt = scm_chash_tbl_get(SCM_MODULETBL(tbl)->tbl,
                           name, (ScmCHashTblVal *)mod, &found);
  if (rslt != 0) return -1;

  if (!found) scm_csetter_setq(mod, SCM_OBJ_NULL);

  return 0;
}

int
scm_moduletbl_clean(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_MODULETBL_TYPE_INFO);

  scm_chash_tbl_clean(SCM_MODULETBL(tbl)->tbl);

  return 0;
}

void
scm_moduletbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_MODULETBL_TYPE_INFO);

  SCM_MODULETBL(obj)->tbl = NULL;
}

void
scm_moduletbl_gc_finalize(ScmObj obj)
{
  scm_moduletbl_finalize(obj);
}

int
scm_moduletbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_MODULETBL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return scm_chash_tbl_gc_accept(SCM_MODULETBL(obj)->tbl, obj, mem, handler);
}
