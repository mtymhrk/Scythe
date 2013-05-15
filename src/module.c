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
    undef = SCM_UNDEF_OBJ;

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
  scm_assert(scm_capi_pair_p(name));

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

  scm_assert(scm_capi_pair_p(name));

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
/*  ModuleTree                                                              */
/****************************************************************************/

ScmTypeInfo SCM_MODULETREE_TYPE_INFO = {
  .name = "moduletree",
  .flags = SCM_TYPE_FLG_MMO,
  .pp_func = NULL,
  .obj_size = sizeof(ScmModuleTree),
  .gc_ini_func = scm_moduletree_gc_initialize,
  .gc_fin_func = scm_moduletree_gc_finalize,
  .gc_accept_func = scm_moduletree_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL
};

enum { ADD, UPDATE, FIND };

scm_local_func ScmModuleTreeNode *
scm_moduletree_make_node(void)
{
  ScmModuleTreeNode *node;

  node = scm_capi_malloc(sizeof(ScmModuleTreeNode));
  if (node == NULL) return NULL;

  node->name = SCM_OBJ_NULL;
  node->module = SCM_OBJ_NULL;

  node->branches = scm_capi_malloc(sizeof(ScmModuleTreeNode *)
                                   * SCM_MODULETREE_DEFAULT_BRANCH_SIZE);
  if (node->branches == NULL) {
    scm_capi_free(node);
    return NULL;
  }

  node->capacity = SCM_MODULETREE_DEFAULT_BRANCH_SIZE;
  node->used = 0;

  return node;
}

scm_local_func int
scm_moduletree_free_node(ScmModuleTreeNode *node)
{
  if (node == NULL) return 0;

  scm_capi_free(node->branches);
  scm_capi_free(node);

  return 0;
}

scm_local_func int
scm_moduletree_free_tree(ScmModuleTreeNode *root)
{
  int r;

  if (root == NULL) return 0;

  for (size_t i = 0; i < root->used; i++) {
    r = scm_moduletree_free_tree(root->branches[i]);
    if (r < 0) return -1;
  }

  r = scm_moduletree_free_node(root);
  if (r < 0) return -1;

  return 0;
}

scm_local_func int
scm_moduletree_node_gc_accept(ScmObj tree, ScmModuleTreeNode *node,
                              ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  if (node == NULL) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, tree, node->name, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, tree, node->module, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < node->used; i++) {
    rslt = scm_moduletree_node_gc_accept(tree, node->branches[i], mem, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return 0;
}

scm_local_func ScmModuleTreeNode *
scm_moduletree_add_branche(ScmModuleTreeNode *node, ScmObj name)
{
  ScmModuleTreeNode *new;

  new = scm_moduletree_make_node();
  if (new == NULL) return NULL;

  new->name = name;

  if (node->used >= node->capacity) {
    size_t new_cap;
    ScmModuleTreeNode **new_bra;

    if (node->capacity == SIZE_MAX) {
      scm_capi_error("faild to register a module: buffer overlfow", 0);
      return NULL;
    }
    else if (node->capacity > SIZE_MAX / 2) {
      new_cap = SIZE_MAX;
    }
    else {
      new_cap = node->capacity * 2;
    }

    new_bra = scm_capi_realloc(node->branches,
                               sizeof(ScmModuleTreeNode) * new_cap);
    if (new_bra == NULL) return NULL;

    node->branches = new_bra;
    node->capacity = new_cap;
  }

  node->branches[node->used++] = new;

  return new;
}

scm_local_func int
scm_moduletree_access(ScmModuleTreeNode *root, ScmObj path, int mode,
                      ScmModuleTreeNode **node)
{
  ScmObj name = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&path,
                       &name);

  scm_assert(root != NULL);
  scm_assert(scm_capi_pair_p(path));
  scm_assert(node != NULL);

  name = scm_api_car(path);
  if (scm_obj_null_p(name)) return -1;

  if (!scm_capi_symbol_p(name)) {
    scm_capi_error("failed to access module: "
                   "module name must be a list whose members are symbols", 0);
    return -1;
  }

  path = scm_api_cdr(path);
  if (scm_obj_null_p(path)) return -1;

  *node = NULL;
  for (size_t i = 0; i < root->used; i++) {
    if (scm_capi_eq_p(name, root->branches[i]->name)) {
      *node = root->branches[i];
      break;
    }
  }

  if (scm_capi_nil_p(path)) {
    if (mode == ADD || mode == UPDATE) {
      if (*node == NULL) {
        ScmModuleTreeNode *new = scm_moduletree_add_branche(root, name);
        if (new == NULL) return -1;
        *node = new;
      }
      else if (mode == ADD) {
        *node = NULL;
      }
    }

    return 0;
  }

  if (*node == NULL && (mode == ADD || mode == UPDATE)) {
    ScmModuleTreeNode *new = scm_moduletree_add_branche(root, name);
    if (new == NULL) return -1;
    *node = new;
  }

  if (*node != NULL)
    return scm_moduletree_access(*node, path, mode, node);
  else
    return 0;
}

scm_local_func ScmObj
scm_moduletree_normailize_name(ScmObj name)
{
  scm_assert(scm_capi_symbol_p(name) || scm_capi_pair_p(name));

  if (scm_capi_pair_p(name)) return name;

  return scm_capi_list(1, name);
}

int
scm_moduletree_initialize(ScmObj tree)
{
  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);

  SCM_MODULETREE(tree)->root = scm_moduletree_make_node();
  if (SCM_MODULETREE(tree)->root == NULL) return -1;

  return 0;
}

void
scm_moduletree_finalize(ScmObj tree)
{
  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);

  scm_moduletree_free_tree(SCM_MODULETREE(tree)->root);
  SCM_MODULETREE(tree)->root = NULL;
}

ScmObj
scm_moduletree_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj tree = SCM_OBJ_INIT;

  tree = scm_capi_mem_alloc(&SCM_MODULETREE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tree)) return SCM_OBJ_NULL;

  if (scm_moduletree_initialize(tree) < 0)
    return SCM_OBJ_NULL;

  return tree;
}

ScmObj
scm_moduletree_module(ScmObj tree, ScmObj name)
{
  ScmObj path = SCM_OBJ_INIT;
  ScmModuleTreeNode *node;
  bool need_copy;
  int rslt;

  SCM_STACK_FRAME_PUSH(&tree, &name,
                       &path);

  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(name) || scm_capi_pair_p(name));

  path = scm_moduletree_normailize_name(name);
  if (scm_obj_null_p(path)) return SCM_OBJ_NULL;

  need_copy = scm_capi_eq_p(path, name);

  rslt = scm_moduletree_access(SCM_MODULETREE(tree)->root, path, UPDATE, &node);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(node->module)) {
    if (need_copy) {
      path = scm_api_list_copy(path);
      if (scm_obj_null_p(path)) return SCM_OBJ_NULL;
    }

    node->module = scm_module_new(SCM_MEM_HEAP, path);
    if (scm_obj_null_p(node->module)) return SCM_OBJ_NULL;
  }

  return node->module;
}

int
scm_moduletree_find(ScmObj tree, ScmObj name, scm_csetter_t *mod)
{
  ScmModuleTreeNode *node;
  int rslt;

  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(name) || scm_capi_pair_p(name));
  scm_assert(mod != NULL);

  name = scm_moduletree_normailize_name(name);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_moduletree_access(SCM_MODULETREE(tree)->root, name, FIND, &node);
  if (rslt < 0) return -1;

  if (node == NULL)
    scm_csetter_setq(mod, SCM_OBJ_NULL);
  else
    scm_csetter_setq(mod, node->module);

  return 0;
}

int
scm_moduletree_clean(ScmObj tree)
{
  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);

  scm_moduletree_free_tree(SCM_MODULETREE(tree)->root);
  SCM_MODULETREE(tree)->root = scm_moduletree_make_node();
  if (SCM_MODULETREE(tree)->root == NULL) return -1;

  return 0;
}

void
scm_moduletree_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_MODULETREE_TYPE_INFO);

  SCM_MODULETREE(obj)->root = NULL;
}

void
scm_moduletree_gc_finalize(ScmObj obj)
{
  scm_moduletree_finalize(obj);
}

int
scm_moduletree_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_MODULETREE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return scm_moduletree_node_gc_accept(obj, SCM_MODULETREE(obj)->root,
                                       mem, handler);
}
