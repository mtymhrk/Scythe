#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/chashtbl.h"
#include "scythe/bedrock.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/equivalence.h"
#include "scythe/format.h"
#include "scythe/exception.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"
#include "scythe/symbol.h"
#include "scythe/module.h"


static ScmObj
get_module(ScmObj spec)
{
  ScmObj mod = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&spec,
                      &mod);

  scm_assert(scm_module_specifier_p(spec));

  if (scm_module_p(spec))
    return spec;

  r = scm_find_module(spec, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_error("failed to find loaded module: no such a module", 1, spec);
    return SCM_OBJ_NULL;
  }

  return mod;
}


/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

ScmTypeInfo SCM_GLOC_TYPE_INFO = {
  .name                = "gloc",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmGLoc),
  .gc_ini_func         = scm_gloc_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_gloc_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static void
scm_gloc_set_kind(ScmObj gloc, unsigned int kind)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  SCM_GLOC(gloc)->flags &= ~(unsigned int)SCM_GLOC_FLG_KIND_MASK;
  SCM_GLOC(gloc)->flags |= (kind & SCM_GLOC_FLG_KIND_MASK);
}

int
scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val));

  SCM_SLOT_SETQ(ScmGLoc, gloc, sym, sym);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
  SCM_GLOC(gloc)->flags = 0;

  return 0;
}

ScmObj
scm_gloc_new(scm_mem_type_t mtype, ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&gloc, &sym);

  gloc = scm_alloc_mem(&SCM_GLOC_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  if (scm_gloc_initialize(gloc, sym, SCM_UNINIT_OBJ) < 0)
    return SCM_OBJ_NULL;

  return gloc;
}

void
scm_gloc_bind_variable(ScmObj gloc, ScmObj val)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val) && !scm_landmine_object_p(val));

  scm_gloc_set_kind(gloc, SCM_GLOC_FLG_KIND_VARIABLE);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
}

void
scm_gloc_bind_keyword(ScmObj gloc, ScmObj val)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val) && !scm_landmine_object_p(val));

  scm_gloc_set_kind(gloc, SCM_GLOC_FLG_KIND_KEYWORD);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
}

void
scm_gloc_forward(ScmObj gloc, ScmObj dst)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert_obj_type(dst, &SCM_GLOC_TYPE_INFO);

  scm_gloc_set_kind(gloc, SCM_GLOC_FLG_KIND_DELEGATOR);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, dst);
}

void
scm_gloc_export(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  SCM_GLOC(gloc)->flags |= SCM_GLOC_FLG_EXPORT;
}

ScmObj
scm_gloc_variable_value(ScmObj gloc)
{
  scm_assert(scm_gloc_p(gloc));

  if (scm_gloc_variable_p(gloc))
    return scm_gloc_value(gloc);
  else
    return SCM_UNINIT_OBJ;
}

ScmObj
scm_gloc_keyword_value(ScmObj gloc)
{
  scm_assert(scm_gloc_p(gloc));

  if (scm_gloc_keyword_p(gloc))
    return scm_gloc_value(gloc);
  else
    return SCM_UNINIT_OBJ;
}

ScmObj
scm_gloc_delegatee(ScmObj gloc)
{
  ScmObj t = SCM_OBJ_INIT, r = SCM_OBJ_INIT;

  scm_assert(scm_gloc_p(gloc));

  t = r = gloc;
  do {
    if (!scm_gloc_delegator_p(r))
      return r;

    r = scm_gloc_value(r);
    if (!scm_gloc_delegator_p(r))
      return r;

    r = scm_gloc_value(r);
    t = scm_gloc_value(t);
  } while(!scm_eq_p(t, r));

  /* forwarding が循環している場合、元の GLoc を返す */
  return gloc;
}

void
scm_gloc_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_GLOC_TYPE_INFO);

  SCM_GLOC(obj)->sym = SCM_OBJ_NULL;
  SCM_GLOC(obj)->val = SCM_OBJ_NULL;
}

int
scm_gloc_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_GLOC_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->sym);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->val);
}


/****************************************************************************/
/*  Module                                                                  */
/****************************************************************************/

ScmTypeInfo SCM_MODULE_TYPE_INFO = {
  .name                = "module",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_module_obj_print,
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
  return scm_symbol_hash_value(SCM_OBJ(key));
}

static bool
scm_module_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_eq_p(SCM_OBJ(key1), SCM_OBJ(key2));
}

static int
scm_module_search_gloc(ScmObj mod, ScmObj sym, scm_csetter_t *setter)
{
  bool found;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod, &sym);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));

  rslt = scm_chash_tbl_get(SCM_MODULE(mod)->gloctbl,
                           sym, (ScmCHashTblVal *)setter, &found);
  if (rslt != 0) return -1;

  if (!found) scm_csetter_setq(setter, SCM_OBJ_NULL);

  return 0;
}

enum {
  SCM_MODULE_VARIABLE,
  SCM_MODULE_KEYWORD,
  SCM_MODULE_ALIAS,
};

static int
scm_module_define(ScmObj mod, ScmObj sym, ScmObj val, bool export, int kind)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mod, &sym, &val,
                      &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val) && !scm_landmine_object_p(val));
  scm_assert(kind != SCM_MODULE_ALIAS || scm_gloc_p(val));

  gloc = scm_module_gloc(mod, sym);
  if (scm_obj_null_p(gloc)) return -1;

  switch (kind) {
  case SCM_MODULE_VARIABLE:
    scm_gloc_bind_variable(gloc, val);
    break;
  case SCM_MODULE_KEYWORD:
    scm_gloc_bind_keyword(gloc, val);
    break;
  case SCM_MODULE_ALIAS:
    scm_gloc_forward(gloc, val);
    break;
  default:
    scm_assert(false);
    break;
  }

  if (export)
    scm_gloc_export(gloc);

  return 0;
}

static int
scm_module_find_exported_sym(ScmObj mod, ScmObj sym, scm_csetter_t *setter)
{
  ScmObj lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  ScmObj res = SCM_OBJ_INIT, gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod, &sym,
                      &lst, &elm, &imp,
                      &res, &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));
  scm_assert(setter != NULL);

  if (SCM_MODULE(mod)->in_searching)
    return 0;

  SCM_MODULE(mod)->in_searching = true;

  rslt = scm_module_search_gloc(mod, sym, setter);
  if (rslt < 0) goto err;

  gloc = scm_csetter_val(setter);
  if (scm_obj_not_null_p(gloc) && scm_gloc_exported_p(gloc))
    goto done;

  scm_csetter_setq(setter, SCM_OBJ_NULL);

  for (lst = SCM_MODULE(mod)->imports; scm_pair_p(lst); lst = scm_cdr(lst)) {
    elm = scm_car(lst);
    imp = scm_car(elm);
    res = scm_cdr(elm);
    if (scm_true_p(res))
      continue;

    rslt = scm_module_find_exported_sym(imp, sym, setter);
    if (rslt < 0) goto err;

    if (scm_obj_not_null_p(scm_csetter_val(setter)))
      goto done;
  }

 done:
  SCM_MODULE(mod)->in_searching = false;
  return 0;

 err:
  SCM_MODULE(mod)->in_searching = false;
  return -1;
}

ScmObj
scm_module_P(ScmObj obj)
{
  return (scm_module_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

int
scm_module_initialize(ScmObj mod, ScmObj name)
{
  SCM_REFSTK_INIT_REG(&mod, &name);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_pair_p(name));

  SCM_SLOT_SETQ(ScmModule, mod, name, name);

  SCM_MODULE(mod)->gloctbl = scm_chash_tbl_new(mod,
                                               SCM_MODULE_GLOCTBL_SIZE,
                                               SCM_CHASH_TBL_SCMOBJ,
                                               SCM_CHASH_TBL_SCMOBJ,
                                               scm_module_hash_func,
                                               scm_module_cmp_func);
  if (SCM_MODULE(mod)->gloctbl == NULL) return -1;

  SCM_MODULE(mod)->imports = SCM_NIL_OBJ;
  SCM_MODULE(mod)->in_searching = false;

  return 0;
}

void
scm_module_finalize(ScmObj mod)
{
  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  if (SCM_MODULE(mod)->gloctbl != NULL) {
    scm_chash_tbl_end(SCM_MODULE(mod)->gloctbl);
    SCM_MODULE(mod)->gloctbl = NULL;
  }
}

ScmObj
scm_module_new(scm_mem_type_t mtype, ScmObj name)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &mod);

  scm_assert(scm_pair_p(name));

  mod = scm_alloc_mem(&SCM_MODULE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(mod)) return SCM_OBJ_NULL;

  if (scm_module_initialize(mod, name) < 0)
    return SCM_OBJ_NULL;

  return mod;
}

ScmObj
scm_make_module(ScmObj name)
{
  ScmObj tree = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&name,
                      &tree, &mod);

  scm_assert(scm_module_name_p(name));

  tree = scm_current_module_tree();
  rslt = scm_moduletree_find(tree, name, SCM_CSETTER_L(mod));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(mod)) {
    scm_error("failed to make a module: already exist", 1, name);
    return SCM_OBJ_NULL;
  }

  return scm_moduletree_module(tree, name);
}

int
scm_module_import(ScmObj mod, ScmObj imp, bool res)
{
  ScmObj elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mod, &imp,
                      &elm);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_module_specifier_p(imp));

  imp = get_module(imp);
  if (scm_obj_null_p(imp)) return -1;

  elm = scm_cons(imp, res ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
  if (scm_obj_null_p(elm)) return -1;

  elm = scm_cons(elm, SCM_MODULE(mod)->imports);
  if (scm_obj_null_p(elm)) return -1;

  SCM_SLOT_SETQ(ScmModule, mod, imports, elm);

  return 0;
}

int
scm_module_define_variable(ScmObj mod, ScmObj sym, ScmObj val, bool export)
{
  return scm_module_define(mod, sym, val, export, SCM_MODULE_VARIABLE);
}

int
scm_module_define_keyword(ScmObj mod, ScmObj sym, ScmObj val, bool export)
{
  return scm_module_define(mod, sym, val, export, SCM_MODULE_KEYWORD);
}

int
scm_module_define_alias(ScmObj mod, ScmObj sym, ScmObj src, bool export)
{
  return scm_module_define(mod, sym, src, export, SCM_MODULE_ALIAS);
}

int
scm_module_export(ScmObj mod, ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mod, &sym,
                      &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));

  gloc = scm_module_gloc(mod, sym);
  if (scm_obj_null_p(gloc)) return -1;

  scm_gloc_export(gloc);

  return 0;
}

ScmObj
scm_module_gloc(ScmObj mod, ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod, &sym,
                      &gloc);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));

  rslt = scm_chash_tbl_get(SCM_MODULE(mod)->gloctbl, sym,
                           (ScmCHashTblVal *)SCM_CSETTER_L(gloc), &found);

  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return gloc;

  gloc = scm_gloc_new(SCM_MEM_HEAP, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_MODULE(mod)->gloctbl, sym, gloc);
  if (rslt != 0) return SCM_OBJ_NULL;

  return gloc;
}

int
scm_module_find_gloc(ScmObj mod, ScmObj sym, scm_csetter_t *setter)
{
  ScmObj lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod, &sym,
                      &lst, &elm, &imp);

  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);
  scm_assert(scm_symbol_p(sym));
  scm_assert(setter != NULL);

  if (SCM_MODULE(mod)->in_searching)
    return 0;

  SCM_MODULE(mod)->in_searching = true;

  rslt = scm_module_search_gloc(mod, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) goto err;

  if (scm_obj_not_null_p(gloc))
    goto done;

  for (lst = SCM_MODULE(mod)->imports;
       scm_pair_p(lst);
       lst = scm_cdr(lst)) {
    elm = scm_car(lst);
    imp = scm_car(elm);
    rslt = scm_module_find_exported_sym(imp, sym, SCM_CSETTER_L(gloc));
    if (rslt < 0) goto err;;

    if (scm_obj_not_null_p(gloc))
      goto done;
  }

 done:
  scm_csetter_setq(setter, (scm_obj_null_p(gloc) ?
                            SCM_OBJ_NULL : scm_gloc_delegatee(gloc)));
  SCM_MODULE(mod)->in_searching = false;
  return 0;

 err:
  SCM_MODULE(mod)->in_searching = false;
  return -1;
}

int
scm_module_obj_print(ScmObj obj, ScmObj port, int kind,
                     ScmObjPrintHandler handler)
{
  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);

  return scm_pformat_cstr(port, "#<module ~a>",
                          SCM_MODULE(obj)->name, SCM_OBJ_NULL);
}

void
scm_module_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);

  SCM_MODULE(obj)->name = SCM_OBJ_NULL;
  SCM_MODULE(obj)->imports = SCM_OBJ_NULL;
  SCM_MODULE(obj)->gloctbl = NULL;
}

void
scm_module_gc_finalize(ScmObj obj)
{
  scm_module_finalize(obj);
}

int
scm_module_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_MODULE_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_MODULE(obj)->name);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_MODULE(obj)->imports);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  if (SCM_MODULE(obj)->gloctbl != NULL) {
    rslt = scm_chash_tbl_gc_accept(SCM_MODULE(obj)->gloctbl,
                                   obj, handler, false);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}


/****************************************************************************/
/*  ModuleTree                                                              */
/****************************************************************************/

ScmTypeInfo SCM_MODULETREE_TYPE_INFO = {
  .name                              = "moduletree",
  .flags                             = SCM_TYPE_FLG_MMO,
  .obj_print_func                    = NULL,
  .obj_size                          = sizeof(ScmModuleTree),
  .gc_ini_func                       = scm_moduletree_gc_initialize,
  .gc_fin_func                       = scm_moduletree_gc_finalize,
  .gc_accept_func                    = scm_moduletree_gc_accept,
  .gc_accept_func_weak               = NULL,
  .extra                             = NULL
};

enum { ADD, UPDATE, FIND };

static ScmModuleTreeNode *
scm_moduletree_make_node(void)
{
  ScmModuleTreeNode *node;

  node = scm_malloc(sizeof(ScmModuleTreeNode));
  if (node == NULL) return NULL;

  node->name = SCM_OBJ_NULL;
  node->module = SCM_OBJ_NULL;

  node->branches = scm_malloc(sizeof(ScmModuleTreeNode *)
                                   * SCM_MODULETREE_DEFAULT_BRANCH_SIZE);
  if (node->branches == NULL) {
    scm_free(node);
    return NULL;
  }

  node->capacity = SCM_MODULETREE_DEFAULT_BRANCH_SIZE;
  node->used = 0;

  return node;
}

static int
scm_moduletree_free_node(ScmModuleTreeNode *node)
{
  if (node == NULL) return 0;

  scm_free(node->branches);
  scm_free(node);

  return 0;
}

static int
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

static int
scm_moduletree_node_gc_accept(ScmObj tree, ScmModuleTreeNode *node,
                              ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  if (node == NULL) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, tree, node->name);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, tree, node->module);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < node->used; i++) {
    rslt = scm_moduletree_node_gc_accept(tree, node->branches[i], handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return 0;
}

static ScmModuleTreeNode *
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
      scm_error("failed to register a module: buffer overlfow", 0);
      return NULL;
    }
    else if (node->capacity > SIZE_MAX / 2) {
      new_cap = SIZE_MAX;
    }
    else {
      new_cap = node->capacity * 2;
    }

    new_bra = scm_realloc(node->branches,
                               sizeof(ScmModuleTreeNode) * new_cap);
    if (new_bra == NULL) return NULL;

    node->branches = new_bra;
    node->capacity = new_cap;
  }

  node->branches[node->used++] = new;

  return new;
}

static int
scm_moduletree_access(ScmModuleTreeNode *root, ScmObj path, int mode,
                      ScmModuleTreeNode **node)
{
  ScmObj name = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&path,
                      &name);

  scm_assert(root != NULL);
  scm_assert(scm_pair_p(path));
  scm_assert(node != NULL);

  name = scm_car(path);
  if (!scm_symbol_p(name)) {
    scm_error("failed to access module: "
                   "module name must be a list whose members are symbols", 0);
    return -1;
  }

  path = scm_cdr(path);
  *node = NULL;
  for (size_t i = 0; i < root->used; i++) {
    if (scm_eq_p(name, root->branches[i]->name)) {
      *node = root->branches[i];
      break;
    }
  }

  if (scm_nil_p(path)) {
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

static ScmObj
scm_moduletree_normailize_name(ScmObj name)
{
  scm_assert(scm_symbol_p(name) || scm_pair_p(name));

  if (scm_pair_p(name)) return name;

  return scm_list(1, name);
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
scm_moduletree_new(scm_mem_type_t mtype)
{
  ScmObj tree = SCM_OBJ_INIT;

  tree = scm_alloc_mem(&SCM_MODULETREE_TYPE_INFO, 0, mtype);
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

  SCM_REFSTK_INIT_REG(&tree, &name,
                      &path);

  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);
  scm_assert(scm_symbol_p(name) || scm_pair_p(name));

  path = scm_moduletree_normailize_name(name);
  if (scm_obj_null_p(path)) return SCM_OBJ_NULL;

  need_copy = scm_eq_p(path, name);

  rslt = scm_moduletree_access(SCM_MODULETREE(tree)->root, path, UPDATE, &node);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(node->module)) {
    if (need_copy) {
      path = scm_list_copy(path);
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

  SCM_REFSTK_INIT_REG(&tree, &name);

  scm_assert_obj_type(tree, &SCM_MODULETREE_TYPE_INFO);
  scm_assert(scm_symbol_p(name) || scm_pair_p(name));
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
scm_moduletree_gc_initialize(ScmObj obj)
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
scm_moduletree_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_MODULETREE_TYPE_INFO);

  return scm_moduletree_node_gc_accept(obj, SCM_MODULETREE(obj)->root,
                                       handler);
}


/****************************************************************************/
/*  Global Veriable                                                         */
/****************************************************************************/

bool
scm_module_name_p(ScmObj obj)
{
  return (scm_symbol_p(obj) || scm_pair_p(obj));

}

bool
scm_module_specifier_p(ScmObj obj)
{
  return (scm_module_p(obj) || scm_symbol_p(obj) || scm_pair_p(obj));
}

int
scm_find_module(ScmObj name, scm_csetter_t *mod)
{
  scm_assert(scm_module_name_p(name));
  return scm_moduletree_find(scm_current_module_tree(), name, mod);
}

int
scm_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export)
{
  SCM_REFSTK_INIT_REG(&module, &sym, &val);

  scm_assert(scm_module_specifier_p(module));
  scm_assert(scm_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val) && !scm_landmine_object_p(val));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_define_variable(module, sym, val, export);
}

int
scm_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  SCM_REFSTK_INIT_REG(&module, &sym, &syx);

  scm_assert(scm_module_specifier_p(module));
  scm_assert(scm_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(syx) && !scm_landmine_object_p(syx));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  return scm_module_define_keyword(module, sym, syx, export);
}

int
scm_refer_global_var(ScmObj module, ScmObj sym, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &gloc, &v);

  scm_assert(scm_module_specifier_p(module));
  scm_assert(scm_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  rslt = scm_module_find_gloc(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_variable_value(gloc);
    if (scm_landmine_object_p(v))
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
scm_refer_global_syx(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module, &sym,
                      &gloc, &v);


  scm_assert(scm_module_specifier_p(module));
  scm_assert(scm_symbol_p(sym));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  rslt = scm_module_find_gloc(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_keyword_value(gloc);
    if (scm_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else
    v = SCM_OBJ_NULL;

  if (syx != NULL)
    scm_csetter_setq(syx, v);

  return 0;
}

int
scm_define_global_alias(ScmObj module, ScmObj sym, ScmObj src, bool export)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module, &sym, &src,
                      &gloc);

  scm_assert(scm_module_specifier_p(module));
  scm_assert(scm_symbol_p(sym));
  scm_assert(scm_symbol_p(src));

  module = get_module(module);
  if (scm_obj_null_p(module)) return -1;

  r = scm_module_find_gloc(module, src, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_null_p(gloc)) {
    scm_error("failed to define alias of global variable: unbound variable",
              1, src);
    return -1;
  }

  return scm_module_define_alias(module, sym, gloc, export);
}

int
scm_find_module_cstr(const char * const *name, size_t n,
                     scm_csetter_t *mod)
{
  ScmObj spec = SCM_OBJ_INIT;
  ScmObj name_syms[n];

  for (size_t i = 0; i < n; i++) name_syms[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&spec);
  SCM_REFSTK_REG_ARY(name_syms, n);

  scm_assert(name != NULL);
  scm_assert(mod != NULL);

  for (size_t i = 0; i < n; i++) {
    name_syms[i] = scm_make_symbol_from_cstr(name[i], SCM_ENC_SRC);
    if (scm_obj_null_p(name_syms[i])) return -1;
  }

  spec = scm_list_cv(name_syms, n);
  if (scm_obj_null_p(spec)) return -1;

  return scm_find_module(spec, mod);
}

int
scm_module_find_gloc_cstr(const char * const *name, size_t n,
                          const char *var, scm_csetter_t *gloc)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&sym, &mod);

  scm_assert(name != NULL);
  scm_assert(var != NULL);
  scm_assert(gloc != NULL);

  r = scm_find_module_cstr(name, n, SCM_CSETTER_L(mod));
  if (r < 0) return -1;

  if (scm_obj_null_p(mod)) {
    scm_csetter_setq(gloc, SCM_OBJ_NULL);
    return 0;
  }

  sym = scm_make_symbol_from_cstr(var, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return -1;

  return scm_module_find_gloc(mod, sym, gloc);
}

int
scm_refer_global_var_cstr(const char * const *name, size_t n,
                          const char *var, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&gloc, &v);

  scm_assert(name != NULL);
  scm_assert(var != NULL);
  scm_assert(val != NULL);

  r = scm_module_find_gloc_cstr(name, n, var, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_variable_value(gloc);
    if (scm_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else {
    v = SCM_OBJ_NULL;
  }

  scm_csetter_setq(val, v);
  return 0;
}
