#ifndef INCLUDE_MODULE_H__
#define INCLUDE_MODULE_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/chashtbl.h"
#include "scythe/memory.h"


/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

typedef struct ScmGLocRec ScmGLoc;

enum {
  SCM_GLOC_FLG_KIND_VARIABLE  = 0x00,
  SCM_GLOC_FLG_KIND_KEYWORD   = 0x01,
  SCM_GLOC_FLG_KIND_DELEGATOR = 0x02,
  SCM_GLOC_FLG_KIND_MASK      = 0x03,
  SCM_GLOC_FLG_EXPORT         = 0x04,
};

struct ScmGLocRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
  unsigned int flags;
};

#define SCM_GLOC(obj) ((ScmGLoc *)(obj))

extern ScmTypeInfo SCM_GLOC_TYPE_INFO;

int scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val);
ScmObj scm_gloc_new(scm_mem_type_t mtype, ScmObj sym);
void scm_gloc_bind_variable(ScmObj gloc, ScmObj val);
void scm_gloc_bind_keyword(ScmObj gloc, ScmObj val);
void scm_gloc_forward(ScmObj gloc, ScmObj dst);
void scm_gloc_export(ScmObj gloc);
ScmObj scm_gloc_variable_value(ScmObj gloc);
ScmObj scm_gloc_keyword_value(ScmObj gloc);
ScmObj scm_gloc_delegatee(ScmObj gloc);
void scm_gloc_gc_initialize(ScmObj obj);
int scm_gloc_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_gloc_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_GLOC_TYPE_INFO);
}

static inline ScmObj
scm_gloc_symbol(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->sym;
}

static inline ScmObj
scm_gloc_value(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->val;
}

static inline bool
scm_gloc_kind_p(ScmObj gloc, unsigned int kind)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return ((SCM_GLOC(gloc)->flags & SCM_GLOC_FLG_KIND_MASK) == kind);
}

static inline bool
scm_gloc_variable_p(ScmObj gloc)
{
  return scm_gloc_kind_p(gloc, SCM_GLOC_FLG_KIND_VARIABLE);
}

static inline bool
scm_gloc_keyword_p(ScmObj gloc)
{
  return scm_gloc_kind_p(gloc, SCM_GLOC_FLG_KIND_KEYWORD);
}

static inline bool
scm_gloc_delegator_p(ScmObj gloc)
{
  return scm_gloc_kind_p(gloc, SCM_GLOC_FLG_KIND_DELEGATOR);
}

static inline bool
scm_gloc_exported_p(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return ((SCM_GLOC(gloc)->flags & SCM_GLOC_FLG_EXPORT) ? true : false);
}


/****************************************************************************/
/*  Module                                                                  */
/****************************************************************************/

typedef struct ScmModuleRec ScmModule;

struct ScmModuleRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj imports;
  ScmCHashTbl *gloctbl;
  bool in_searching;
};

#define SCM_MODULE(obj) ((ScmModule*)(obj))

extern ScmTypeInfo SCM_MODULE_TYPE_INFO;

ScmObj scm_module_P(ScmObj obj);
int scm_module_initialize(ScmObj mod, ScmObj name);
void scm_module_finalize(ScmObj mod);
ScmObj scm_module_new(scm_mem_type_t mtype, ScmObj name);
ScmObj scm_make_module(ScmObj name);
int scm_module_import(ScmObj mod, ScmObj imp, bool restrictive);
int scm_module_define_variable(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_define_keyword(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_define_alias(ScmObj mod, ScmObj sym, ScmObj src, bool export);
int scm_module_export(ScmObj mod, ScmObj sym);
ScmObj scm_module_gloc(ScmObj mod, ScmObj sym);
int scm_module_find_gloc(ScmObj mod, ScmObj sym, scm_csetter_t *setter);

int scm_module_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_module_gc_initialize(ScmObj obj);
void scm_module_gc_finalize(ScmObj obj);
int scm_module_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_module_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MODULE_TYPE_INFO);
}

static inline ScmObj
scm_module_name(ScmObj mod)
{
  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  return SCM_MODULE(mod)->name;
}


/****************************************************************************/
/*  ModuleTree                                                              */
/****************************************************************************/

#define SCM_MODULETREE_DEFAULT_BRANCH_SIZE 16

typedef struct ScmModuleTreeNodeRec ScmModuleTreeNode;
typedef struct ScmModuleTreeRec ScmModuleTree;

struct ScmModuleTreeNodeRec {
  ScmObj name;
  ScmObj module;
  ScmModuleTreeNode **branches;
  size_t capacity;
  size_t used;
};

struct ScmModuleTreeRec {
  ScmObjHeader header;
  ScmModuleTreeNode *root;
};

#define SCM_MODULETREE(obj) ((ScmModuleTree *)(obj))

extern ScmTypeInfo SCM_MODULETREE_TYPE_INFO;

int scm_moduletree_initialize(ScmObj tree);
ScmObj scm_moduletree_new(scm_mem_type_t mtype);
void scm_moduletree_finalize(ScmObj tree);
ScmObj scm_moduletree_module(ScmObj tree, ScmObj name);
int scm_moduletree_find(ScmObj tree, ScmObj name, scm_csetter_t *mod);
int scm_moduletree_clean(ScmObj tree);
void scm_moduletree_gc_initialize(ScmObj obj);
void scm_moduletree_gc_finalize(ScmObj obj);
int scm_moduletree_gc_accept(ScmObj obj, ScmGCRefHandler handler);


/****************************************************************************/
/*  Global Veriable                                                         */
/****************************************************************************/

bool scm_module_name_p(ScmObj obj);
bool scm_module_specifier_p(ScmObj obj);
int scm_find_module(ScmObj name, scm_csetter_t *mod);

int scm_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export);
int scm_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export);
int scm_refer_global_var(ScmObj module, ScmObj sym, scm_csetter_t *val);
int scm_refer_global_syx(ScmObj module, ScmObj sym, scm_csetter_t *syx);
int scm_define_global_alias(ScmObj module, ScmObj sym, ScmObj src, bool export);

int scm_find_module_cstr(const char * const *name, size_t n,
                         scm_csetter_t *mod);
int scm_module_find_gloc_cstr(const char * const *name, size_t n,
                              const char *var, scm_csetter_t *gloc);
int scm_refer_global_var_cstr(const char * const *name, size_t n,
                              const char *var, scm_csetter_t *val);

#endif  /* INCLUDE_MODULE_H__ */
