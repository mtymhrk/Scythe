#ifndef INCLUDE_MODULE_H__
#define INCLUDE_MODULE_H__

typedef struct ScmGLocRec ScmGLoc;
typedef struct ScmModuleRec ScmModule;
typedef struct ScmModuleTreeNodeRec ScmModuleTreeNode;
typedef struct ScmModuleTreeRec ScmModuleTree;

#define SCM_GLOC(obj) ((ScmGLoc *)(obj))
#define SCM_MODULE(obj) ((ScmModule*)(obj))
#define SCM_MODULETREE(obj) ((ScmModuleTree *)(obj))

#include "scythe/object.h"
#include "scythe/chashtbl.h"


/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

enum { SCM_GLOC_FLG_KEYWORD = 0x01, SCM_GLOC_FLG_EXPORT = 0x02 };

extern ScmTypeInfo SCM_GLOC_TYPE_INFO;

struct ScmGLocRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
  unsigned int flags;
};

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
scm_gloc_keyword_p(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return ((SCM_GLOC(gloc)->flags & SCM_GLOC_FLG_KEYWORD) ? true : false);
}

static inline bool
scm_gloc_variable_p(ScmObj gloc)
{
  return (!scm_gloc_keyword_p(gloc));
}

static inline bool
scm_gloc_exported_p(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return ((SCM_GLOC(gloc)->flags & SCM_GLOC_FLG_EXPORT) ? true : false);
}


int scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val);
void scm_gloc_bind_variable(ScmObj gloc, ScmObj val);
void scm_gloc_bind_keyword(ScmObj gloc, ScmObj val);
void scm_gloc_export(ScmObj gloc);

void scm_gloc_gc_initialize(ScmObj obj);
int scm_gloc_gc_accept(ScmObj obj, ScmGCRefHandler handler);


/****************************************************************************/
/*  Module                                                                  */
/****************************************************************************/

extern ScmTypeInfo SCM_MODULE_TYPE_INFO;

struct ScmModuleRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj imports;
  ScmCHashTbl *gloctbl;
  bool in_searching;
};


int scm_module_initialize(ScmObj mod, ScmObj name);
void scm_module_finalize(ScmObj mod);
int scm_module_import(ScmObj mod, ScmObj imp, bool restrictive);
int scm_module_define_variable(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_define_keyword(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_export(ScmObj mod, ScmObj sym);
ScmObj scm_module_gloc(ScmObj mod, ScmObj sym);
int scm_module_find_sym(ScmObj mod, ScmObj sym, scm_csetter_t *setter);

int scm_module_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_module_gc_initialize(ScmObj obj);
void scm_module_gc_finalize(ScmObj obj);
int scm_module_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_module_name(ScmObj mod)
{
  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  return SCM_MODULE(mod)->name;
}


/****************************************************************************/
/*  ModuleTree                                                              */
/****************************************************************************/

extern ScmTypeInfo SCM_MODULETREE_TYPE_INFO;

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

#define SCM_MODULETREE_DEFAULT_BRANCH_SIZE 16


int scm_moduletree_initialize(ScmObj tree);
void scm_moduletree_finalize(ScmObj tree);
ScmObj scm_moduletree_module(ScmObj tree, ScmObj name);
int scm_moduletree_find(ScmObj tree, ScmObj name, scm_csetter_t *mod);
int scm_moduletree_clean(ScmObj tree);

void scm_moduletree_gc_initialize(ScmObj obj);
void scm_moduletree_gc_finalize(ScmObj obj);
int scm_moduletree_gc_accept(ScmObj obj, ScmGCRefHandler handler);


#endif  /* INCLUDE_MODULE_H__ */
