#ifndef INCLUDE_MODULE_H__
#define INCLUDE_MODULE_H__

typedef struct ScmGLocRec ScmGLoc;
typedef struct ScmModuleRec ScmModule;
typedef struct ScmModuleTblRec ScmModuleTbl;

#define SCM_GLOC(obj) ((ScmGLoc *)(obj))
#define SCM_MODULE(obj) ((ScmModule*)(obj))
#define SCM_MODULETBL(obj) ((ScmModuleTbl*)(obj))

#include "object.h"
#include "api_enum.h"
#include "chashtbl.h"

/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

extern ScmTypeInfo SCM_GLOC_TYPE_INFO;

struct ScmGLocRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
  bool exported;
};

inline void
scm_gloc_bind(ScmObj gloc, ScmObj val)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
}

inline ScmObj
scm_gloc_symbol(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->sym;
}

inline ScmObj
scm_gloc_value(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->val;
}

inline bool
scm_gloc_exported_p(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->exported;
}

inline void
scm_gloc_export(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  SCM_GLOC(gloc)->exported = true;
}

int scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val);
ScmObj scm_gloc_new(SCM_MEM_TYPE_T mtype, ScmObj sym);

int scm_gloc_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_gloc_gc_initialize(ScmObj obj, ScmObj mem);
int scm_gloc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


/****************************************************************************/
/*  Module                                                                  */
/****************************************************************************/

extern ScmTypeInfo SCM_MODULE_TYPE_INFO;

struct ScmModuleRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj *imports;
  size_t imp_capacity;
  size_t nr_imp;
  ScmCHashTbl *eval_gloctbl;
  ScmCHashTbl *cmpl_gloctbl;
};

#ifdef SCM_UNIT_TEST

int scm_module_expand_imports_if_needed(ScmObj mod, unsigned int add);
int scm_module_search_gloc(ScmObj mod, ScmObj sym, int type,
                           scm_csetter_t *setter);
ScmObj scm_module_gloc(ScmObj mod, ScmObj sym, int type);
int scm_module_define(ScmObj mod,
                      ScmObj sym, ScmObj val, bool export, int type);
int scm_module_export(ScmObj mod, ScmObj sym, int type);
int scm_module_find_exported_sym(ScmObj mod,
                                 ScmObj sym, int type, scm_csetter_t *setter);
int scm_module_find_sym(ScmObj mod,
                        ScmObj sym, int type, scm_csetter_t *setter);

#endif

int scm_module_initialize(ScmObj mod, ScmObj name);
void scm_module_finalize(ScmObj mod);
ScmObj scm_module_new(SCM_MEM_TYPE_T mtype, ScmObj name);
int scm_module_import(ScmObj mod, ScmObj imp);
int scm_module_define_eval(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_define_cmpl(ScmObj mod, ScmObj sym, ScmObj val, bool export);
int scm_module_export_eval(ScmObj mod, ScmObj sym);
int scm_module_export_cmpl(ScmObj mod, ScmObj sym);
ScmObj scm_module_gloc_eval(ScmObj mod, ScmObj sym);
ScmObj scm_module_gloc_cmpl(ScmObj mod, ScmObj sym);
int scm_module_find_sym_eval(ScmObj mod, ScmObj sym, scm_csetter_t *setter);
int scm_module_find_sym_cmpl(ScmObj mod, ScmObj sym, scm_csetter_t *setter);

int scm_module_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_module_gc_initialize(ScmObj obj, ScmObj mem);
void scm_module_gc_finalize(ScmObj obj);
int scm_module_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_module_name(ScmObj mod)
{
  scm_assert_obj_type(mod, &SCM_MODULE_TYPE_INFO);

  return SCM_MODULE(mod)->name;
}


/****************************************************************************/
/*  ModuleTbl                                                               */
/****************************************************************************/

extern ScmTypeInfo SCM_MODULETBL_TYPE_INFO;

struct ScmModuleTblRec {
  ScmObjHeader header;
  ScmCHashTbl *tbl;
};

int scm_moduletbl_initialize(ScmObj tbl);
void scm_moduletbl_finalize(ScmObj tbl);
ScmObj scm_moduletbl_new(SCM_MEM_TYPE_T mtype);
ScmObj scm_moduletbl_module(ScmObj tbl, ScmObj name);
int scm_moduletbl_find(ScmObj tbl, ScmObj name, scm_csetter_t *mod);
int scm_moduletbl_clean(ScmObj tbl);

void scm_moduletbl_gc_initialize(ScmObj obj, ScmObj mem);
void scm_moduletbl_gc_finalize(ScmObj obj);
int scm_moduletbl_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

#endif  /* INCLUDE_MODULE_H__ */
