#include <stdint.h>
#include <stdbool.h>

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "symbol.h"
#include "chashtbl.h"
#include "gloc.h"

ScmTypeInfo SCM_GLOC_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmGLoc),
  .gc_ini_func = scm_gloc_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_gloc_gc_accept,
  .gc_accept_func_weak = NULL
};

void
scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(gloc, &SCM_GLOC_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ(SCM_GLOC(gloc)->sym, sym);
  SCM_SETQ(SCM_GLOC(gloc)->val, val);
}

ScmObj
scm_gloc_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj sym) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &sym);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_GLOC_TYPE_INFO, mtype, SCM_REF_MAKE(gloc));
  if (SCM_OBJ_IS_NULL(gloc)) return SCM_OBJ_NULL;

  scm_gloc_initialize(gloc, sym, SCM_OBJ_NULL);

  return gloc;
}

void
scm_gloc_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_GLOC_TYPE_INFO);

  SCM_SETQ(SCM_GLOC(obj)->sym, SCM_OBJ_NULL);
  SCM_SETQ(SCM_GLOC(obj)->val, SCM_OBJ_NULL);
}

int
scm_gloc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_GLOC_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->sym, mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOC(obj)->val, mem);
}






ScmTypeInfo SCM_GLOCTBL_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmGLocTbl),
  .gc_ini_func = scm_gloctbl_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_gloctbl_gc_accept,
  .gc_accept_func_weak = NULL
};

#define SCM_GLOCTBL_SIZE 256

static size_t
scm_gloctbl_hash_func(ScmCHashTblKey key)
{
  return scm_symbol_hash_value(SCM_OBJ(key));
}

static bool
scm_gloctbl_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return (SCM_OBJ_IS_SAME_INSTANCE(key1, key2) ? true : false);
}

void
scm_gloctbl_initialize(ScmObj tbl) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&tbl);

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_GLOCTBL_TYPE_INFO);

  SCM_SETQ(SCM_GLOCTBL(tbl)->tbl,
           scm_chash_tbl_new(SCM_MEM_ALLOC_HEAP, SCM_GLOCTBL_SIZE,
                             SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                             scm_gloctbl_hash_func, scm_gloctbl_cmp_func));
  if (SCM_OBJ_IS_NULL(tbl)) return;
}

ScmObj
scm_gloctbl_new(SCM_MEM_ALLOC_TYPE_T mtype) /* GC OK */
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_GLOCTBL_TYPE_INFO, mtype, SCM_REF_MAKE(tbl));
  if (SCM_OBJ_IS_NULL(tbl)) return SCM_OBJ_NULL;

  scm_gloctbl_initialize(tbl);

  return tbl;
}

ScmObj
scm_gloctbl_gloc(ScmObj tbl, ScmObj sym) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym);

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_GLOCTBL_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  rslt = scm_chash_tbl_get(SCM_GLOCTBL(tbl)->tbl, sym, &gloc, &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return gloc;

  SCM_SETQ(gloc, scm_gloc_new(SCM_MEM_ALLOC_HEAP, sym));
  if (SCM_OBJ_IS_NULL(gloc)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_GLOCTBL(tbl)->tbl, sym, gloc);
  if (rslt != 0) return SCM_OBJ_NULL;

  return gloc;
}

ScmObj
scm_gloctbl_bind(ScmObj tbl, ScmObj sym, ScmObj val) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym, &val);

  SCM_SETQ(gloc, scm_gloctbl_gloc(tbl, sym));
  if (SCM_OBJ_IS_NULL(gloc)) return SCM_OBJ_NULL;

  scm_gloc_bind(gloc, val);

  return gloc;
}

void
scm_gloctbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_GLOCTBL_TYPE_INFO);

  SCM_SETQ(SCM_GLOCTBL(obj)->tbl, SCM_OBJ_NULL);
}

int
scm_gloctbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_GLOCTBL_TYPE_INFO);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_GLOCTBL(obj)->tbl, mem);
}
