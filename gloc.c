#include <stdint.h>
#include <stdbool.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "reference.h"
#include "symbol.h"
#include "chashtbl.h"
#include "gloc.h"

/****************************************************************************/
/*  GLoc                                                                    */
/****************************************************************************/

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
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SLOT_SETQ(ScmGLoc, gloc, sym, sym);
  SCM_SLOT_SETQ(ScmGLoc, gloc, val, val);
}

ScmObj
scm_gloc_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj sym) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &sym);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_GLOC_TYPE_INFO, mtype, SCM_REF_MAKE(gloc));
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  scm_gloc_initialize(gloc, sym, SCM_OBJ_NULL);

  return gloc;
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
/*  GLocTbl                                                                 */
/****************************************************************************/

ScmTypeInfo SCM_GLOCTBL_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmGLocTbl),
  .gc_ini_func = scm_gloctbl_gc_initialize,
  .gc_fin_func = scm_gloctbl_gc_finalize,
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
  return (scm_obj_same_instance_p(key1, key2) ? true : false);
}

void
scm_gloctbl_initialize(ScmObj tbl) /* GC OK */
{
  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);

  SCM_GLOCTBL(tbl)->tbl =
    scm_chash_tbl_new(SCM_GLOCTBL_SIZE, tbl,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                      scm_gloctbl_hash_func, scm_gloctbl_cmp_func);

  if (SCM_GLOCTBL(tbl)->tbl == NULL) return;
}

void
scm_gloctbl_finalize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);

  if (SCM_GLOCTBL(tbl)->tbl != NULL) {
    scm_chash_tbl_end(SCM_GLOCTBL(tbl)->tbl);
    SCM_GLOCTBL(tbl)->tbl = NULL;
  }
}

ScmObj
scm_gloctbl_new(SCM_MEM_ALLOC_TYPE_T mtype) /* GC OK */
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_GLOCTBL_TYPE_INFO, mtype, SCM_REF_MAKE(tbl));
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL;

  scm_gloctbl_initialize(tbl);

  return tbl;
}

int
scm_gloctbl_find(ScmObj tbl, ScmObj sym, ScmRef gloc) /* GC OK */
{
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym);

  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  rslt = scm_chash_tbl_get(SCM_GLOCTBL(tbl)->tbl, sym, gloc, &found);
  if (rslt != 0) return -1;

  if (!found) SCM_REF_SETQ(gloc, SCM_OBJ_NULL);

  return 0;
}

ScmObj
scm_gloctbl_gloc(ScmObj tbl, ScmObj sym) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym);

  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  rslt = scm_chash_tbl_get(SCM_GLOCTBL(tbl)->tbl, sym, &gloc, &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return gloc;

  gloc = scm_gloc_new(SCM_MEM_ALLOC_HEAP, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_GLOCTBL(tbl)->tbl, sym, gloc);
  if (rslt != 0) return SCM_OBJ_NULL;

  return gloc;
}

ScmObj
scm_gloctbl_bind(ScmObj tbl, ScmObj sym, ScmObj val) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym, &val);

  gloc = scm_gloctbl_gloc(tbl, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  scm_gloc_bind(gloc, val);

  return gloc;
}

void
scm_gloctbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_GLOCTBL_TYPE_INFO);

  SCM_GLOCTBL(obj)->tbl = NULL;
}

void
scm_gloctbl_gc_finalize(ScmObj obj)
{
  scm_gloctbl_finalize(obj);
}

int
scm_gloctbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_GLOCTBL_TYPE_INFO);

  return scm_chash_tbl_gc_accept(SCM_GLOCTBL(obj)->tbl, obj, mem, handler);
}
