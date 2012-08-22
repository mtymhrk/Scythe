#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "chashtbl.h"
#include "gloc.h"

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
/*  GLocTbl                                                                 */
/****************************************************************************/

ScmTypeInfo SCM_GLOCTBL_TYPE_INFO = {
  .name                = "gloctbl",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_gloctbl_pretty_print,
  .obj_size            = sizeof(ScmGLocTbl),
  .gc_ini_func         = scm_gloctbl_gc_initialize,
  .gc_fin_func         = scm_gloctbl_gc_finalize,
  .gc_accept_func      = scm_gloctbl_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

#define SCM_GLOCTBL_SIZE 256

static size_t
scm_gloctbl_hash_func(ScmCHashTblKey key)
{
  return scm_capi_symbol_hash_value(SCM_OBJ(key));
}

static bool
scm_gloctbl_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return (scm_capi_eq_p(SCM_OBJ(key1), SCM_OBJ(key2)) ? true : false);
}

int
scm_gloctbl_initialize(ScmObj tbl) /* GC OK */
{
  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);

  SCM_GLOCTBL(tbl)->tbl =
    scm_chash_tbl_new(tbl, SCM_GLOCTBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                      scm_gloctbl_hash_func, scm_gloctbl_cmp_func);

  if (SCM_GLOCTBL(tbl)->tbl == NULL) return -1; /* [ERR]: [through] */

  return 0;
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
scm_gloctbl_new(SCM_MEM_TYPE_T mtype) /* GC OK */
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  tbl = scm_capi_mem_alloc(&SCM_GLOCTBL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_gloctbl_initialize(tbl) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return tbl;
}

int
scm_gloctbl_find(ScmObj tbl, ScmObj sym, scm_csetter_t *setter) /* GC OK */
{
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&tbl, &sym);

  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));

  rslt = scm_chash_tbl_get(SCM_GLOCTBL(tbl)->tbl,
                           sym, (ScmCHashTblVal *)setter, &found);
  if (rslt != 0) return -1;     /* [ERR]: [through] */

  if (!found) scm_csetter_setq(setter, SCM_OBJ_NULL);

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
  scm_assert(scm_capi_symbol_p(sym));

  rslt = scm_chash_tbl_get(SCM_GLOCTBL(tbl)->tbl, sym,
                           (ScmCHashTblVal *)SCM_CSETTER_L(gloc), &found);
  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (found) return gloc;

  gloc = scm_gloc_new(SCM_MEM_HEAP, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_chash_tbl_insert(SCM_GLOCTBL(tbl)->tbl, sym, gloc);
  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return gloc;
}

ScmObj
scm_gloctbl_bind(ScmObj tbl, ScmObj sym, ScmObj val) /* GC OK */
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&gloc, &tbl, &sym, &val);

  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(sym));
  scm_assert(scm_obj_not_null_p(val));

  gloc = scm_gloctbl_gloc(tbl, sym);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  scm_gloc_bind(gloc, val);

  return gloc;
}

void
scm_gloctbl_clean(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_GLOCTBL_TYPE_INFO);

  scm_chash_tbl_clean(SCM_GLOCTBL(tbl)->tbl);
}

int
scm_gloctbl_pretty_print(ScmObj obj, ScmObj port, bool wirte_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_GLOCTBL_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<gloctbl %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
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
