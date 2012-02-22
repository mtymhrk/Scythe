#include <unistd.h>
#include <string.h>
#include <assert.h>


#include "object.h"
#include "reference.h"
#include "api.h"
#include "chashtbl.h"
#include "string.h"
#include "symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256


ScmTypeInfo SCM_SYMBOL_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmSymbol),
  .gc_ini_func         = scm_symbol_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_symbol_gc_accept,
  .gc_accept_func_weak = NULL,
};

void
scm_symbol_initialize(ScmObj sym, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&sym, &str);

  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  scm_obj_init(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SLOT_SETQ(ScmSymbol, sym, str, scm_string_dup(str));
}

ScmObj
scm_symbol_new(SCM_CAPI_MEM_TYPE_T mtype, ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  sym = scm_capi_mem_alloc(&SCM_SYMBOL_TYPE_INFO, mtype);
  scm_symbol_initialize(sym, str);

  return sym;
}

size_t
scm_symbol_length(ScmObj sym)   /* GC OK */
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_length(SCM_SYMBOL_STR(sym));
}

ScmObj
scm_symbol_string(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_dup(SCM_SYMBOL_STR(sym));
}

size_t
scm_symbol_hash_value(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_hash_value(SCM_SYMBOL_STR(sym));
}

void
scm_symbol_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  SCM_SYMBOL_STR(obj) = SCM_OBJ_NULL;
}

int
scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_STR(obj), mem);

  return rslt;
}






ScmTypeInfo SCM_SYMTBL_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmSymTbl),
  .gc_ini_func = scm_symtbl_gc_initialize,
  .gc_fin_func = scm_symtbl_gc_finalize,
  .gc_accept_func = scm_symtbl_gc_accept,
  .gc_accept_func_weak = scm_symtbl_gc_accept_weak
};

#define SCM_SYMTBL_SIZE 256

static size_t
scm_symtbl_hash_func(ScmCHashTblKey key)
{
  return scm_string_hash_value(SCM_OBJ(key));
}

static bool
scm_symtbl_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_string_is_equal(key1, key2);
}

void
scm_symtbl_initialize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  SCM_SYMTBL(tbl)->tbl =
    scm_chash_tbl_new(SCM_SYMTBL_SIZE, tbl,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ_W,
                      scm_symtbl_hash_func, scm_symtbl_cmp_func);
  if (scm_obj_null_p(tbl)) return;
}

void
scm_symtbl_finalize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  if (SCM_SYMTBL(tbl)->tbl != NULL) {
    scm_chash_tbl_end(SCM_SYMTBL(tbl)->tbl);
    SCM_SYMTBL(tbl)->tbl = NULL;
  }
}

ScmObj
scm_symtbl_new(SCM_CAPI_MEM_TYPE_T mtype)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  tbl = scm_capi_mem_alloc(&SCM_SYMTBL_TYPE_INFO, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL;

  scm_symtbl_initialize(tbl);

  return tbl;
}

ScmObj
scm_symtbl_symbol(ScmObj tbl, ScmObj str)
{
  ScmObj sym = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &tbl, &str);

  rslt = scm_chash_tbl_get(SCM_SYMTBL(tbl)->tbl,
                           str,
                           (ScmCHashTblVal *)SCM_CSETTER_L(sym),
                           &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return sym;

  sym = scm_symbol_new(SCM_CAPI_MEM_HEAP, str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_SYMTBL(tbl)->tbl, str, sym);
  if (rslt != 0) return SCM_OBJ_NULL;

  return sym;
}

void
scm_symtbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  SCM_SYMTBL(obj)->tbl = NULL;
}

void
scm_symtbl_gc_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  scm_symtbl_finalize(obj);
}

int
scm_symtbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  return scm_chash_tbl_gc_accept(SCM_SYMTBL(obj)->tbl, obj, mem, handler);
}

int
scm_symtbl_gc_accept_weak(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  return scm_chash_tbl_gc_accept_weak(SCM_SYMTBL(obj)->tbl, obj, mem, handler);
}
