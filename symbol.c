#include <unistd.h>
#include <string.h>
#include <assert.h>


#include "object.h"
#include "memory.h"
#include "chashtbl.h"
#include "reference.h"
#include "string.h"
#include "vm.h"
#include "symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256


ScmTypeInfo SCM_SYMBOL_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmSymbol),            /* obj_size             */
  scm_symbol_gc_initialize,     /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  scm_symbol_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

void
scm_symbol_initialize(ScmObj sym, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&sym, &str);

  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_obj_init(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ(SCM_SYMBOL_STR(sym), scm_string_dup(str));
}

ScmObj
scm_symbol_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_SYMBOL_TYPE_INFO, mtype, SCM_REF_MAKE(sym));
  scm_symbol_initialize(sym, str);

  return sym;
}

bool
scm_symbol_is_symbol(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_NOT_NULL_P(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);
}

size_t
scm_symbol_length(ScmObj sym)   /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_length(SCM_SYMBOL_STR(sym));
}

ScmObj
scm_symbol_string(ScmObj sym)
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_dup(SCM_SYMBOL_STR(sym));
}

size_t
scm_symbol_hash_value(ScmObj sym)
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_hash_value(SCM_SYMBOL_STR(sym));
}

void
scm_symbol_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ_PRIM(SCM_SYMBOL_STR(obj), SCM_OBJ_NULL);
}

int
scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_STR(obj), mem);

  return rslt;
}






ScmTypeInfo SCM_SYMTBL_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmSymTbl),
  .gc_ini_func = scm_symtbl_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_symtbl_gc_accept,
  .gc_accept_func_weak = NULL
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
  SCM_STACK_FRAME_PUSH(&tbl);

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_SYMTBL_TYPE_INFO);

  SCM_SETQ(SCM_SYMTBL(tbl)->tbl,
           scm_chash_tbl_new(SCM_MEM_ALLOC_HEAP, SCM_SYMTBL_SIZE,
                             SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ_W,
                             scm_symtbl_hash_func, scm_symtbl_cmp_func));
  if (scm_obj_null_p(tbl)) return;
}

ScmObj
scm_symtbl_new(SCM_MEM_ALLOC_TYPE_T mtype)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_SYMTBL_TYPE_INFO, mtype, SCM_REF_MAKE(tbl));
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

  rslt = scm_chash_tbl_get(SCM_SYMTBL(tbl)->tbl, str, &sym, &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return sym;

  SCM_SETQ(sym, scm_symbol_new(SCM_MEM_ALLOC_HEAP, str));
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_SYMTBL(tbl)->tbl, str, sym);
  if (rslt != 0) return SCM_OBJ_NULL;

  return sym;
}

void
scm_symtbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMTBL_TYPE_INFO);

  SCM_SETQ(SCM_SYMTBL(obj)->tbl, SCM_OBJ_NULL);
}

int
scm_symtbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMTBL_TYPE_INFO);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMTBL(obj)->tbl, mem);
}
