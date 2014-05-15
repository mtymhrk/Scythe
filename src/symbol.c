#include <unistd.h>
#include <stdio.h>
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
  .name                = "symbol",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_symbol_obj_print,
  .obj_size            = sizeof(ScmSymbol),
  .gc_ini_func         = scm_symbol_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_symbol_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_symbol_initialize(ScmObj sym, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&sym, &str);

  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  SCM_SLOT_SETQ(ScmSymbol, sym, str, scm_string_dup(str));

  return 0;
}

ScmObj
scm_symbol_new(SCM_MEM_TYPE_T mtype, ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  sym = scm_capi_mem_alloc(&SCM_SYMBOL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_symbol_initialize(sym, str) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

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

  /* TODO: string を複製するのではなく、シンボルが immutable な文字列を保持す
   *       るに変更し、それを直接返すようにする
   */
  return scm_string_dup(SCM_SYMBOL_STR(sym));
}

size_t
scm_symbol_hash_value(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_hash_value(SCM_SYMBOL_STR(sym));
}

int
scm_symbol_cmp(ScmObj s1, ScmObj s2, int *rslt)
{
  scm_assert_obj_type(s1, &SCM_SYMBOL_TYPE_INFO);
  scm_assert_obj_type(s1, &SCM_SYMBOL_TYPE_INFO);

  if (rslt == NULL) return 0;

  if (scm_obj_same_instance_p(s1, s2)) {
    *rslt = 0;
    return 0;
  }
  else {
    return scm_string_cmp(SCM_SYMBOL_STR(s1), SCM_SYMBOL_STR(s2), rslt);
  }
}

int
scm_symbol_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  if (ext_rep) {
    int r = scm_string_escape_ctrl_and_nonascii_write(SCM_SYMBOL_STR(obj),
                                                      port);
    if (r < 0) return -1;    /* [ERR]: [through] */
  }
  else {
    ssize_t r = scm_capi_write_string(SCM_SYMBOL_STR(obj), port, -1, -1);
    if (r < 0) return -1; /* [ERR]: [through] */
  }

  return 0;
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
  .name                = "symtbl",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmSymTbl),
  .gc_ini_func         = scm_symtbl_gc_initialize,
  .gc_fin_func         = scm_symtbl_gc_finalize,
  .gc_accept_func      = scm_symtbl_gc_accept,
  .gc_accept_func_weak = scm_symtbl_gc_accept_weak,
  .extra               = NULL,
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

int
scm_symtbl_initialize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  SCM_SYMTBL(tbl)->tbl =
    scm_chash_tbl_new(tbl, SCM_SYMTBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ_W,
                      scm_symtbl_hash_func, scm_symtbl_cmp_func);
  if (scm_obj_null_p(tbl)) return -1; /* [ERR]: [thourhg] */

  return 0;
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
scm_symtbl_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  tbl = scm_capi_mem_alloc(&SCM_SYMTBL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_symtbl_initialize(tbl) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return tbl;
}

ScmObj
scm_symtbl_symbol(ScmObj tbl, ScmObj str)
{
  ScmObj sym = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &tbl, &str);

  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  str = scm_string_dup(str);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_get(SCM_SYMTBL(tbl)->tbl,
                           str,
                           (ScmCHashTblVal *)SCM_CSETTER_L(sym),
                           &found);
  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (found) return sym;

  sym = scm_symbol_new(SCM_MEM_HEAP, str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_chash_tbl_insert(SCM_SYMTBL(tbl)->tbl, str, sym);
  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return sym;
}

void
scm_symtbl_clean(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  scm_chash_tbl_clean(SCM_SYMTBL(tbl)->tbl);
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
