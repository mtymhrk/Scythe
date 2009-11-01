#include <unistd.h>
#include <string.h>
#include <assert.h>


#include "object.h"
#include "memory.h"
#include "obuffer.h"
#include "basichash.h"
#include "reference.h"
#include "string.h"
#include "vm.h"
#include "symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256


ScmTypeInfo SCM_SYMBOL_TYPE_INFO = {
  scm_symbol_pretty_print,      /* pp_func              */
  sizeof(ScmSymbol),            /* obj_size             */
  scm_symbol_gc_initialize,     /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  scm_symbol_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

ScmTypeInfo SCM_SYMTABLE_TYPE_INFO = {
  scm_symtable_pretty_print,      /* pp_func              */
  sizeof(ScmSymTable),            /* obj_size             */
  scm_symtable_gc_initialize,     /* gc_ini_func          */
  scm_symtable_gc_finalize,       /* gc_fin_func          */
  scm_symtable_gc_accept,         /* gc_accept_func       */
  scm_symtable_gc_accept_week,    /* gc_accpet_func_weak  */
};


static unsigned int
scm_symtable_hash_func(ScmBasicHashKey key) /* GC OK */
{
  return scm_string_hash_value(SCM_OBJ(key));
}

static bool
scm_symtable_comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2) /* GC OK */
{
  return scm_string_is_equal(SCM_OBJ(key1), SCM_OBJ(key2));
}

void
scm_symbol_initialize(ScmObj sym, ScmObj table, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&sym, &table, &str);

  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE_ACCEPT_NULL(table, &SCM_SYMTABLE_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_obj_init(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SYMBOL_TABLE(sym) = table;
  SCM_SETQ(SCM_SYMBOL_STR(sym), scm_string_dup(str));
}

ScmObj
scm_symbol_construct(ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_SYMBOL_TYPE_INFO, SCM_REF_MAKE(sym));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_SYMBOL_TYPE_INFO, SCM_REF_MAKE(sym)); */
  scm_symbol_initialize(sym, SCM_OBJ_NULL, str);

  return sym;
}

ScmObj
scm_symbol_instance(ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&str);

  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);
  return scm_symtable_symbol(scm_vm_current_symtbl(), str);
}

bool
scm_symbol_is_symbol(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);
}

void
scm_symbol_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  assert(obj != NULL); assert(scm_symbol_is_symbol(obj));
  assert(obuffer != NULL);

  /* scm_obuffer_concatenate_string(obuffer, SCM_SYMBOL(obj)->name); */

  /* XXX: once-over implementation */
  /* TODO: rewrite */
  scm_obuffer_concatenate_string(obuffer, "#<Symbol>");
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


void
scm_symbol_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ_PRIM(SCM_SYMBOL_TABLE(obj), SCM_OBJ_NULL);
  SCM_SETQ_PRIM(SCM_SYMBOL_STR(obj), SCM_OBJ_NULL);
}

int
scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_TABLE(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_STR(obj), mem);

  return rslt;
}


void
scm_symtable_initialize(ScmObj obj) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&obj);

  assert(SCM_OBJ_IS_NOT_NULL(obj));

  SCM_SYMTABLE_TBL(obj) = scm_basic_hash_construct(SCM_SYMBOL_TABLE_SIZE,
                                                   scm_symtable_hash_func,
                                                   scm_symtable_comp_func);
}

void
scm_symtable_finalize(ScmObj obj) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&obj);

  scm_basic_hash_destruct(SCM_SYMTABLE_TBL(obj));
  SCM_SYMTABLE_TBL(obj) = NULL;
}

bool
scm_symtable_is_symtable(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_SYMTABLE_TYPE_INFO);
}

void
scm_symtable_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  assert(obj != NULL);
  assert(scm_symtable_is_symtable(obj));
  assert(obuffer != NULL);

  scm_obuffer_concatenate_string(obuffer, "#<SymTable>");
}

ScmObj
scm_symtable_symbol(ScmObj symtbl, ScmObj str) /* GC OK */
{
  ScmBasicHashEntry *ent;
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&symtbl, &str, &sym);

  SCM_OBJ_ASSERT_TYPE(symtbl, &SCM_SYMTABLE_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  ent = scm_basic_hash_get(SCM_SYMTABLE_TBL(symtbl), SCM_BASIC_HASH_KEY(str));
  if (ent != NULL) return SCM_OBJ(SCM_BASIC_HASH_ENTRY_VALUE(ent));

  SCM_MEM_ALLOC(&SCM_SYMBOL_TYPE_INFO, SCM_MEM_ALLOC_HEAP, SCM_REF_MAKE(sym));
  scm_symbol_initialize(sym, symtbl, str);

  scm_symtable_add(symtbl, sym);

  return sym;
}

void
scm_symtable_add(ScmObj symtbl, ScmObj sym)
{
  ScmBasicHashEntry *e;

  SCM_STACK_FRAME_PUSH(&symtbl, &sym);

  SCM_OBJ_ASSERT_TYPE(symtbl, &SCM_SYMTABLE_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  e = scm_basic_hash_put(SCM_SYMTABLE_TBL(symtbl),
                         SCM_BASIC_HASH_KEY(SCM_SYMBOL_STR(sym)),
                         SCM_BASIC_HASH_VALUE(sym));
  if (e == NULL)
    ;                           /* TODO: error handling */
}

void
scm_symtable_delete(ScmObj symtbl, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&symtbl, &str);

  SCM_OBJ_ASSERT_TYPE(symtbl, &SCM_SYMTABLE_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_basic_hash_delete(SCM_SYMTABLE_TBL(symtbl), SCM_BASIC_HASH_KEY(str));
}

void
scm_symtable_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMTABLE_TYPE_INFO);

  SCM_SYMTABLE_TBL(obj) = NULL;
}

void
scm_symtable_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_symtable_finalize(obj);
}

int
scm_symtable_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  ScmBasicHashItr itr;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMTABLE_TYPE_INFO);

  if (SCM_SYMTABLE_TBL(obj) == NULL) return rslt;

  for (scm_basic_hash_itr_begin(SCM_SYMTABLE_TBL(obj), &itr);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       scm_basic_hash_itr_next(&itr)) {
    /* TODO: to use implemetation of the hash table specific to symbol table */
    ScmObj child = SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr));
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, child, mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt))
      return rslt;
    if (SCM_BASIC_HASH_ITR_KEY(itr) != SCM_BASIC_HASH_KEY(child)) {
      int r = scm_basic_hash_itr_update_key(&itr, SCM_BASIC_HASH_KEY(child));
      assert(r == 0);
    }
  }

  return rslt;
}

int
scm_symtable_gc_accept_week(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler) /* GC OK */
{
  ScmBasicHashItr itr;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMTABLE_TYPE_INFO);

  if (SCM_SYMTABLE_TBL(obj) == NULL) return rslt;

  for (scm_basic_hash_itr_begin(SCM_SYMTABLE_TBL(obj), &itr);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       scm_basic_hash_itr_next(&itr)) {
    /* TODO: to use implemetation of the hash table specific to the symbol table */
    ScmObj child = SCM_OBJ(SCM_BASIC_HASH_ITR_VALUE(itr));
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, child, mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt))
      return rslt;
    if (SCM_OBJ_IS_NULL(child))
       /* 参照先の Symbol オブジェクトが回収されたのでシンボルテーブルから削除する */
      scm_symtable_delete(obj, SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr)));
  }

  return rslt;
}


