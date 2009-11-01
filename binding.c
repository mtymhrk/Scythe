#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "basichash.h"
#include "obuffer.h"
#include "binding.h"
#include "object.h"
#include "symbol.h"
#include "vm.h"
#include "memory.h"


static unsigned int
hash_func(ScmBasicHashKey key)
{
  return (unsigned int)key;
}

ScmTypeInfo SCM_BIND_REF_TYPE_INFO = {
  scm_bind_ref_pretty_print,    /* pp_func              */
  sizeof(ScmBindRef),           /* obj_size             */
  NULL,                         /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  NULL,                         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

static bool
comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2)? true : false;
}

void
scm_bind_ref_initialize(ScmObj bref, ScmObj sym, ScmObj val) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(bref, &SCM_BIND_REF_TYPE_INFO);

  SCM_SETQ(SCM_BIND_REF_SYM(bref), sym);
  SCM_SETQ(SCM_BIND_REF_VAL(bref), val);
}

void
scm_bind_ref_finalize(ScmBindRef *ref) /* GC OK */
{
  return;                        /* nothing to do */
}

ScmObj
scm_bind_ref_construct(ScmObj sym, ScmObj val) /* GC OK */
{
  ScmObj bref = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bref);

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_BIND_REF_TYPE_INFO, SCM_REF_MAKE(bref));
  /* TODO: replace above to below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_BIND_REF_TYPE_INFO, SCM_REF_MAKE(bref)); */
  if (SCM_OBJ_IS_NULL(bref)) return SCM_OBJ_NULL;

  scm_bind_ref_initialize(bref, sym, val);

  return bref;
}

ScmObj
scm_bind_ref_symbol(ScmObj bref) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(bref, &SCM_BIND_REF_TYPE_INFO);

  return SCM_BIND_REF_SYM(bref);
}

ScmObj
scm_bind_ref_value(ScmObj bref) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(bref, &SCM_BIND_REF_TYPE_INFO);

  return SCM_BIND_REF_VAL(bref);
}

void
scm_bind_ref_bind(ScmObj bref, ScmObj obj) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(bref, &SCM_BIND_REF_TYPE_INFO);

  SCM_SETQ(SCM_BIND_REF_VAL(bref), obj);
}

void
scm_bind_tbl_initialize(ScmBindTable *tbl, size_t size)
{
  assert(tbl != NULL);

  tbl->table = scm_basic_hash_construct(size, hash_func, comp_func);
  tbl->work = SCM_OBJ(NULL); /* TODO: initialize to nil object*/
}

void
scm_bind_tbl_finalize(ScmBindTable *tbl)
{
  assert(tbl != NULL);
  scm_bind_tbl_clear(tbl);
}

ScmBindTable *
scm_bind_tbl_construct(size_t size)
{
  ScmBindTable *tbl;

  tbl = scm_memory_allocate(sizeof(ScmBindTable));
  scm_bind_tbl_initialize(tbl, size);

  return tbl;
}

void
scm_bind_tbl_destruct(ScmBindTable *tbl)
{
  assert(tbl != NULL);

  scm_bind_tbl_finalize(tbl);
  scm_memory_release(tbl);
}

void
scm_bind_tbl_clear(ScmBindTable *tbl)
{
  assert(tbl != NULL);
  scm_basic_hash_clear(tbl->table);
}

ScmObj
scm_bind_tbl_bind(ScmBindTable *tbl, ScmObj sym, ScmObj val)
{
  ScmBasicHashEntry *ent;
  ScmObj ref;

  SCM_STACK_FRAME_PUSH(&sym, &val, &ref);

  assert(tbl != NULL);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_BIND_REF_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(val));

  /* scm_mem_alloc(scm_vm_current_mm(), SCM_OBJ_TYPE_BIND_REF, &tbl->work); */
  /* ref = SCM_BIND_REF(tbl->work); */
  /* scm_bind_ref_initialize(ref, sym, val); */

  SCM_SETQ(ref, scm_bind_ref_construct(sym, val));

  ent = scm_basic_hash_get(tbl->table, SCM_BASIC_HASH_KEY(sym));
  if (ent != NULL) {
    scm_memory_release(SCM_BIND_REF(SCM_BASIC_HASH_ENTRY_VALUE(ent)));
    scm_basic_hash_update_entry(tbl->table, ent, SCM_BASIC_HASH_VALUE(ref));
  }
  else {
    scm_basic_hash_put(tbl->table,
                       SCM_BASIC_HASH_KEY(sym),
                       SCM_BASIC_HASH_VALUE(ref));
  }

  /* tbl->work = SCM_OBJ(NULL); /\* TODO: set nil object *\/ */

  return ref;
}

void
scm_bind_tbl_unbind(ScmBindTable *tbl, ScmObj sym)
{
  assert(tbl != NULL);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  scm_basic_hash_delete(tbl->table, SCM_BASIC_HASH_KEY(sym));
}

ScmObj
scm_bind_tbl_lookup(ScmBindTable *tbl, ScmObj sym)
{
  ScmBasicHashEntry *ent;

  assert(tbl != NULL);
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  ent = scm_basic_hash_get(tbl->table, SCM_BASIC_HASH_KEY(sym));
  if (ent == NULL) return SCM_OBJ_NULL;
  return SCM_OBJ(SCM_BASIC_HASH_ENTRY_VALUE(ent));
}

void
scm_bind_ref_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  /* TODO: write me */
  return;
}
