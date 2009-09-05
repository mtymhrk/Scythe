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

struct ScmBindRefRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
};

struct ScmBindTableRec {
  ScmBasicHashTable *table;
  ScmObj work; /* root of GC */
};

static unsigned int
hash_func(ScmBasicHashKey key)
{
  return (unsigned int)key;
}

const ScmTypeInfo SCM_BIND_REF_TYPE_INFO = {
  SCM_OBJ_TYPE_BIND_REF,        /* type            */
  scm_bind_ref_pretty_print,    /* pp_func         */
  sizeof(ScmBindRef),           /* obj_size        */
  NULL,                         /* gc_ini_func     */
  NULL,                         /* gc_fin_func     */
  NULL,                         /* gc_accept_func  */
  false                         /* has_weak_ref    */
};

static bool
comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2)? true : false;
}

void
scm_bind_ref_initialize(ScmBindRef *ref, ScmObj sym, ScmObj val)
{
  assert(ref != NULL);

  scm_obj_init(SCM_OBJ(ref), SCM_OBJ_TYPE_BIND_REF);
  ref->sym = sym;
  ref->val = val;
}

void
scm_bind_ref_finalize(ScmBindRef *ref)
{
  assert(ref != NULL);
}

ScmBindRef *
scm_bind_ref_construct(ScmObj sym, ScmObj val)
{
  ScmBindRef *ref;

  ref = scm_memory_allocate(sizeof(ScmBindRef));
  scm_bind_ref_initialize(ref, sym, val);
  return ref;
}

void
scm_bind_ref_destruct(ScmBindRef *ref)
{
  scm_bind_ref_finalize(ref);
  scm_memory_release(ref);
}

ScmObj
scm_bind_ref_symbol(ScmBindRef *ref)
{
  assert(ref != NULL);

  return ref->sym;
}

ScmObj
scm_bind_ref_value(ScmBindRef *ref)
{
  assert(ref != NULL);

  return ref->val;
}

ScmObj
scm_bind_ref_bind(ScmBindRef *ref, ScmObj obj)
{
  assert(ref != NULL);

  ref->val = obj;
  return obj;
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
  ScmBasicHashItr itr;

  assert(tbl != NULL);

  for (SCM_BASIC_HASH_ITR_BEGIN(tbl->table, itr);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       SCM_BASIC_HASH_ITR_NEXT(itr))
    scm_bind_ref_destruct(SCM_BIND_REF(SCM_BASIC_HASH_ITR_VALUE(itr)));

  scm_basic_hash_clear(tbl->table);
}

ScmBindRef *
scm_bind_tbl_bind(ScmBindTable *tbl, ScmObj sym, ScmObj val)
{
  ScmBasicHashEntry *ent;
  ScmBindRef *ref;

  assert(tbl != NULL);

  /* scm_mem_alloc(scm_vm_current_mm(), SCM_OBJ_TYPE_BIND_REF, &tbl->work); */
  /* ref = SCM_BIND_REF(tbl->work); */
  /* scm_bind_ref_initialize(ref, sym, val); */

  ref = scm_bind_ref_construct(sym, val);

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
  ScmBasicHashEntry *ent;

  assert(tbl != NULL);

  ent = scm_basic_hash_get(tbl->table, SCM_BASIC_HASH_KEY(sym));
  if (ent != NULL) {
    ScmBindRef *ref = SCM_BIND_REF(SCM_BASIC_HASH_ENTRY_VALUE(ent));
    scm_memory_release(ref);
  }
  scm_basic_hash_delete(tbl->table, SCM_BASIC_HASH_KEY(sym));
}

ScmBindRef *
scm_bind_tbl_lookup(ScmBindTable *tbl, ScmObj sym)
{
  ScmBasicHashEntry *ent;

  assert(tbl != NULL);
  
  ent = scm_basic_hash_get(tbl->table, SCM_BASIC_HASH_KEY(sym));
  if (ent == NULL) return NULL;
  return SCM_BIND_REF(SCM_BASIC_HASH_ENTRY_VALUE(ent));
}

void
scm_bind_ref_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  /* TODO: write me */
  return;
}
