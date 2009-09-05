#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "basichash.h"
#include "memory.h"
#include "object.h"

enum { TO_HEAP, FROM_HEAP };

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

#define SCM_FORWARD(obj) ((ScmForward *)(obj))
#define SCM_FORWARD_FORWARD(obj) (SCM_FORWARD(obj)->forward)
#define SCM_FORWARD_INITIALIZE(obj, fwd) \
  do { \
    scm_obj_init(obj, SCM_OBJ_TYPE_FORWARD);    \
    SCM_FORWARD(obj)->forward = fwd;            \
  } while(0)

#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_EXTRA_RFRN_SIZE 32

const ScmTypeInfo SCM_FORWARD_TYPE_INFO = {
  SCM_OBJ_TYPE_FORWARD,    /* type            */
  NULL,                    /* pp_func         */
  sizeof(ScmForward),      /* obj_size        */
  NULL,                    /* gc_ini_func     */
  NULL,                    /* gc_fin_func     */
  NULL,                    /* gc_ref_itr_func */
  false                    /* has_weak_ref    */
};

static ScmMemRootBlock *shared_roots = NULL;



void *
scm_memory_allocate(size_t size)
{
  
  return malloc(size);
}

void *
scm_memory_release(void *block)
{
  free(block);
  return NULL;
}




static unsigned int
object_table_hash_func(ScmBasicHashKey key)
{
  return (unsigned int)key;
}

static bool
object_table_comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2) ? true : false;
}

static int
scm_mem_expand_heap(ScmMem *mem, int inc_block)
{
  int i;
  size_t sz;
  ScmMemHeapBlock *to_block, *from_block;

  assert(mem != NULL);
  assert(inc_block >= 0);

  sz = SCM_MEM_HEAP_TAIL_BLOCK_SIZE(mem->to_heap) * 2;
  if (sz == 0) sz = SCM_MEM_HEAP_INIT_BLOCK_SIZE;

  for (i = 0; i < inc_block; i++) {
    SCM_MEM_HEAP_NEW_BLOCK(to_block, sz);
    SCM_MEM_HEAP_NEW_BLOCK(from_block, sz);
    if (to_block == NULL || from_block == NULL)
      goto err;

    SCM_MEM_HEAP_ADD_BLOCK(mem->to_heap, to_block);
    SCM_MEM_HEAP_ADD_BLOCK(mem->from_heap, from_block);

    sz *= 2;
  }

  return i;

 err:
  if (to_block != NULL) free(to_block);
  if (from_block != NULL) free(from_block);
  return i;
}

static int
scm_mem_release_redundancy_heap_blocks(ScmMem *mem, int nr_margin)
{
  int nr_release;
  int nr_leave;

  assert(mem != NULL);
  assert(nr_margin >= 0);

  nr_release = SCM_MEM_HEAP_NR_FREE_BLOCK(mem->to_heap) - nr_margin;
  nr_release = (nr_release < 0) ? 0 : nr_release;
  nr_leave = SCM_MEM_HEAP_NR_BLOCK(mem->to_heap) - nr_release;

  SCM_MEM_HEAP_RELEASE_BLOCKS(mem->to_heap, nr_leave);
  SCM_MEM_HEAP_RELEASE_BLOCKS(mem->from_heap, nr_leave);

  return nr_leave;
}

static int
scm_mem_expand_persistent(ScmMem *mem, int inc_block)
{
  int i;
  ScmMemHeapBlock *block;

  assert(mem != NULL);
  assert(inc_block >= 0);

  for (i = 0; i < inc_block; i++) {
    SCM_MEM_HEAP_NEW_BLOCK(block, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
    if (block == NULL) return i;
    SCM_MEM_HEAP_ADD_BLOCK(mem->persistent, block);
  }

  return i;
}

static int
scm_mem_register_obj_if_needed(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj obj)
{
  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);

  if (SCM_TYPE_INFO_HAS_GC_FIN(type)) {
    ScmBasicHashEntry *e;
    e = scm_basic_hash_put(mem->to_obj_tbl,
                           SCM_BASIC_HASH_KEY(obj),
                           SCM_BASIC_HASH_VALUE(NULL));
    if (e == NULL) {
      return -1;
    }
  }

  return 0;
}

static void
scm_mem_unregister_obj(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);

  if (SCM_TYPE_INFO_HAS_GC_FIN_FROM_OBJ(obj))
    scm_basic_hash_delete(mem->from_obj_tbl, SCM_BASIC_HASH_KEY(obj));
}

static void
scm_mem_finalize_obj(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);
  assert(obj != NULL);

  if (SCM_TYPE_INFO_HAS_GC_FIN_FROM_OBJ(obj)) {
    ScmGCFinalizeFunc fin = SCM_TYPE_INFO_GC_FIN_FROM_OBJ(obj);
    fin(obj);
  }
}

static void
scm_mem_finalize_heap_obj(ScmMem *mem, int which)
{
  ScmBasicHashItr itr;
  ScmBasicHashTable *tbl;

  assert(mem != NULL);
  assert(which == TO_HEAP || which == FROM_HEAP);

  if (which == TO_HEAP)
    tbl = mem->to_obj_tbl;
  else
    tbl = mem->from_obj_tbl;

  for (SCM_BASIC_HASH_ITR_BEGIN(tbl, itr);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       SCM_BASIC_HASH_ITR_NEXT(itr)) {
    scm_mem_finalize_obj(mem, SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr)));
  }

  scm_basic_hash_clear(tbl);
}

static void
scm_mem_clean_heap(ScmMem *mem, int which)
{
  ScmMemHeap *heap;
  ScmMemHeapBlock *block;

  assert(mem != NULL);
  assert(which == TO_HEAP || which == FROM_HEAP);

  scm_mem_finalize_heap_obj(mem, which);

  if (which == TO_HEAP)
    heap = mem->to_heap;
  else
    heap = mem->from_heap;

  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    SCM_MEM_HEAP_BLOCK_CLEAN(block);
  }
  SCM_MEM_HEAP_REWIND(heap);
}

static void
scm_mem_clean_persistent(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_finalize_obj(mem, obj);
    }
    SCM_MEM_HEAP_BLOCK_CLEAN(block);
  }
  SCM_MEM_HEAP_REWIND(mem->persistent);

  assert(mem != NULL);
}

static void
scm_mem_switch_heap(ScmMem *mem)
{
  ScmBasicHashTable *tmp_tbl;
  ScmMemHeap *tmp_heap;

  assert(mem != NULL);

  tmp_tbl = mem->from_obj_tbl;
  mem->from_obj_tbl = mem->to_obj_tbl;
  mem->to_obj_tbl = tmp_tbl;

  tmp_heap = mem->from_heap;
  mem->from_heap = mem->to_heap;
  mem->to_heap = tmp_heap;
}

static bool
scm_mem_is_obj_in_heap(ScmMem *mem, ScmObj obj, int which)
{
  ScmMemHeap *heap;
  ScmMemHeapBlock *block;

  assert(mem != NULL);
  assert(obj != NULL);
  assert(which == TO_HEAP || which == FROM_HEAP);

  if (which == TO_HEAP)
    heap = mem->to_heap;
  else
    heap = mem->from_heap;

  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    if (SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block, obj))
      return true;
  }

  return false;
}

static void
scm_mem_alloc_heap_mem(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmRef ref)
{
  size_t size;

  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(ref != SCM_REF_NULL);

  size = SCM_TYPE_INFO_OBJ_SIZE(type);
  size = (size > SCM_MEM_MIN_OBJ_SIZE) ? size : SCM_MEM_MIN_OBJ_SIZE;

  SCM_MEM_HEAP_ALLOC(mem->to_heap, size, SCM_REF_TO_PTR(ref));
  if (SCM_REF_OBJ(ref) == NULL) return;

  if (scm_mem_register_obj_if_needed(mem, type, SCM_REF_OBJ(ref)) < 0) {
    SCM_MEM_HEAP_CANCEL_ALLOC(mem->to_heap, size);
    SCM_REF_UPDATE(ref,  NULL);
    return;
  }
}

static void
scm_mem_obj_init(ScmMem *mem, ScmObj obj, SCM_OBJ_TYPE_T type)
{
  assert(mem != NULL);
  assert(obj != NULL);
  assert(type < SCM_OBJ_NR_TYPE);

  scm_obj_init(obj, type);
  if (SCM_TYPE_INFO_HAS_GC_INI(type)) {
    ScmGCInitializeFunc ini = SCM_TYPE_INFO_GC_INI(type);
    ini(obj, mem);
  }
}

static ScmObj
scm_mem_copy_obj(ScmMem *mem, ScmObj obj)
{
  ScmObj box;
  SCM_OBJ_TYPE_T type;

  assert(mem != NULL);
  assert(obj != NULL);

  if (!scm_mem_is_obj_in_heap(mem, obj, FROM_HEAP))
    return obj;

  type = scm_obj_type(obj);
  if (type == SCM_OBJ_TYPE_FORWARD) 
    return SCM_FORWARD_FORWARD(obj);

  scm_mem_alloc_heap_mem(mem, type, SCM_REF_MAKE(box));
  if (box == NULL) {
    if (scm_mem_expand_heap(mem, 1) != 1) {
      /* TODO: write error handling (fail to allocate memory)*/
      return NULL;
    }
    scm_mem_alloc_heap_mem(mem, type, SCM_REF_MAKE(box));
    if (box == NULL) {
      /* TODO: write error handling (fail to allocate memory)*/
      return NULL;
    }
  }
  memcpy(box, obj, SCM_TYPE_INFO_OBJ_SIZE(type));
  scm_mem_unregister_obj(mem, obj);
  SCM_FORWARD_INITIALIZE(obj, box);

  return box;
}

static int
scm_mem_copy_children_func(ScmMem *mem, ScmObj obj, ScmRef child)
{
  assert(mem != NULL);
  assert(obj != NULL);
  assert(child != SCM_REF_NULL);

  if (SCM_REF_OBJ(child) != NULL) {
    ScmObj cpy = scm_mem_copy_obj(mem, SCM_REF_OBJ(child));
    if (cpy == NULL)  return -1; // error
    SCM_REF_UPDATE(child, cpy);
  }

  return 0;
}

static void
scm_mem_copy_children(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);
  assert(obj != NULL);

  if (SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC_FROM_OBJ(obj)) {
    ScmGCAcceptFunc func = SCM_TYPE_INFO_GC_ACCEPT_FUNC_FROM_OBJ(obj);
    int rslt = func(obj, mem, scm_mem_copy_children_func);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) {
      ; /* TODO: write error handling */
    }
  }
}

static void
scm_mem_copy_children_of_persistent(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_copy_children(mem, obj);
    }
  }
}

static void
scm_mem_copy_children_of_root_obj(ScmMem *mem, ScmMemRootBlock *head)
{
  ScmMemRootBlock *block;

  for (block = head; block != NULL; block = SCM_MEM_ROOT_BLOCK_NEXT(block)) {
    ScmObj obj = SCM_MEM_ROOT_BLOCK_OBJECT(block);
    scm_mem_copy_children(mem, obj);
  }
}

static void
scm_mem_copy_children_of_shared_root(ScmMem *mem)
{
  scm_mem_copy_children_of_root_obj(mem, shared_roots);
}

static void
scm_mem_copy_children_of_root(ScmMem *mem)
{
  scm_mem_copy_children_of_root_obj(mem, mem->roots);
}

static void
scm_mem_copy_extra_obj(ScmMem *mem)
{
  size_t i;

  assert(mem != NULL);

  for (i = 0; i < mem->nr_extra; i++) {
    if (SCM_REF_OBJ(mem->extra_rfrn[i]) != NULL) {
      ScmObj cpy = scm_mem_copy_obj(mem, SCM_REF_OBJ(mem->extra_rfrn[i]));
      if (cpy == NULL) {
        ; /* TODO: wirte error handling */
      }
      SCM_REF_UPDATE(mem->extra_rfrn[i], cpy);
    }
  }
}

static void
scm_mem_copy_obj_referred_by_root(ScmMem *mem)
{
  assert(mem != NULL);

  scm_mem_copy_children_of_shared_root(mem);
  scm_mem_copy_children_of_root(mem);
  scm_mem_copy_extra_obj(mem);
  scm_mem_copy_children_of_persistent(mem);
}

static void
scm_mem_scan_obj(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;
 
  assert(mem != NULL);
 
  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_copy_children(mem, obj);
    }
  }
}

static ScmObj
scm_mem_alloc_root_obj(SCM_OBJ_TYPE_T type, ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;
  ScmObj obj;

  assert(type < SCM_OBJ_NR_TYPE);
  assert(head != NULL);

  block = malloc(SCM_TYPE_INFO_OBJ_SIZE(type) + sizeof(ScmMemRootBlock));
  if (block == NULL)
    return NULL;

  obj = SCM_MEM_ROOT_BLOCK_OBJECT(block);
  scm_mem_obj_init(mem, obj, type);

  SCM_MEM_ADD_TO_ROOT_SET(*head, block);

  return obj;
}

static ScmObj
scm_mem_free_root_obj(ScmObj obj, ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;

  assert(mem != NULL);
  assert(SCM_MEM_ROOT_BLOCK_IS_OBJ_IN_BLOK(obj));

  block = SCM_MEM_ROOT_BLOCK_HEADER(obj);
  SCM_MEM_DEL_FROM_ROOT_SET(*head, block);

  free(block);

  return NULL;
}

static void
scm_mem_free_all_obj_in_root_set(ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;

  assert(mem != NULL);

  for (block = *head; block != NULL; block = *head) {
    ScmObj obj = SCM_MEM_ROOT_BLOCK_OBJECT(block);
    scm_mem_free_root_obj(obj, mem, head);
  }
}

static ScmMem *
scm_mem_initialize(ScmMem *mem)
{
  assert(mem != NULL);

  mem->to_obj_tbl = NULL;
  mem->from_obj_tbl = NULL;
  mem->to_heap = NULL;
  mem->from_heap = NULL;
  mem->roots = NULL;
  mem->extra_rfrn = NULL;
  
  mem->to_obj_tbl = scm_basic_hash_construct(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                             object_table_hash_func,
                                             object_table_comp_func);
  if (mem->to_obj_tbl == NULL) goto err;

  mem->from_obj_tbl = scm_basic_hash_construct(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                               object_table_hash_func,
                                               object_table_comp_func);
  if (mem->from_obj_tbl == NULL) goto err;

  SCM_MEM_HEAP_NEW_HEAP(mem->to_heap, 1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->to_heap == NULL) goto err;

  SCM_MEM_HEAP_NEW_HEAP(mem->from_heap, 1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->from_heap == NULL) goto err;

  SCM_MEM_HEAP_NEW_HEAP(mem->persistent, 1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->persistent == NULL) goto err;

  mem->extra_rfrn = malloc(sizeof(ScmRef) * SCM_MEM_EXTRA_RFRN_SIZE);
  if (mem->extra_rfrn == NULL) goto err;
  mem->nr_extra = 0;

  return mem;

 err:
  if (mem->to_obj_tbl != NULL) scm_basic_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl != NULL) scm_basic_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->to_heap);
  if (mem->from_heap != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->from_heap);
  if (mem->persistent != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->persistent);
  if (mem->extra_rfrn != NULL) free(mem->extra_rfrn);

  return NULL;
}

static ScmMem *
scm_mem_finalize(ScmMem *mem)
{
  assert(mem != NULL);

  scm_mem_free_all_obj_in_root_set(mem, &mem->roots);
  scm_mem_clean(mem);

  if (mem->to_obj_tbl) scm_basic_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl) scm_basic_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->to_heap);
  if (mem->from_heap != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->from_heap);
  if (mem->persistent != NULL) SCM_MEM_HEAP_DELETE_HEAP(mem->persistent);
  if (mem->extra_rfrn != NULL) free(mem->extra_rfrn);
  
  return NULL;
}

ScmMem *
scm_mem_construct(void)
{
  ScmMem *mem = NULL;

  mem = malloc(sizeof(*mem));
  if (mem == NULL) return NULL;

  return scm_mem_initialize(mem);
}

ScmMem *
scm_mem_destruct(ScmMem *mem)
{
  if (mem == NULL) return NULL;

  scm_mem_finalize(mem);
  free(mem);

  return NULL;
}

ScmMem *
scm_mem_clean(ScmMem *mem)
{
  scm_mem_clean_persistent(mem);
  scm_mem_clean_heap(mem, TO_HEAP);
  scm_mem_clean_heap(mem, FROM_HEAP);
  return mem;
}


ScmMem *
scm_mem_alloc_heap(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmRef ref)
{
  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, NULL);

  scm_mem_alloc_heap_mem(mem, type, ref);
  if (SCM_REF_OBJ(ref) == NULL) {
    scm_mem_gc_start(mem);
    scm_mem_alloc_heap_mem(mem, type, ref);
    if (SCM_REF_OBJ(ref) == NULL) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
  }

  scm_mem_obj_init(mem, SCM_REF_OBJ(ref), type);

  return mem;
}

ScmMem *
scm_mem_alloc_persist(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmRef ref)
{
  size_t size;

  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(ref != SCM_REF_NULL);

  size = SCM_TYPE_INFO_OBJ_SIZE(type);
  SCM_MEM_HEAP_ALLOC(mem->persistent, size, SCM_REF_TO_PTR(ref));
  if (SCM_REF_OBJ(ref) == NULL) {
    if (scm_mem_expand_persistent(mem, 1) != 1) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
    SCM_MEM_HEAP_ALLOC(mem->persistent, size, SCM_REF_TO_PTR(ref));
    if (SCM_REF_OBJ(ref) == NULL) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
  }

  scm_mem_obj_init(mem, SCM_REF_OBJ(ref), type);

  return mem;
}

ScmMem *
scm_mem_alloc_root(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmRef ref)
{
  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, scm_mem_alloc_root_obj(type, mem, &mem->roots));
  if (SCM_REF_OBJ(ref) == NULL) return NULL;

  return mem;
}

ScmObj
scm_mem_free_root(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);
  assert(SCM_MEM_ROOT_BLOCK_IS_OBJ_IN_BLOK(obj));

  return scm_mem_free_root_obj(obj, mem, &mem->roots);
}

ScmMem *
scm_mem_alloc_plain(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmRef ref)
{
  ScmObj obj;

  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, NULL);

  obj = malloc(SCM_TYPE_INFO_OBJ_SIZE(type));
  if (obj == NULL) return NULL;

  SCM_REF_UPDATE(ref, obj);

  scm_mem_obj_init(mem, obj, type);

  return mem;
}

ScmObj 
scm_mem_free_plain(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);
  assert(obj != NULL);

  free(obj);

  return NULL;
}

ScmRef
scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref)
{
  assert(mem != NULL);
  assert(ref != SCM_REF_NULL);

  if (mem->nr_extra >= SCM_MEM_EXTRA_RFRN_SIZE)
    return SCM_REF_NULL;

  mem->extra_rfrn[mem->nr_extra++] = ref;

  return ref;
}

ScmMem *
scm_mem_alloc(ScmMem *mem, SCM_OBJ_TYPE_T type,
              SCM_MEM_ALLOC_TYPE_T alloc, ScmRef ref)
{
  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);
  assert(alloc <  SCM_MEM_NR_ALLOC_TYPE);
  assert(ref != SCM_REF_NULL);

  switch(alloc) {
  case SCM_MEM_ALLOC_PLAIN:
    return scm_mem_alloc_plain(mem, type, ref);
    break;
  case SCM_MEM_ALLOC_HEAP:
    return scm_mem_alloc_heap(mem, type, ref);
    break;
  case SCM_MEM_ALLOC_ROOT:
    return scm_mem_alloc_root(mem, type, ref);
    break;
  case SCM_MEM_ALLOC_SHARED_ROOT:
    SCM_REF_UPDATE(ref, scm_memory_alloc_shared_root(type));
    return (SCM_REF_OBJ(ref) == NULL) ? NULL : mem;
    break;
  case SCM_MEM_NR_ALLOC_TYPE:
    return NULL;
    break;
  }
  
  return NULL;
}

void
scm_mem_gc_start(ScmMem *mem)
{
  size_t nr_free;

  assert(mem != NULL);

  scm_mem_switch_heap(mem);
  scm_mem_copy_obj_referred_by_root(mem);
  scm_mem_scan_obj(mem);
  scm_mem_clean_heap(mem, FROM_HEAP);

  nr_free = SCM_MEM_HEAP_NR_FREE_BLOCK(mem->to_heap);
  if (nr_free == 0)
    scm_mem_expand_heap(mem, 1);
  else if (nr_free > 1) 
    scm_mem_release_redundancy_heap_blocks(mem, 1);
}


ScmObj
scm_memory_alloc_shared_root(SCM_OBJ_TYPE_T type)
{
  return scm_mem_alloc_root_obj(type, NULL, &shared_roots);
}

ScmObj
scm_memory_free_shared_root(ScmObj obj)
{
  return scm_mem_free_root_obj(obj, NULL, &shared_roots);
}

void
scm_memory_free_all_shared_root(void)
{
  scm_mem_free_all_obj_in_root_set(NULL, &shared_roots);
  return;
}
