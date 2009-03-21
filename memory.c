#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "basichash.h"
#include "memory.h"
#include "object.h"

#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  size_t size;
  size_t used;
  uint8_t heap[0];
};

#define SCM_MEM_HEAP_NEW_BLOCK(block, sz)              \
  do {                                                 \
    (block) = malloc(sizeof(ScmMemHeapBlock) + (sz));  \
    if ((block) != NULL) {                             \
      (block)->next = NULL;                            \
      (block)->size = (sz);                            \
      (block)->used = 0;                               \
    }                                                  \
  } while(0)

#define SCM_MEM_HEAP_DELEATE_BLOCK(block)            \
  do {                                               \
    free(block);                                     \
    (block) = NULL;                                  \
  } while(0)

#define SCM_MEM_HEAP_BLOCK_NEXT(block) ((block)->next)
#define SCM_MEM_HEAP_BLOCK_SIZE(block) ((block)->size)
#define SCM_MEM_HEAP_BLOCK_USED(block) ((block)->used)
#define SCM_MEM_HEAP_BLOCK_FREE(block) ((block)->size - (block)->used)
#define SCM_MEM_HEAP_BLOCK_HEAD(block) ((block)->heap)
#define SCM_MEM_HEAP_BLOCK_ALLOCATED(block, sz) ((block)->used += (sz))
#define SCM_MEM_HEAP_BLOCK_FREE_PTR(block) \
  ((void *)((block)->heap + (block)->used))
#define SCM_MEM_HEAP_BLOCK_PTR_OFFSET(block, ptr) \
  ((uint8_t *)(ptr) - block->heap)
#define SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr) \
  (SCM_MEM_HEAP_BLOCK_PTR_OFFSET(block, ptr) < SCM_MEM_HEAP_BLOCK_USED(block))
#define SCM_MEM_HEAP_BLOCK_NEXT_OBJ(block, obj) \
  SCM_OBJ((uint8_t *)obj + SCM_TYPE_INFO_OBJ_SIZE_FROM_OBJ(obj))
#define SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj)                     \
  for ((obj) = SCM_OBJ(SCM_MEM_HEAP_BLOCK_HEAD(block));                 \
       SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, obj);                 \
       obj = SCM_MEM_HEAP_BLOCK_NEXT_OBJ(block, obj))
                                                      
struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  void *free;
  int nr_block;
  int nr_free_block;
};

#define SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap) \
  (((heap)->current == NULL) ? 0 : SCM_MEM_HEAP_BLOCK_FREE((heap)->current))

#define SCM_MEM_HEAP_ADD_BLOCK(heap, block)            \
  do {                                                 \
    if ((heap)->head == NULL)                          \
      (heap)->head = (block);                          \
    else                                               \
      (heap)->tail->next = (block);                    \
    (heap)->tail = (block);                            \
    (block)->next = NULL;                              \
    (heap)->nr_block++;                                \
    (heap)->nr_free_block++;                           \
  } while(0)

#define SCM_MEM_HEAP_RELEASE_BLOCKS(heap, block, prev)   \
  do {                                                   \
    ScmMemHeapBlock *p, *q;                              \
    for (p = (block); p != NULL; p = q) {                \
      q = p->next;                                       \
      SCM_MEM_HEAP_DELEATE_BLOCK(p);                     \
      (heap)->nr_block--;                                \
      (heap)->nr_free_block--;                           \
    }                                                    \
    if ((heap)->head == (block))                         \
      (heap)->head = (heap)->tail = NULL;                \
    else                                                 \
      (heap)->tail = (prev);                             \
  } while(0)

#define SCM_MEM_HEAP_NEXT(heap)                                         \
  do {                                                                  \
    if ((heap)->current != NULL) {                                      \
      (heap)->current = SCM_MEM_HEAP_BLOCK_NEXT((heap)->current);       \
      if ((heap)->current == NULL) {                                    \
        (heap)->free = NULL;                                            \
      }                                                                 \
      else {                                                            \
        (heap)->free = SCM_MEM_HEAP_BLOCK_FREE_PTR((heap)->current);    \
        (heap)->nr_free_block--;                                        \
      }                                                                 \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_ALLOC(heap, size, ptr)                             \
  do {                                                                  \
    *(ptr) = NULL;                                                      \
    while ((heap)->current != NULL && *(ptr) == NULL) {                 \
      if ((size) <= SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap)) {           \
        *(ptr) = (heap)->free;                                          \
        SCM_MEM_HEAP_BLOCK_ALLOCATED((heap)->current, (size));          \
        (heap)->free = SCM_MEM_HEAP_BLOCK_FREE_PTR((heap)->current);    \
      }                                                                 \
      else {                                                            \
        SCM_MEM_HEAP_NEXT(heap);                                        \
      }                                                                 \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = heap->head;                     \
       (block) != NULL;                          \
       (block) = SCM_MEM_HEAP_BLOCK_NEXT(block))


struct ScmMemRec {
  ScmBasicHashTable *to_obj_tbl;
  ScmBasicHashTable *from_obj_tbl;
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
  ScmMemHeap *persistent;
  ScmObj **extra_root_set;
  int nr_extra_root;
};

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)

const ScmTypeInfo SCM_FORWARD_TYPE_INFO = {
  SCM_OBJ_TYPE_FORWARD,    /* type            */
  NULL,                    /* pp_func         */
  sizeof(ScmForward),      /* obj_size        */
  NULL,                    /* gc_fin_func     */
  NULL                     /* gc_ref_itr_func */
};



/* struct ScmMemoryRec { */
/*   int dummy; */
/* }; */

/* typedef struct ScmMemoryRec ScmMemory; */

/* static ScmMemory *memory_instance = NULL; */

/* static ScmMemory * */
/* scm_memory_instance(void) */
/* { */
/*   if (memory_instance == NULL) { */
/*     memory_instance = malloc(sizeof(ScmMemory)); */
/*   } */

/*   return memory_instance; */
/* } */

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

static void
scm_mem_delete_heap(ScmMemHeap *heap)
{
  if (heap == NULL) return;

  SCM_MEM_HEAP_RELEASE_BLOCKS(heap, heap->head, NULL);
  free(heap);
}

static ScmMemHeap *
scm_mem_new_heap(int nr_block, size_t size)
{
  ScmMemHeap *heap;
  int i;

  heap = malloc(sizeof(*heap));
  if (heap == NULL) return NULL;
  heap->head = NULL;
  heap->tail = NULL;
  heap->current = NULL;
  heap->free = NULL;
  heap->nr_block = nr_block;
  heap->nr_free_block = nr_block - 1;

  for (i = 0; i < nr_block; i++) {
    ScmMemHeapBlock *block;
    SCM_MEM_HEAP_NEW_BLOCK(block, size);
    if (block == NULL) {
      scm_mem_delete_heap(heap);
      return NULL;
    }
    SCM_MEM_HEAP_ADD_BLOCK(heap, block);
  }

  heap->current = heap->head;

  return heap;
}

static int
scm_mem_expand_heap(ScmMem *mem, size_t inc_block)
{
  int i;
  ScmMemHeapBlock *to_block, *from_block;

  assert(mem != NULL);

  for (i = 0; i < inc_block; i++) {
    SCM_MEM_HEAP_NEW_BLOCK(to_block, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
    SCM_MEM_HEAP_NEW_BLOCK(from_block, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
    if (to_block == NULL || from_block == NULL)
      goto err;

    SCM_MEM_HEAP_ADD_BLOCK(mem->to_heap, to_block);
    SCM_MEM_HEAP_ADD_BLOCK(mem->from_heap, from_block);
  }

  return i;

 err:
  if (to_block != NULL) free(to_block);
  if (from_block != NULL) free(from_block);
  return i;
}

static int
scm_mem_expand_persistent(ScmMem *mem, size_t inc_block)
{
  int i;
  ScmMemHeapBlock *block;

  assert(mem != NULL);

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
scm_mem_fianlize_from_heap_obj(ScmMem *mem)
{
  ScmBasicHashItr itr;

  for (itr = SCM_BASIC_HASH_ITR_BEGIN(mem->from_obj_tbl);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       itr = SCM_BASIC_HASH_ITR_NEXT(itr)) {
    scm_mem_finalize_obj(mem, SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr)));
  }

  scm_basic_hash_clear(mem->from_obj_tbl);
}

static void
scm_mem_finalize_persistent_obj(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_finalize_obj(mem, obj);
    }
  }

  assert(mem != NULL);
}

static ScmMem *
scm_mem_initialize(ScmMem *mem)
{
  assert(mem != NULL);

  mem->to_obj_tbl = NULL;
  mem->from_obj_tbl = NULL;
  mem->to_heap = NULL;
  mem->from_heap = NULL;

  mem->to_obj_tbl = scm_basic_hash_construct(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                             object_table_hash_func,
                                             object_table_comp_func);
  if (mem->to_obj_tbl == NULL) goto err;

  mem->from_obj_tbl = scm_basic_hash_construct(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                               object_table_hash_func,
                                               object_table_comp_func);
  if (mem->from_obj_tbl == NULL) goto err;

  mem->to_heap = scm_mem_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->to_heap == NULL) goto err;

  mem->from_heap = scm_mem_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->from_heap == NULL) goto err;

  mem->persistent = scm_mem_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->persistent == NULL) goto err;

  mem->extra_root_set = malloc(sizeof(ScmObj *) * SCM_MEM_EXTRA_ROOT_SET_SIZE);
  if (mem->extra_root_set == NULL) goto err;
  mem->nr_extra_root = 0;

  return mem;

 err:
  if (mem->to_obj_tbl != NULL) scm_basci_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl != NULL) scm_basci_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) scm_mem_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) scm_mem_delete_heap(mem->from_heap);
  if (mem->persistent != NULL) scm_mem_delete_heap(mem->persistent);
  if (mem->extra_root_set != NULL) free(mem->extra_root_set);

  return NULL;
}

static ScmMem *
scm_mem_finalize(ScmMem *mem)
{
  assert(mem != NULL);

  scm_mem_finalize_persistent_obj(mem);

  if (mem->to_obj_tbl) scm_basci_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl) scm_basci_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) scm_mem_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) scm_mem_delete_heap(mem->from_heap);  
  if (mem->persistent != NULL) scm_mem_delete_heap(mem->persistent);
  if (mem->extra_root_set != NULL) free(mem->extra_root_set);

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
scm_mem_alloc(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box)
{
  size_t size;

  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);

  *box = NULL;

  size = SCM_TYPE_INFO_OBJ_SIZE(type);
  size = (size > SCM_MEM_MIN_OBJ_SIZE) ? size : SCM_MEM_MIN_OBJ_SIZE;

  SCM_MEM_HEAP_ALLOC(mem->to_heap, size, box);
  if (*box == NULL) {
    scm_mem_gc_start(mem);
    SCM_MEM_HEAP_ALLOC(mem->to_heap, size, box);
    if (*box == NULL) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
  }

  scm_obj_init(*box, type);
  if (scm_mem_register_obj_if_needed(mem, type, *box) < 0) {
    ; /* TODO: write error handling */
    return NULL;
  }

  return mem;
}

ScmMem *
scm_mem_alloc_persist(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box)
{
  size_t size;

  assert(mem != NULL);
  assert(type < SCM_OBJ_NR_TYPE);

  size = SCM_TYPE_INFO_OBJ_SIZE(type);
  SCM_MEM_HEAP_ALLOC(mem->persistent, size, box);
  if (*box == NULL) {
    if (scm_mem_expand_persistent(mem, 1) != 1) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
    SCM_MEM_HEAP_ALLOC(mem->persistent, size, box);
    if (*box == NULL) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
  }

  scm_obj_init(*box, type);

  return mem;
}

void
scm_mem_gc_start(ScmMem *mem)
{
  ; // TODO: write me
}
