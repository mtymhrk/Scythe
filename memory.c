#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "basichash.h"
#include "memory.h"
#include "object.h"

#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

enum { TO_HEAP, FROM_HEAP };

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
#define SCM_MEM_HEAP_BLOCK_CLEAN(block) ((block)->used = 0)
#define SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block, obj) \
  ((block)->heap <= (uint8_t *)obj \
   && (uint8_t *)obj < (block)->heap + (block)->used)

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
#define SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(heap)  ((heap)->current == (heap)->tail)

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

#define SCM_FORWARD(obj) ((ScmForward *)(obj))
#define SCM_FORWARD_FORWARD(obj) (SCM_FORWARD(obj)->forward)
#define SCM_FORWARD_INITIALIZE(obj, fwd) \
  do { \
    scm_obj_init(obj, SCM_OBJ_TYPE_FORWARD);    \
    SCM_FORWARD(obj)->forward = fwd;            \
  } while(0)


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

  for (itr = SCM_BASIC_HASH_ITR_BEGIN(tbl);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       itr = SCM_BASIC_HASH_ITR_NEXT(itr)) {
    scm_mem_finalize_obj(mem, SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr)));
  }

  scm_basic_hash_clear(tbl);
}

static void
scm_mem_cleanup_heap(ScmMem *mem, int which)
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
}

static void
scm_mem_cleanup_persistent(ScmMem *mem)
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
  
  scm_mem_alloc(mem, type, &box);
  if (box == NULL) {
    /* TODO: write error handling */
    return NULL;
  }
  memcpy(box, obj, SCM_TYPE_INFO_OBJ_SIZE(type));
  SCM_FORWARD_INITIALIZE(obj, box);

  return box;
}

static void
scm_mem_copy_children(ScmMem *mem, ScmObj obj)
{
  assert(mem != NULL);
  assert(obj != NULL);

  if (SCM_TYPE_INFO_HAS_REF_ITR_FROM_OBJ(obj)) {
    ScmGCRefItrFunc itr_begin = SCM_TYPE_INFO_GC_REF_ITR_FROM_OBJ(obj);
    ScmGCRefItr itr;

    for (itr = itr_begin(obj);
         SCM_GC_REF_ITR_IS_END(itr);
         itr = SCM_GC_REF_ITR_NEXT(itr)) {
      ScmObj c = scm_mem_copy_obj(mem, obj);
      if (c == NULL) {
        ; /* TODO: write error handling */
      }
      SCM_GC_REF_ITR_SET(itr, c);
    }
  }
}

static void
scm_mem_copy_extra_root_obj(ScmMem *mem)
{
  int i;

  assert(mem != NULL);

  for (i = 0; i < mem->nr_extra_root; i++) {
    if (mem->extra_root_set[i] != NULL)
      *mem->extra_root_set[i] = scm_mem_copy_obj(mem, *mem->extra_root_set[i]);
  }
}

static void
scm_mem_copy_children_of_persistent(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_copy_children(mem, obj);
    }
  }
}

static void
scm_mem_copy_root_obj(ScmMem *mem)
{
  assert(mem != NULL);

  /* TODO: copy objects in  VM's stack frame, register, and so on. */

  scm_mem_copy_extra_root_obj(mem);
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

  scm_mem_cleanup_persistent(mem);
  scm_mem_cleanup_heap(mem, TO_HEAP);

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
scm_mem_register_root(ScmMem *mem, ScmObj *box)
{
  assert(mem != NULL);
  
  if (mem->nr_extra_root < SCM_MEM_EXTRA_ROOT_SET_SIZE) {
    mem->extra_root_set[mem->nr_extra_root++] = box;
    return mem;
  }
  else
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
  assert(mem != NULL);

  scm_mem_switch_heap(mem);
  scm_mem_copy_root_obj(mem);
  scm_mem_scan_obj(mem);
  scm_mem_cleanup_heap(mem, FROM_HEAP);

  if (SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(mem->to_heap))
    scm_mem_expand_heap(mem, 1);
}
