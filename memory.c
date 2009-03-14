#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "basichash.h"
#include "memory.h"
#include "object.h"

#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024


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
  heap->rest_in_cur = 0;

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
  heap->rest_in_cur = SCM_MEM_HEAP_BLOCK_SIZE(heap->head);

  return heap;
}

static int
scm_mem_expand_heap(ScmMem *mem, size_t inc_block)
{
  int i;
  ScmMemHeapBlock *to_block, *from_block;

  if (mem == NULL) return -1;

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

ScmMem *
scm_mem_construct(void)
{
  ScmMem *mem = NULL;

  mem = malloc(sizeof(*mem));
  if (mem == NULL) return NULL;

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

  return mem;

 err:
  if (mem->to_obj_tbl) scm_basci_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl) scm_basci_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) scm_mem_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) scm_mem_delete_heap(mem->from_heap);

  return NULL;
}

ScmMem *
scm_mem_destruct(ScmMem *mem)
{
  if (mem == NULL) return NULL;

  if (mem->to_obj_tbl) scm_basci_hash_destruct(mem->to_obj_tbl);
  if (mem->from_obj_tbl) scm_basci_hash_destruct(mem->from_obj_tbl);
  if (mem->to_heap != NULL) scm_mem_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) scm_mem_delete_heap(mem->from_heap);  

  return NULL;
}

void *
scm_mem_alloc(ScmObj *box,
              SCM_OBJ_TYPE_T type, SCM_MEM_FINALIZER finalizer)
{
  /* TODO: write me */
  return NULL;
}


