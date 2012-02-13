#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>


typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRootBlockRec ScmMemRootBlock;
typedef struct ScmMemRec ScmMem;
typedef struct ScmForwardRec ScmForward;

#define SCM_MEM(obj) ((ScmMem *)(obj))
#define SCM_FORWARD(obj) ((ScmForward *)(obj))

typedef enum {
  SCM_MEM_ALLOC_HEAP,
  SCM_MEM_ALLOC_ROOT,
  SCM_MEM_ALLOC_SHARED_ROOT,
} SCM_MEM_ALLOC_TYPE_T;

enum { SCM_MEM_NR_ALLOC_TYPE = SCM_MEM_ALLOC_SHARED_ROOT + 1 };

#include "object.h"
#include "basichash.h"
#include "impl_utils.h"

/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

extern ScmTypeInfo SCM_FORWARD_TYPE_INFO;

inline ScmObj
scm_forward_forward(ScmObj obj)
{
  return SCM_FORWARD(obj)->forward;
}

inline void
scm_forward_initialize(ScmObj obj, ScmObj fwd)
{
  scm_obj_init(obj, &SCM_FORWARD_TYPE_INFO);
  SCM_FORWARD(obj)->forward = fwd;
}


/****************************************************************************/
/* Memory Manager                                                           */
/****************************************************************************/

#define SCM_MEM_ALIGN_BYTE 8

inline size_t
scm_mem_align_size(size_t size)
{
  return ((size + SCM_MEM_ALIGN_BYTE - 1)
          / SCM_MEM_ALIGN_BYTE
          * SCM_MEM_ALIGN_BYTE);
}

inline void *
scm_mem_align_ptr(void *ptr)
{
  return (void *)scm_mem_align_size((uintptr_t)ptr);
}

size_t scm_mem_alloc_size_in_heap(ScmTypeInfo *type);
size_t scm_mem_alloc_size_in_heap_aligned(ScmTypeInfo *type);
size_t scm_mem_alloc_size_in_root(ScmTypeInfo *type);




struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  struct ScmMemHeapBlockRec *prev;
  size_t size;
  size_t used;
  uint8_t heap[0];
};

inline ScmMemHeapBlock *
scm_mem_heap_new_block(size_t sz)
{
  ScmMemHeapBlock *block = malloc(sizeof(ScmMemHeapBlock) + (sz));
  if (block != NULL) {
    uint8_t *p;
    block->next = NULL;
    block->prev = NULL;
    block->size = sz;
    p = scm_mem_align_ptr(block->heap);
    block->used = (size_t)p - (size_t)block->heap;
  }

  return block;
}

inline void *
scm_mem_heap_delete_block(ScmMemHeapBlock *block)
{
  free(block);
  return NULL;
}

inline size_t
scm_mem_heap_block_used(ScmMemHeapBlock *block)
{
  return block->used;
}

inline size_t
scm_mem_heap_block_free(ScmMemHeapBlock *block)
{
  return block->size - block->used;
}

inline uint8_t *
scm_mem_heap_block_head(ScmMemHeapBlock *block)
{
  return (uint8_t *)block->heap;
}

inline void
scm_mem_heap_block_allocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used += scm_mem_align_size(sz);
}

inline void
scm_mem_heap_block_deallocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used -= scm_mem_align_size(sz);
}

inline bool
scm_mem_heap_block_allocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_free(block)) ?
          true : false);
}

inline bool
scm_mem_heap_block_deallocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_used(block)) ?
          true : false);
}

inline void *
scm_mem_heap_block_free_ptr(ScmMemHeapBlock *block)
{
  return (void *)(block->heap + block->used);
}

inline size_t
scm_mem_heap_block_ptr_offset(ScmMemHeapBlock *block, void *ptr)
{
  return (size_t)((uint8_t *)ptr - block->heap);
}

inline bool
scm_mem_heap_block_ptr_allocated_p(ScmMemHeapBlock *block, void *ptr)
{
  return ((scm_mem_heap_block_ptr_offset(block, ptr)
           < scm_mem_heap_block_used(block)) ?
          true : false);
}

inline ScmObj
scm_mem_heap_block_next_obj(ScmMemHeapBlock *block, ScmObj obj)
{
  return SCM_OBJ((uint8_t *)obj
                 + scm_mem_alloc_size_in_heap_aligned(scm_obj_type(obj)));
}

inline void
scm_mem_heap_block_clean(ScmMemHeapBlock *block)
{
  uint8_t *p = scm_mem_heap_block_head(block);
  block->used = (size_t)p - (size_t)(block)->heap;
}

inline bool
scm_mem_heap_block_has_obj_p(ScmMemHeapBlock *block, ScmObj obj)
{
  return ((block->heap <= (uint8_t *)obj
           && (uint8_t *)obj < block->heap + block->used) ?
          true : false);
}

#define SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj)                  \
  for ((obj) = SCM_OBJ(scm_mem_heap_block_head(block));              \
       scm_mem_heap_block_ptr_allocated_p(block, (void *)(obj));     \
       (obj) = scm_mem_heap_block_next_obj(block, obj))


struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  void *weak_list;
  int nr_block;
  int nr_free_block;
};

inline size_t
scm_mem_heap_cur_block_free_size(ScmMemHeap *heap)
{
  return ((heap->current == NULL) ?
          0 : scm_mem_heap_block_free(heap->current));
}

inline bool
scm_mem_heap_cur_block_tail_p(ScmMemHeap *heap)
{
  return ((heap->current == heap->tail) ? true : false);
}

inline int
scm_mem_heap_nr_block(ScmMemHeap *heap)
{
  return heap->nr_block;
}

inline int
scm_mem_heap_nr_free_block(ScmMemHeap *heap)
{
  return heap->nr_free_block;
}

inline int
scm_mem_heap_nr_used_block(ScmMemHeap *heap)
{
  return heap->nr_block - heap->nr_free_block;
}

inline size_t
scm_mem_heap_tail_block_size(ScmMemHeap *heap)
{
  return ((heap->tail == NULL) ? 0U : heap->tail->size);
}

inline void
scm_mem_heap_add_block(ScmMemHeap *heap, ScmMemHeapBlock *block)
{
  if (heap->head == NULL)
    heap->head = block;
  else
    heap->tail->next = block;
  block->next = NULL;
  block->prev = heap->tail;
  heap->tail = block;
  heap->nr_block++;
  if (heap->current == NULL)
    heap->current = block;
  else
    heap->nr_free_block++;
}

inline void
scm_mem_heap_del_block(ScmMemHeap *heap)
{
  if (heap->tail != NULL) {
    ScmMemHeapBlock *b = heap->tail;

    if (scm_mem_heap_cur_block_tail_p(heap))
      heap->current = b->prev;
    else
      heap->nr_free_block--;

    if (b->prev == NULL) {
      heap->head = NULL;
      heap->tail = NULL;
    }
    else {
      heap->tail = b->prev;
      heap->tail->next = NULL;
    }
    heap->nr_block--;

    scm_mem_heap_delete_block(b);
  }
}

inline void
scm_mem_heap_release_blocks(ScmMemHeap *heap, int nr_leave)
{
  int n = scm_mem_heap_nr_block(heap) - nr_leave;
  for (int i = 0; i < n; i++)
    scm_mem_heap_del_block(heap);
}

inline ScmMemHeap *
scm_mem_heap_delete_heap(ScmMemHeap *heap)
{
  scm_mem_heap_release_blocks(heap, 0);
  free(heap);
  return heap;
}

inline ScmMemHeap *
scm_mem_heap_new_heap(int nr_blk, size_t sz)
{
  ScmMemHeap *heap;
  int i;

  heap = malloc(sizeof(*heap));
  if (heap == NULL) return NULL;

  heap->head = NULL;
  heap->tail = NULL;
  heap->current = NULL;
  heap->weak_list = NULL;
  heap->nr_block = 0;
  heap->nr_free_block = 0;

  for (i = 0; i < nr_blk; i++) {
    ScmMemHeapBlock *block;
    block = scm_mem_heap_new_block(sz);
    if (block == NULL) {
      scm_mem_heap_delete_heap(heap);
      return NULL;
    }
    scm_mem_heap_add_block(heap, block);
  }

  heap->current = heap->head;

  return heap;
}

inline void
scm_mem_heap_shift(ScmMemHeap *heap)
{
  if (heap->current != NULL) {
    heap->current = heap->current->next;
    if (heap->current != NULL)
      heap->nr_free_block--;
  }
}

inline void
scm_mem_heap_unshift(ScmMemHeap *heap)
{
  if (heap->current != heap->head) {
    if (heap->current == NULL) {
      heap->current = heap->tail;
    }
    else {
      heap->current = heap->current->prev;
      heap->nr_free_block++;
    }
  }
}

inline void
scm_mem_heap_rewind(ScmMemHeap *heap)
{
  heap->current = heap->head;
  if ((heap)->nr_block > 1)
    (heap)->nr_free_block = heap->nr_block - 1;
  else
    (heap)->nr_free_block = 0;
}

inline void *
scm_mem_heap_alloc(ScmMemHeap *heap, size_t size)
{
  void *ptr = NULL;

  while (heap->current != NULL && ptr == NULL) {
    if (scm_mem_heap_block_allocable_p(heap->current, size)) {
      ptr = scm_mem_heap_block_free_ptr(heap->current);
      scm_mem_heap_block_allocated(heap->current, size);
    }
    else {
      scm_mem_heap_shift(heap);
    }
  }
  if (ptr == NULL) {
    do {
      scm_mem_heap_unshift(heap);
    } while (heap->current != heap->head
             && scm_mem_heap_block_used(heap->current) < SCM_MEM_ALIGN_BYTE);
  }

  return ptr;
}

inline void
scm_mem_heap_cancel_alloc(ScmMemHeap *heap, size_t size)
{
  if (heap->nr_block <= 0) return;

  if (heap->current == NULL)
    scm_mem_heap_unshift(heap);

  if (scm_mem_heap_block_deallocable_p(heap->current, size))
    scm_mem_heap_block_deallocated(heap->current, size);
  else
    scm_mem_heap_block_clean(heap->current);

  while (scm_mem_heap_block_used(heap->current) < SCM_MEM_ALIGN_BYTE
         && heap->current != heap->head) {
    scm_mem_heap_unshift(heap);
  }
}


#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = (heap)->head;                   \
       (block) != NULL;                          \
       (block) = (block)->next)

typedef struct ScmMemRootBlockHdrRec {
  ScmMemRootBlock *next;
  ScmMemRootBlock *prev;
} ScmMemRootBlockHdr;

struct ScmMemRootBlockRec {
  ScmMemRootBlockHdr hdr;
  uint8_t object[SCM_MEM_ALIGN_BYTE];
};

inline uint8_t
scm_mem_root_block_shift_byte(ScmMemRootBlock *block)
{
  return (uint8_t)(SCM_MEM_ALIGN_BYTE
                   - (uintptr_t)(block)->object % SCM_MEM_ALIGN_BYTE);

}

inline ScmObj
scm_mem_root_block_object(ScmMemRootBlock *block)
{
  return SCM_OBJ(block->object + scm_mem_root_block_shift_byte(block));
}

inline uint8_t
scm_mem_root_block_obj_shift_byte(ScmObj obj)
{
  return *((uint8_t *)obj - 1);
}

inline void
scm_mem_root_block_obj_set_shit_byte(ScmObj obj, uint8_t sf)
{
  *((uint8_t *)obj - 1) = sf;
}

inline ScmMemRootBlock *
scm_mem_root_block_obj_header(ScmObj obj)
{
  return (ScmMemRootBlock *)((uint8_t *)obj
                             - scm_mem_root_block_obj_shift_byte(obj)
                             - sizeof(ScmMemRootBlockHdr));
}

inline ScmMemRootBlock *
scm_mem_root_block_new(size_t sz)
{
  ScmMemRootBlock *block;

  block = malloc(sz);
  if (block == NULL) return NULL;

  uint8_t shift = scm_mem_root_block_shift_byte(block);
  ScmObj obj = scm_mem_root_block_object(block);
  block->hdr.next = NULL;
  block->hdr.prev = NULL;
  scm_mem_root_block_obj_set_shit_byte(obj, shift);

  return block;
}

inline void
scm_mem_root_block_free(ScmMemRootBlock *block)
{
  free(block);
}

inline bool
scm_mem_root_block_obj_in_blok_p(ScmObj obj)
{
  return (((unsigned int)(obj) > sizeof(ScmMemRootBlock)) ?
          true : false);
}

/* ScmMem を ScmObj の一種(kind of) として定義する。                     */
/* これは object.h が ScmMem シンボルへの依存するのを避けるため。            */
/* ScmMem を GC で管理することはしない (scm_mem_alloc で生成することは不可)  */
struct ScmMemRec {
  ScmObjHeader header;
  ScmBasicHashTable *to_obj_tbl;
  ScmBasicHashTable *from_obj_tbl;
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
  ScmMemHeap *persistent;
  ScmMemRootBlock *roots;
  ScmRef *extra_rfrn;
  size_t nr_extra;
  bool gc_enabled;
};

inline void
scm_mem_add_to_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block)
{
  block->hdr.next = *head;
  block->hdr.prev = NULL;
  if (*head != NULL)
    (*head)->hdr.prev = block;
  *head = block;
}

inline void
scm_mem_del_from_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block)
{
  ScmMemRootBlock *nxt = block->hdr.next;
  ScmMemRootBlock *prv = block->hdr.prev;

  if (prv == NULL)
    *(head) = nxt;
  else
    prv->hdr.next = nxt;

  if (nxt != NULL)
    nxt->hdr.prev = prv;
}

inline ScmRef
scm_mem_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj)
{
  return ((ScmRef)((uintptr_t)obj + scm_type_info_obj_size(type)));
}

inline size_t
scm_mem_size_of_obj_has_weak_ref(size_t size)
{
  return size + sizeof(ScmObj);
}

inline void
scm_mem_set_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj, ScmObj nxt)
{
  ScmRef r = scm_mem_next_obj_has_weak_ref(type, obj);
  SCM_REF_UPDATE(r, nxt);
}

inline void
scm_mem_add_obj_to_weak_list(ScmMemHeap *heap, ScmRef ref, ScmTypeInfo *type)
{
  ScmObj obj = SCM_REF_DEREF(ref);
  ScmObj nxt = SCM_OBJ(heap->weak_list);
  scm_mem_set_next_obj_has_weak_ref(type, obj, nxt);
  heap->weak_list = (void *)obj;
}


#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256




inline void
scm_mem_enable_gc(ScmMem *mem)
{
  scm_assert(mem != NULL);

  mem->gc_enabled = true;
}

inline void
scm_mem_disable_gc(ScmMem *mem)
{
  scm_assert(mem != NULL);

  mem->gc_enabled = false;
}

inline bool
scm_mem_gc_enabled_p(ScmMem *mem)
{
  scm_assert(mem != NULL);

  return mem->gc_enabled;
}

ScmMem *scm_mem_new(void);
ScmMem *scm_mem_end(ScmMem *mem);
ScmMem *scm_mem_clean(ScmMem *mem);
ScmMem *scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, ScmRef ref);
ScmMem *scm_mem_alloc_persist(ScmMem *mem, ScmTypeInfo *type, ScmRef ref);
ScmMem *scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, ScmRef ref);
ScmObj scm_mem_free_root(ScmMem *mem, ScmObj obj);
// ScmMem *scm_mem_alloc_plain(ScmMem *mem, ScmTypeInfo *type, ScmRef ref);
// ScmObj scm_mem_free_plain(ScmMem *mem, ScmObj obj);
ScmRef scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref);
ScmMem *scm_mem_alloc(ScmMem *mem, ScmTypeInfo *type,
                      SCM_MEM_ALLOC_TYPE_T alloc, ScmRef ref);
void scm_mem_gc_start(ScmMem *mem);
ScmObj scm_memory_alloc_shared_root(ScmTypeInfo *type);
ScmObj scm_memory_free_shared_root(ScmObj obj);
void scm_memory_free_all_shared_root(void);

void scm_mem_enable_current_mem_gc(void);
void scm_mem_disable_current_mem_gc(void);
bool scm_mem_current_mem_gc_enabled_p(void);

#define SCM_MEM_ALLOC(type, alloc, ref) \
  scm_mem_alloc(scm_vm_current_mm(), type, alloc, ref)


inline void *
scm_memory_allocate(size_t size)
{
  return malloc(size);
}

inline void *
scm_memory_release(void *block)
{
  free(block);
  return NULL;
}

#endif /* INCLUDED_MEMORY_H__ */
