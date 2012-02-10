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

static inline ScmObj
scm_forward_forward(ScmObj obj)
{
  return SCM_FORWARD(obj)->forward;
}

static inline void
scm_forward_initialize(ScmObj obj, ScmObj fwd)
{
  scm_obj_init(obj, &SCM_FORWARD_TYPE_INFO);
  SCM_FORWARD(obj)->forward = fwd;
}


/****************************************************************************/
/* Memory Manager                                                           */
/****************************************************************************/

#define SCM_MEM_ALIGN_BYTE 8

static inline size_t
scm_mem_align_size(size_t size)
{
  return ((size + SCM_MEM_ALIGN_BYTE - 1)
          / SCM_MEM_ALIGN_BYTE
          * SCM_MEM_ALIGN_BYTE);
}

static inline void *
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

static inline ScmMemHeapBlock *
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

static inline void *
scm_mem_heap_delete_block(ScmMemHeapBlock *block)
{
  free(block);
  return NULL;
}

static inline size_t
scm_mem_heap_block_used(ScmMemHeapBlock *block)
{
  return block->used;
}

static inline size_t
scm_mem_heap_block_free(ScmMemHeapBlock *block)
{
  return block->size - block->used;
}

static inline uint8_t *
scm_mem_heap_block_head(ScmMemHeapBlock *block)
{
  return (uint8_t *)block->heap;
}

static inline void
scm_mem_heap_block_allocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used += scm_mem_align_size(sz);
}

static inline void
scm_mem_heap_block_deallocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used -= scm_mem_align_size(sz);
}

static inline bool
scm_mem_heap_block_allocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_free(block)) ?
          true : false);
}

static inline bool
scm_mem_heap_block_deallocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_used(block)) ?
          true : false);
}

static inline void *
scm_mem_heap_block_free_ptr(ScmMemHeapBlock *block)
{
  return (void *)(block->heap + block->used);
}

static inline size_t
scm_mem_heap_block_ptr_offset(ScmMemHeapBlock *block, void *ptr)
{
  return (size_t)((uint8_t *)ptr - block->heap);
}

static inline bool
scm_mem_heap_block_ptr_allocated_p(ScmMemHeapBlock *block, void *ptr)
{
  return ((scm_mem_heap_block_ptr_offset(block, ptr)
           < scm_mem_heap_block_used(block)) ?
          true : false);
}

static inline ScmObj
scm_mem_heap_block_next_obj(ScmMemHeapBlock *block, ScmObj obj)
{
  return SCM_OBJ((uint8_t *)obj
                 + scm_mem_alloc_size_in_heap_aligned(scm_obj_type(obj)));
}

static inline void
scm_mem_heap_block_clean(ScmMemHeapBlock *block)
{
  uint8_t *p = scm_mem_heap_block_head(block);
  block->used = (size_t)p - (size_t)(block)->heap;
}

static inline bool
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

#define SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap) \
  (((heap)->current == NULL) ? 0 : scm_mem_heap_block_free((heap)->current))
#define SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(heap)  ((heap)->current == (heap)->tail)
#define SCM_MEM_HEAP_WEAK_LIST(heap) ((heap)->weak_list)
#define SCM_MEM_HEAP_SET_WEAK_LIST(heap, p) ((heap)->weak_list = (void *)(p))
#define SCM_MEM_HEAP_NR_BLOCK(heap) ((heap)->nr_block)
#define SCM_MEM_HEAP_NR_FREE_BLOCK(heap) ((heap)->nr_free_block)
#define SCM_MEM_HEAP_NR_USED_BLOCK(heap) \
  ((heap)->nr_block - (heap)->nr_free_block)
#define SCM_MEM_HEAP_TAIL_BLOCK_SIZE(heap) \
  (((heap)->tail == NULL) ? 0U : (heap)->tail->size)

#define SCM_MEM_HEAP_ADD_BLOCK(heap, block)                             \
  do {                                                                  \
    if ((heap)->head == NULL)                                           \
      (heap)->head = (block);                                           \
    else                                                                \
      (heap)->tail->next = (block);                                     \
    (block)->next = NULL;                                               \
    (block)->prev = (heap)->tail;                                       \
    (heap)->tail = (block);                                             \
    (heap)->nr_block++;                                                 \
    if ((heap)->current == NULL)                                        \
      (heap)->current = (block);                                        \
    else                                                                \
      (heap)->nr_free_block++;                                          \
  } while(0)

#define SCM_MEM_HEAP_DEL_BLOCK(heap)                                    \
  do {                                                                  \
    if ((heap)->tail != NULL) {                                         \
      ScmMemHeapBlock *b = (heap)->tail;                                \
                                                                        \
      if (SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(heap))                         \
        (heap)->current = b->prev;                   \
      else                                                              \
        (heap)->nr_free_block--;                                        \
                                                                        \
      if (b->prev == NULL) {                         \
        (heap)->head = NULL;                                            \
        (heap)->tail = NULL;                                            \
      }                                                                 \
      else {                                                            \
        (heap)->tail = b->prev;                      \
        (heap)->tail->next = NULL;                                      \
      }                                                                 \
      (heap)->nr_block--;                                               \
                                                                        \
      scm_mem_heap_delete_block(b);                                    \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_RELEASE_BLOCKS(heap, nr_leave)             \
  do {                                                          \
    int i, n = SCM_MEM_HEAP_NR_BLOCK(heap) - (nr_leave);        \
    for (i = 0; i < n; i++)                                     \
      SCM_MEM_HEAP_DEL_BLOCK(heap);                             \
  } while(0)

#define SCM_MEM_HEAP_DELETE_HEAP(heap)          \
  do {                                          \
    SCM_MEM_HEAP_RELEASE_BLOCKS(heap, 0);       \
    free(heap);                                 \
    (heap) = NULL;                              \
  } while(0)

#define SCM_MEM_HEAP_NEW_HEAP(heap, nr_blk, sz)     \
  do {                                              \
    int i;                                          \
                                                    \
    (heap) = malloc(sizeof(*(heap)));               \
    if ((heap) != NULL) {                           \
      (heap)->head = NULL;                          \
      (heap)->tail = NULL;                          \
      (heap)->current = NULL;                       \
      (heap)->weak_list = NULL;                     \
      (heap)->nr_block = 0;                         \
      (heap)->nr_free_block = 0;                    \
                                                    \
      for (i = 0; i < (nr_blk); i++) {              \
        ScmMemHeapBlock *block;                     \
        block = scm_mem_heap_new_block(sz);         \
        if ((block) == NULL) {                      \
          SCM_MEM_HEAP_DELETE_HEAP(heap);           \
          break;                                    \
        }                                           \
        SCM_MEM_HEAP_ADD_BLOCK(heap, block);        \
      }                                             \
                                                    \
      if ((heap) != NULL)                           \
        (heap)->current = (heap)->head;             \
    }                                               \
  } while(0)

#define SCM_MEM_HEAP_SHIFT(heap)                                        \
  do {                                                                  \
    if ((heap)->current != NULL) {                                      \
      (heap)->current = (heap)->current->next;                          \
      if ((heap)->current != NULL)                                      \
        (heap)->nr_free_block--;                                        \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_UNSHIFT(heap)                                      \
  do {                                                                  \
    if ((heap)->current != (heap)->head) {                              \
      if ((heap)->current == NULL) {                                    \
        (heap)->current = (heap)->tail;                                 \
      }                                                                 \
      else {                                                            \
        (heap)->current = (heap)->current->prev;     \
        (heap)->nr_free_block++;                                        \
      }                                                                 \
    }                                                                   \
  } while(0)


#define SCM_MEM_HEAP_REWIND(heap)                                       \
  do {                                                                  \
    (heap)->current = (heap)->head;                                     \
    if ((heap)->nr_block > 1)                                           \
      (heap)->nr_free_block = (heap)->nr_block - 1;                     \
    else                                                                \
      (heap)->nr_free_block = 0;                                        \
  } while(0)

#define SCM_MEM_HEAP_ALLOC(heap, size, ptr)                             \
  do {                                                                  \
    *(ptr) = NULL;                                                      \
    while ((heap)->current != NULL && *(ptr) == NULL) {                 \
      if (scm_mem_heap_block_allocable_p(heap->current, size)) {       \
        *(ptr) = scm_mem_heap_block_free_ptr((heap)->current);          \
        scm_mem_heap_block_allocated((heap)->current, size);            \
      }                                                                 \
      else {                                                            \
        SCM_MEM_HEAP_SHIFT(heap);                                       \
      }                                                                 \
    }                                                                   \
    if (*(ptr) == NULL) {                                               \
      SCM_MEM_HEAP_UNSHIFT(heap);                                       \
      while ((scm_mem_heap_block_used((heap)->current) < SCM_MEM_ALIGN_BYTE) \
             && (heap)->current != (heap)->head) {                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      }                                                                 \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_CANCEL_ALLOC(heap, size)                           \
  do {                                                                  \
    if ((heap)->nr_block > 0) {                                         \
      if ((heap)->current == NULL)                                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      if (scm_mem_heap_block_deallocable_p((heap)->current, size))     \
        scm_mem_heap_block_deallocated((heap)->current, size);          \
      else                                                              \
        scm_mem_heap_block_clean((heap)->current);                      \
      while (scm_mem_heap_block_used((heap)->current) < SCM_MEM_ALIGN_BYTE \
             && (heap)->current != (heap)->head) {                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      }                                                                 \
    }                                                                   \
  } while(0)

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

#define SCM_MEM_ROOT_BLOCK_NEXT(block) ((block)->hdr.next)
#define SCM_MEM_ROOT_BLOCK_PREV(block) ((block)->hdr.prev)
#define SCM_MEM_ROOT_BLOCK_SET_NEXT(block, n) ((block)->hdr.next = n)
#define SCM_MEM_ROOT_BLOCK_SET_PREV(block, p) ((block)->hdr.prev = p)
#define SCM_MEM_ROOT_BLOCK_SHIFT_BYTE(block) \
  ((uint8_t)(SCM_MEM_ALIGN_BYTE \
             - (uintptr_t)(block)->object % SCM_MEM_ALIGN_BYTE))
#define SCM_MEM_ROOT_BLOCK_OBJECT(block) \
  (SCM_OBJ((block)->object + SCM_MEM_ROOT_BLOCK_SHIFT_BYTE(block)))
#define SCM_MEM_ROOT_BLOCK_OBJ_SHIFT_BYTE(obj) (*((uint8_t *)(obj) - 1))
#define SCM_MEM_ROOT_BLOCK_OBJ_SET_SHIT_BYTE(obj, sf) \
  ((*((uint8_t *)(obj) - 1)) = sf)
#define SCM_MEM_ROOT_BLOCK_OBJ_HEADER(obj) \
  ((ScmMemRootBlock *)((uint8_t *)obj \
                       - SCM_MEM_ROOT_BLOCK_OBJ_SHIFT_BYTE(obj) \
                       - sizeof(ScmMemRootBlockHdr)))

#define SCM_MEM_ROOT_BLOCK_NEW(bp, sz)          \
  do {                                          \
    *(bp) = malloc(sz);                         \
    if (*(bp) != NULL) {                        \
      uint8_t shift = SCM_MEM_ROOT_BLOCK_SHIFT_BYTE(*(bp));         \
      ScmObj obj = SCM_MEM_ROOT_BLOCK_OBJECT(*(bp));                \
      (*(bp))->hdr.next = NULL;                                     \
      (*(bp))->hdr.prev = NULL;                                     \
      SCM_MEM_ROOT_BLOCK_OBJ_SET_SHIT_BYTE(obj, shift);             \
    }                                                               \
  } while(0)

#define SCM_MEM_ROOT_BLOCK_FREE(block) (free(block))

#define SCM_MEM_ROOT_BLOCK_IS_OBJ_IN_BLOK(obj) \
  ((unsigned int)(obj) > sizeof(ScmMemRootBlock))


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

#define SCM_MEM_ADD_TO_ROOT_SET(head, block)        \
  do {                                              \
    SCM_MEM_ROOT_BLOCK_SET_NEXT(block, *(head));    \
    SCM_MEM_ROOT_BLOCK_SET_PREV(block, NULL);       \
    if (*(head) != NULL)                            \
      SCM_MEM_ROOT_BLOCK_SET_PREV(*(head), block);  \
    *(head) = (block);                              \
  } while(0)

#define SCM_MEM_DEL_FROM_ROOT_SET(head, block)                   \
  do {                                                           \
    ScmMemRootBlock *nxt = SCM_MEM_ROOT_BLOCK_NEXT(block);       \
    ScmMemRootBlock *prv = SCM_MEM_ROOT_BLOCK_PREV(block);       \
                                                                 \
    if (prv == NULL)                                             \
      *(head) = nxt;                                             \
    else                                                         \
      SCM_MEM_ROOT_BLOCK_SET_NEXT(prv, nxt);                     \
                                                                 \
    if (nxt != NULL)                                             \
      SCM_MEM_ROOT_BLOCK_SET_PREV(nxt, prv);                     \
  } while(0)


#define SCM_MEM_NEXT_OBJ_HAS_WEAK_REF(type, obj)                \
  ((ScmRef)((uintptr_t)(obj) + scm_type_info_obj_size(type)))
#define SCM_MEM_SIZE_OF_OBJ_HAS_WEAK_REF(size) ((size) + sizeof(ScmObj))
#define SCM_MEM_SET_NEXT_OBJ_HAS_WEAK_REF(type, obj, nxt) \
  do {                                                    \
    ScmRef r;                                             \
    r = SCM_MEM_NEXT_OBJ_HAS_WEAK_REF(type, obj);         \
    SCM_REF_UPDATE(r, nxt);                               \
  } while(0)
#define SCM_MEM_ADD_OBJ_TO_WEAK_LIST(heap, ref, type)                   \
  do {                                                                  \
    ScmObj obj = SCM_REF_DEREF(ref);                                      \
    ScmObj nxt = SCM_OBJ(SCM_MEM_HEAP_WEAK_LIST(heap));                 \
    SCM_MEM_SET_NEXT_OBJ_HAS_WEAK_REF(type, obj, nxt);                  \
    SCM_MEM_HEAP_SET_WEAK_LIST(heap, obj);                              \
  } while(0)


#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256




static inline void
scm_mem_enable_gc(ScmMem *mem)
{
  scm_assert(mem != NULL);

  mem->gc_enabled = true;
}

static inline void
scm_mem_disable_gc(ScmMem *mem)
{
  scm_assert(mem != NULL);

  mem->gc_enabled = false;
}

static inline bool
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


static inline void *
scm_memory_allocate(size_t size)
{
  return malloc(size);
}

static inline void *
scm_memory_release(void *block)
{
  free(block);
  return NULL;
}

#endif /* INCLUDED_MEMORY_H__ */
