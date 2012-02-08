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
#define SCM_FORWORD(obj) ((ScmFoward *)(obj))

typedef enum {
  SCM_MEM_ALLOC_HEAP,
  SCM_MEM_ALLOC_ROOT,
  SCM_MEM_ALLOC_SHARED_ROOT,
} SCM_MEM_ALLOC_TYPE_T;

enum { SCM_MEM_NR_ALLOC_TYPE = SCM_MEM_ALLOC_SHARED_ROOT + 1 };

#include "object.h"
#include "basichash.h"


struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

#define SCM_FORWARD(obj) ((ScmForward *)(obj))
#define SCM_FORWARD_FORWARD(obj) (SCM_FORWARD(obj)->forward)
#define SCM_FORWARD_INITIALIZE(obj, fwd)          \
  do {                                            \
    scm_obj_init(obj, &SCM_FORWARD_TYPE_INFO);    \
    SCM_FORWARD(obj)->forward = fwd;              \
  } while(0)


#define SCM_MEM_ALIGN_BYTE 8
#define SCM_MEM_ALIGN_SIZE(size)        \
  (((size) % SCM_MEM_ALIGN_BYTE == 0) ?                                 \
   (size) : (size) + (SCM_MEM_ALIGN_BYTE - (size) % SCM_MEM_ALIGN_BYTE))
#define SCM_MEM_ALIGN_PTR(ptr)                  \
  ((void *)SCM_MEM_ALIGN_SIZE((uintptr_t)ptr))

struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  struct ScmMemHeapBlockRec *prev;
  size_t size;
  size_t used;
  uint8_t heap[0];
};

#define SCM_MEM_HEAP_NEW_BLOCK(block, sz)                       \
  do {                                                          \
    (block) = malloc(sizeof(ScmMemHeapBlock) + (sz));           \
    if ((block) != NULL) {                                      \
      uint8_t *p;                                               \
      (block)->next = NULL;                                     \
      (block)->prev = NULL;                                     \
      (block)->size = (sz);                                     \
      p = SCM_MEM_ALIGN_PTR((block)->heap);                     \
      (block)->used = (size_t)p - (size_t)(block)->heap;        \
    }                                                           \
  } while(0)

#define SCM_MEM_HEAP_DELEATE_BLOCK(block)            \
  do {                                               \
    free(block);                                     \
    (block) = NULL;                                  \
  } while(0)

#define SCM_MEM_HEAP_BLOCK_NEXT(block) ((block)->next)
#define SCM_MEM_HEAP_BLOCK_PREV(block) ((block)->prev)
#define SCM_MEM_HEAP_BLOCK_SIZE(block) ((block)->size)
#define SCM_MEM_HEAP_BLOCK_USED(block) ((block)->used)
#define SCM_MEM_HEAP_BLOCK_FREE(block) ((block)->size - (block)->used)
#define SCM_MEM_HEAP_BLOCK_HEAD(block) \
  ((uint8_t *)SCM_MEM_ALIGN_PTR(block->heap))
#define SCM_MEM_HEAP_BLOCK_ALLOCATED(block, sz) \
  ((block)->used += SCM_MEM_ALIGN_SIZE(sz))
#define SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, sz) \
  ((block)->used -= SCM_MEM_ALIGN_SIZE(sz))
#define SCM_MEM_HEAP_BLOCK_IS_ALLOCABLE(block, sz) \
  (SCM_MEM_ALIGN_SIZE(sz) <= SCM_MEM_HEAP_BLOCK_FREE(block))
#define SCM_MEM_HEAP_BLOCK_IS_DEALLOCABLE(block, sz) \
  (SCM_MEM_ALIGN_SIZE(sz) <= SCM_MEM_HEAP_BLOCK_USED(block))
#define SCM_MEM_HEAP_BLOCK_FREE_PTR(block)      \
  ((void *)((block)->heap + (block)->used))
#define SCM_MEM_HEAP_BLOCK_PTR_OFFSET(block, ptr)       \
  ((size_t)((uint8_t *)(ptr) - block->heap))
#define SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr)                 \
  (SCM_MEM_HEAP_BLOCK_PTR_OFFSET(block, ptr) < SCM_MEM_HEAP_BLOCK_USED(block))
#define SCM_MEM_HEAP_BLOCK_NEXT_OBJ(block, obj)                         \
  SCM_OBJ((uint8_t *)obj                                                \
          + scm_mem_alloc_size_in_heap_aligned(scm_obj_type(obj)))
#define SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj)                     \
  for ((obj) = SCM_OBJ(SCM_MEM_HEAP_BLOCK_HEAD(block));         \
       SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, obj);                 \
       obj = SCM_MEM_HEAP_BLOCK_NEXT_OBJ(block, obj))
#define SCM_MEM_HEAP_BLOCK_CLEAN(block)                         \
  do {                                                          \
    uint8_t *p = SCM_MEM_HEAP_BLOCK_HEAD(block);                \
    (block)->used = (size_t)p - (size_t)(block)->heap;          \
  } while(0)

#define SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block, obj) \
  ((block)->heap <= (uint8_t *)obj \
   && (uint8_t *)obj < (block)->heap + (block)->used)

struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  void *weak_list;
  int nr_block;
  int nr_free_block;
};

#define SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap) \
  (((heap)->current == NULL) ? 0 : SCM_MEM_HEAP_BLOCK_FREE((heap)->current))
#define SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(heap)  ((heap)->current == (heap)->tail)
#define SCM_MEM_HEAP_WEAK_LIST(heap) ((heap)->weak_list)
#define SCM_MEM_HEAP_SET_WEAK_LIST(heap, p) ((heap)->weak_list = (void *)(p))
#define SCM_MEM_HEAP_NR_BLOCK(heap) ((heap)->nr_block)
#define SCM_MEM_HEAP_NR_FREE_BLOCK(heap) ((heap)->nr_free_block)
#define SCM_MEM_HEAP_NR_USED_BLOCK(heap) \
  ((heap)->nr_block - (heap)->nr_free_block)
#define SCM_MEM_HEAP_TAIL_BLOCK_SIZE(heap) \
  (((heap)->tail == NULL) ? 0U : SCM_MEM_HEAP_BLOCK_SIZE((heap)->tail))

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
        (heap)->current = SCM_MEM_HEAP_BLOCK_PREV(b);                   \
      else                                                              \
        (heap)->nr_free_block--;                                        \
                                                                        \
      if (SCM_MEM_HEAP_BLOCK_PREV(b) == NULL) {                         \
        (heap)->head = NULL;                                            \
        (heap)->tail = NULL;                                            \
      }                                                                 \
      else {                                                            \
        (heap)->tail = SCM_MEM_HEAP_BLOCK_PREV(b);                      \
        (heap)->tail->next = NULL;                                      \
      }                                                                 \
      (heap)->nr_block--;                                               \
                                                                        \
      SCM_MEM_HEAP_DELEATE_BLOCK(b);                                    \
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
        SCM_MEM_HEAP_NEW_BLOCK(block, sz);          \
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
      (heap)->current = SCM_MEM_HEAP_BLOCK_NEXT((heap)->current);       \
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
        (heap)->current = SCM_MEM_HEAP_BLOCK_PREV((heap)->current);     \
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
      if (SCM_MEM_HEAP_BLOCK_IS_ALLOCABLE(heap->current, size)) {       \
        *(ptr) = SCM_MEM_HEAP_BLOCK_FREE_PTR((heap)->current);          \
        SCM_MEM_HEAP_BLOCK_ALLOCATED((heap)->current, size);            \
      }                                                                 \
      else {                                                            \
        SCM_MEM_HEAP_SHIFT(heap);                                       \
      }                                                                 \
    }                                                                   \
    if (*(ptr) == NULL) {                                               \
      SCM_MEM_HEAP_UNSHIFT(heap);                                       \
      while ((SCM_MEM_HEAP_BLOCK_USED((heap)->current) < SCM_MEM_ALIGN_BYTE) \
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
      if (SCM_MEM_HEAP_BLOCK_IS_DEALLOCABLE((heap)->current, size))     \
        SCM_MEM_HEAP_BLOCK_DEALLOCATED((heap)->current, size);          \
      else                                                              \
        SCM_MEM_HEAP_BLOCK_CLEAN((heap)->current);                      \
      while (SCM_MEM_HEAP_BLOCK_USED((heap)->current) < SCM_MEM_ALIGN_BYTE \
             && (heap)->current != (heap)->head) {                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      }                                                                 \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = (heap)->head;                   \
       (block) != NULL;                          \
       (block) = SCM_MEM_HEAP_BLOCK_NEXT(block))

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
    ScmObj obj = SCM_REF_OBJ(ref);                                      \
    ScmObj nxt = SCM_OBJ(SCM_MEM_HEAP_WEAK_LIST(heap));                 \
    SCM_MEM_SET_NEXT_OBJ_HAS_WEAK_REF(type, obj, nxt);                  \
    SCM_MEM_HEAP_SET_WEAK_LIST(heap, obj);                              \
  } while(0)


#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

extern ScmTypeInfo SCM_FORWARD_TYPE_INFO;

void *scm_memory_allocate(size_t size);
void *scm_memory_release(void *block);

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
size_t scm_mem_alloc_size_in_heap(ScmTypeInfo *type);
size_t scm_mem_alloc_size_in_heap_aligned(ScmTypeInfo *type);
size_t scm_mem_alloc_size_in_root(ScmTypeInfo *type);
ScmObj scm_memory_alloc_shared_root(ScmTypeInfo *type);
ScmObj scm_memory_free_shared_root(ScmObj obj);
void scm_memory_free_all_shared_root(void);

#define SCM_MEM_ALLOC(type, alloc, ref) \
  scm_mem_alloc(scm_vm_current_mm(), type, alloc, ref)

#endif /* INCLUDED_MEMORY_H__ */
