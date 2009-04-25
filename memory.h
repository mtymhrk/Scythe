#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

typedef uintptr_t ScmObjRef;
typedef struct ScmObjStackElemRec ScmObjStackElem;
typedef struct ScmObjStackBlockRec ScmObjStackBlock;
typedef struct ScmObjStackRec ScmObjStack;

typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRootBlockRec ScmMemRootBlock;
typedef struct ScmMemRec ScmMem;
typedef struct ScmForwardRec ScmForward;

#define SCM_FORWORD(obj) ((ScmFoward *)(obj))

#include "basichash.h"
#include "object.h"
#include "vm.h"

#define SCM_OBJ_REF_MAKE(obj) ((ScmObjRef)&(obj))
#define SCM_OBJ_REF_NULL ((ScmObjRef)NULL)
#define SCM_OBJ_REF_OBJ(ref) (*((ScmObj *)(ref)))
#define SCM_OBJ_REF_UPDATE(ref, obj) (*((ScmObj *)(ref)) = (obj))

enum { SCM_OBJ_STACK_ELEM_OBJ, SCM_OBJ_STACK_ELEM_PTR };

struct ScmObjStackElemRec {
  int type;
  union {
    ScmObj obj;
    ScmObj *ptr;
  } val;
};

#define SCM_OBJ_STACK_ELEM_TYPE(elem) ((elem)->type)

#define SCM_OBJ_STACK_ELEM_VAL(elem, value)                 \
  do {                                                      \
    switch ((elem)->type) {                                 \
    case SCM_OBJ_STACK_ELEM_OBJ:                            \
      SCM_OBJ(value) = (elem)->val.obj;                     \
      break;                                                \
    case SCM_OBJ_STACK_ELEM_PTR:                            \
      (ScmObj *)(value) = (elem)->val.ptr;                  \
      break;                                                \
    }                                                       \
  } while(0)

#define SCM_OBJ_STACK_ELEM_TO_OBJ_REF(elem) (SCM_OBJ_REF_MAKE((elem)->val.obj))

#define SCM_OBJ_STACK_ELEM_SET_VAL(elem, value)             \
  do {                                                      \
    switch ((elem)->type) {                                 \
    case SCM_OBJ_STACK_ELEM_OBJ:                            \
      (elem)->val.obj = SCM_OBJ((value));                   \
      break;                                                \
    case SCM_OBJ_STACK_ELEM_PTR:                            \
      (elem)->val.ptr = (ScmObj *)(value);                  \
      break;                                                \
    }                                                       \
  } while(0)


#define SCM_OBJ_STACK_ELEM_INIT(elem, t, v)                 \
  do {                                                      \
    (elem)->type = t;                                       \
    SCM_OBJ_STACK_ELEM_SET_VAL(elem, v);                    \
  } while(0)

struct ScmObjStackBlockRec {
  ScmObjStackBlock *next;
  ScmObjStackBlock *prev;
  size_t size;
  ScmObjStackElem *sp;
  ScmObjStackElem stack[0];
};

#define SCM_OBJ_STACK_BLOCK_NEXT(block) ((block)->next)
#define SCM_OBJ_STACK_BLOCK_PREV(block) ((block)->prev)
#define SCM_OBJ_STACK_BLOCK_SIZE(block) ((block)->size)
#define SCM_OBJ_STACK_BLOCK_SP(block) ((block)->sp)
#define SCM_OBJ_STACK_BLOCK_BOTTOM(block) ((block)->stack)
#define SCM_OBJ_STACK_BLOCK_TOP(block)                                  \
  (SCM_OBJ_STACK_BLOCK_BOTTOM(block) + SCM_OBJ_STACK_BLOCK_SIZE(block))
#define SCM_OBJ_STACK_BLOCK_IS_FULL(block)                              \
  (SCM_OBJ_STACK_BLOCK_TOP(block) <= SCM_OBJ_STACK_BLOCK_SP(block))
#define SCM_OBJ_STACK_BLOCK_SET_NEXT(block, nxt) ((block)->next = (nxt))
#define SCM_OBJ_STACK_BLOCK_SET_PREV(block, prv) ((block)->prev = (prv))
#define SCM_OBJ_STACK_BLOCK_SET_SIZE(block, sz) ((block)->size = (sz))
#define SCM_OBJ_STACK_BLOCK_SET_SP(block, ptr) ((block)->sp = (ptr))

#define SCM_OBJ_STACK_NEW_BLOCK(block, sz)                              \
  do {                                                                  \
    (block) = scm_memory_allocate(sizeof(ScmObjStackBlock)              \
                                  + sizeof(ScmObjStackElem) * sz);      \
    if ((block) != NULL) {                                              \
      SCM_OBJ_STACK_BLOCK_SET_NEXT(block, NULL);                        \
      SCM_OBJ_STACK_BLOCK_SET_PREV(block, NULL);                        \
      SCM_OBJ_STACK_BLOCK_SET_SIZE(block, sz);                          \
      SCM_OBJ_STACK_BLOCK_SET_SP(block,                                 \
                                 SCM_OBJ_STACK_BLOCK_BOTTOM(block));    \
    }                                                                   \
  } while(0)

#define SCM_OBJ_STACK_BLOCK_PUSH(block, obj_ptr)                        \
  do {                                                                  \
    SCM_OBJ_STACK_ELEM_INIT((block)->sp,                                \
                            SCM_OBJ_STACK_ELEM_PTR, obj_ptr);           \
    (block)->sp++;                                                      \
  } while(0)

#define SCM_OBJ_STACK_BLOCK_ALLOC(block, obj_ref, init)                 \
  do {                                                                  \
    SCM_OBJ_STACK_ELEM_INIT((block)->sp,                                \
                            SCM_OBJ_STACK_ELEM_OBJ, init);          \
    (obj_ref) = SCM_OBJ_STACK_ELEM_TO_OBJ_REF((block)->sp);             \
    (block)->sp++;                                                      \
  } while(0)

struct ScmObjStackRec {
  ScmObjStackBlock *head;
  ScmObjStackBlock *tail;
  ScmObjStackBlock *current;
};

#define SCM_OBJ_STACK_ADD_BLOCK(stack, block)                           \
  do {                                                                  \
    if ((stack)->head == NULL) {                                        \
      (stack)->head = (stack)->tail = (stack)->current = (block);       \
    }                                                                   \
    else {                                                              \
      (stack)->tail->next = (block);                                    \
      SCM_OBJ_STACK_BLOCK_SET_NEXT(block, NULL);                        \
      SCM_OBJ_STACK_BLOCK_SET_PREV(block, (stack)->tail);               \
      (stack)->tail = (block);                                          \
    }                                                                   \
  } while(0)

#define SCM_OBJ_STACK_DEL_BLOCK(stack, block)                        \
  do {                                                               \
    (block) = (stack)->tail;                                         \
    if ((block) != NULL) {                                           \
      (stack)->tail = SCM_OBJ_STACK_BLOCK_PREV(block);               \
                                                                     \
      if ((stack)->tail == NULL)                                     \
        (stack)->head = (stack)->current = NULL;                     \
      else                                                           \
        SCM_OBJ_STACK_BLOCK_SET_NEXT((stack)->tail, NULL);           \
                                                                     \
      if ((stack)->current == (block))                               \
        (stack)->current = (stack)->tail;                            \
    }                                                                \
  } while(0)


typedef void (*SCM_MEM_FINALIZER)(ScmObj obj);

struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  struct ScmMemHeapBlockRec *prev;
  size_t size;
  size_t used;
  uint8_t heap[0];
};

#define SCM_MEM_HEAP_NEW_BLOCK(block, sz)              \
  do {                                                 \
    (block) = malloc(sizeof(ScmMemHeapBlock) + (sz));  \
    if ((block) != NULL) {                             \
      (block)->next = NULL;                            \
      (block)->prev = NULL;                            \
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
#define SCM_MEM_HEAP_BLOCK_PREV(block) ((block)->prev)
#define SCM_MEM_HEAP_BLOCK_SIZE(block) ((block)->size)
#define SCM_MEM_HEAP_BLOCK_USED(block) ((block)->used)
#define SCM_MEM_HEAP_BLOCK_FREE(block) ((block)->size - (block)->used)
#define SCM_MEM_HEAP_BLOCK_HEAD(block) ((block)->heap)
#define SCM_MEM_HEAP_BLOCK_ALLOCATED(block, sz) ((block)->used += (sz))
#define SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, sz) ((block)->used -= (sz))
#define SCM_MEM_HEAP_BLOCK_FREE_PTR(block) \
  ((void *)((block)->heap + (block)->used))
#define SCM_MEM_HEAP_BLOCK_PTR_OFFSET(block, ptr) \
  ((size_t)((uint8_t *)(ptr) - block->heap))
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
  int nr_block;
  int nr_free_block;
};

#define SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap) \
  (((heap)->current == NULL) ? 0 : SCM_MEM_HEAP_BLOCK_FREE((heap)->current))
#define SCM_MEM_HEAP_IS_CUR_BLOCK_TAIL(heap)  ((heap)->current == (heap)->tail)
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
    if ((heap)->current == NULL) {                                      \
      (heap)->current = (block);                                        \
    }                                                                   \
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

#define SCM_MEM_HEAP_NEW_HEAP(heap, nr_blk, sz) \
  do {                                              \
    int i;                                          \
                                                    \
    (heap) = malloc(sizeof(*(heap)));               \
    if ((heap) != NULL) {                           \
      (heap)->head = NULL;                          \
      (heap)->tail = NULL;                          \
      (heap)->current = NULL;                       \
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
                                                    \
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
      if ((size) <= SCM_MEM_HEAP_CUR_BLOCK_FREE_SIZE(heap)) {           \
        *(ptr) = SCM_MEM_HEAP_BLOCK_FREE_PTR((heap)->current);          \
        SCM_MEM_HEAP_BLOCK_ALLOCATED((heap)->current, (size));          \
      }                                                                 \
      else {                                                            \
        SCM_MEM_HEAP_SHIFT(heap);                                       \
      }                                                                 \
    }                                                                   \
    if (*(ptr) == NULL)                                                 \
      SCM_MEM_HEAP_CANCEL_ALLOC(heap, 0);                               \
  } while(0)

#define SCM_MEM_HEAP_CANCEL_ALLOC(heap, size)                           \
  do {                                                                  \
    if ((heap)->nr_block > 0) {                                         \
      size_t sz = (size);                                               \
      if ((heap)->current == NULL)                                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      if (sz > SCM_MEM_HEAP_BLOCK_USED((heap)->current))                \
        sz = SCM_MEM_HEAP_BLOCK_USED((heap)->current);                  \
      SCM_MEM_HEAP_BLOCK_DEALLOCATED((heap)->current, sz);              \
      while (SCM_MEM_HEAP_BLOCK_USED((heap)->current) == 0              \
             && (heap)->current != (heap)->head) {                      \
        SCM_MEM_HEAP_UNSHIFT(heap);                                     \
      }                                                                 \
    }                                                                   \
  } while(0)

#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = heap->head;                     \
       (block) != NULL;                          \
       (block) = SCM_MEM_HEAP_BLOCK_NEXT(block))


struct ScmMemRootBlockRec {
  ScmMemRootBlock *next;
  ScmMemRootBlock *prev;
  uint8_t object[0];
};

#define SCM_MEM_ROOT_BLOCK_NEXT(block) ((block)->next)
#define SCM_MEM_ROOT_BLOCK_PREV(block) ((block)->prev)
#define SCM_MEM_ROOT_BLOCK_OBJECT(block) (SCM_OBJ((block)->object))
#define SCM_MEM_ROOT_BLOCK_HEADER(obj) ((ScmMemRootBlock *)((uint8_t *)obj - 8))
#define SCM_MEM_ROOT_BLOCK_IS_OBJ_IN_BLOK(obj) ((unsigned int)(obj) > 8U)

struct ScmMemRec {
  ScmBasicHashTable *to_obj_tbl;
  ScmBasicHashTable *from_obj_tbl;
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
  ScmMemHeap *persistent;
  ScmMemRootBlock *roots;
};

#define SCM_MEM_ADD_TO_ROOT_SET(head, block)    \
  do {                                          \
    (block)->next = (head);                     \
    (block)->prev = NULL;                       \
    if ((head) != NULL)                         \
      (head)->prev = (block);                   \
    (head) = (block);                           \
  } while(0)

#define SCM_MEM_DEL_FROM_ROOT_SET(head, block)                   \
  do {                                                           \
    ScmMemRootBlock *nxt = SCM_MEM_ROOT_BLOCK_NEXT(block);       \
    ScmMemRootBlock *prv = SCM_MEM_ROOT_BLOCK_PREV(block);       \
                                                                 \
    if (prv == NULL)                                             \
      (head) = nxt;                                              \
    else                                                         \
      prv->next = nxt;                                           \
                                                                 \
    if (nxt != NULL)                                             \
      nxt->prev = prv;                                           \
  } while(0)

#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

extern const ScmTypeInfo SCM_FORWARD_TYPE_INFO;

void *scm_memory_allocate(size_t size);
void *scm_memory_release(void *block);

ScmMem *scm_mem_construct(void);
ScmMem *scm_mem_destruct(ScmMem *mem);
ScmMem *scm_mem_clean(ScmMem *mem);
ScmMem *scm_mem_attach_vm(ScmMem *mem, ScmVM *vm);
ScmMem *scm_mem_alloc(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box);
ScmMem * scm_mem_alloc_root(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box);
ScmMem *scm_mem_free_root(ScmMem *mem, ScmObj obj);
void scm_mem_gc_start(ScmMem *mem);
ScmMem *scm_mem_alloc_persist(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box);

ScmObj scm_memory_alloc_shared_root(SCM_OBJ_TYPE_T type);
ScmObj scm_memory_free_shared_root(ScmObj obj);
void scm_memory_free_all_shared_root(void);

#endif /* INCLUDED_MEMORY_H__ */
