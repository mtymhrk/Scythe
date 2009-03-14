#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>

typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRec ScmMem;

#include "basichash.h"
#include "object.h"

typedef void (*SCM_MEM_FINALIZER)(ScmObj obj);

struct ScmMemHeapBlockRec {
  size_t size;
  struct ScmMemHeapBlockRec *next;
  char heap[0];
};

#define SCM_MEM_HEAP_NEW_BLOCK(block, sz)              \
  do {                                                 \
    (block) = malloc(sizeof(ScmMemHeapBlock) + (sz));  \
    if ((block) != NULL) {                             \
      (block)->size = (sz);                            \
      (block)->next = NULL;                            \
    }                                                  \
  } while(0)

#define SCM_MEM_HEAP_DELEATE_BLOCK(block)            \
  do {                                               \
    free(block);                                     \
    (block) = NULL;                                  \
  } while(0)

#define SCM_MEM_HEAP_BLOCK_SIZE(block) ((block)->size)
#define SCM_MEM_HEAP_BLOCK_NEXT(block) ((block)->next)
#define SCM_MEM_HEAP_BLOCK_HEAD(block) ((block)->heap)

struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  void *free;
  size_t rest_in_cur;
};

#define SCM_MEM_HEAP_ADD_BLOCK(heap, block)            \
  do {                                                 \
    if ((heap)->head == NULL)                          \
      (heap)->head = (block);                          \
    else                                               \
      (heap)->tail->next = (block);                    \
    (heap)->tail = (block);                            \
    (block)->next = NULL;                              \
  } while(0)

#define SCM_MEM_HEAP_RELEASE_BLOCKS(heap, block, prev)   \
  do {                                                   \
    ScmMemHeapBlock *p, *q;                              \
    for (p = (block); p != NULL; p = q) {                \
      q = p->next;                                       \
      SCM_MEM_HEAP_DELEATE_BLOCK(p);                     \
    }                                                    \
    if ((heap)->head == (block))                         \
      (heap)->head = (heap)->tail = NULL;                \
    else                                                 \
      (heap)->tail = (prev);                             \
  } while(0)

struct ScmMemRec {
  ScmBasicHashTable *to_obj_tbl;
  ScmBasicHashTable *from_obj_tbl;
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
};

void *scm_memory_allocate(size_t size);
void *scm_memory_release(void *block);

ScmMem *scm_mem_construct(void);
ScmMem *scm_mem_destruct(ScmMem *mem);
void *scm_mem_alloc(ScmObj *box,
                    SCM_OBJ_TYPE_T type, SCM_MEM_FINALIZER finalizer);

#endif /* INCLUDED_MEMORY_H__ */
