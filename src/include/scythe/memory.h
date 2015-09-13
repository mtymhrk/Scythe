#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"


/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

typedef struct ScmForwardRec ScmForward;

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

#define SCM_FORWARD(obj) ((ScmForward *)(obj))

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

#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

typedef struct ScmMemRec ScmMem;
typedef struct ScmMemHeapCellFinInfoRec ScmMemHeapCellFinInfo;
typedef struct ScmMemHeapCellWRefInfoRec ScmMemHeapCellWRefInfo;
typedef struct ScmMemHeapCellTSortInfoRec ScmMemHeapCellTSortInfoRec;
typedef struct ScmMemHeapCellRec ScmMemHeapCell;
typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRootBlockRec ScmMemRootBlock;

typedef enum scm_mem_type scm_mem_type_t;

enum scm_mem_type {
  SCM_MEM_HEAP,
  SCM_MEM_ROOT,
};

struct ScmMemHeapCellFinInfoRec {
  ScmObj next;
};

struct ScmMemHeapCellWRefInfoRec {
  ScmObj next;
};

struct ScmMemHeapCellRec {
  size_t size;
  ScmMemHeapCellFinInfo fin_info;
  ScmMemHeapCellWRefInfo wref_info;
  scm_byte_t body[0] __attribute((aligned(SCM_MEM_ALIGN_BYTE)));
};

struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  struct ScmMemHeapBlockRec *prev;
  size_t size;
  size_t used;
  scm_byte_t heap[0];
};

struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  ScmObj fin_list;
  void *weak_list;
  int nr_block;
  int nr_free_block;
  size_t total_size;
};

typedef struct ScmMemRootBlockHdrRec {
  ScmMemRootBlock *next;
  ScmMemRootBlock *prev;
} ScmMemRootBlockHdr;

struct ScmMemRootBlockRec {
  ScmMemRootBlockHdr hdr;
  scm_byte_t body[0] __attribute((aligned(SCM_MEM_ALIGN_BYTE)));
};

struct ScmMemRec {
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
  ScmMemRootBlock *roots;
  ScmRef *extra_rfrn;
  size_t nr_extra;
  bool gc_enabled;
  size_t alloc_cnt;
};

#define SCM_MEM(obj) ((ScmMem *)(obj))

#define SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL(block, cell)                 \
  for ((cell) = (ScmMemHeapCell *)scm_mem_heap_block_head(block);     \
       scm_mem_heap_block_ptr_allocated_p(block, (cell));             \
       (cell) = scm_mem_heap_block_next_cell(block, (cell)))

#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = (heap)->head;                   \
       (block) != NULL;                          \
       (block) = (block)->next)


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

ScmMem *scm_mem_initialize(ScmMem *mem);
ScmMem *scm_mem_finalize(ScmMem *mem);
ScmMem *scm_mem_new(void);
ScmMem *scm_mem_end(ScmMem *mem);
ScmMem *scm_mem_clean(ScmMem *mem);
ScmObj scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_free_root(ScmMem *mem, ScmObj obj);
ScmRef scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref);
int scm_mem_gc_start(ScmMem *mem);


/****************************************************************************/
/* Facade                                                                   */
/****************************************************************************/

ScmObj scm_alloc_heap(ScmTypeInfo *type, size_t add_size);
ScmObj scm_alloc_root(ScmTypeInfo *type, size_t add_size);
ScmObj scm_free_root(ScmObj obj);
ScmRef scm_register_extra_rfrn(ScmRef ref);
void scm_gc_start(void);
void scm_gc_enable(void);
void scm_gc_disable(void);

static inline ScmObj
scm_alloc_mem(ScmTypeInfo *otype, size_t add_size, scm_mem_type_t mtype)
{
  switch(mtype) {
  case SCM_MEM_HEAP:
    return scm_alloc_heap(otype, add_size);
    break;
  case SCM_MEM_ROOT:
    return scm_alloc_root(otype, add_size);
    break;
  default:
    scm_assert(false);
    return SCM_OBJ_NULL;
    break;
  };
}


/****************************************************************************/
/* Wrapper                                                                  */
/****************************************************************************/

void *scm_malloc(size_t size);
void *scm_free(void *ptr);
void *scm_realloc(void *ptr, size_t size);


#endif /* INCLUDED_MEMORY_H__ */
