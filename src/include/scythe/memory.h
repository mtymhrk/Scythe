#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct ScmMemHeapCellFinInfoRec ScmMemHeapCellFinInfo;
typedef struct ScmMemHeapCellWRefInfoRec ScmMemHeapCellWRefInfo;
typedef struct ScmMemHeapCellTSortInfoRec ScmMemHeapCellTSortInfoRec;
typedef struct ScmMemHeapCellRec ScmMemHeapCell;
typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRootBlockRec ScmMemRootBlock;

#define SCM_FORWARD(obj) ((ScmForward *)(obj))

#define SCM_MEM(obj) ((ScmMem *)(obj))


#include "scythe/object.h"
#include "scythe/api_type.h"
#include "scythe/impl_utils.h"


/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};

extern ScmTypeInfo SCM_FORWARD_TYPE_INFO;

ScmObj scm_forward_forward(ScmObj obj);
void scm_forward_initialize(ScmObj obj, ScmObj fwd);


/****************************************************************************/
/* Memory Manager                                                           */
/****************************************************************************/

#define SCM_MEM_ALIGN_BYTE 8

#define SCM_MEM_MIN_OBJ_SIZE sizeof(ScmForward)
#define SCM_MEM_HEAP_INIT_BLOCK_SIZE 4096
#define SCM_MEM_OBJ_TBL_HASH_SIZE 1024
#define SCM_MEM_EXTRA_ROOT_SET_SIZE 256

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

/* ScmMem を ScmObj の一種(kind of) として定義する。                         */
/* これは object.h が ScmMem シンボルへの依存するのを避けるため。            */
/* ScmMem を GC で管理することはしない (scm_mem_alloc で生成することは不可)  */
struct ScmMemRec {
  ScmObjHeader header;
  ScmMemHeap *to_heap;
  ScmMemHeap *from_heap;
  ScmMemRootBlock *roots;
  ScmRef *extra_rfrn;
  size_t nr_extra;
  bool gc_enabled;
  size_t alloc_cnt;
};

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
ScmMem *scm_mem_clean(ScmMem *mem);
ScmObj scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_free_root(ScmMem *mem, ScmObj obj);
ScmRef scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref);
int scm_mem_gc_start(ScmMem *mem);

#endif /* INCLUDED_MEMORY_H__ */
