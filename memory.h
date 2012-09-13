#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct ScmMemHeapCellRec ScmMemHeapCell;
typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRootBlockRec ScmMemRootBlock;
typedef struct ScmMemRec ScmMem;

#define SCM_FORWARD(obj) ((ScmForward *)(obj))

#define SCM_MEM(obj) ((ScmMem *)(obj))

typedef enum {
  SCM_MEM_ALLOC_HEAP,
  SCM_MEM_ALLOC_ROOT,
} SCM_MEM_ALLOC_TYPE_T;

enum { SCM_MEM_NR_ALLOC_TYPE = SCM_MEM_ALLOC_ROOT + 1 };

#include "object.h"
#include "api_type.h"
#include "impl_utils.h"


/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

/* ScmForward オブジェクトの構造体定義を api_enum へ移動。ScmEFBox オブジェク
 * トが ScmForward オブジェクトのサイズを必要とするため。
 */

/* struct ScmForwardRec { */
/*   ScmObjHeader header; */
/*   ScmObj forward; */
/* }; */

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

struct ScmMemHeapCellRec {
  size_t size;
  uint8_t body[0] __attribute((aligned(SCM_MEM_ALIGN_BYTE)));
};

struct ScmMemHeapBlockRec {
  struct ScmMemHeapBlockRec *next;
  struct ScmMemHeapBlockRec *prev;
  size_t size;
  size_t used;
  uint8_t heap[0];
};

struct ScmMemHeapRec {
  ScmMemHeapBlock *head;
  ScmMemHeapBlock *tail;
  ScmMemHeapBlock *current;
  ScmObj fin_list;
  void *weak_list;
  int nr_block;
  int nr_free_block;
};

typedef struct ScmMemRootBlockHdrRec {
  ScmMemRootBlock *next;
  ScmMemRootBlock *prev;
} ScmMemRootBlockHdr;

struct ScmMemRootBlockRec {
  ScmMemRootBlockHdr hdr;
  uint8_t object[SCM_MEM_ALIGN_BYTE];
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
};

#define SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL(block, cell)                 \
  for ((cell) = (ScmMemHeapCell *)scm_mem_heap_block_head(block);     \
       scm_mem_heap_block_ptr_allocated_p(block, (cell));             \
       (cell) = scm_mem_heap_block_next_cell(block, (cell)))

#define SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) \
  for ((block) = (heap)->head;                   \
       (block) != NULL;                          \
       (block) = (block)->next)


/** private functions *****************************************************/

#ifdef SCM_UNIT_TEST

size_t scm_mem_align_size(size_t size);
void *scm_mem_align_ptr(void *ptr);
size_t scm_mem_alloc_size_of_obj_has_fin_func(size_t size);
size_t scm_mem_alloc_size_of_obj_has_weak_ref(size_t size);
size_t scm_mem_alloc_size_in_heap(ScmTypeInfo *type, size_t add);
size_t scm_mem_alloc_size_to_obj_size_in_heap(ScmTypeInfo *type, size_t size);
size_t scm_mem_alloc_size_in_root(ScmTypeInfo *type, size_t add);

ScmMemHeapBlock *scm_mem_heap_new_block(size_t sz);
void *scm_mem_heap_delete_block(ScmMemHeapBlock *block);
size_t scm_mem_heap_block_used(ScmMemHeapBlock *block);
size_t scm_mem_heap_block_free(ScmMemHeapBlock *block);
uint8_t *scm_mem_heap_block_head(ScmMemHeapBlock *block);
void scm_mem_heap_block_allocated(ScmMemHeapBlock *block, size_t sz);
void scm_mem_heap_block_deallocated(ScmMemHeapBlock *block, size_t sz);
bool scm_mem_heap_block_allocable_p(ScmMemHeapBlock *block, size_t sz);
bool scm_mem_heap_block_deallocable_p(ScmMemHeapBlock *block, size_t sz);
void *scm_mem_heap_block_free_ptr(ScmMemHeapBlock *block);
size_t scm_mem_heap_block_ptr_offset(ScmMemHeapBlock *block, void *ptr);
bool scm_mem_heap_block_ptr_allocated_p(ScmMemHeapBlock *block, void *ptr);
ScmMemHeapCell * scm_mem_heap_block_next_cell(ScmMemHeapBlock *block,
                                              ScmMemHeapCell *cell);
void scm_mem_heap_block_clean(ScmMemHeapBlock *block);
bool scm_mem_heap_block_has_obj_p(ScmMemHeapBlock *block, ScmObj obj);

size_t scm_mem_heap_cur_block_free_size(ScmMemHeap *heap);
bool scm_mem_heap_cur_block_tail_p(ScmMemHeap *heap);
int scm_mem_heap_nr_block(ScmMemHeap *heap);
int scm_mem_heap_nr_free_block(ScmMemHeap *heap);
int scm_mem_heap_nr_used_block(ScmMemHeap *heap);
size_t scm_mem_heap_tail_block_size(ScmMemHeap *heap);
void scm_mem_heap_add_block(ScmMemHeap *heap, ScmMemHeapBlock *block);
void scm_mem_heap_del_block(ScmMemHeap *heap);
void scm_mem_heap_release_blocks(ScmMemHeap *heap, int nr_leave);
ScmMemHeap *scm_mem_heap_delete_heap(ScmMemHeap *heap);
ScmMemHeap *scm_mem_heap_new_heap(int nr_blk, size_t sz);
void scm_mem_heap_shift(ScmMemHeap *heap);
void scm_mem_heap_unshift(ScmMemHeap *heap);
void scm_mem_heap_rewind(ScmMemHeap *heap);
ScmMemHeapCell *scm_mem_heap_alloc(ScmMemHeap *heap, size_t size);
void scm_mem_heap_cancel_alloc(ScmMemHeap *heap, size_t size);

uint8_t scm_mem_root_block_shift_byte(ScmMemRootBlock *block);
ScmObj scm_mem_root_block_object(ScmMemRootBlock *block);
uint8_t scm_mem_root_block_obj_shift_byte(ScmObj obj);
void scm_mem_root_block_obj_set_shit_byte(ScmObj obj, uint8_t sf);
ScmMemRootBlock *scm_mem_root_block_obj_header(ScmObj obj);
ScmMemRootBlock *scm_mem_root_block_new(size_t sz);
void scm_mem_root_block_free(ScmMemRootBlock *block);
bool scm_mem_root_block_obj_in_blok_p(ScmObj obj);

void scm_mem_add_to_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block);
void scm_mem_del_from_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block);

ScmObj scm_mem_cell_to_obj(ScmMemHeapCell *cell);
ScmMemHeapCell *scm_mem_obj_to_cell(ScmObj obj);

ScmRef scm_mem_prev_obj_has_fin_func(ScmTypeInfo *type, ScmObj obj);
ScmRef scm_mem_next_obj_has_fin_func(ScmTypeInfo *type, ScmObj obj);
void scm_mem_set_prev_obj_has_fin_func(ScmTypeInfo *type, ScmObj obj,
                                       ScmObj prv);
void scm_mem_set_next_obj_has_fin_func(ScmTypeInfo *type, ScmObj obj,
                                       ScmObj prv);
void scm_mem_add_obj_to_fin_list(ScmMemHeap *heap, ScmObj obj,
                                 ScmTypeInfo *type);
void scm_mem_del_obj_from_fin_list(ScmMemHeap *heap, ScmObj obj);

ScmRef scm_mem_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj);
void scm_mem_set_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj,
                                       ScmObj nxt);
void scm_mem_add_obj_to_weak_list(ScmMemHeap *heap, ScmObj obj,
                                  ScmTypeInfo *type);

int scm_mem_expand_heap(ScmMem *mem, int inc_block);
int scm_mem_release_redundancy_heap_blocks(ScmMem *mem, int nr_margin);
void scm_mem_register_obj_on_fin_list(ScmMem *mem,
                                      ScmTypeInfo *type, ScmObj obj);
void scm_mem_register_obj_on_weak_list(ScmMem *mem,
                                       ScmTypeInfo *type, ScmObj obj);
void scm_mem_unregister_obj_from_fin_list(ScmMem *mem, ScmObj obj);
void scm_mem_finalize_obj(ScmMem *mem, ScmObj obj);
void scm_mem_finalize_heap_obj(ScmMem *mem, int which);
void scm_mem_clean_heap(ScmMem *mem, int which);
void scm_mem_clean_root(ScmMem *mem);
void scm_mem_switch_heap(ScmMem *mem);
bool scm_mem_is_obj_in_heap(ScmMem *mem, ScmObj obj, int which);
int scm_mem_alloc_heap_mem_obj(ScmMem *mem, ScmTypeInfo *type, size_t size,
                               ScmRef ref);
int scm_mem_alloc_heap_mem(ScmMem *mem, ScmTypeInfo *type, size_t add,
                           ScmRef ref);
void scm_mem_obj_init(ScmMem *mem, ScmObj obj, ScmTypeInfo *type);
ScmObj scm_mem_copy_obj(ScmMem *mem, ScmObj obj);
int scm_mem_copy_children_func(ScmObj mem, ScmObj obj, ScmRef child);
int scm_mem_copy_children(ScmMem *mem, ScmObj obj);
int scm_mem_copy_children_of_root_obj(ScmMem *mem, ScmMemRootBlock *head);
int scm_mem_copy_children_of_root(ScmMem *mem);
int scm_mem_copy_extra_obj(ScmMem *mem);
int scm_mem_copy_obj_referred_by_root(ScmMem *mem);
int scm_mem_scan_obj(ScmMem *mem);
int scm_mem_adjust_weak_ref_of_obj_func(ScmObj mem, ScmObj obj, ScmRef child);
int scm_mem_adjust_weak_ref_of_obj(ScmMem *mem, ScmObj obj);
int scm_mem_adjust_weak_ref_of_root_obj(ScmMem *mem, ScmMemRootBlock *head);
int scm_mem_adjust_weak_ref_of_heap_obj(ScmMem *mem);
int scm_mem_adjust_weak_ref(ScmMem *mem);
ScmObj scm_mem_alloc_root_obj(ScmMem *mem, ScmTypeInfo *type, size_t add,
                              ScmMemRootBlock **head);
ScmObj scm_mem_free_root_obj(ScmMem *mem, ScmObj obj, ScmMemRootBlock **head);
void scm_mem_free_all_obj_in_root_set(ScmMem *mem, ScmMemRootBlock **head);
ScmMem *scm_mem_initialize(ScmMem *mem);
ScmMem *scm_mem_finalize(ScmMem *mem);

#endif

/** public functions *****************************************************/

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
ScmObj scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, size_t add_size);
ScmObj scm_mem_free_root(ScmMem *mem, ScmObj obj);
ScmRef scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref);
ScmObj scm_mem_alloc(ScmMem *mem, ScmTypeInfo *type, size_t add_size,
                     SCM_MEM_ALLOC_TYPE_T alloc);
int scm_mem_gc_start(ScmMem *mem);

#endif /* INCLUDED_MEMORY_H__ */
