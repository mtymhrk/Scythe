#include <unistd.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/impl_utils.h"
#include "scythe/memory.h"

/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

ScmTypeInfo SCM_FORWARD_TYPE_INFO = {
  .name                = "forward",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmForward),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

extern inline ScmObj
scm_forward_forward(ScmObj obj)
{
  return SCM_FORWARD(obj)->forward;
}

extern inline void
scm_forward_initialize(ScmObj obj, ScmObj fwd)
{
  scm_obj_init(obj, &SCM_FORWARD_TYPE_INFO);
  SCM_FORWARD(obj)->forward = fwd;
}


/****************************************************************************/
/* Memory Manager                                                           */
/****************************************************************************/

#define SCM_MEM_EXTRA_RFRN_SIZE 32

enum { TO_HEAP, FROM_HEAP };

static ScmTypeInfo SCM_MEM_TYPE_INFO = {
  .name                = "mm",
  .flags               = 0,
  .obj_print_func      = NULL,
  .obj_size            = 0,
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};


/** alignment ***************************************************************/

static inline size_t
scm_mem_align_size_by(size_t size, size_t align)
{
  return ((size + align - 1) / align * align);
}

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

static inline size_t
scm_mem_alloc_size_in_heap(ScmTypeInfo *type, size_t add)
{
  size_t size;

  scm_assert(type != NULL);

  size = scm_type_info_obj_size(type) + add;
  if (size < sizeof(ScmMMObj))
    size = sizeof(ScmMMObj);
  if (size < SCM_MEM_MIN_OBJ_SIZE)
    size = SCM_MEM_MIN_OBJ_SIZE;

  return size;
}

static inline size_t
scm_mem_alloc_size_to_obj_size_in_heap(ScmTypeInfo *type, size_t size)
{
  scm_assert(type != NULL);

  return size;
}

static inline size_t
scm_mem_alloc_size_in_root(ScmTypeInfo *type, size_t add)
{
  size_t size;

  scm_assert(type != NULL);

  size = scm_type_info_obj_size(type) + add;
  if (size < sizeof(ScmMMObj))
    size = sizeof(ScmMMObj);
  size += sizeof(ScmMemRootBlock);

  return size;
}

/** ScmMemHeapCell **********************************************************/

static inline size_t
scm_mem_heap_cell_size(ScmMemHeapCell *cell)
{
  return cell->size;
}

static inline size_t
scm_mem_heap_cell_body_size(ScmMemHeapCell *cell)
{
  return cell->size - sizeof(ScmMemHeapCell);
}

static inline scm_byte_t *
scm_mem_heap_cell_tail(ScmMemHeapCell *cell)
{
  return (scm_byte_t *)cell + cell->size;
}


/** ScmMemHeapBlock *********************************************************/

static ScmMemHeapBlock *
scm_mem_heap_new_block(size_t sz)
{
  ScmMemHeapBlock *block = scm_capi_malloc(sizeof(ScmMemHeapBlock) + (sz));
  if (block != NULL) {
    scm_byte_t *p;
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
  scm_capi_free(block);
  return NULL;
}

static inline size_t
scm_mem_heap_block_size(ScmMemHeapBlock *block)
{
  return block->size;
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

static inline scm_byte_t *
scm_mem_heap_block_head(ScmMemHeapBlock *block)
{
  return scm_mem_align_ptr(block->heap);
}

static inline void *
scm_mem_heap_block_free_ptr(ScmMemHeapBlock *block)
{
  return (void *)(block->heap + block->used);
}

static inline void
scm_mem_heap_block_allocated(ScmMemHeapBlock *block, size_t sz)
{
  ScmMemHeapCell *p = scm_mem_heap_block_free_ptr(block);
  p->size = sizeof(ScmMemHeapCell) + sz;
  block->used += scm_mem_align_size(p->size);
}

static inline void
scm_mem_heap_block_deallocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used -= scm_mem_align_size(sizeof(ScmMemHeapCell) + sz);
}

static inline bool
scm_mem_heap_block_allocable_p(ScmMemHeapBlock *block, size_t sz)
{
  /* scm_mem_align_size で size を調整しているのは不要 ? */
  return ((scm_mem_align_size(sizeof(ScmMemHeapCell) + sz)
           <= scm_mem_heap_block_free(block)) ?
          true : false);
}

static inline bool
scm_mem_heap_block_deallocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sizeof(ScmMemHeapCell) + sz)
           <= scm_mem_heap_block_used(block)) ?
          true : false);
}

static inline size_t
scm_mem_heap_block_ptr_offset(ScmMemHeapBlock *block, void *ptr)
{
  return (size_t)((scm_byte_t *)ptr - block->heap);
}

static inline bool
scm_mem_heap_block_ptr_allocated_p(ScmMemHeapBlock *block, void *ptr)
{
  return ((scm_mem_heap_block_ptr_offset(block, ptr)
           < scm_mem_heap_block_used(block)) ?
          true : false);
}

static inline ScmMemHeapCell *
scm_mem_heap_block_next_cell(ScmMemHeapBlock *block, ScmMemHeapCell *cell)
{
  return (ScmMemHeapCell *)scm_mem_align_ptr(scm_mem_heap_cell_tail(cell));
}

static inline void
scm_mem_heap_block_clean(ScmMemHeapBlock *block)
{
  scm_byte_t *p = scm_mem_heap_block_head(block);
  block->used = (size_t)p - (size_t)(block)->heap;
}

static inline bool
scm_mem_heap_block_has_obj_p(ScmMemHeapBlock *block, ScmObj obj)
{
  return ((block->heap <= (scm_byte_t *)obj
           && (scm_byte_t *)obj < block->heap + block->used) ?
          true : false);
}


/** ScmMemHeap **************************************************************/

static inline size_t
scm_mem_heap_cur_block_free_size(ScmMemHeap *heap)
{
  return ((heap->current == NULL) ?
          0 : scm_mem_heap_block_free(heap->current));
}

static inline bool
scm_mem_heap_cur_block_tail_p(ScmMemHeap *heap)
{
  return ((heap->current == heap->tail) ? true : false);
}

static inline int
scm_mem_heap_nr_block(ScmMemHeap *heap)
{
  return heap->nr_block;
}

static inline int
scm_mem_heap_nr_free_block(ScmMemHeap *heap)
{
  return heap->nr_free_block;
}

static inline int
scm_mem_heap_nr_used_block(ScmMemHeap *heap)
{
  return heap->nr_block - heap->nr_free_block;
}

static inline size_t
scm_mem_heap_tail_block_size(ScmMemHeap *heap)
{
  return ((heap->tail == NULL) ? 0U : heap->tail->size);
}

static void
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
  heap->total_size += scm_mem_heap_block_size(block);
}

static void
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

    heap->total_size -= scm_mem_heap_block_size(b);
    scm_mem_heap_delete_block(b);
  }
}

static void
scm_mem_heap_release_blocks(ScmMemHeap *heap, int nr_leave)
{
  int n = scm_mem_heap_nr_block(heap) - nr_leave;
  for (int i = 0; i < n; i++)
    scm_mem_heap_del_block(heap);
}

static ScmMemHeap *
scm_mem_heap_delete_heap(ScmMemHeap *heap)
{
  scm_mem_heap_release_blocks(heap, 0);
  scm_capi_free(heap);
  return heap;
}

static ScmMemHeap *
scm_mem_heap_new_heap(int nr_blk, size_t sz)
{
  ScmMemHeap *heap;
  int i;

  heap = scm_capi_malloc(sizeof(*heap));
  if (heap == NULL) return NULL;

  heap->head = NULL;
  heap->tail = NULL;
  heap->current = NULL;
  heap->fin_list = SCM_OBJ_NULL;
  heap->weak_list = NULL;
  heap->nr_block = 0;
  heap->nr_free_block = 0;
  heap->total_size = 0;

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

static size_t
scm_mem_heap_total_size(ScmMemHeap *heap)
{
  return heap->total_size;
}

static void
scm_mem_heap_shift(ScmMemHeap *heap)
{
  if (heap->current != NULL) {
    heap->current = heap->current->next;
    if (heap->current != NULL)
      heap->nr_free_block--;
  }
}

static void
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

static void
scm_mem_heap_rewind(ScmMemHeap *heap)
{
  heap->current = heap->head;
  if ((heap)->nr_block > 1)
    (heap)->nr_free_block = heap->nr_block - 1;
  else
    (heap)->nr_free_block = 0;
}

static ScmMemHeapCell *
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

  return (ScmMemHeapCell *)ptr;
}

/* void scm_mem_heap_cancel_alloc(ScmMemHeap *heap, size_t size)
 *
 *   scm_mem_alloc_heap_mem_obj 関数内で、scm_mem_heap_alloc の呼出し後にエ
 *   ラーが発生した場合に scm_mem_heap_alloc で行ったヒープの割り当てを取り
 *   消す関数。今現在、エラーケースが無いため未使用
 */
static void
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


/** ScmMemRootBlock *********************************************************/

static inline ScmObj
scm_mem_root_block_object(ScmMemRootBlock *block)
{
  return SCM_OBJ(block->body);
}

static inline void
scm_mem_root_block_obj_set_shit_byte(ScmObj obj, scm_byte_t sf)
{
  *((scm_byte_t *)obj - 1) = sf;
}

static inline ScmMemRootBlock *
scm_mem_root_block_obj_header(ScmObj obj)
{
  return (ScmMemRootBlock *)((scm_byte_t *)obj
                             - offsetof(ScmMemRootBlock, body));
}

static ScmMemRootBlock *
scm_mem_root_block_new(size_t sz)
{
  ScmMemRootBlock *block;

  block = scm_capi_malloc(sz);
  if (block == NULL) return NULL;

  block->hdr.next = NULL;
  block->hdr.prev = NULL;

  return block;
}

static inline void
scm_mem_root_block_free(ScmMemRootBlock *block)
{
  scm_capi_free(block);
}

static inline bool
scm_mem_root_block_obj_in_blok_p(ScmObj obj)
{
  return (((unsigned int)(obj) > sizeof(ScmMemRootBlock)) ?
          true : false);
}


/** root object list ********************************************************/

static void
scm_mem_add_to_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block)
{
  block->hdr.next = *head;
  block->hdr.prev = NULL;
  if (*head != NULL)
    (*head)->hdr.prev = block;
  *head = block;
}

static void
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


/** relation of ScmObj and ScmMemHeapCell ***********************************/

static inline ScmObj
scm_mem_cell_to_obj(ScmMemHeapCell *cell)
{
  return SCM_OBJ(cell->body);
}

static inline ScmMemHeapCell *
scm_mem_obj_to_cell(ScmObj obj)
{
  return (ScmMemHeapCell *)((scm_byte_t *)obj - offsetof(ScmMemHeapCell, body));
}


/** list of objects has finalize function ***********************************/

static inline ScmRef
scm_mem_next_obj_has_fin_func(ScmObj obj)
{
  ScmMemHeapCell *cell = scm_mem_obj_to_cell(obj);
  return SCM_REF_MAKE(cell->fin_info.next);
}

static inline void
scm_mem_set_next_obj_has_fin_func(ScmObj obj, ScmObj prv)
{
  ScmRef r = scm_mem_next_obj_has_fin_func(obj);
  SCM_REF_UPDATE(r, prv);
}

static inline void
scm_mem_add_obj_to_fin_list(ScmMemHeap *heap, ScmObj obj)
{
  ScmObj nxt = SCM_OBJ(heap->fin_list);
  scm_mem_set_next_obj_has_fin_func(obj, nxt);
  heap->fin_list = obj;
}


/** list of objects has weak reference **************************************/

static inline ScmRef
scm_mem_next_obj_has_weak_ref(ScmObj obj)
{
  ScmMemHeapCell *cell = scm_mem_obj_to_cell(obj);
  return SCM_REF_MAKE(cell->wref_info.next);
}

static inline void
scm_mem_set_next_obj_has_weak_ref(ScmObj obj, ScmObj nxt)
{
  ScmRef r = scm_mem_next_obj_has_weak_ref(obj);
  SCM_REF_UPDATE(r, nxt);
}

static void
scm_mem_add_obj_to_weak_list(ScmMemHeap *heap, ScmObj obj)
{
  ScmObj nxt = SCM_OBJ(heap->weak_list);
  scm_mem_set_next_obj_has_weak_ref(obj, nxt);
  heap->weak_list = (void *)obj;
}


/** ScmMem ******************************************************************/

static int
scm_mem_expand_heap(ScmMem *mem, int inc_block)
{
  int i;
  size_t sz;
  ScmMemHeapBlock *to_block, *from_block;

  scm_assert(mem != NULL);
  scm_assert(inc_block >= 0);

  sz = scm_mem_heap_tail_block_size(mem->to_heap) * 2;
  if (sz == 0) sz = SCM_MEM_HEAP_INIT_BLOCK_SIZE;

  for (i = 0; i < inc_block; i++) {
    to_block = scm_mem_heap_new_block(sz);
    from_block = scm_mem_heap_new_block(sz);
    if (to_block == NULL || from_block == NULL)
      goto err;

    scm_mem_heap_add_block(mem->to_heap, to_block);
    scm_mem_heap_add_block(mem->from_heap, from_block);

    sz *= 2;
  }

  return i;

 err:
  if (to_block != NULL) scm_capi_free(to_block);
  if (from_block != NULL) scm_capi_free(from_block);
  return i;
}

static int
scm_mem_release_redundancy_heap_blocks(ScmMem *mem, int nr_margin)
{
  int nr_release;
  int nr_leave;

  scm_assert(mem != NULL);
  scm_assert(nr_margin >= 0);

  nr_release = scm_mem_heap_nr_free_block(mem->to_heap) - nr_margin;
  nr_release = (nr_release < 0) ? 0 : nr_release;
  nr_leave = scm_mem_heap_nr_block(mem->to_heap) - nr_release;

  scm_mem_heap_release_blocks(mem->to_heap, nr_leave);
  scm_mem_heap_release_blocks(mem->from_heap, nr_leave);

  return nr_leave;
}

static void
scm_mem_register_obj_on_fin_list(ScmMem *mem, ScmTypeInfo *type, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  if (scm_type_info_has_gc_fin_func_p(type))
    scm_mem_add_obj_to_fin_list(mem->to_heap, obj);
}


static void
scm_mem_register_obj_on_weak_list(ScmMem *mem, ScmTypeInfo *type, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  if (scm_type_info_has_instance_weak_ref_p(type))
    scm_mem_add_obj_to_weak_list(mem->to_heap, obj);
}

static void
scm_mem_finalize_obj(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);

  if (scm_obj_has_gc_fin_func_p(obj))
    scm_obj_call_gc_fin_func(obj);
}

static void
scm_mem_finalize_heap_obj(ScmMem *mem, int which)
{
  ScmMemHeap *heap;

  scm_assert(mem != NULL);
  scm_assert(which == TO_HEAP || which == FROM_HEAP);

  if (which == TO_HEAP)
    heap = mem->to_heap;
  else
    heap = mem->from_heap;

  for (ScmObj obj = heap->fin_list;
       scm_obj_not_null_p(obj);
       obj = SCM_REF_DEREF(scm_mem_next_obj_has_fin_func(obj)))
    if (!scm_obj_type_p(obj, &SCM_FORWARD_TYPE_INFO))
      scm_mem_finalize_obj(mem, obj);

  heap->fin_list = SCM_OBJ_NULL;
}

static void
scm_mem_clean_heap(ScmMem *mem, int which)
{
  ScmMemHeap *heap;
  ScmMemHeapBlock *block;

  scm_assert(mem != NULL);
  scm_assert(which == TO_HEAP || which == FROM_HEAP);

  scm_mem_finalize_heap_obj(mem, which);

  if (which == TO_HEAP)
    heap = mem->to_heap;
  else
    heap = mem->from_heap;

  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    scm_mem_heap_block_clean(block);
  }
  scm_mem_heap_rewind(heap);

 heap->weak_list = NULL;
}

static void
scm_mem_clean_root(ScmMem *mem)
{
  ScmMemRootBlock *block;

  scm_assert(mem != NULL);

  while ((block = mem->roots) != NULL) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_free_root(mem, obj);
  }
}


static void
scm_mem_switch_heap(ScmMem *mem)
{
  ScmMemHeap *tmp_heap;

  scm_assert(mem != NULL);

  tmp_heap = mem->from_heap;
  mem->from_heap = mem->to_heap;
  mem->to_heap = tmp_heap;
}

static bool
scm_mem_is_obj_in_heap(ScmMem *mem, ScmObj obj, int which)
{
  ScmMemHeap *heap;
  ScmMemHeapBlock *block;

  scm_assert(mem != NULL);
  scm_assert(which == TO_HEAP || which == FROM_HEAP);

  if (!scm_obj_mem_managed_p(obj))
    return false;

  if (which == TO_HEAP)
    heap = mem->to_heap;
  else
    heap = mem->from_heap;

  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    if (scm_mem_heap_block_has_obj_p(block, obj))
      return true;
  }

  return false;
}

static int
scm_mem_alloc_heap_mem_obj(ScmMem *mem, ScmTypeInfo *type, size_t size,
                           ScmRef ref)
{
  ScmObj obj = SCM_OBJ_INIT;
  ScmMemHeapCell *cell;

  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  cell = scm_mem_heap_alloc(mem->to_heap, size);
  if (cell == NULL)
    return 0;

  mem->alloc_cnt += scm_mem_heap_cell_size(cell);
  obj = scm_mem_cell_to_obj(cell);

  scm_mem_register_obj_on_fin_list(mem, type, obj);
  scm_mem_register_obj_on_weak_list(mem, type, obj);

  SCM_REF_UPDATE(ref, obj);

  return 0;
}

static int
scm_mem_alloc_heap_mem(ScmMem *mem, ScmTypeInfo *type, size_t add, ScmRef ref)
{
  scm_assert(type != NULL);

  return scm_mem_alloc_heap_mem_obj(mem, type,
                                    scm_mem_alloc_size_in_heap(type, add), ref);
}

static void
scm_mem_obj_init(ScmMem *mem, ScmObj obj, ScmTypeInfo *type)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  scm_obj_init(obj, type);
  scm_type_info_call_gc_ini_func(type, obj, SCM_OBJ(mem));
}

static ScmObj
scm_mem_copy_obj(ScmMem *mem, ScmObj obj)
{
  ScmObj box;
  ScmTypeInfo *type;
  size_t size;
  int rslt;

  scm_assert(mem != NULL);

  /* SCM_OBJ_NULL エラー戻り値として使用しているため、引数 obj が
     SCM_OBJ_NULL 値なのは受け付けない */
  scm_assert(scm_obj_not_null_p(obj));


  if (!scm_mem_is_obj_in_heap(mem, obj, FROM_HEAP))
    return obj;

  type = scm_obj_type(obj);
  if (scm_type_info_same_p(type, &SCM_FORWARD_TYPE_INFO))
    return scm_forward_forward(obj);

  size = scm_mem_heap_cell_body_size(scm_mem_obj_to_cell(obj));
  rslt = scm_mem_alloc_heap_mem_obj(mem, type, size, SCM_REF_MAKE(box));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(box)) {
    rslt = scm_mem_expand_heap(mem, 1);
    if (rslt != 1) return SCM_OBJ_NULL;

    rslt = scm_mem_alloc_heap_mem_obj(mem, type, size, SCM_REF_MAKE(box));
    if (rslt < 0) return SCM_OBJ_NULL;

    if (scm_obj_null_p(box)) {
      scm_capi_fatal("Memory Manager Heap Accesss Error");
      return SCM_OBJ_NULL;
    }
  }

  size = scm_mem_alloc_size_to_obj_size_in_heap(type, size);
  memcpy(SCM_MMOBJ(box), SCM_MMOBJ(obj), size);
  scm_forward_initialize(obj, box);

  return box;
}

static int
scm_mem_copy_children_func(ScmObj mem, ScmObj obj, ScmRef child)
{
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(scm_obj_type_p(mem, &SCM_MEM_TYPE_INFO));
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(child != SCM_REF_NULL);

  if (scm_obj_mem_managed_p(SCM_REF_DEREF(child))) {
    ScmObj cpy = scm_mem_copy_obj(SCM_MEM(mem), SCM_REF_DEREF(child));
    if (scm_obj_null_p(cpy))  return -1;
    SCM_REF_UPDATE(child, cpy);
  }

  return 0;
}

static int
scm_mem_copy_children(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);

 int rslt = scm_obj_call_gc_accept_func(obj, SCM_OBJ(mem),
                                        scm_mem_copy_children_func);
 if (scm_gc_ref_handler_failure_p(rslt))
   return -1;

 return 0;
}

static int
scm_mem_copy_children_of_root_obj(ScmMem *mem, ScmMemRootBlock *head)
{
  ScmMemRootBlock *block;

  for (block = head; block != NULL; block = block->hdr.next) {
    ScmObj obj = scm_mem_root_block_object(block);
    int rslt = scm_mem_copy_children(mem, obj);
    if (rslt < 0) return -1;
  }

  return 0;
}


static int
scm_mem_copy_children_of_root(ScmMem *mem)
{
  return scm_mem_copy_children_of_root_obj(mem, mem->roots);
}

static int
scm_mem_copy_extra_obj(ScmMem *mem)
{
  size_t i;

  scm_assert(mem != NULL);

  for (i = 0; i < mem->nr_extra; i++) {
    if (scm_obj_not_null_p(SCM_REF_DEREF(mem->extra_rfrn[i]))) {
      ScmObj cpy = scm_mem_copy_obj(mem, SCM_REF_DEREF(mem->extra_rfrn[i]));
      if (scm_obj_null_p(cpy)) return -1;
      SCM_REF_UPDATE(mem->extra_rfrn[i], cpy);
    }
  }

  return 0;
}

static int
scm_mem_copy_obj_referred_by_root(ScmMem *mem)
{
  int rslt;

  scm_assert(mem != NULL);

  rslt = scm_mem_copy_children_of_root(mem);
  if (rslt < 0) return -1;

  rslt = scm_mem_copy_extra_obj(mem);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_mem_scan_obj(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmMemHeapCell *cell;
  int rslt;

  scm_assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL(block, cell) {
      rslt = scm_mem_copy_children(mem, scm_mem_cell_to_obj(cell));
      if (rslt < 0) return -1;
    }
  }

  return 0;
}

static int
scm_mem_adjust_weak_ref_of_obj_func(ScmObj mem, ScmObj obj, ScmRef child)
{
  ScmObj co;

  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(scm_obj_type_p(mem, &SCM_MEM_TYPE_INFO));
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(child != SCM_REF_NULL);

  co = SCM_REF_DEREF(child);
  if (scm_obj_not_null_p(co)) {
    if (scm_mem_is_obj_in_heap(SCM_MEM(mem), co, FROM_HEAP)) {
      ScmTypeInfo *type = scm_obj_type(co);
      if (scm_type_info_same_p(type, &SCM_FORWARD_TYPE_INFO))
        SCM_REF_UPDATE(child, scm_forward_forward(co));
      else
        SCM_REF_UPDATE(child, SCM_OBJ_NULL);
    }
  }

  return 0;
}

static int
scm_mem_adjust_weak_ref_of_obj(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);

  int rslt =
    scm_obj_call_gc_accept_func_weak(obj, SCM_OBJ(mem),
                                     scm_mem_adjust_weak_ref_of_obj_func);
  if (scm_gc_ref_handler_failure_p(rslt))
    return -1;

  return 0;
}

static int
scm_mem_adjust_weak_ref_of_root_obj(ScmMem *mem, ScmMemRootBlock *head)
{
  ScmMemRootBlock *block;

  for (block = head; block != NULL; block = block->hdr.next) {
    ScmObj obj = scm_mem_root_block_object(block);
    int rslt = scm_mem_adjust_weak_ref_of_obj(mem, obj);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_mem_adjust_weak_ref_of_heap_obj(ScmMem *mem)
{
  ScmObj obj;
  int rslt;

  scm_assert(mem != NULL);

  for (obj = SCM_OBJ(mem->to_heap->weak_list);
       scm_obj_not_null_p(obj);
       obj = SCM_REF_DEREF(scm_mem_next_obj_has_weak_ref(obj))) {
    rslt = scm_mem_adjust_weak_ref_of_obj(mem, obj);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_mem_adjust_weak_ref(ScmMem *mem)
{
  int rslt;

  scm_assert(mem != NULL);

  rslt = scm_mem_adjust_weak_ref_of_root_obj(mem, mem->roots);
  if (rslt < 0) return -1;

  rslt = scm_mem_adjust_weak_ref_of_heap_obj(mem);
  if (rslt < 0) return -1;

  return 0;
}

static ScmObj
scm_mem_alloc_root_obj(ScmMem *mem, ScmTypeInfo *type, size_t add,
                       ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;
  ScmObj obj;
  size_t size;

  scm_assert(type != NULL);
  scm_assert(head != NULL);

  size = scm_mem_alloc_size_in_root(type, add);
  block = scm_mem_root_block_new(size);
  if (block == NULL)
    return SCM_OBJ_NULL;

  obj = scm_mem_root_block_object(block);
  scm_mem_obj_init(mem, obj, type);

  scm_mem_add_to_root_set(head, block);

  return obj;
}

static ScmObj
scm_mem_free_root_obj(ScmMem *mem, ScmObj obj, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;

  scm_assert(mem != NULL);
  scm_assert(scm_mem_root_block_obj_in_blok_p(obj));

  block = scm_mem_root_block_obj_header(obj);
  scm_mem_del_from_root_set(head, block);

  scm_mem_finalize_obj(mem, obj);

  scm_mem_root_block_free(block);

  return SCM_OBJ_NULL;
}

static void
scm_mem_free_all_obj_in_root_set(ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;

  scm_assert(mem != NULL);

  for (block = *head; block != NULL; block = *head) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_free_root_obj(mem, obj, head);
  }
}

static bool
scm_mem_need_to_exec_gc_p(ScmMem *mem)
{
  if (!scm_mem_gc_enabled_p(mem))
    return false;
  else if (mem->alloc_cnt < scm_mem_heap_total_size(mem->to_heap) / 2)
    return false;
  else
    return true;
}

static ScmMem *
scm_mem_initialize(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_obj_init(SCM_OBJ(mem), &SCM_MEM_TYPE_INFO);
  mem->to_heap = NULL;
  mem->from_heap = NULL;
  mem->roots = NULL;
  mem->extra_rfrn = NULL;
  mem->gc_enabled = true;
  mem->alloc_cnt = 0;

  mem->to_heap = scm_mem_heap_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->to_heap == NULL) goto err;

  mem->from_heap = scm_mem_heap_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->from_heap == NULL) goto err;

  mem->extra_rfrn = scm_capi_malloc(sizeof(ScmRef) * SCM_MEM_EXTRA_RFRN_SIZE);
  if (mem->extra_rfrn == NULL) goto err;
  mem->nr_extra = 0;

  return mem;

 err:
  if (mem->to_heap != NULL) mem->to_heap = scm_mem_heap_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) mem->from_heap = scm_mem_heap_delete_heap(mem->from_heap);
  if (mem->extra_rfrn != NULL) scm_capi_free(mem->extra_rfrn);

  scm_capi_fatal("Memory Manager Initialization Error");

  return NULL;
}

static ScmMem *
scm_mem_finalize(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_mem_free_all_obj_in_root_set(mem, &mem->roots);
  scm_mem_clean(mem);

  if (mem->to_heap != NULL) mem->to_heap = scm_mem_heap_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) mem->from_heap = scm_mem_heap_delete_heap(mem->from_heap);
  if (mem->extra_rfrn != NULL) scm_capi_free(mem->extra_rfrn);

  return NULL;
}


ScmMem *
scm_mem_new(void)
{
  ScmMem *mem = NULL;

  mem = scm_capi_malloc(sizeof(*mem));
  if (mem == NULL) return NULL;

  return scm_mem_initialize(mem);
}

ScmMem *
scm_mem_end(ScmMem *mem)
{
  if (mem == NULL) return NULL;

  scm_mem_finalize(mem);
  scm_capi_free(mem);

  return NULL;
}

ScmMem *
scm_mem_clean(ScmMem *mem)
{
  scm_mem_clean_root(mem);
  scm_mem_clean_heap(mem, TO_HEAP);
  scm_mem_clean_heap(mem, FROM_HEAP);
  return mem;
}


ScmObj
scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, size_t add_size)
{
  ScmObj obj = SCM_OBJ_INIT;
  int rslt;

  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  obj = SCM_OBJ_INIT;

  rslt = scm_mem_alloc_heap_mem(mem, type, add_size, SCM_REF_MAKE(obj));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(obj)) goto success;

  if (scm_mem_need_to_exec_gc_p(mem)) {
    rslt = scm_mem_gc_start(mem);
    if (rslt < 0) return SCM_OBJ_NULL;
  }
  else {
    rslt = scm_mem_expand_heap(mem, 1);
    if (rslt != 1) return SCM_OBJ_NULL;
  }

  rslt = scm_mem_alloc_heap_mem(mem, type, add_size, SCM_REF_MAKE(obj));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(obj)) goto success;

  scm_capi_fatal("Memory Manager Memory Allocation Error");
  return SCM_OBJ_NULL;

 success:
  scm_mem_obj_init(mem, obj, type);

  return obj;
}

ScmObj
scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, size_t add_size)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  return scm_mem_alloc_root_obj(mem, type, add_size, &mem->roots);
}

ScmObj
scm_mem_free_root(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(scm_mem_root_block_obj_in_blok_p(obj));

  return scm_mem_free_root_obj(mem, obj, &mem->roots);
}

ScmRef
scm_mem_register_extra_rfrn(ScmMem *mem, ScmRef ref)
{
  scm_assert(mem != NULL);
  scm_assert(ref != SCM_REF_NULL);

  if (mem->nr_extra >= SCM_MEM_EXTRA_RFRN_SIZE)
    return SCM_REF_NULL;

  mem->extra_rfrn[mem->nr_extra++] = ref;

  return ref;
}

ScmObj
scm_mem_alloc(ScmMem *mem, ScmTypeInfo *type, size_t add_size,
              SCM_MEM_ALLOC_TYPE_T alloc)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  /* scm_assert(alloc <  SCM_MEM_NR_ALLOC_TYPE); */

  switch(alloc) {
  case SCM_MEM_ALLOC_HEAP:
    return scm_mem_alloc_heap(mem, type, add_size);
    break;
  case SCM_MEM_ALLOC_ROOT:
    return scm_mem_alloc_root(mem, type, add_size);
    break;
  default:
    scm_assert(true);
    return SCM_OBJ_NULL;
  }

  return SCM_OBJ_NULL;
}

int
scm_mem_gc_start(ScmMem *mem)
{
  int nr_free;
  int rslt;

  scm_assert(mem != NULL);

  scm_mem_switch_heap(mem);

  rslt = scm_mem_copy_obj_referred_by_root(mem);
  if (rslt < 0) return -1;

  rslt = scm_mem_scan_obj(mem);
  if (rslt < 0) return -1;

  rslt = scm_mem_adjust_weak_ref(mem);
  if (rslt < 0) return -1;

  scm_mem_clean_heap(mem, FROM_HEAP);

  nr_free = scm_mem_heap_nr_free_block(mem->to_heap);
  if (nr_free > 1)
    scm_mem_release_redundancy_heap_blocks(mem, 1);

  mem->alloc_cnt = 0;

  return 0;
}
