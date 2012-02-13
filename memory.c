#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "basichash.h"
#include "vm.h"
#include "memory.h"
#include "object.h"

/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

ScmTypeInfo SCM_FORWARD_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmForward),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
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
  .pp_func             = NULL,
  .obj_size            = 0,
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};

static ScmMemRootBlock *shared_roots = NULL;


static size_t
object_table_hash_func(ScmBasicHashKey key)
{
  return (size_t)key;
}

static bool
object_table_comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2) ? true : false;
}


/** private functions  for alignment *****************************************/

extern inline size_t
scm_mem_align_size(size_t size)
{
  return ((size + SCM_MEM_ALIGN_BYTE - 1)
          / SCM_MEM_ALIGN_BYTE
          * SCM_MEM_ALIGN_BYTE);
}

extern inline void *
scm_mem_align_ptr(void *ptr)
{
  return (void *)scm_mem_align_size((uintptr_t)ptr);
}

extern inline size_t
scm_mem_alloc_size_in_heap(ScmTypeInfo *type)
{
  size_t size;

  scm_assert(type != NULL);

  size = scm_type_info_obj_size(type);
  if (size < sizeof(ScmMMObj))
    size = sizeof(ScmMMObj);
  if (scm_type_info_has_instance_weak_ref_p(type))
    size = scm_mem_size_of_obj_has_weak_ref(size);
  if (size < SCM_MEM_MIN_OBJ_SIZE)
    size = SCM_MEM_MIN_OBJ_SIZE;

  return size;
}

extern inline size_t
scm_mem_alloc_size_in_heap_aligned(ScmTypeInfo *type)
{
  size_t size = scm_mem_alloc_size_in_heap(type);
  return scm_mem_align_size(size);
}

extern inline size_t
scm_mem_alloc_size_in_root(ScmTypeInfo *type)
{
  size_t size;

  scm_assert(type != NULL);

  size = scm_type_info_obj_size(type);
  if (size < sizeof(ScmMMObj))
    size = sizeof(ScmMMObj);
  size += sizeof(ScmMemRootBlock);

  return size;
}


/** private functions for ScmMemHeapBlock ************************************/

ScmMemHeapBlock *
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

extern inline void *
scm_mem_heap_delete_block(ScmMemHeapBlock *block)
{
  free(block);
  return NULL;
}

extern inline size_t
scm_mem_heap_block_used(ScmMemHeapBlock *block)
{
  return block->used;
}

extern inline size_t
scm_mem_heap_block_free(ScmMemHeapBlock *block)
{
  return block->size - block->used;
}

extern inline uint8_t *
scm_mem_heap_block_head(ScmMemHeapBlock *block)
{
  return (uint8_t *)block->heap;
}

extern inline void
scm_mem_heap_block_allocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used += scm_mem_align_size(sz);
}

extern inline void
scm_mem_heap_block_deallocated(ScmMemHeapBlock *block, size_t sz)
{
  block->used -= scm_mem_align_size(sz);
}

extern inline bool
scm_mem_heap_block_allocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_free(block)) ?
          true : false);
}

extern inline bool
scm_mem_heap_block_deallocable_p(ScmMemHeapBlock *block, size_t sz)
{
  return ((scm_mem_align_size(sz) <= scm_mem_heap_block_used(block)) ?
          true : false);
}

extern inline void *
scm_mem_heap_block_free_ptr(ScmMemHeapBlock *block)
{
  return (void *)(block->heap + block->used);
}

extern inline size_t
scm_mem_heap_block_ptr_offset(ScmMemHeapBlock *block, void *ptr)
{
  return (size_t)((uint8_t *)ptr - block->heap);
}

extern inline bool
scm_mem_heap_block_ptr_allocated_p(ScmMemHeapBlock *block, void *ptr)
{
  return ((scm_mem_heap_block_ptr_offset(block, ptr)
           < scm_mem_heap_block_used(block)) ?
          true : false);
}

extern inline ScmObj
scm_mem_heap_block_next_obj(ScmMemHeapBlock *block, ScmObj obj)
{
  return SCM_OBJ((uint8_t *)obj
                 + scm_mem_alloc_size_in_heap_aligned(scm_obj_type(obj)));
}

extern inline void
scm_mem_heap_block_clean(ScmMemHeapBlock *block)
{
  uint8_t *p = scm_mem_heap_block_head(block);
  block->used = (size_t)p - (size_t)(block)->heap;
}

extern inline bool
scm_mem_heap_block_has_obj_p(ScmMemHeapBlock *block, ScmObj obj)
{
  return ((block->heap <= (uint8_t *)obj
           && (uint8_t *)obj < block->heap + block->used) ?
          true : false);
}


/** private functions for ScmMemHeap  ****************************************/

extern inline size_t
scm_mem_heap_cur_block_free_size(ScmMemHeap *heap)
{
  return ((heap->current == NULL) ?
          0 : scm_mem_heap_block_free(heap->current));
}

extern inline bool
scm_mem_heap_cur_block_tail_p(ScmMemHeap *heap)
{
  return ((heap->current == heap->tail) ? true : false);
}

extern inline int
scm_mem_heap_nr_block(ScmMemHeap *heap)
{
  return heap->nr_block;
}

extern inline int
scm_mem_heap_nr_free_block(ScmMemHeap *heap)
{
  return heap->nr_free_block;
}

extern inline int
scm_mem_heap_nr_used_block(ScmMemHeap *heap)
{
  return heap->nr_block - heap->nr_free_block;
}

extern inline size_t
scm_mem_heap_tail_block_size(ScmMemHeap *heap)
{
  return ((heap->tail == NULL) ? 0U : heap->tail->size);
}

void
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

void
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

void
scm_mem_heap_release_blocks(ScmMemHeap *heap, int nr_leave)
{
  int n = scm_mem_heap_nr_block(heap) - nr_leave;
  for (int i = 0; i < n; i++)
    scm_mem_heap_del_block(heap);
}

ScmMemHeap *
scm_mem_heap_delete_heap(ScmMemHeap *heap)
{
  scm_mem_heap_release_blocks(heap, 0);
  free(heap);
  return heap;
}

ScmMemHeap *
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

void
scm_mem_heap_shift(ScmMemHeap *heap)
{
  if (heap->current != NULL) {
    heap->current = heap->current->next;
    if (heap->current != NULL)
      heap->nr_free_block--;
  }
}

void
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

void
scm_mem_heap_rewind(ScmMemHeap *heap)
{
  heap->current = heap->head;
  if ((heap)->nr_block > 1)
    (heap)->nr_free_block = heap->nr_block - 1;
  else
    (heap)->nr_free_block = 0;
}

void *
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

void
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


/** private functions for ScmMemRootBlock ************************************/

extern inline uint8_t
scm_mem_root_block_shift_byte(ScmMemRootBlock *block)
{
  return (uint8_t)(SCM_MEM_ALIGN_BYTE
                   - (uintptr_t)(block)->object % SCM_MEM_ALIGN_BYTE);

}

extern inline ScmObj
scm_mem_root_block_object(ScmMemRootBlock *block)
{
  return SCM_OBJ(block->object + scm_mem_root_block_shift_byte(block));
}

extern inline uint8_t
scm_mem_root_block_obj_shift_byte(ScmObj obj)
{
  return *((uint8_t *)obj - 1);
}

extern inline void
scm_mem_root_block_obj_set_shit_byte(ScmObj obj, uint8_t sf)
{
  *((uint8_t *)obj - 1) = sf;
}

extern inline ScmMemRootBlock *
scm_mem_root_block_obj_header(ScmObj obj)
{
  return (ScmMemRootBlock *)((uint8_t *)obj
                             - scm_mem_root_block_obj_shift_byte(obj)
                             - sizeof(ScmMemRootBlockHdr));
}

ScmMemRootBlock *
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

extern inline void
scm_mem_root_block_free(ScmMemRootBlock *block)
{
  free(block);
}

extern inline bool
scm_mem_root_block_obj_in_blok_p(ScmObj obj)
{
  return (((unsigned int)(obj) > sizeof(ScmMemRootBlock)) ?
          true : false);
}


/** private functions for root object list ***********************************/

void
scm_mem_add_to_root_set(ScmMemRootBlock **head, ScmMemRootBlock *block)
{
  block->hdr.next = *head;
  block->hdr.prev = NULL;
  if (*head != NULL)
    (*head)->hdr.prev = block;
  *head = block;
}

void
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


/** private functions for list of objects hss weak reference *****************/

extern inline ScmRef
scm_mem_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj)
{
  return ((ScmRef)((uintptr_t)obj + scm_type_info_obj_size(type)));
}

extern inline size_t
scm_mem_size_of_obj_has_weak_ref(size_t size)
{
  return size + sizeof(ScmObj);
}

extern inline void
scm_mem_set_next_obj_has_weak_ref(ScmTypeInfo *type, ScmObj obj, ScmObj nxt)
{
  ScmRef r = scm_mem_next_obj_has_weak_ref(type, obj);
  SCM_REF_UPDATE(r, nxt);
}

void
scm_mem_add_obj_to_weak_list(ScmMemHeap *heap, ScmRef ref, ScmTypeInfo *type)
{
  ScmObj obj = SCM_REF_DEREF(ref);
  ScmObj nxt = SCM_OBJ(heap->weak_list);
  scm_mem_set_next_obj_has_weak_ref(type, obj, nxt);
  heap->weak_list = (void *)obj;
}


/** private functions for ScmMem *********************************************/

int
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
  if (to_block != NULL) free(to_block);
  if (from_block != NULL) free(from_block);
  return i;
}

int
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

int
scm_mem_expand_persistent(ScmMem *mem, int inc_block)
{
  int i;
  ScmMemHeapBlock *block;

  scm_assert(mem != NULL);
  scm_assert(inc_block >= 0);

  for (i = 0; i < inc_block; i++) {
    block = scm_mem_heap_new_block(SCM_MEM_HEAP_INIT_BLOCK_SIZE);
    if (block == NULL) return i;
    scm_mem_heap_add_block(mem->persistent, block);
  }

  return i;
}

int
scm_mem_register_obj_if_needed(ScmMem *mem, ScmTypeInfo *type, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);

  if (scm_type_info_has_gc_fin_func_p(type)) {
    ScmBasicHashEntry *e;
    e = scm_basic_hash_put(mem->to_obj_tbl,
                           SCM_BASIC_HASH_KEY(obj),
                           SCM_BASIC_HASH_VALUE(NULL));
    if (e == NULL) {
      return -1;
    }
  }

  return 0;
}

void
scm_mem_unregister_obj(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);

  if (scm_obj_has_gc_fin_func_p(obj))
    scm_basic_hash_delete(mem->from_obj_tbl, SCM_BASIC_HASH_KEY(obj));
}

void
scm_mem_finalize_obj(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));;

  if (scm_obj_has_gc_fin_func_p(obj))
    scm_obj_call_gc_fin_func(obj);
}

void
scm_mem_finalize_heap_obj(ScmMem *mem, int which)
{
  ScmBasicHashItr itr;
  ScmBasicHashTable *tbl;

  scm_assert(mem != NULL);
  scm_assert(which == TO_HEAP || which == FROM_HEAP);

  if (which == TO_HEAP)
    tbl = mem->to_obj_tbl;
  else
    tbl = mem->from_obj_tbl;

  for (SCM_BASIC_HASH_ITR_BEGIN(tbl, itr);
       !SCM_BASIC_HASH_ITR_IS_END(itr);
       SCM_BASIC_HASH_ITR_NEXT(itr)) {
    scm_mem_finalize_obj(mem, SCM_OBJ(SCM_BASIC_HASH_ITR_KEY(itr)));
  }

  scm_basic_hash_clear(tbl);
}

void
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

void
scm_mem_clean_root(ScmMem *mem)
{
  ScmMemRootBlock *block;

  scm_assert(mem != NULL);

  while ((block = mem->roots) != NULL) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_free_root(mem, obj);
  }
}


void
scm_mem_clean_persistent(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  scm_assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_finalize_obj(mem, obj);
    }
    scm_mem_heap_block_clean(block);
  }
  scm_mem_heap_rewind(mem->persistent);

  scm_assert(mem != NULL);
}

void
scm_mem_switch_heap(ScmMem *mem)
{
  ScmBasicHashTable *tmp_tbl;
  ScmMemHeap *tmp_heap;

  scm_assert(mem != NULL);

  tmp_tbl = mem->from_obj_tbl;
  mem->from_obj_tbl = mem->to_obj_tbl;
  mem->to_obj_tbl = tmp_tbl;

  tmp_heap = mem->from_heap;
  mem->from_heap = mem->to_heap;
  mem->to_heap = tmp_heap;
}

bool
scm_mem_is_obj_in_heap(ScmMem *mem, ScmObj obj, int which)
{
  ScmMemHeap *heap;
  ScmMemHeapBlock *block;

  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(which == TO_HEAP || which == FROM_HEAP);

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

void
scm_mem_alloc_heap_mem(ScmMem *mem, ScmTypeInfo *type, ScmRef ref)
{
  size_t size;
  void *ptr;

  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  scm_assert(ref != SCM_REF_NULL);

  size = scm_mem_alloc_size_in_heap(type);
  ptr = scm_mem_heap_alloc(mem->to_heap, size);
  if (ptr == NULL) {
    SCM_REF_UPDATE(ref, SCM_OBJ_NULL);
    return;
  }
  SCM_REF_UPDATE(ref, ptr);

  if (scm_mem_register_obj_if_needed(mem, type, SCM_REF_DEREF(ref)) < 0) {
    scm_mem_heap_cancel_alloc(mem->to_heap, size);
    SCM_REF_UPDATE(ref, SCM_OBJ_NULL);
    return;
  }

  if (scm_type_info_has_instance_weak_ref_p(type))
    scm_mem_add_obj_to_weak_list(mem->to_heap, ref, type);
}

void
scm_mem_obj_init(ScmMem *mem, ScmObj obj, ScmTypeInfo *type)
{
  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(type != NULL);

  scm_obj_init(obj, type);
  scm_type_info_call_gc_ini_func(type, obj, SCM_OBJ(mem));
}

ScmObj
scm_mem_copy_obj(ScmMem *mem, ScmObj obj)
{
  ScmObj box;
  ScmTypeInfo *type;

  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));


  if (!scm_mem_is_obj_in_heap(mem, obj, FROM_HEAP))
    return obj;

  type = scm_obj_type(obj);
  if (scm_type_info_same_p(type, &SCM_FORWARD_TYPE_INFO))
    return scm_forward_forward(obj);

  scm_mem_alloc_heap_mem(mem, type, SCM_REF_MAKE(box));
  if (scm_obj_null_p(box)) {
    if (scm_mem_expand_heap(mem, 1) != 1) {
      /* TODO: write error handling (fail to allocate memory)*/
      return SCM_OBJ_NULL;
    }
    scm_mem_alloc_heap_mem(mem, type, SCM_REF_MAKE(box));
    if (scm_obj_null_p(box)) {
      /* TODO: write error handling (fail to allocate memory)*/
      return SCM_OBJ_NULL;
    }
  }
  memcpy(SCM_MMOBJ(box), SCM_MMOBJ(obj), scm_type_info_obj_size(type));
  scm_mem_unregister_obj(mem, obj);
  scm_forward_initialize(obj, box);

  return box;
}

int
scm_mem_copy_children_func(ScmObj mem, ScmObj obj, ScmRef child)
{
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(scm_obj_type_p(mem, &SCM_MEM_TYPE_INFO));
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(child != SCM_REF_NULL);

  if (scm_obj_not_null_p(SCM_REF_DEREF(child))
      && scm_obj_mem_managed_p(SCM_REF_DEREF(child))) {
    ScmObj cpy = scm_mem_copy_obj(SCM_MEM(mem), SCM_REF_DEREF(child));
    if (scm_obj_null_p(cpy))  return -1; // error
    SCM_REF_UPDATE(child, cpy);
  }

  return 0;
}

void
scm_mem_copy_children(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));

 int rslt = scm_obj_call_gc_accept_func(obj, SCM_OBJ(mem),
                                        scm_mem_copy_children_func);
 if (scm_gc_ref_handler_failure_p(rslt)) {
   ; /* TODO: write error handling */
  }
}

void
scm_mem_copy_children_of_persistent(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  scm_assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->persistent, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_copy_children(mem, obj);
    }
  }
}

void
scm_mem_copy_children_of_root_obj(ScmMem *mem, ScmMemRootBlock *head)
{
  ScmMemRootBlock *block;

  for (block = head; block != NULL; block = block->hdr.next) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_copy_children(mem, obj);
  }
}

void
scm_mem_copy_children_of_shared_root(ScmMem *mem)
{
  scm_mem_copy_children_of_root_obj(mem, shared_roots);
}

void
scm_mem_copy_children_of_root(ScmMem *mem)
{
  scm_mem_copy_children_of_root_obj(mem, mem->roots);
}

void
scm_mem_copy_extra_obj(ScmMem *mem)
{
  size_t i;

  scm_assert(mem != NULL);

  for (i = 0; i < mem->nr_extra; i++) {
    if (scm_obj_not_null_p(SCM_REF_DEREF(mem->extra_rfrn[i]))) {
      ScmObj cpy = scm_mem_copy_obj(mem, SCM_REF_DEREF(mem->extra_rfrn[i]));
      if (scm_obj_null_p(cpy)) {
        ; /* TODO: wirte error handling */
      }
      SCM_REF_UPDATE(mem->extra_rfrn[i], cpy);
    }
  }
}

void
scm_mem_copy_obj_referred_by_root(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_mem_copy_children_of_shared_root(mem);
  scm_mem_copy_children_of_root(mem);
  scm_mem_copy_extra_obj(mem);
  scm_mem_copy_children_of_persistent(mem);
}

void
scm_mem_scan_obj(ScmMem *mem)
{
  ScmMemHeapBlock *block;
  ScmObj obj;

  scm_assert(mem != NULL);

  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
      scm_mem_copy_children(mem, obj);
    }
  }
}

int
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

void
scm_mem_adjust_weak_ref_of_obj(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(scm_obj_not_null_p(obj));

  int rslt =
    scm_obj_call_gc_accept_func_weak(obj, SCM_OBJ(mem),
                                     scm_mem_adjust_weak_ref_of_obj_func);
  if (scm_gc_ref_handler_failure_p(rslt)) {
    ; /* TODO: write error handling */
  }
}

void
scm_mem_adjust_weak_ref_of_root_obj(ScmMem *mem, ScmMemRootBlock *head)
{
  ScmMemRootBlock *block;

  for (block = head; block != NULL; block = block->hdr.next) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_adjust_weak_ref_of_obj(mem, obj);
  }
}

void
scm_mem_adjust_weak_ref_of_heap_obj(ScmMem *mem)
{
  ScmObj obj;
  scm_assert(mem != NULL);

  for (obj = SCM_OBJ(mem->to_heap->weak_list);
       scm_obj_not_null_p(obj);
       obj = SCM_REF_DEREF(scm_mem_next_obj_has_weak_ref(scm_obj_type(obj), obj))) {
    scm_mem_adjust_weak_ref_of_obj(mem, obj);
  }
}

void
scm_mem_adjust_weak_ref(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_mem_adjust_weak_ref_of_root_obj(mem, shared_roots);
  scm_mem_adjust_weak_ref_of_root_obj(mem, mem->roots);
  scm_mem_adjust_weak_ref_of_heap_obj(mem);
}

ScmObj
scm_mem_alloc_root_obj(ScmTypeInfo *type, ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;
  ScmObj obj;
  size_t size;

  scm_assert(type != NULL);
  scm_assert(head != NULL);

  size = scm_mem_alloc_size_in_root(type);
  block = scm_mem_root_block_new(size);
  if (block == NULL)
    return SCM_OBJ_NULL;

  obj = scm_mem_root_block_object(block);
  scm_mem_obj_init(mem, obj, type);

  scm_mem_add_to_root_set(head, block);

  return obj;
}

ScmObj
scm_mem_free_root_obj(ScmObj obj, ScmMem *mem, ScmMemRootBlock **head)
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

void
scm_mem_free_all_obj_in_root_set(ScmMem *mem, ScmMemRootBlock **head)
{
  ScmMemRootBlock *block;

  scm_assert(mem != NULL);

  for (block = *head; block != NULL; block = *head) {
    ScmObj obj = scm_mem_root_block_object(block);
    scm_mem_free_root_obj(obj, mem, head);
  }
}

ScmMem *
scm_mem_initialize(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_obj_init(SCM_OBJ(mem), &SCM_MEM_TYPE_INFO);
  mem->to_obj_tbl = NULL;
  mem->from_obj_tbl = NULL;
  mem->to_heap = NULL;
  mem->from_heap = NULL;
  mem->roots = NULL;
  mem->extra_rfrn = NULL;
  mem->gc_enabled = true;

  mem->to_obj_tbl = scm_basic_hash_new(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                             object_table_hash_func,
                                             object_table_comp_func);
  if (mem->to_obj_tbl == NULL) goto err;

  mem->from_obj_tbl = scm_basic_hash_new(SCM_MEM_OBJ_TBL_HASH_SIZE,
                                               object_table_hash_func,
                                               object_table_comp_func);
  if (mem->from_obj_tbl == NULL) goto err;

  mem->to_heap = scm_mem_heap_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->to_heap == NULL) goto err;

  mem->from_heap = scm_mem_heap_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->from_heap == NULL) goto err;

  mem->persistent = scm_mem_heap_new_heap(1, SCM_MEM_HEAP_INIT_BLOCK_SIZE);
  if (mem->persistent == NULL) goto err;

  mem->extra_rfrn = malloc(sizeof(ScmRef) * SCM_MEM_EXTRA_RFRN_SIZE);
  if (mem->extra_rfrn == NULL) goto err;
  mem->nr_extra = 0;

  return mem;

 err:
  if (mem->to_obj_tbl != NULL) scm_basic_hash_end(mem->to_obj_tbl);
  if (mem->from_obj_tbl != NULL) scm_basic_hash_end(mem->from_obj_tbl);
  if (mem->to_heap != NULL) mem->to_heap = scm_mem_heap_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) mem->from_heap = scm_mem_heap_delete_heap(mem->from_heap);
  if (mem->persistent != NULL) mem->persistent = scm_mem_heap_delete_heap(mem->persistent);
  if (mem->extra_rfrn != NULL) free(mem->extra_rfrn);

  return NULL;
}

ScmMem *
scm_mem_finalize(ScmMem *mem)
{
  scm_assert(mem != NULL);

  scm_mem_free_all_obj_in_root_set(mem, &mem->roots);
  scm_mem_clean(mem);

  if (mem->to_obj_tbl) scm_basic_hash_end(mem->to_obj_tbl);
  if (mem->from_obj_tbl) scm_basic_hash_end(mem->from_obj_tbl);
  if (mem->to_heap != NULL) mem->to_heap = scm_mem_heap_delete_heap(mem->to_heap);
  if (mem->from_heap != NULL) mem->from_heap = scm_mem_heap_delete_heap(mem->from_heap);
  if (mem->persistent != NULL) mem->persistent = scm_mem_heap_delete_heap(mem->persistent);
  if (mem->extra_rfrn != NULL) free(mem->extra_rfrn);

  return NULL;
}

ScmMem *
scm_mem_new(void)
{
  ScmMem *mem = NULL;

  mem = malloc(sizeof(*mem));
  if (mem == NULL) return NULL;

  return scm_mem_initialize(mem);
}

ScmMem *
scm_mem_end(ScmMem *mem)
{
  if (mem == NULL) return NULL;

  scm_mem_finalize(mem);
  free(mem);

  return NULL;
}

ScmMem *
scm_mem_clean(ScmMem *mem)
{
  scm_mem_clean_persistent(mem);
  scm_mem_clean_root(mem);
  scm_mem_clean_heap(mem, TO_HEAP);
  scm_mem_clean_heap(mem, FROM_HEAP);
  return mem;
}


ScmMem *
scm_mem_alloc_heap(ScmMem *mem, ScmTypeInfo *type, ScmRef ref)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  scm_assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, SCM_OBJ_NULL);

  scm_mem_alloc_heap_mem(mem, type, ref);
  if (scm_obj_null_p(SCM_REF_DEREF(ref))) {
    if (mem->gc_enabled)
      scm_mem_gc_start(mem);
    else
      scm_mem_expand_heap(mem, 1);
    scm_mem_alloc_heap_mem(mem, type, ref);
    if (scm_obj_null_p(SCM_REF_DEREF(ref))) {
      ; /* TODO: write error handling (fail to allocate memory) */
      return NULL;
    }
  }

  scm_mem_obj_init(mem, SCM_REF_DEREF(ref), type);

  return mem;
}

ScmMem *
scm_mem_alloc_persist(ScmMem *mem, ScmTypeInfo *type, ScmRef ref)
{
  size_t size;
  void *ptr;

  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  scm_assert(ref != SCM_REF_NULL);

  size = scm_mem_alloc_size_in_heap(type);
  ptr = scm_mem_heap_alloc(mem->persistent, size);
  if (ptr == NULL) {
    if (scm_mem_expand_persistent(mem, 1) != 1) {
      ; /* TODO: write error handling (fail to allocate memory) */
      SCM_REF_UPDATE(ref, SCM_OBJ_NULL);
      return NULL;
    }
    ptr = scm_mem_heap_alloc(mem->persistent, size);
    if (ptr == NULL) {
      ; /* TODO: write error handling (fail to allocate memory) */
      SCM_REF_UPDATE(ref, SCM_OBJ_NULL);
      return NULL;
    }
  }
  SCM_REF_UPDATE(ref, ptr);

  if (scm_type_info_has_instance_weak_ref_p(type))
    scm_mem_add_obj_to_weak_list(mem->persistent, ref, type);

  scm_mem_obj_init(mem, SCM_REF_DEREF(ref), type);

  return mem;
}

ScmMem *
scm_mem_alloc_root(ScmMem *mem, ScmTypeInfo *type, ScmRef ref)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  scm_assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, scm_mem_alloc_root_obj(type, mem, &mem->roots));
  if (scm_obj_null_p(SCM_REF_DEREF(ref))) return NULL;

  return mem;
}

ScmObj
scm_mem_free_root(ScmMem *mem, ScmObj obj)
{
  scm_assert(mem != NULL);
  scm_assert(scm_mem_root_block_obj_in_blok_p(obj));

  return scm_mem_free_root_obj(obj, mem, &mem->roots);
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

ScmMem *
scm_mem_alloc(ScmMem *mem, ScmTypeInfo *type,
              SCM_MEM_ALLOC_TYPE_T alloc, ScmRef ref)
{
  scm_assert(mem != NULL);
  scm_assert(type != NULL);
  scm_assert(alloc <  SCM_MEM_NR_ALLOC_TYPE);
  scm_assert(ref != SCM_REF_NULL);

  SCM_REF_UPDATE(ref, SCM_OBJ_NULL);

  switch(alloc) {
  case SCM_MEM_ALLOC_HEAP:
    return scm_mem_alloc_heap(mem, type, ref);
    break;
  case SCM_MEM_ALLOC_ROOT:
    return scm_mem_alloc_root(mem, type, ref);
    break;
  case SCM_MEM_ALLOC_SHARED_ROOT:
    SCM_REF_UPDATE(ref, scm_memory_alloc_shared_root(type));
    return scm_obj_null_p(SCM_REF_DEREF(ref)) ? NULL : mem;
    break;
  default:
    return NULL;
  }

  return NULL;
}

void
scm_mem_gc_start(ScmMem *mem)
{
  int nr_free;

  scm_assert(mem != NULL);

  scm_mem_switch_heap(mem);
  scm_mem_copy_obj_referred_by_root(mem);
  scm_mem_scan_obj(mem);
  scm_mem_adjust_weak_ref(mem);
  scm_mem_clean_heap(mem, FROM_HEAP);

  nr_free = scm_mem_heap_nr_free_block(mem->to_heap);
  if (nr_free == 0)
    scm_mem_expand_heap(mem, 1);
  else if (nr_free > 1)
    scm_mem_release_redundancy_heap_blocks(mem, 1);
}





ScmObj
scm_memory_alloc_shared_root(ScmTypeInfo *type)
{
  return scm_mem_alloc_root_obj(type, NULL, &shared_roots);
}

ScmObj
scm_memory_free_shared_root(ScmObj obj)
{
  return scm_mem_free_root_obj(obj, NULL, &shared_roots);
}

void
scm_memory_free_all_shared_root(void)
{
  scm_mem_free_all_obj_in_root_set(NULL, &shared_roots);
  return;
}




void
scm_mem_enable_current_mem_gc(void)
{
  scm_mem_enable_gc(scm_vm_current_mm());
}

void
scm_mem_disable_current_mem_gc(void)
{
  scm_mem_disable_gc(scm_vm_current_mm());
}

bool
scm_mem_current_mem_gc_enabled_p(void)
{
  return scm_mem_gc_enabled_p(scm_vm_current_mm());
}
