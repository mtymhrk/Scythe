#include <cutter.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "pair.h"
#include "string.h"
#include "symbol.h"

typedef struct StubObjRc {
  ScmObjHeader header;
  ScmObj obj;
  ScmObj weak_obj;
  int obj_num;
  int  nr_call_ini_func;
  int  nr_call_fin_func;
  int  nr_call_accept_func;
  int  nr_call_accept_func_weak;
} StubObj;

enum { MAX_STUB_OBJ = 1024 };
StubObj stub_obj_table[MAX_STUB_OBJ];
static int nr_stub_obj = 0;

#define STUB_OBJ(obj) ((StubObj *)(obj))

void
stub_obj_gc_init_func(ScmObj obj, ScmObj mem)
{
  StubObj *so = (StubObj *)obj;

  assert(scm_obj_not_null_p(obj));
  assert(scm_obj_not_null_p(mem));

  so->obj = SCM_OBJ_NULL;
  so->weak_obj = SCM_OBJ_NULL;
  so->obj_num = nr_stub_obj++;
  so->nr_call_ini_func = 0;
  so->nr_call_fin_func = 0;
  so->nr_call_accept_func = 0;
  so->nr_call_accept_func_weak = 0;

  so->nr_call_ini_func++;

  memcpy(stub_obj_table + so->obj_num, so, sizeof(StubObj));
}

void
stub_obj_gc_fin_func(ScmObj obj)
{
  StubObj *so = (StubObj *)obj;

  assert(scm_obj_not_null_p(obj));

  so->nr_call_fin_func++;
  memcpy(stub_obj_table + so->obj_num, so, sizeof(StubObj));
}

int
stub_obj_gc_accept_func(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  StubObj *so = (StubObj *)obj;
  int rslt;

  so->nr_call_accept_func++;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, so->obj, mem);

  memcpy(stub_obj_table + so->obj_num, so, sizeof(StubObj));

  return rslt;
}

int
stub_obj_gc_accept_func_weak(ScmObj obj, ScmObj mem,
                             ScmGCRefHandlerFunc handler)
{
  StubObj *so = (StubObj *)obj;
  int rslt;

  so->nr_call_accept_func_weak++;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, so->weak_obj, mem);

  memcpy(stub_obj_table + so->obj_num, so, sizeof(StubObj));

  return rslt;
}

size_t
cell_alloc_size(size_t alloc)
{
  return sizeof(ScmMemHeapCell) + alloc;
}


void
test_scm_new_mem_block(void)
{
  ScmMemHeapBlock *block;

  /* action */
  block = scm_mem_heap_new_block(1024);

  /* postcondition check */
  cut_assert_not_null(block);
  cut_assert_null(block->next);
  cut_assert_null(block->prev);
  cut_assert_equal_uint(1024, block->size);
  cut_assert(/* 0 <= scm_mem_heap_block_used(block) && */
             scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
             && scm_mem_heap_block_free(block) <= 1024);
  cut_assert_not_null(scm_mem_heap_block_head(block));
  cut_assert_true(scm_mem_heap_block_free_ptr(block)
                  == scm_mem_heap_block_head(block));

  block = scm_mem_heap_delete_block(block);
  cut_assert_null(block);
}

void
test_scm_mem_block_allocated(void)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  block = scm_mem_heap_new_block(10240);

  /* action */
  scm_mem_heap_block_allocated(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(10240, block->size);
  cut_assert(cell_alloc_size(256) <= scm_mem_heap_block_used(block));
  cut_assert(scm_mem_heap_block_used(block)
             < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));
  cut_assert((10240 - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE)
             < scm_mem_heap_block_free(block));
  cut_assert(scm_mem_heap_block_free(block) <= (10240 - cell_alloc_size(256)));
  cut_assert_true((scm_mem_heap_block_head(block) + cell_alloc_size(256))
                  == scm_mem_heap_block_free_ptr(block));

  /* action */
  scm_mem_heap_block_allocated(block, 768);

  /* postcondition check */
  cut_assert_equal_uint(10240, block->size);
  cut_assert((cell_alloc_size(256) + cell_alloc_size(768))
              <= scm_mem_heap_block_used(block));
  cut_assert(scm_mem_heap_block_used(block)
             < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE
                + cell_alloc_size(768) + SCM_MEM_ALIGN_BYTE));
  cut_assert((10240
              - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE
              - cell_alloc_size(768) - SCM_MEM_ALIGN_BYTE)
             < scm_mem_heap_block_free(block));
  cut_assert(scm_mem_heap_block_free(block)
             <= (10240 - cell_alloc_size(256) - cell_alloc_size(768)));
  cut_assert_true((scm_mem_heap_block_head(block)
                   + cell_alloc_size(256) + cell_alloc_size(768))
                  == scm_mem_heap_block_free_ptr(block));

  /* postprocess */
  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_deallocated(void)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);
  scm_mem_heap_block_allocated(block, 256);

  /* action */
  scm_mem_heap_block_deallocated(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(1024, block->size);
  cut_assert(cell_alloc_size(256) <= scm_mem_heap_block_used(block));
  cut_assert(scm_mem_heap_block_used(block)
             < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));
  cut_assert((1024 - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE)
             < scm_mem_heap_block_free(block));
  cut_assert(scm_mem_heap_block_free(block) <= (1024 - cell_alloc_size(256)));
  cut_assert_true((scm_mem_heap_block_head(block) + cell_alloc_size(256))
                  == scm_mem_heap_block_free_ptr(block));

  /* action */
  scm_mem_heap_block_deallocated(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(1024, block->size);
  cut_assert(/* 0 <= scm_mem_heap_block_used(block) && */
             scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
             && scm_mem_heap_block_free(block) <= 1024);
  cut_assert_true(scm_mem_heap_block_head(block)
                  == scm_mem_heap_block_free_ptr(block));

  /* postprocess */
  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_ptr_is_allocated(void)
{
  ScmMemHeapBlock *block;
  uint8_t *ptr;

  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);

  ptr = scm_mem_heap_block_head(block);
  cut_assert_true(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(128);
  cut_assert_true(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(256) - 1;
  cut_assert_true(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(256);
  cut_assert_false(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_clean(void)
{
  ScmMemHeapBlock *block;

  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);

  scm_mem_heap_block_clean(block);

  cut_assert_equal_uint(1024, block->size);
  cut_assert(/* 0 <= scm_mem_heap_block_used(block) && */
             scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
             && scm_mem_heap_block_free(block) <= 1024);
  cut_assert_true(scm_mem_heap_block_free_ptr(block)
                  == scm_mem_heap_block_head(block));

  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_for_each_cell(void)
{
  ScmMemHeapBlock *block;
  size_t size[] = { 100, 256, 128 };
  ScmMemHeapCell *allocated[sizeof(size)/sizeof(size[0])];
  ScmMemHeapCell *cell;
  unsigned int i;

  block = scm_mem_heap_new_block(1024);

  for (i = 0; i < sizeof(size)/sizeof(size[0]); i++) {
    allocated[i] = scm_mem_heap_block_free_ptr(block);
    scm_mem_heap_block_allocated(block, size[i]);
  }

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL(block, cell) {
    cut_assert_true(allocated[i] == cell);
    cut_assert(cell->size == cell_alloc_size(size[i]));
    i++;
  };

  cut_assert_equal_uint(3, i);

  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_for_each_obj_deallocated_last_obj(void)
{
  ScmMemHeapBlock *block;
  size_t size[] = { 100, 256, 128 };
  ScmMemHeapCell *allocated[sizeof(size)/sizeof(size[0])];
  ScmMemHeapCell *cell;
  unsigned int i;

  block = scm_mem_heap_new_block(1024);

  for (i = 0; i < sizeof(size)/sizeof(size[0]); i++) {
    allocated[i] = scm_mem_heap_block_free_ptr(block);
    scm_mem_heap_block_allocated(block, size[i]);
  }

  scm_mem_heap_block_deallocated(block, size[i - 1]);

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL(block, cell) {
    cut_assert_true(allocated[i] == cell);
    cut_assert(cell->size == cell_alloc_size(size[i]));
    i++;
  };

  cut_assert_equal_uint(2, i);

  scm_mem_heap_delete_block(block);
}

void
test_scm_mem_block_is_obj_in_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmObj obj_in_blk1, obj_in_blk2;

  block1 = scm_mem_heap_new_block(1024);
  block2 = scm_mem_heap_new_block(1024);

  obj_in_blk1 = scm_mem_cell_to_obj(scm_mem_heap_block_free_ptr(block1));
  scm_mem_heap_block_allocated(block1, 100);

  obj_in_blk2 = scm_mem_cell_to_obj(scm_mem_heap_block_free_ptr(block2));
  scm_mem_heap_block_allocated(block2, 200);

  cut_assert_true(scm_mem_heap_block_has_obj_p(block1, obj_in_blk1));
  cut_assert_false(scm_mem_heap_block_has_obj_p(block2, obj_in_blk1));

  cut_assert_false(scm_mem_heap_block_has_obj_p(block1, obj_in_blk2));
  cut_assert_true(scm_mem_heap_block_has_obj_p(block2, obj_in_blk2));

  scm_mem_heap_delete_block(block1);
  scm_mem_heap_delete_block(block2);
}

void
test_scm_new_mem_heap_have_no_block(void)
{
  ScmMemHeap *heap;

  /* process */
  heap = scm_mem_heap_new_heap(0, 0);

  /* postcondition check */
  cut_assert_not_null(heap);
  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_null(heap->weak_list);
  cut_assert_equal_int(0, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_used_block(heap));

  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_add_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  block1 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block1);

  /* precondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  block2 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block2);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block2);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_del_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  block1 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block1);

  block2 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block2);

  /* action */
  scm_mem_heap_del_block(heap);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_del_block(heap);

  /* postcondition check */
  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_used_block(heap));

  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_new_mem_heap_have_block(void)
{
  ScmMemHeap *heap;

  /* action */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* postcondition check */
  cut_assert_not_null(heap);
  cut_assert_not_null(heap->head);
  cut_assert_not_null(heap->tail);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  cut_assert_equal_uint(1024, heap->head->size);
  cut_assert_equal_uint(1024, heap->tail->size);

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_shift(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* precondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_shift(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->head->next);
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_shift(heap);

  /* postcondition check */
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(2, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_unshift(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* precondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  scm_mem_heap_shift(heap);
  scm_mem_heap_shift(heap);

  /* precondition check */
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->tail);
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->tail->prev);
  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_rewind(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);
  scm_mem_heap_shift(heap);

  /* action */
  scm_mem_heap_rewind(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  scm_mem_heap_shift(heap);
  scm_mem_heap_shift(heap);

  /* action */
  scm_mem_heap_rewind(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_del_current_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  block1 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block1);

  block2 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block2);

  scm_mem_heap_shift(heap);

  /* action */
  scm_mem_heap_del_block(heap);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, scm_mem_heap_nr_block(heap));
  cut_assert_equal_int(0, scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_alloc(void)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr;

  /* preproces */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* action */
  ptr = scm_mem_heap_alloc(heap, 256);

  /* postcondition check */
  cut_assert_not_null(ptr);
  cut_assert_equal_uint(0u, (uintptr_t)ptr % SCM_MEM_ALIGN_BYTE);
  cut_assert(cell_alloc_size(256) <= scm_mem_heap_block_used(heap->head)
             && (scm_mem_heap_block_used(heap->head)
                 < cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_alloc_aligned(void)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr1, *ptr2;

  /* preproces */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* action */
  ptr1 = scm_mem_heap_alloc(heap, 17);
  ptr2 = scm_mem_heap_alloc(heap, 16);

  /* postcondition check */
  cut_assert_not_null(ptr1);
  cut_assert_equal_uint(0u, (uintptr_t)ptr1 % SCM_MEM_ALIGN_BYTE);
  cut_assert_not_null(ptr2);
  cut_assert_equal_uint(0u, (uintptr_t)ptr2 % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_alloc_next_block(void)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr1, *ptr2;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);
  ptr1 = scm_mem_heap_alloc(heap, 512);

  /* action */
  ptr2 = scm_mem_heap_alloc(heap, 768);

  /* postcondition check */
  cut_assert_not_null(ptr1);
  cut_assert_not_null(ptr2);

  cut_assert_true(heap->current == heap->head->next);
  cut_assert(cell_alloc_size(768) <= scm_mem_heap_block_used(heap->current));
  cut_assert(scm_mem_heap_block_used(heap->current)
             < (cell_alloc_size(768) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_alloc_fail_to_allocate(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr;

  /* preprocess */
  heap = scm_mem_heap_new_heap(1, 1024);

  ptr = scm_mem_heap_alloc(heap, 256);

  cut_assert_not_null(ptr);

  ptr = scm_mem_heap_alloc(heap, 512);

  cut_assert_not_null(ptr);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  /* action */
  ptr = scm_mem_heap_alloc(heap, 512);

  /* postprocess check */
  cut_assert_null(ptr);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, scm_mem_heap_block_used(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_cancel_alloc(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  ptr = scm_mem_heap_alloc(heap, 256);

  cut_assert_not_null(ptr);

  /* action */
  scm_mem_heap_cancel_alloc(heap, 256);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, scm_mem_heap_block_used(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_cancel_alloc_not_allocated(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;

  /* preproces */
  heap = scm_mem_heap_new_heap(2, 1024);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  /* action */
  scm_mem_heap_cancel_alloc(heap, 256);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, scm_mem_heap_block_used(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_cancel_alloc_shoud_unshift(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);
  ptr = scm_mem_heap_alloc(heap, 768);

  cut_assert_not_null(ptr);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  ptr = scm_mem_heap_alloc(heap, 768);

  cut_assert_not_null(ptr);

  /* action */
  scm_mem_heap_cancel_alloc(heap, 768);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, scm_mem_heap_block_used(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       scm_mem_heap_nr_free_block(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_for_each_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    blocks[i] = scm_mem_heap_new_block(1024);
    scm_mem_heap_add_block(heap, blocks[i]);
  }

  /* action */
  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);       /* invariant check */
    i++;
  }

  /* postcondition check */
  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]), i);

  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_heap_for_each_block_delete_last_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    blocks[i] = scm_mem_heap_new_block(1024);
    scm_mem_heap_add_block(heap, blocks[i]);
  }

  scm_mem_heap_del_block(heap);

  /* action */
  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);      /* invariant check */
    i++;
  }

  /* postcondition check */
  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]) - 1, i);

  heap = scm_mem_heap_delete_heap(heap);
}

void
test_scm_mem_new(void)
{
  ScmMem *mem;

  /* action */
  mem = scm_mem_new();

  /* postcondition check */
  cut_assert_not_null(mem);
  cut_assert_not_null(mem->to_heap);
  cut_assert_not_null(mem->from_heap);
  cut_assert(mem->gc_enabled);

  cut_assert_equal_int(1, scm_mem_heap_nr_block(mem->to_heap));
  cut_assert_equal_int(1, scm_mem_heap_nr_block(mem->from_heap));
  cut_assert_equal_uint(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                        mem->to_heap->head->size);
  cut_assert_equal_uint(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                        mem->from_heap->head->size);

  /* preprocess */
  scm_mem_end(mem);
}


void
test_scm_mem_alloc_size_in_heap__size_is_smaller_then_forward(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(ScmForward) - 1,
    .gc_ini_func         = NULL,
    .gc_fin_func         = NULL,
    .gc_accept_func      = NULL,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type, 0);

  /* postcondition check */
  cut_assert_equal_uint(sizeof(ScmForward), actual_size);
}

void
test_scm_mem_alloc_size_in_heap__size_is_greater_then_forward(void)
{
  size_t expected_size = sizeof(ScmForward) + 1;
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = expected_size,
    .gc_ini_func         = NULL,
    .gc_fin_func         = NULL,
    .gc_accept_func      = NULL,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type, 0);

  /* postcondition check */
  cut_assert_equal_uint(expected_size, actual_size);
}

void
test_scm_mem_alloc_size_in_heap__obj_has_weak_ref(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = NULL,
    .gc_fin_func         = NULL,
    .gc_accept_func      = NULL,
    .gc_accept_func_weak = stub_obj_gc_accept_func_weak,
    .extra               = NULL,
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type, 0);

  /* postcondition check */
  cut_assert_equal_uint(scm_mem_align_size(sizeof(StubObj)), actual_size);
}

void
test_scm_mem_alloc_size_in_root__size_is_smaller_than_atom(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(ScmMMObj) - 1,
    .gc_ini_func         = NULL,
    .gc_fin_func         = NULL,
    .gc_accept_func      = NULL,
    .gc_accept_func_weak = NULL,
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_root(&type, 0);

  /* postcondition check */
  cut_assert_equal_uint(sizeof(ScmMMObj) + sizeof(ScmMemRootBlock),
                        actual_size);
}

void
test_scm_mem_alloc_size_in_root__size_is_greater_than_atom(void)
{
  size_t obj_size = sizeof(ScmMMObj) * 2;
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = obj_size,
    .gc_ini_func         = NULL,
    .gc_fin_func         = NULL,
    .gc_accept_func      = NULL,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_root(&type, 0);

  /* postcondition check */
  cut_assert_equal_uint(obj_size + sizeof(ScmMemRootBlock), actual_size);
}

void
test_scm_mem_root_block_new(void)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;

  /* action */
  block = scm_mem_root_block_new(1024);

  obj = scm_mem_root_block_object(block);

  /* postcondition check */
  cut_assert_not_null(block);
  cut_assert_null(block->hdr.next);
  cut_assert_null(block->hdr.prev);
  cut_assert_equal_uint(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  scm_mem_root_block_free(block);
}

void
test_scm_mem_root_block_obj_header(void)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;

  /* action */
  block = scm_mem_root_block_new(1024);

  obj = scm_mem_root_block_object(block);

  /* postcondition check */
  cut_assert_equal_pointer(block, scm_mem_root_block_obj_header(obj));

  /* postprocess */
  scm_mem_root_block_free(block);
}

void
test_scm_mem_add_to_root_set(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  block1 = scm_mem_root_block_new(1024);
  block2 = scm_mem_root_block_new(1024);
  block3 = scm_mem_root_block_new(1024);

  /* action */
  scm_mem_add_to_root_set(&list_head, block1);

  /* postcondition check */
  cut_assert_equal_pointer(block1, list_head);
  cut_assert_equal_pointer(NULL, block1->hdr.next);
  cut_assert_equal_pointer(NULL, block1->hdr.prev);


  /* action */
  scm_mem_add_to_root_set(&list_head, block2);

  /* postcondition check */
  cut_assert_equal_pointer(block2, list_head);

  cut_assert_equal_pointer(block1, block2->hdr.next);
  cut_assert_equal_pointer(NULL, block2->hdr.prev);

  cut_assert_equal_pointer(NULL, block1->hdr.next);
  cut_assert_equal_pointer(block2, block1->hdr.prev);


  /* action */
  scm_mem_add_to_root_set(&list_head, block3);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block2, block3->hdr.next);
  cut_assert_equal_pointer(NULL, block3->hdr.prev);

  cut_assert_equal_pointer(block1, block2->hdr.next);
  cut_assert_equal_pointer(block3, block2->hdr.prev);

  cut_assert_equal_pointer(NULL, block1->hdr.next);
  cut_assert_equal_pointer(block2, block1->hdr.prev);

  /* postprocess */
  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

void
test_scm_mem_del_from_root_set__delte_tail(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  block1 = scm_mem_root_block_new(1024);
  block2 = scm_mem_root_block_new(1024);
  block3 = scm_mem_root_block_new(1024);

  scm_mem_add_to_root_set(&list_head, block1);
  scm_mem_add_to_root_set(&list_head, block2);
  scm_mem_add_to_root_set(&list_head, block3);

  /* action */
  scm_mem_del_from_root_set(&list_head, block1);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block2, block3->hdr.next);
  cut_assert_equal_pointer(NULL, block3->hdr.prev);

  cut_assert_equal_pointer(NULL, block2->hdr.next);
  cut_assert_equal_pointer(block3, block2->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

void
test_scm_mem_del_from_root_set__delte_head(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  block1 = scm_mem_root_block_new(1024);
  block2 = scm_mem_root_block_new(1024);
  block3 = scm_mem_root_block_new(1024);

  scm_mem_add_to_root_set(&list_head, block1);
  scm_mem_add_to_root_set(&list_head, block2);
  scm_mem_add_to_root_set(&list_head, block3);

  /* action */
  scm_mem_del_from_root_set(&list_head, block3);

  /* postcondition check */
  cut_assert_equal_pointer(block2, list_head);

  cut_assert_equal_pointer(block1, block2->hdr.next);
  cut_assert_equal_pointer(NULL, block2->hdr.prev);

  cut_assert_equal_pointer(NULL, block1->hdr.next);
  cut_assert_equal_pointer(block2, block1->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

void
test_scm_mem_del_from_root_set__delte_middle(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  block1 = scm_mem_root_block_new(1024);
  block2 = scm_mem_root_block_new(1024);
  block3 = scm_mem_root_block_new(1024);

  scm_mem_add_to_root_set(&list_head, block1);
  scm_mem_add_to_root_set(&list_head, block2);
  scm_mem_add_to_root_set(&list_head, block3);

  /* action */
  scm_mem_del_from_root_set(&list_head, block2);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block1, block3->hdr.next);
  cut_assert_equal_pointer(NULL, block3->hdr.prev);

  cut_assert_equal_pointer(NULL, block1->hdr.next);
  cut_assert_equal_pointer(block3, block1->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

void
test_scm_mem_alloc_heap(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  bool in_to_heap;
  ScmMemHeapBlock *block;
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;

  /* preprocess */
  mem = scm_mem_new();

  scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(obj));

  /* action */
  obj = scm_mem_alloc_heap(mem, &type, 0);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(obj));
  cut_assert(scm_obj_type_p(obj, &type));
  cut_assert(scm_obj_mem_managed_p(obj));
  cut_assert_equal_int(1, ((StubObj *)obj)->nr_call_ini_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_fin_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  in_to_heap = false;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    if (scm_mem_heap_block_has_obj_p(block, obj))
      in_to_heap = true;
  }

  cut_assert_true(in_to_heap);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_alloc_heap__alignment(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj obj1 = SCM_OBJ_INIT;
  ScmObj obj2 = SCM_OBJ_INIT;

  /* preprocess */
  mem = scm_mem_new();

  scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(obj1));
  scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(obj2));

  /* action */
  obj1 = scm_mem_alloc_heap(mem, &type, 0);
  obj2 = scm_mem_alloc_heap(mem, &type, 0);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(obj1));
  cut_assert(scm_obj_type_p(obj1, &type));
  cut_assert(scm_obj_mem_managed_p(obj1));

  cut_assert_true(scm_obj_not_null_p(obj2));
  cut_assert(scm_obj_type_p(obj2, &type));
  cut_assert(scm_obj_mem_managed_p(obj2));


  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_alloc_root(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;

  /* preprocess */
  mem = scm_mem_new();

  /* action */
  obj = scm_mem_alloc_root(mem, &type, 0);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(obj));
  cut_assert(scm_obj_type_p(obj, &type));
  cut_assert_equal_int(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);
  cut_assert_equal_uint(scm_mem_root_block_object(mem->roots), obj);

  cut_assert_equal_int(1, ((StubObj *)obj)->nr_call_ini_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_fin_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  scm_mem_free_root(mem, obj);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_free_root(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;
  int obj_num;

  /* preprocess */
  mem = scm_mem_new();

  obj = scm_mem_alloc_root(mem, &type, 0);
  obj_num = ((StubObj *)obj)->obj_num;

  /* action */
  scm_mem_free_root(mem, obj);

  /* postcondition check */
  cut_assert_equal_int(1, stub_obj_table[obj_num].nr_call_ini_func);
  cut_assert_equal_int(1, stub_obj_table[obj_num].nr_call_fin_func);
  cut_assert_equal_int(0, stub_obj_table[obj_num].nr_call_accept_func);
  cut_assert_equal_int(0, stub_obj_table[obj_num].nr_call_accept_func_weak);

  cut_assert_null(mem->roots);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__not_scavenged(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj = scm_mem_alloc_root(mem, &type, 0);
  STUB_OBJ(root_obj)->obj = scm_mem_alloc_heap(mem, &type, 0);
  heap_obj1 = STUB_OBJ(root_obj)->obj;
  STUB_OBJ(heap_obj1)->obj = scm_mem_alloc_heap(mem, &type, 0);
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj1_num = STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(STUB_OBJ(root_obj)->obj));
  cut_assert_true(scm_obj_not_null_p(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__scavenged(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj = scm_mem_alloc_root(mem, &type, 0);
  STUB_OBJ(root_obj)->obj = scm_mem_alloc_heap(mem, &type, 0);
  heap_obj1 = STUB_OBJ(root_obj)->obj;
  STUB_OBJ(heap_obj1)->obj = scm_mem_alloc_heap(mem, &type, 0);
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj1_num = STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->obj)->obj_num;

  STUB_OBJ(heap_obj1)->obj = SCM_OBJ_NULL; /* heap_obj2 become garbage */

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(STUB_OBJ(root_obj)->obj));
  cut_assert_true(scm_obj_null_p(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__referred_by_two_objects(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT, root_obj2 = SCM_OBJ_INIT;
  int root_obj1_num, root_obj2_num, heap_obj_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj1 = scm_mem_alloc_root(mem, &type, 0);
  STUB_OBJ(root_obj1)->obj = scm_mem_alloc_heap(mem, &type, 0);
  root_obj1_num = STUB_OBJ(root_obj1)->obj_num;
  heap_obj_num = STUB_OBJ(STUB_OBJ(root_obj1)->obj)->obj_num;

  root_obj2 = scm_mem_alloc_root(mem, &type, 0);
  /* heap_obj is referred by root_obj1 and root_obj2 */
  STUB_OBJ(root_obj2)->obj = STUB_OBJ(root_obj1)->obj;
  root_obj2_num = STUB_OBJ(root_obj2)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(scm_obj_same_instance_p(STUB_OBJ(root_obj1)->obj,
                                           STUB_OBJ(root_obj2)->obj));

  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[root_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[root_obj2_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__root_obj_referred_by_heap_obj(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = NULL,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj = SCM_OBJ_INIT;
  int root_obj_num, heap_obj_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj = scm_mem_alloc_root(mem, &type, 0);
  heap_obj = scm_mem_alloc_heap(mem, &type, 0);
  STUB_OBJ(root_obj)->obj = heap_obj;
  STUB_OBJ(heap_obj)->obj = root_obj;   /* heap_obj refers root_obj; */
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj_num = STUB_OBJ(heap_obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  heap_obj = STUB_OBJ(root_obj)->obj;

  cut_assert_true(scm_obj_same_instance_p(root_obj, STUB_OBJ(heap_obj)->obj));

  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__weak_reference_refer_to_obj_not_scavenged(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = stub_obj_gc_accept_func_weak,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT;
  ScmObj root_obj2 = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj1_num, root_obj2_num,  heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj1 = scm_mem_alloc_root(mem, &type, 0);
  root_obj2 = scm_mem_alloc_root(mem, &type, 0);
  STUB_OBJ(root_obj1)->obj = scm_mem_alloc_heap(mem, &type, 0);
  STUB_OBJ(root_obj2)->obj = scm_mem_alloc_heap(mem, &type, 0);
  heap_obj1 = STUB_OBJ(root_obj1)->obj;
  STUB_OBJ(heap_obj1)->weak_obj = STUB_OBJ(root_obj2)->obj;
  root_obj1_num = STUB_OBJ(root_obj1)->obj_num;
  root_obj2_num = STUB_OBJ(root_obj2)->obj_num;
  heap_obj1_num = STUB_OBJ(STUB_OBJ(root_obj1)->obj)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(root_obj2)->obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  heap_obj1 = STUB_OBJ(root_obj1)->obj;

  /* postcondition check */
  cut_assert_true(scm_obj_same_instance_p(STUB_OBJ(root_obj2)->obj,
                                           STUB_OBJ(heap_obj1)->weak_obj));


  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[root_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[root_obj2_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);


  /* postprocess */
  scm_mem_end(mem);
}

void
test_scm_mem_gc_start__weak_reference_refer_to_obj_scavenged(void)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .pp_func             = NULL,
    .obj_size            = sizeof(StubObj),
    .gc_ini_func         = stub_obj_gc_init_func,
    .gc_fin_func         = stub_obj_gc_fin_func,
    .gc_accept_func      = stub_obj_gc_accept_func,
    .gc_accept_func_weak = stub_obj_gc_accept_func_weak,
    .extra               = NULL,
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj1_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_new();
  root_obj1 = scm_mem_alloc_root(mem, &type, 0);
  STUB_OBJ(root_obj1)->obj = scm_mem_alloc_heap(mem, &type, 0);
  heap_obj1 = STUB_OBJ(root_obj1)->obj;
  STUB_OBJ(heap_obj1)->weak_obj = scm_mem_alloc_heap(mem, &type, 0);
  root_obj1_num = STUB_OBJ(root_obj1)->obj_num;
  heap_obj1_num = STUB_OBJ(heap_obj1)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->weak_obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  heap_obj1 = STUB_OBJ(root_obj1)->obj;

  /* postcondition check */
  cut_assert_true(scm_obj_null_p(STUB_OBJ(heap_obj1)->weak_obj));

  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  cut_assert_equal_int(1,
                       stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  cut_assert_equal_int(1, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  cut_assert_equal_int(0, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  cut_assert_equal_int(0,
                       stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);


  /* postprocess */
  scm_mem_end(mem);
}

