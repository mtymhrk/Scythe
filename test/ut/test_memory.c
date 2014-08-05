#include "../../src/memory.c"

#include "object.h"

#include "test.h"

TEST_GROUP(memory);

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

TEST_SETUP(memory)
{
}

TEST_TEAR_DOWN(memory)
{
}

TEST(memory, scm_mem_heap_new_block)
{
  ScmMemHeapBlock *block;

  /* action */
  block = scm_mem_heap_new_block(1024);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(block);
  TEST_ASSERT_NULL(block->next);
  TEST_ASSERT_NULL(block->prev);
  TEST_ASSERT_EQUAL_UINT(1024, block->size);
  TEST_ASSERT(/* 0 <= scm_mem_heap_block_used(block) && */
              scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
              && scm_mem_heap_block_free(block) <= 1024);
  TEST_ASSERT_NOT_NULL(scm_mem_heap_block_head(block));
  TEST_ASSERT_TRUE(scm_mem_heap_block_free_ptr(block)
                   == scm_mem_heap_block_head(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  block = scm_mem_heap_delete_block(block);
  TEST_ASSERT_NULL(block);
}

TEST(memory, scm_mem_heap_block_allocated)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  block = scm_mem_heap_new_block(10240);

  /* action */
  scm_mem_heap_block_allocated(block, 256);

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(10240, block->size);
  TEST_ASSERT(cell_alloc_size(256) <= scm_mem_heap_block_used(block));
  TEST_ASSERT(scm_mem_heap_block_used(block)
              < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));
  TEST_ASSERT((10240 - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE)
              < scm_mem_heap_block_free(block));
  TEST_ASSERT(scm_mem_heap_block_free(block) <= (10240 - cell_alloc_size(256)));
  TEST_ASSERT((scm_mem_heap_block_head(block) + cell_alloc_size(256))
              == scm_mem_heap_block_free_ptr(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  scm_mem_heap_delete_block(block);
}

TEST(memory, scm_mem_heap_block_allocated__2_times)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  block = scm_mem_heap_new_block(10240);

  /* action */
  scm_mem_heap_block_allocated(block, 256);
  scm_mem_heap_block_allocated(block, 768);

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(10240, block->size);
  TEST_ASSERT((cell_alloc_size(256) + cell_alloc_size(768))
              <= scm_mem_heap_block_used(block));
  TEST_ASSERT(scm_mem_heap_block_used(block)
              < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE
                 + cell_alloc_size(768) + SCM_MEM_ALIGN_BYTE));
  TEST_ASSERT((10240
               - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE
               - cell_alloc_size(768) - SCM_MEM_ALIGN_BYTE)
              < scm_mem_heap_block_free(block));
  TEST_ASSERT(scm_mem_heap_block_free(block)
              <= (10240 - cell_alloc_size(256) - cell_alloc_size(768)));
  TEST_ASSERT((scm_mem_heap_block_head(block)
               + cell_alloc_size(256) + cell_alloc_size(768))
              == scm_mem_heap_block_free_ptr(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  scm_mem_heap_delete_block(block);
}

TEST(memory, scm_mem_heap_block_deallocated)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);
  scm_mem_heap_block_allocated(block, 256);

  /* action */
  scm_mem_heap_block_deallocated(block, 256);

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(1024, block->size);
  TEST_ASSERT(cell_alloc_size(256) <= scm_mem_heap_block_used(block));
  TEST_ASSERT(scm_mem_heap_block_used(block)
              < (cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));
  TEST_ASSERT((1024 - cell_alloc_size(256) - SCM_MEM_ALIGN_BYTE)
              < scm_mem_heap_block_free(block));
  TEST_ASSERT(scm_mem_heap_block_free(block) <= (1024 - cell_alloc_size(256)));
  TEST_ASSERT((scm_mem_heap_block_head(block) + cell_alloc_size(256))
              == scm_mem_heap_block_free_ptr(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  /* action */
  scm_mem_heap_block_deallocated(block, 256);

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(1024, block->size);
  TEST_ASSERT(/* 0 <= scm_mem_heap_block_used(block) && */
              scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
              && scm_mem_heap_block_free(block) <= 1024);
  TEST_ASSERT(scm_mem_heap_block_head(block)
              == scm_mem_heap_block_free_ptr(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  scm_mem_heap_delete_block(block);
}

TEST(memory, scm_mem_heap_block_allocable_p)
{
  ScmMemHeapBlock *block;
  uint8_t *ptr;

  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);

  ptr = scm_mem_heap_block_head(block);
  TEST_ASSERT_TRUE(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(128);
  TEST_ASSERT_TRUE(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(256) - 1;
  TEST_ASSERT_TRUE(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  ptr = scm_mem_heap_block_head(block) + cell_alloc_size(256);
  TEST_ASSERT_FALSE(scm_mem_heap_block_ptr_allocated_p(block, ptr));

  scm_mem_heap_delete_block(block);
}

TEST(memory, scm_mem_heap_block_clean)
{
  ScmMemHeapBlock *block;

  block = scm_mem_heap_new_block(1024);

  scm_mem_heap_block_allocated(block, 256);

  scm_mem_heap_block_clean(block);

  TEST_ASSERT_EQUAL_UINT(1024, block->size);
  TEST_ASSERT(/* 0 <= scm_mem_heap_block_used(block) && */
              scm_mem_heap_block_used(block) < SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT(1024 - SCM_MEM_ALIGN_BYTE < scm_mem_heap_block_free(block)
              && scm_mem_heap_block_free(block) <= 1024);
  TEST_ASSERT(scm_mem_heap_block_free_ptr(block)
              == scm_mem_heap_block_head(block));
  TEST_ASSERT_EQUAL_UINT(0u,
                         (uintptr_t)scm_mem_heap_block_free_ptr(block) % SCM_MEM_ALIGN_BYTE);

  scm_mem_heap_delete_block(block);
}

TEST(memory, SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL)
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
    TEST_ASSERT(allocated[i] == cell);
    TEST_ASSERT(cell->size == cell_alloc_size(size[i]));
    i++;
  };

  TEST_ASSERT_EQUAL_UINT(3, i);

  scm_mem_heap_delete_block(block);
}

TEST(memory, SCM_MEM_HEAP_BLOCK_FOR_EACH_CELL__eallocated_last_obj)
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
    TEST_ASSERT(allocated[i] == cell);
    TEST_ASSERT(cell->size == cell_alloc_size(size[i]));
    i++;
  };

  TEST_ASSERT_EQUAL_UINT(2, i);

  scm_mem_heap_delete_block(block);
}

TEST(memory, scm_mem_heap_block_has_obj_p)
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

  TEST_ASSERT_TRUE(scm_mem_heap_block_has_obj_p(block1, obj_in_blk1));
  TEST_ASSERT_FALSE(scm_mem_heap_block_has_obj_p(block2, obj_in_blk1));

  TEST_ASSERT_FALSE(scm_mem_heap_block_has_obj_p(block1, obj_in_blk2));
  TEST_ASSERT_TRUE(scm_mem_heap_block_has_obj_p(block2, obj_in_blk2));

  scm_mem_heap_delete_block(block1);
  scm_mem_heap_delete_block(block2);
}

TEST(memory, scm_mem_heap_new__heap_has_no_block)
{
  ScmMemHeap *heap;

  /* process */
  heap = scm_mem_heap_new_heap(0, 0);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(heap);
  TEST_ASSERT_NULL(heap->head);
  TEST_ASSERT_NULL(heap->tail);
  TEST_ASSERT_NULL(heap->current);
  TEST_ASSERT_NULL(heap->weak_list);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_used_block(heap));

  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_add_block)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(0, 0);

  block1 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block1);

  /* precondition check */
  TEST_ASSERT_TRUE(heap->head == block1);
  TEST_ASSERT_TRUE(heap->tail == block1);
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  block2 = scm_mem_heap_new_block(1024);
  scm_mem_heap_add_block(heap, block2);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->head == block1);
  TEST_ASSERT_TRUE(heap->tail == block2);
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_del_block)
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
  TEST_ASSERT_TRUE(heap->head == block1);
  TEST_ASSERT_TRUE(heap->tail == block1);
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_del_block(heap);

  /* postcondition check */
  TEST_ASSERT_NULL(heap->head);
  TEST_ASSERT_NULL(heap->tail);
  TEST_ASSERT_NULL(heap->current);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_used_block(heap));

  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_new__heap_has_blocks)
{
  ScmMemHeap *heap;

  /* action */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(heap);
  TEST_ASSERT_NOT_NULL(heap->head);
  TEST_ASSERT_NOT_NULL(heap->tail);
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  TEST_ASSERT_EQUAL_UINT(1024, heap->head->size);
  TEST_ASSERT_EQUAL_UINT(1024, heap->tail->size);

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_shift)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* precondition check */
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_shift(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == heap->head->next);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_shift(heap);

  /* postcondition check */
  TEST_ASSERT_NULL(heap->current);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_unshift)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* precondition check */
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  scm_mem_heap_shift(heap);
  scm_mem_heap_shift(heap);

  /* precondition check */
  TEST_ASSERT_NULL(heap->current);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == heap->tail);
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(2, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == heap->tail->prev);
  TEST_ASSERT_TRUE(heap->current == (heap)->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* action */
  scm_mem_heap_unshift(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == (heap)->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_rewind)
{
  ScmMemHeap *heap;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);
  scm_mem_heap_shift(heap);

  /* action */
  scm_mem_heap_rewind(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  scm_mem_heap_shift(heap);
  scm_mem_heap_shift(heap);

  /* action */
  scm_mem_heap_rewind(heap);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_del_block__current_block)
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
  TEST_ASSERT_TRUE(heap->head == block1);
  TEST_ASSERT_TRUE(heap->tail == block1);
  TEST_ASSERT_TRUE(heap->current == heap->head);
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_block(heap));
  TEST_ASSERT_EQUAL_INT(0, scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_used_block(heap));

  /* preprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_alloc)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr;

  /* preproces */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* action */
  ptr = scm_mem_heap_alloc(heap, 256);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(ptr);
  TEST_ASSERT_EQUAL_UINT(0u, (uintptr_t)ptr % SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT(cell_alloc_size(256) <= scm_mem_heap_block_used(heap->head)
              && (scm_mem_heap_block_used(heap->head)
                  < cell_alloc_size(256) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_alloc__aligned)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr1, *ptr2;

  /* preproces */
  heap = scm_mem_heap_new_heap(2, 1024);

  /* action */
  ptr1 = scm_mem_heap_alloc(heap, 17);
  ptr2 = scm_mem_heap_alloc(heap, 16);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(ptr1);
  TEST_ASSERT_EQUAL_UINT(0u, (uintptr_t)ptr1 % SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT_NOT_NULL(ptr2);
  TEST_ASSERT_EQUAL_UINT(0u, (uintptr_t)ptr2 % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_alloc__next_block)
{
  ScmMemHeap *heap;
  ScmMemHeapCell *ptr1, *ptr2;

  /* preprocess */
  heap = scm_mem_heap_new_heap(2, 1024);
  ptr1 = scm_mem_heap_alloc(heap, 512);

  /* action */
  ptr2 = scm_mem_heap_alloc(heap, 768);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(ptr1);
  TEST_ASSERT_NOT_NULL(ptr2);

  TEST_ASSERT_TRUE(heap->current == heap->head->next);
  TEST_ASSERT(cell_alloc_size(768) <= scm_mem_heap_block_used(heap->current));
  TEST_ASSERT(scm_mem_heap_block_used(heap->current)
              < (cell_alloc_size(768) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_alloc__fail_to_allocate)
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

  TEST_ASSERT_NOT_NULL(ptr);

  ptr = scm_mem_heap_alloc(heap, 512);

  TEST_ASSERT_NOT_NULL(ptr);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  /* action */
  ptr = scm_mem_heap_alloc(heap, 512);

  /* postprocess check */
  TEST_ASSERT_NULL(ptr);

  TEST_ASSERT_TRUE(heap->current == expected_current);
  TEST_ASSERT_EQUAL_UINT(expected_used, scm_mem_heap_block_used(heap->current));
  TEST_ASSERT_EQUAL_INT(expected_nr_free_block,
                        scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(expected_nr_used_block,
                        scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_cancel_alloc)
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

  TEST_ASSERT_NOT_NULL(ptr);

  /* action */
  scm_mem_heap_cancel_alloc(heap, 256);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == expected_current);
  TEST_ASSERT_EQUAL_UINT(expected_used, scm_mem_heap_block_used(heap->current));
  TEST_ASSERT_EQUAL_INT(expected_nr_free_block,
                        scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(expected_nr_used_block,
                        scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_cancel__not_allocated)
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
  TEST_ASSERT_TRUE(heap->current == expected_current);
  TEST_ASSERT_EQUAL_UINT(expected_used, scm_mem_heap_block_used(heap->current));
  TEST_ASSERT_EQUAL_INT(expected_nr_free_block,
                        scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(expected_nr_used_block,
                        scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_heap_cancel__shoud_unshift)
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

  TEST_ASSERT_NOT_NULL(ptr);

  expected_current = heap->current;
  expected_used = scm_mem_heap_block_used(heap->current);
  expected_nr_free_block = scm_mem_heap_nr_free_block(heap);
  expected_nr_used_block = scm_mem_heap_nr_used_block(heap);

  ptr = scm_mem_heap_alloc(heap, 768);

  TEST_ASSERT_NOT_NULL(ptr);

  /* action */
  scm_mem_heap_cancel_alloc(heap, 768);

  /* postcondition check */
  TEST_ASSERT_TRUE(heap->current == expected_current);
  TEST_ASSERT_EQUAL_UINT(expected_used, scm_mem_heap_block_used(heap->current));
  TEST_ASSERT_EQUAL_INT(expected_nr_free_block,
                        scm_mem_heap_nr_free_block(heap));
  TEST_ASSERT_EQUAL_INT(expected_nr_used_block,
                        scm_mem_heap_nr_used_block(heap));

  /* postprocess */
  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, SCM_MEM_HEAP_FOR_EACH_BLOCK)
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
    TEST_ASSERT_TRUE(blocks[i] == block);       /* invariant check */
    i++;
  }

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(sizeof(blocks)/sizeof(blocks[0]), i);

  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, SCM_MEM_HEAP_FOR_EACH_BLOCK__delete_last_block)
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
    TEST_ASSERT_TRUE(blocks[i] == block);      /* invariant check */
    i++;
  }

  /* postcondition check */
  TEST_ASSERT_EQUAL_UINT(sizeof(blocks)/sizeof(blocks[0]) - 1, i);

  heap = scm_mem_heap_delete_heap(heap);
}

TEST(memory, scm_mem_new)
{
  ScmMem *mem;

  /* action */
  mem = scm_mem_new();

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(mem);
  TEST_ASSERT_NOT_NULL(mem->to_heap);
  TEST_ASSERT_NOT_NULL(mem->from_heap);
  TEST_ASSERT(mem->gc_enabled);

  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_block(mem->to_heap));
  TEST_ASSERT_EQUAL_INT(1, scm_mem_heap_nr_block(mem->from_heap));
  TEST_ASSERT_EQUAL_UINT(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                         mem->to_heap->head->size);
  TEST_ASSERT_EQUAL_UINT(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                         mem->from_heap->head->size);

  /* preprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_alloc_size_in_heap__size_is_smaller_than_forward)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_UINT(sizeof(ScmForward), actual_size);
}

TEST(memory, scm_mem_alloc_size_in_heap__size_is_greater_than_forward)
{
  size_t expected_size = sizeof(ScmForward) + 1;
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_UINT(expected_size, actual_size);
}

TEST(memory, scm_mem_alloc_size_in_heap__obj_has_weak_ref)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_UINT(scm_mem_align_size(sizeof(StubObj)), actual_size);
}

TEST(memory, scm_mem_alloc_size_in_root__size_is_smaller_than_atom)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_UINT(sizeof(ScmMMObj) + sizeof(ScmMemRootBlock),
                         actual_size);
}

TEST(memory, scm_mem_alloc_size_in_root__size_is_greater_than_atom)
{
  size_t obj_size = sizeof(ScmMMObj) * 2;
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_UINT(obj_size + sizeof(ScmMemRootBlock), actual_size);
}

TEST(memory, scm_mem_root_block_new)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;

  /* action */
  block = scm_mem_root_block_new(1024);

  obj = scm_mem_root_block_object(block);

  /* postcondition check */
  TEST_ASSERT_NOT_NULL(block);
  TEST_ASSERT_NULL(block->hdr.next);
  TEST_ASSERT_NULL(block->hdr.prev);
  TEST_ASSERT_EQUAL_UINT(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  scm_mem_root_block_free(block);
}

TEST(memory, scm_mem_root_block_obj_header)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;

  /* action */
  block = scm_mem_root_block_new(1024);

  obj = scm_mem_root_block_object(block);

  /* postcondition check */
  TEST_ASSERT_EQUAL_PTR(block, scm_mem_root_block_obj_header(obj));

  /* postprocess */
  scm_mem_root_block_free(block);
}

TEST(memory, scm_mem_add_to_root_set)
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
  TEST_ASSERT_EQUAL_PTR(block1, list_head);
  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.prev);


  /* action */
  scm_mem_add_to_root_set(&list_head, block2);

  /* postcondition check */
  TEST_ASSERT_EQUAL_PTR(block2, list_head);

  TEST_ASSERT_EQUAL_PTR(block1, block2->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block2->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block2, block1->hdr.prev);


  /* action */
  scm_mem_add_to_root_set(&list_head, block3);

  /* postcondition check */
  TEST_ASSERT_EQUAL_PTR(block3, list_head);

  TEST_ASSERT_EQUAL_PTR(block2, block3->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block3->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(block1, block2->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block3, block2->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block2, block1->hdr.prev);

  /* postprocess */
  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

TEST(memory, cm_mem_del_from_root_set__delete_tail)
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
  TEST_ASSERT_EQUAL_PTR(block3, list_head);

  TEST_ASSERT_EQUAL_PTR(block2, block3->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block3->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(NULL, block2->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block3, block2->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

TEST(memory, scm_mem_del_from_root_set__delete_head)
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
  TEST_ASSERT_EQUAL_PTR(block2, list_head);

  TEST_ASSERT_EQUAL_PTR(block1, block2->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block2->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block2, block1->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

TEST(memory, scm_mem_del_from_root_set__delete_middle)
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
  TEST_ASSERT_EQUAL_PTR(block3, list_head);

  TEST_ASSERT_EQUAL_PTR(block1, block3->hdr.next);
  TEST_ASSERT_EQUAL_PTR(NULL, block3->hdr.prev);

  TEST_ASSERT_EQUAL_PTR(NULL, block1->hdr.next);
  TEST_ASSERT_EQUAL_PTR(block3, block1->hdr.prev);

  scm_mem_root_block_free(block1);
  scm_mem_root_block_free(block2);
  scm_mem_root_block_free(block3);
}

TEST(memory, scm_mem_alloc_heap)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_not_null_p(obj));
  TEST_ASSERT(scm_obj_type_p(obj, &type));
  TEST_ASSERT(scm_obj_mem_managed_p(obj));
  TEST_ASSERT_EQUAL_INT(1, ((StubObj *)obj)->nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  in_to_heap = false;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    if (scm_mem_heap_block_has_obj_p(block, obj))
      in_to_heap = true;
  }

  TEST_ASSERT_TRUE(in_to_heap);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_alloc_heap__alignment)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_not_null_p(obj1));
  TEST_ASSERT(scm_obj_type_p(obj1, &type));
  TEST_ASSERT(scm_obj_mem_managed_p(obj1));

  TEST_ASSERT_TRUE(scm_obj_not_null_p(obj2));
  TEST_ASSERT(scm_obj_type_p(obj2, &type));
  TEST_ASSERT(scm_obj_mem_managed_p(obj2));

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_alloc_root)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_not_null_p(obj));
  TEST_ASSERT(scm_obj_type_p(obj, &type));
  TEST_ASSERT_EQUAL_INT(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);
  TEST_ASSERT_EQUAL_UINT(scm_mem_root_block_object(mem->roots), obj);

  TEST_ASSERT_EQUAL_INT(1, ((StubObj *)obj)->nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  scm_mem_free_root(mem, obj);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, test_scm_mem_free_root)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[obj_num].nr_call_accept_func_weak);

  TEST_ASSERT_NULL(mem->roots);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_gc_start__not_scavenged)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_not_null_p(STUB_OBJ(root_obj)->obj));
  TEST_ASSERT_TRUE(scm_obj_not_null_p(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_gc_start__scavenged)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_not_null_p(STUB_OBJ(root_obj)->obj));
  TEST_ASSERT_TRUE(scm_obj_null_p(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, test_scm_mem_gc_start__referred_by_two_objects)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_same_instance_p(STUB_OBJ(root_obj1)->obj,
                                           STUB_OBJ(root_obj2)->obj));

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[root_obj2_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_gc_start__root_obj_referred_by_heap_obj)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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

  TEST_ASSERT_TRUE(scm_obj_same_instance_p(root_obj, STUB_OBJ(heap_obj)->obj));

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[root_obj_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj_num].nr_call_accept_func_weak);

  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_gc_start__weak_reference_refer_to_obj_not_scavenged)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_same_instance_p(STUB_OBJ(root_obj2)->obj,
                                           STUB_OBJ(heap_obj1)->weak_obj));


  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[root_obj2_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);


  /* postprocess */
  scm_mem_end(mem);
}

TEST(memory, scm_mem_gc_start__weak_reference_refer_to_obj_scavenged)
{
  ScmTypeInfo type = {
    .name                = "test",
    .flags               = 0,
    .obj_print_func      = NULL,
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
  TEST_ASSERT_TRUE(scm_obj_null_p(STUB_OBJ(heap_obj1)->weak_obj));

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[root_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[root_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[root_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj1_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj1_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(1,
                        stub_obj_table[heap_obj1_num].nr_call_accept_func_weak);

  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_ini_func);
  TEST_ASSERT_EQUAL_INT(1, stub_obj_table[heap_obj2_num].nr_call_fin_func);
  TEST_ASSERT_EQUAL_INT(0, stub_obj_table[heap_obj2_num].nr_call_accept_func);
  TEST_ASSERT_EQUAL_INT(0,
                        stub_obj_table[heap_obj2_num].nr_call_accept_func_weak);


  /* postprocess */
  scm_mem_end(mem);
}
