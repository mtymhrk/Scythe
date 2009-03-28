#include <cutter.h>

#include "memory.h"

void
test_new_mem_block(void)
{
  ScmMemHeapBlock *block;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  cut_assert_not_null(block);
  cut_assert_null(SCM_MEM_HEAP_BLOCK_NEXT(block));
  cut_assert_null(SCM_MEM_HEAP_BLOCK_PREV(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(0, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_not_null(SCM_MEM_HEAP_BLOCK_HEAD(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_FREE_PTR(block)
                  == SCM_MEM_HEAP_BLOCK_HEAD(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
  cut_assert_null(block);
}

void
test_mem_block_allocated(void)
{
  ScmMemHeapBlock *block;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(256, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(768, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_HEAD(block) + 256
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));


  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 768);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(0, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_HEAD(block) + 1024
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);  
}

void
test_mem_block_deallocated(void)
{
  ScmMemHeapBlock *block;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, 256);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(256, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(768, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_HEAD(block) + 256
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, 256);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(0, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_HEAD(block)
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_mem_block_ptr_is_allocated(void)
{
  ScmMemHeapBlock *block;
  uint8_t *ptr;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block);
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + 128;
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + 255;
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + 256;
  cut_assert_false(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_mem_block_clean(void)
{
  ScmMemHeapBlock *block;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  SCM_MEM_HEAP_BLOCK_CLEAN(block);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert_equal_uint(0, SCM_MEM_HEAP_BLOCK_USED(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_FREE(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_FREE_PTR(block)
                  == SCM_MEM_HEAP_BLOCK_HEAD(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_mem_block_for_each_obj(void)
{
  ScmMemHeapBlock *block;
  SCM_OBJ_TYPE_T types[] = { SCM_OBJ_TYPE_PAIR,
                             SCM_OBJ_TYPE_STRING,
                             SCM_OBJ_TYPE_SYMBOL};
  ScmObj allocated[sizeof(types)/sizeof(types[0])];
  ScmObj obj;
  unsigned int i;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  for (i = 0; i < sizeof(types)/sizeof(types[0]); i++) {
    allocated[i] = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block));
    size_t s = SCM_TYPE_INFO_OBJ_SIZE(types[i]);

    scm_obj_init(allocated[i], types[i]);
    SCM_MEM_HEAP_BLOCK_ALLOCATED(block, s);
  }

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
    cut_assert_true(allocated[i] == obj);
    cut_assert_equal_uint(types[i], scm_obj_type(obj));
    i++;
  };

  cut_assert_equal_uint(3, i);

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_mem_block_for_each_obj_deallocated_last_obj(void)
{
  ScmMemHeapBlock *block;
  SCM_OBJ_TYPE_T types[] = { SCM_OBJ_TYPE_PAIR,
                             SCM_OBJ_TYPE_STRING,
                             SCM_OBJ_TYPE_SYMBOL};
  ScmObj allocated[sizeof(types)/sizeof(types[0])];
  ScmObj obj;
  unsigned int i;
  size_t s;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  for (i = 0; i < sizeof(types)/sizeof(types[0]); i++) {
    allocated[i] = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block));
    size_t s = SCM_TYPE_INFO_OBJ_SIZE(types[i]);

    scm_obj_init(allocated[i], types[i]);
    SCM_MEM_HEAP_BLOCK_ALLOCATED(block, s);
  }

  s = SCM_TYPE_INFO_OBJ_SIZE(types[i - 1]);
  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, s);

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
    cut_assert_true(allocated[i] == obj);
    cut_assert_equal_uint(types[i], scm_obj_type(obj));
    i++;
  };

  cut_assert_equal_uint(2, i);

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_mem_block_is_obj_in_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmObj obj_in_blk1, obj_in_blk2;  
  size_t s;

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);

  obj_in_blk1 = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block1));
  scm_obj_init(obj_in_blk1, SCM_OBJ_TYPE_PAIR);
  s = SCM_TYPE_INFO_OBJ_SIZE(SCM_OBJ_TYPE_PAIR);
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block1, s);

  obj_in_blk2 = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block2));
  scm_obj_init(obj_in_blk2, SCM_OBJ_TYPE_PAIR);
  s = SCM_TYPE_INFO_OBJ_SIZE(SCM_OBJ_TYPE_PAIR);
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block2, s);

  cut_assert_true(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block1, obj_in_blk1));
  cut_assert_false(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block2, obj_in_blk1));

  cut_assert_false(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block1, obj_in_blk2));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block2, obj_in_blk2));

  SCM_MEM_HEAP_DELEATE_BLOCK(block1);
  SCM_MEM_HEAP_DELEATE_BLOCK(block2);
}

void
test_new_mem_heap_have_no_block(void)
{
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  cut_assert_not_null(heap);
  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_add_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);

  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block2);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_del_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);
 
  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  SCM_MEM_HEAP_DEL_BLOCK(heap);

  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DEL_BLOCK(heap);

  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_new_mem_heap_have_block(void)
{
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  cut_assert_not_null(heap);
  cut_assert_not_null(heap->head);
  cut_assert_not_null(heap->tail);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  cut_assert_equal_int(1024, SCM_MEM_HEAP_BLOCK_SIZE(heap->head));
  cut_assert_equal_int(1024, SCM_MEM_HEAP_BLOCK_SIZE(heap->tail));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_shift(void)
{
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_SHIFT(heap);

  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_NEXT(heap->head));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_SHIFT(heap);

  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_unshift(void)
{
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_SHIFT(heap);

  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_UNSHIFT(heap);

  cut_assert_true(heap->current == heap->tail);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_UNSHIFT(heap);

  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_PREV(heap->tail));
  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_UNSHIFT(heap);

  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_rewind(void)
{
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_REWIND(heap);

  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));
  
  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_REWIND(heap);

  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_del_current_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);
 
  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_DEL_BLOCK(heap);
  
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));
  
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_alloc(void)
{
  ScmMemHeap *heap;
  ScmObj obj;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  SCM_MEM_HEAP_ALLOC(heap, 256, &obj);

  cut_assert_not_null(obj);
  cut_assert_equal_uint(256, SCM_MEM_HEAP_BLOCK_USED(heap->head));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_alloc_next_block(void)
{
  ScmMemHeap *heap;
  ScmObj obj1, obj2;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  SCM_MEM_HEAP_ALLOC(heap, 512, &obj1);
  SCM_MEM_HEAP_ALLOC(heap, 1024, &obj2);

  cut_assert_not_null(obj1);
  cut_assert_not_null(obj2);

  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_NEXT(heap->head));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_USED(heap->current));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_alloc_fail_to_allocate(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmObj obj;

  SCM_MEM_HEAP_NEW_HEAP(heap, 1, 1024);

  SCM_MEM_HEAP_ALLOC(heap, 256, &obj);

  cut_assert_not_null(obj);

  SCM_MEM_HEAP_ALLOC(heap, 512, &obj);

  cut_assert_not_null(obj);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_ALLOC(heap, 512, &obj);

  cut_assert_null(obj);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);  
}

void
test_mem_heap_cancel_alloc(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmObj obj;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_ALLOC(heap, 256, &obj);
  
  cut_assert_not_null(obj);

  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 256);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);  
}

void
test_mem_heap_cancel_alloc_not_allocated(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 256);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);  
}

void
test_mem_heap_cancel_alloc_shoud_unshift(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  ScmObj obj;

  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);
  SCM_MEM_HEAP_ALLOC(heap, 768, &obj);
  
  cut_assert_not_null(obj);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_ALLOC(heap, 768, &obj);

  cut_assert_not_null(obj);

  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 768);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);  
}

void
test_mem_heap_for_each_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    SCM_MEM_HEAP_NEW_BLOCK(blocks[i], 1024);
    SCM_MEM_HEAP_ADD_BLOCK(heap, blocks[i]);
  }

  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);
    i++;
  }

  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]), i);

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_mem_heap_for_each_block_delete_last_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    SCM_MEM_HEAP_NEW_BLOCK(blocks[i], 1024);
    SCM_MEM_HEAP_ADD_BLOCK(heap, blocks[i]);
  }

  SCM_MEM_HEAP_DEL_BLOCK(heap);

  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);
    i++;
  }

  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]) - 1, i);

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}
