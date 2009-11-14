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

  assert(SCM_OBJ_IS_NOT_NULL(obj));
  assert(SCM_OBJ_IS_NOT_NULL(mem));

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

  assert(SCM_OBJ_IS_NOT_NULL(obj));

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


void
test_scm_new_mem_block(void)
{
  ScmMemHeapBlock *block;

  /* action */
  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  /* postcondition check */
  cut_assert_not_null(block);
  cut_assert_null(SCM_MEM_HEAP_BLOCK_NEXT(block));
  cut_assert_null(SCM_MEM_HEAP_BLOCK_PREV(block));
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert(/* 0 <= SCM_MEM_HEAP_BLOCK_USED(block) && */
             SCM_MEM_HEAP_BLOCK_USED(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < SCM_MEM_HEAP_BLOCK_FREE(block)
             && SCM_MEM_HEAP_BLOCK_FREE(block) <= 1024);
  cut_assert_not_null(SCM_MEM_HEAP_BLOCK_HEAD(block));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_FREE_PTR(block)
                  == SCM_MEM_HEAP_BLOCK_HEAD(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
  cut_assert_null(block);
}

void
test_scm_mem_block_allocated(void)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  SCM_MEM_HEAP_NEW_BLOCK(block, 10240);

  /* action */
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(10240, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert(SCM_MEM_ALIGN_SIZE(256) <= SCM_MEM_HEAP_BLOCK_USED(block)
             && (SCM_MEM_HEAP_BLOCK_USED(block)
                 < SCM_MEM_ALIGN_SIZE(256) + SCM_MEM_ALIGN_BYTE));
 cut_assert((10240 - SCM_MEM_ALIGN_SIZE(256) - SCM_MEM_ALIGN_BYTE
              < SCM_MEM_HEAP_BLOCK_FREE(block))
             && (SCM_MEM_HEAP_BLOCK_FREE(block)
                 <= 10240 - SCM_MEM_ALIGN_SIZE(256)));
 cut_assert_true((SCM_MEM_HEAP_BLOCK_HEAD(block)
                  + SCM_MEM_ALIGN_SIZE(256))
                 == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  /* action */
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 768);

  /* postcondition check */
  cut_assert_equal_uint(10240, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert((SCM_MEM_ALIGN_SIZE(256) + SCM_MEM_ALIGN_SIZE(768)
              <= SCM_MEM_HEAP_BLOCK_USED(block))
             && (SCM_MEM_HEAP_BLOCK_USED(block)
                 < (SCM_MEM_ALIGN_SIZE(256)
                    + SCM_MEM_ALIGN_SIZE(768) + SCM_MEM_ALIGN_BYTE)));
  cut_assert(((10240 - SCM_MEM_ALIGN_SIZE(256) - SCM_MEM_ALIGN_SIZE(768)
               - SCM_MEM_ALIGN_BYTE)
              < SCM_MEM_HEAP_BLOCK_FREE(block))
             && (SCM_MEM_HEAP_BLOCK_FREE(block)
                 <= (10240 - SCM_MEM_ALIGN_SIZE(256)
                     - SCM_MEM_ALIGN_SIZE(768))));
  cut_assert_true((SCM_MEM_HEAP_BLOCK_HEAD(block)
                   + SCM_MEM_ALIGN_SIZE(256) + SCM_MEM_ALIGN_SIZE(768))
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  /* postprocess */
  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_deallocated(void)
{
  ScmMemHeapBlock *block;

  /* preprocess */
  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  /* action */
  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert((SCM_MEM_ALIGN_SIZE(256) <= SCM_MEM_HEAP_BLOCK_USED(block))
             && (SCM_MEM_HEAP_BLOCK_USED(block)
                 < SCM_MEM_ALIGN_SIZE(256) + SCM_MEM_ALIGN_BYTE));
  cut_assert((1024 - SCM_MEM_ALIGN_SIZE(256) - SCM_MEM_ALIGN_BYTE
              < SCM_MEM_HEAP_BLOCK_FREE(block))
             && (SCM_MEM_HEAP_BLOCK_FREE(block) <= 1024 - SCM_MEM_ALIGN_SIZE(256)));

  cut_assert_true((SCM_MEM_HEAP_BLOCK_HEAD(block)
                   + SCM_MEM_ALIGN_SIZE(256))
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  /* action */
  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, 256);

  /* postcondition check */
  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert(/* 0 <= SCM_MEM_HEAP_BLOCK_USED(block) && */
             SCM_MEM_HEAP_BLOCK_USED(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < SCM_MEM_HEAP_BLOCK_FREE(block)
             && SCM_MEM_HEAP_BLOCK_FREE(block) <= 1024);
  cut_assert_true(SCM_MEM_HEAP_BLOCK_HEAD(block)
                  == SCM_MEM_HEAP_BLOCK_FREE_PTR(block));

  /* postprocess */
  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_ptr_is_allocated(void)
{
  ScmMemHeapBlock *block;
  uint8_t *ptr;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block);
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + SCM_MEM_ALIGN_SIZE(128);
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + SCM_MEM_ALIGN_SIZE(256) - 1;
  cut_assert_true(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  ptr = SCM_MEM_HEAP_BLOCK_HEAD(block) + SCM_MEM_ALIGN_SIZE(256);
  cut_assert_false(SCM_MEM_HEAP_BLOCK_PTR_IS_ALLOCATED(block, ptr));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_clean(void)
{
  ScmMemHeapBlock *block;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  SCM_MEM_HEAP_BLOCK_ALLOCATED(block, 256);

  SCM_MEM_HEAP_BLOCK_CLEAN(block);

  cut_assert_equal_uint(1024, SCM_MEM_HEAP_BLOCK_SIZE(block));
  cut_assert(/* 0 <= SCM_MEM_HEAP_BLOCK_USED(block) && */
             SCM_MEM_HEAP_BLOCK_USED(block) < SCM_MEM_ALIGN_BYTE);
  cut_assert(1024 - SCM_MEM_ALIGN_BYTE < SCM_MEM_HEAP_BLOCK_FREE(block)
             && SCM_MEM_HEAP_BLOCK_FREE(block) <= 1024);
  cut_assert_true(SCM_MEM_HEAP_BLOCK_FREE_PTR(block)
                  == SCM_MEM_HEAP_BLOCK_HEAD(block));

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_for_each_obj(void)
{
  ScmMemHeapBlock *block;
  ScmTypeInfo *types[] = { &SCM_PAIR_TYPE_INFO,
                           &SCM_STRING_TYPE_INFO,
                           &SCM_SYMBOL_TYPE_INFO };
  ScmObj allocated[sizeof(types)/sizeof(types[0])];
  ScmObj obj;
  unsigned int i;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  for (i = 0; i < sizeof(types)/sizeof(types[0]); i++) {
    allocated[i] = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block));
    size_t s = SCM_MEM_ALIGN_SIZE(SCM_TYPE_INFO_OBJ_SIZE(types[i]));

    scm_obj_init(allocated[i], types[i]);
    SCM_MEM_HEAP_BLOCK_ALLOCATED(block, s);
  }

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
    cut_assert_true(allocated[i] == obj);
    cut_assert(SCM_TYPE_INFO_IS_SAME(types[i], SCM_OBJ_TYPE(obj)));
    i++;
  };

  cut_assert_equal_uint(3, i);

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_for_each_obj_deallocated_last_obj(void)
{
  ScmMemHeapBlock *block;
  ScmTypeInfo *types[] = { &SCM_PAIR_TYPE_INFO,
                           &SCM_STRING_TYPE_INFO,
                           &SCM_SYMBOL_TYPE_INFO };
  ScmObj allocated[sizeof(types)/sizeof(types[0])];
  ScmObj obj;
  unsigned int i;
  size_t s;

  SCM_MEM_HEAP_NEW_BLOCK(block, 1024);

  for (i = 0; i < sizeof(types)/sizeof(types[0]); i++) {
    allocated[i] = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block));
    size_t s = SCM_MEM_ALIGN_SIZE(SCM_TYPE_INFO_OBJ_SIZE(types[i]));

    scm_obj_init(allocated[i], types[i]);
    SCM_MEM_HEAP_BLOCK_ALLOCATED(block, s);
  }

  s = SCM_MEM_ALIGN_SIZE(SCM_TYPE_INFO_OBJ_SIZE(types[i - 1]));
  SCM_MEM_HEAP_BLOCK_DEALLOCATED(block, s);

  i = 0;
  SCM_MEM_HEAP_BLOCK_FOR_EACH_OBJ(block, obj) {
    cut_assert_true(allocated[i] == obj);
    cut_assert(SCM_TYPE_INFO_IS_SAME(types[i], SCM_OBJ_TYPE(obj)));
    i++;
  };

  cut_assert_equal_uint(2, i);

  SCM_MEM_HEAP_DELEATE_BLOCK(block);
}

void
test_scm_mem_block_is_obj_in_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmObj obj_in_blk1, obj_in_blk2;  
  size_t s;

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);

  obj_in_blk1 = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block1));
  scm_obj_init(obj_in_blk1, &SCM_PAIR_TYPE_INFO);
  s = SCM_MEM_ALIGN_SIZE(SCM_TYPE_INFO_OBJ_SIZE(&SCM_PAIR_TYPE_INFO));
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block1, s);

  obj_in_blk2 = SCM_OBJ(SCM_MEM_HEAP_BLOCK_FREE_PTR(block2));
  scm_obj_init(obj_in_blk2, &SCM_PAIR_TYPE_INFO);
  s = SCM_MEM_ALIGN_SIZE(SCM_TYPE_INFO_OBJ_SIZE(&SCM_PAIR_TYPE_INFO));
  SCM_MEM_HEAP_BLOCK_ALLOCATED(block2, s);

  cut_assert_true(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block1, obj_in_blk1));
  cut_assert_false(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block2, obj_in_blk1));

  cut_assert_false(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block1, obj_in_blk2));
  cut_assert_true(SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block2, obj_in_blk2));

  SCM_MEM_HEAP_DELEATE_BLOCK(block1);
  SCM_MEM_HEAP_DELEATE_BLOCK(block2);
}

void
test_scm_new_mem_heap_have_no_block(void)
{
  ScmMemHeap *heap;

  /* process */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  /* postcondition check */
  cut_assert_not_null(heap);
  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_null(heap->weak_list);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_add_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);

  /* precondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block2);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_del_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);

  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  /* action */
  SCM_MEM_HEAP_DEL_BLOCK(heap);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_DEL_BLOCK(heap);

  /* postcondition check */
  cut_assert_null(heap->head);
  cut_assert_null(heap->tail);
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_new_mem_heap_have_block(void)
{
  ScmMemHeap *heap;

  /* action */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  /* postcondition check */
  cut_assert_not_null(heap);
  cut_assert_not_null(heap->head);
  cut_assert_not_null(heap->tail);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  cut_assert_equal_int(1024, SCM_MEM_HEAP_BLOCK_SIZE(heap->head));
  cut_assert_equal_int(1024, SCM_MEM_HEAP_BLOCK_SIZE(heap->tail));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_shift(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  /* precondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_SHIFT(heap);

  /* postcondition check */
  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_NEXT(heap->head));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_SHIFT(heap);

  /* postcondition check */
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_unshift(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  /* precondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* preprocess */
  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_SHIFT(heap);

  /* precondition check */
  cut_assert_null(heap->current);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_UNSHIFT(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->tail);
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(2, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_UNSHIFT(heap);

  /* postcondition check */
  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_PREV(heap->tail));
  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* action */
  SCM_MEM_HEAP_UNSHIFT(heap);

  /* postcondition check */
  cut_assert_true(heap->current == (heap)->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_rewind(void)
{
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);
  SCM_MEM_HEAP_SHIFT(heap);

  /* action */
  SCM_MEM_HEAP_REWIND(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* preprocess */
  SCM_MEM_HEAP_SHIFT(heap);
  SCM_MEM_HEAP_SHIFT(heap);

  /* action */
  SCM_MEM_HEAP_REWIND(heap);

  /* postcondition check */
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_del_current_block(void)
{
  ScmMemHeapBlock *block1;
  ScmMemHeapBlock *block2;
  ScmMemHeap *heap;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  SCM_MEM_HEAP_NEW_BLOCK(block1, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block1);

  SCM_MEM_HEAP_NEW_BLOCK(block2, 1024);
  SCM_MEM_HEAP_ADD_BLOCK(heap, block2);

  SCM_MEM_HEAP_SHIFT(heap);

  /* action */
  SCM_MEM_HEAP_DEL_BLOCK(heap);

  /* postcondition check */
  cut_assert_true(heap->head == block1);
  cut_assert_true(heap->tail == block1);
  cut_assert_true(heap->current == heap->head);
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(heap));
  cut_assert_equal_int(0, SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* preprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_alloc(void)
{
  ScmMemHeap *heap;
  void *ptr;

  /* preproces */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  /* action */
  SCM_MEM_HEAP_ALLOC(heap, 256, &ptr);

  /* postcondition check */
  cut_assert_not_null(ptr);
  cut_assert_equal_uint(0u, (uintptr_t)ptr % SCM_MEM_ALIGN_BYTE);
  cut_assert(SCM_MEM_ALIGN_SIZE(256) <= SCM_MEM_HEAP_BLOCK_USED(heap->head)
             && (SCM_MEM_HEAP_BLOCK_USED(heap->head)
                 < SCM_MEM_ALIGN_SIZE(256) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_alloc_aligned(void)
{
  ScmMemHeap *heap;
  void *ptr1, *ptr2;

  /* preproces */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  /* action */
  SCM_MEM_HEAP_ALLOC(heap, 17, &ptr1);
  SCM_MEM_HEAP_ALLOC(heap, 16, &ptr2);

  /* postcondition check */
  cut_assert_not_null(ptr1);
  cut_assert_equal_uint(0u, (uintptr_t)ptr1 % SCM_MEM_ALIGN_BYTE);
  cut_assert_not_null(ptr2);
  cut_assert_equal_uint(0u, (uintptr_t)ptr2 % SCM_MEM_ALIGN_BYTE);

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_alloc_next_block(void)
{
  ScmMemHeap *heap;
  void *ptr1, *ptr2;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);
  SCM_MEM_HEAP_ALLOC(heap, 512, &ptr1);

  /* action */
  SCM_MEM_HEAP_ALLOC(heap, 768, &ptr2);

  /* postcondition check */
  cut_assert_not_null(ptr1);
  cut_assert_not_null(ptr2);

  cut_assert_true(heap->current == SCM_MEM_HEAP_BLOCK_NEXT(heap->head));
  cut_assert((SCM_MEM_ALIGN_SIZE(768)
              <= SCM_MEM_HEAP_BLOCK_USED(heap->current))
             && (SCM_MEM_HEAP_BLOCK_USED(heap->current)
                 < SCM_MEM_ALIGN_SIZE(768) + SCM_MEM_ALIGN_BYTE));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_alloc_fail_to_allocate(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  void *ptr;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 1, 1024);

  SCM_MEM_HEAP_ALLOC(heap, 256, &ptr);

  cut_assert_not_null(ptr);

  SCM_MEM_HEAP_ALLOC(heap, 512, &ptr);

  cut_assert_not_null(ptr);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  /* action */
  SCM_MEM_HEAP_ALLOC(heap, 512, &ptr);

  /* postprocess check */
  cut_assert_null(ptr);

  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_cancel_alloc(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  void *ptr;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_ALLOC(heap, 256, &ptr);

  cut_assert_not_null(ptr);

  /* action */
  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 256);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
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
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  /* action */
  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 256);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_cancel_alloc_shoud_unshift(void)
{
  ScmMemHeapBlock *expected_current;
  size_t expected_used;
  int expected_nr_free_block;
  int expected_nr_used_block;
  ScmMemHeap *heap;
  void *ptr;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 2, 1024);
  SCM_MEM_HEAP_ALLOC(heap, 768, &ptr);

  cut_assert_not_null(ptr);

  expected_current = heap->current;
  expected_used = SCM_MEM_HEAP_BLOCK_USED(heap->current);
  expected_nr_free_block = SCM_MEM_HEAP_NR_FREE_BLOCK(heap);
  expected_nr_used_block = SCM_MEM_HEAP_NR_USED_BLOCK(heap);

  SCM_MEM_HEAP_ALLOC(heap, 768, &ptr);

  cut_assert_not_null(ptr);

  /* action */
  SCM_MEM_HEAP_CANCEL_ALLOC(heap, 768);

  /* postcondition check */
  cut_assert_true(heap->current == expected_current);
  cut_assert_equal_uint(expected_used, SCM_MEM_HEAP_BLOCK_USED(heap->current));
  cut_assert_equal_int(expected_nr_free_block,
                       SCM_MEM_HEAP_NR_FREE_BLOCK(heap));
  cut_assert_equal_int(expected_nr_used_block,
                       SCM_MEM_HEAP_NR_USED_BLOCK(heap));

  /* postprocess */
  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_for_each_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    SCM_MEM_HEAP_NEW_BLOCK(blocks[i], 1024);
    SCM_MEM_HEAP_ADD_BLOCK(heap, blocks[i]);
  }

  /* action */
  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);       /* invariant check */
    i++;
  }

  /* postcondition check */
  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]), i);

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_heap_for_each_block_delete_last_block(void)
{
  ScmMemHeapBlock *blocks[3];
  ScmMemHeapBlock *block;
  ScmMemHeap *heap;
  size_t i;

  /* preprocess */
  SCM_MEM_HEAP_NEW_HEAP(heap, 0, 0);

  for (i = 0; i < sizeof(blocks)/sizeof(blocks[0]); i++) {
    SCM_MEM_HEAP_NEW_BLOCK(blocks[i], 1024);
    SCM_MEM_HEAP_ADD_BLOCK(heap, blocks[i]);
  }

  SCM_MEM_HEAP_DEL_BLOCK(heap);

  /* action */
  i = 0;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(heap, block) {
    cut_assert_true(blocks[i] == block);      /* invariant check */
    i++;
  }

  /* postcondition check */
  cut_assert_equal_uint(sizeof(blocks)/sizeof(blocks[0]) - 1, i);

  SCM_MEM_HEAP_DELETE_HEAP(heap);
}

void
test_scm_mem_construct(void)
{
  ScmMem *mem;

  /* action */
  mem = scm_mem_construct();

  /* postcondition check */
  cut_assert_not_null(mem);
  cut_assert_not_null(mem->to_obj_tbl);
  cut_assert_not_null(mem->from_obj_tbl);
  cut_assert_not_null(mem->to_heap);
  cut_assert_not_null(mem->from_heap);
  cut_assert_not_null(mem->persistent);


  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(mem->to_heap));
  cut_assert_equal_int(1, SCM_MEM_HEAP_NR_BLOCK(mem->from_heap));
  cut_assert_equal_uint(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                        SCM_MEM_HEAP_BLOCK_SIZE(mem->to_heap->head));
  cut_assert_equal_uint(SCM_MEM_HEAP_INIT_BLOCK_SIZE,
                        SCM_MEM_HEAP_BLOCK_SIZE(mem->from_heap->head));

  /* preprocess */
  scm_mem_destruct(mem);
}


void
test_scm_mem_alloc_size_in_heap__size_is_smaller_then_forward(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(ScmForward) - 1,   /* obj_size             */
    NULL,                     /* gc_ini_func          */
    NULL,                     /* gc_fin_func          */
    NULL,                     /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type);

  /* postcondition check */
  cut_assert_equal_uint(sizeof(ScmForward), actual_size);
}

void
test_scm_mem_alloc_size_in_heap__size_is_greater_then_forward(void)
{
  size_t expected_size = sizeof(ScmForward) + 1;
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    expected_size,            /* obj_size             */
    NULL,                     /* gc_ini_func          */
    NULL,                     /* gc_fin_func          */
    NULL,                     /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type);

  /* postcondition check */
  cut_assert_equal_uint(expected_size, actual_size);
}

void
test_scm_mem_alloc_size_in_heap__obj_has_weak_ref(void)
{
  ScmTypeInfo type = {
    NULL,                          /* pp_func              */
    sizeof(StubObj),               /* obj_size             */
    NULL,                          /* gc_ini_func          */
    NULL,                          /* gc_fin_func          */
    NULL,                          /* gc_accept_func       */
    stub_obj_gc_accept_func_weak   /* gc_accpet_func_weak  */
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_heap(&type);

  /* postcondition check */
  cut_assert_equal_uint(sizeof(StubObj) + sizeof(void *), actual_size);
}

void
test_scm_mem_alloc_size_in_root__size_is_smaller_than_atom(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(ScmMMObj) - 1,     /* obj_size             */
    NULL,                     /* gc_ini_func          */
    NULL,                     /* gc_fin_func          */
    NULL,                     /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_root(&type);

  /* postcondition check */
  cut_assert_equal_uint(sizeof(ScmMMObj) + sizeof(ScmMemRootBlock),
                        actual_size);
}

void
test_scm_mem_alloc_size_in_root__size_is_greater_than_atom(void)
{
  size_t obj_size = sizeof(ScmMMObj) * 2;
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    obj_size,                 /* obj_size             */
    NULL,                     /* gc_ini_func          */
    NULL,                     /* gc_fin_func          */
    NULL,                     /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  size_t actual_size;

  /* action */
  actual_size = scm_mem_alloc_size_in_root(&type);

  /* postcondition check */
  cut_assert_equal_uint(obj_size + sizeof(ScmMemRootBlock), actual_size);
}

void
test_scm_mem_root_block_new(void)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;
  unsigned int shift;

  /* action */
  SCM_MEM_ROOT_BLOCK_NEW(&block, 1024);

  obj = SCM_MEM_ROOT_BLOCK_OBJECT(block);
  shift = SCM_MEM_ROOT_BLOCK_OBJ_SHIFT_BYTE(obj);

  /* postcondition check */
  cut_assert_not_null(block);
  cut_assert_null(SCM_MEM_ROOT_BLOCK_NEXT(block));
  cut_assert_null(SCM_MEM_ROOT_BLOCK_PREV(block));
  cut_assert_equal_uint(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);
  cut_assert((uintptr_t)block + sizeof(ScmMemRootBlockHdr) + shift
             == (uintptr_t)obj);

  /* postprocess */
  SCM_MEM_ROOT_BLOCK_FREE(block);
}

void
test_scm_mem_root_block_obj_header(void)
{
  ScmMemRootBlock *block = NULL;
  ScmObj obj;

  /* action */
  SCM_MEM_ROOT_BLOCK_NEW(&block, 1024);

  obj = SCM_MEM_ROOT_BLOCK_OBJECT(block);

  /* postcondition check */
  cut_assert_equal_pointer(block, SCM_MEM_ROOT_BLOCK_OBJ_HEADER(obj));

  /* postprocess */
  SCM_MEM_ROOT_BLOCK_FREE(block);
}

void
test_scm_mem_add_to_root_set(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  SCM_MEM_ROOT_BLOCK_NEW(&block1, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block2, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block3, 1024);

  /* action */
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block1);

  /* postcondition check */
  cut_assert_equal_pointer(block1, list_head);
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block1));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block1));


  /* action */
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block2);

  /* postcondition check */
  cut_assert_equal_pointer(block2, list_head);

  cut_assert_equal_pointer(block1, SCM_MEM_ROOT_BLOCK_NEXT(block2));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block2));

  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block1));
  cut_assert_equal_pointer(block2, SCM_MEM_ROOT_BLOCK_PREV(block1));


  /* action */
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block3);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block2, SCM_MEM_ROOT_BLOCK_NEXT(block3));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block3));

  cut_assert_equal_pointer(block1, SCM_MEM_ROOT_BLOCK_NEXT(block2));
  cut_assert_equal_pointer(block3, SCM_MEM_ROOT_BLOCK_PREV(block2));

  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block1));
  cut_assert_equal_pointer(block2, SCM_MEM_ROOT_BLOCK_PREV(block1));

  /* postprocess */
  SCM_MEM_ROOT_BLOCK_FREE(block1);
  SCM_MEM_ROOT_BLOCK_FREE(block2);
  SCM_MEM_ROOT_BLOCK_FREE(block3);
}

void
test_scm_mem_del_from_root_set__delte_tail(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  SCM_MEM_ROOT_BLOCK_NEW(&block1, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block2, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block3, 1024);

  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block1);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block2);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block3);

  /* action */
  SCM_MEM_DEL_FROM_ROOT_SET(&list_head, block1);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block2, SCM_MEM_ROOT_BLOCK_NEXT(block3));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block3));

  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block2));
  cut_assert_equal_pointer(block3, SCM_MEM_ROOT_BLOCK_PREV(block2));

  SCM_MEM_ROOT_BLOCK_FREE(block1);
  SCM_MEM_ROOT_BLOCK_FREE(block2);
  SCM_MEM_ROOT_BLOCK_FREE(block3);
}

void
test_scm_mem_del_from_root_set__delte_head(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  SCM_MEM_ROOT_BLOCK_NEW(&block1, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block2, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block3, 1024);

  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block1);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block2);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block3);

  /* action */
  SCM_MEM_DEL_FROM_ROOT_SET(&list_head, block3);

  /* postcondition check */
  cut_assert_equal_pointer(block2, list_head);

  cut_assert_equal_pointer(block1, SCM_MEM_ROOT_BLOCK_NEXT(block2));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block2));

  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block1));
  cut_assert_equal_pointer(block2, SCM_MEM_ROOT_BLOCK_PREV(block1));

  SCM_MEM_ROOT_BLOCK_FREE(block1);
  SCM_MEM_ROOT_BLOCK_FREE(block2);
  SCM_MEM_ROOT_BLOCK_FREE(block3);
}

void
test_scm_mem_del_from_root_set__delte_middle(void)
{
  ScmMemRootBlock *list_head = NULL;
  ScmMemRootBlock *block1 = NULL, *block2 = NULL, *block3;

  /* preprocess */
  SCM_MEM_ROOT_BLOCK_NEW(&block1, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block2, 1024);
  SCM_MEM_ROOT_BLOCK_NEW(&block3, 1024);

  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block1);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block2);
  SCM_MEM_ADD_TO_ROOT_SET(&list_head, block3);

  /* action */
  SCM_MEM_DEL_FROM_ROOT_SET(&list_head, block2);

  /* postcondition check */
  cut_assert_equal_pointer(block3, list_head);

  cut_assert_equal_pointer(block1, SCM_MEM_ROOT_BLOCK_NEXT(block3));
  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_PREV(block3));

  cut_assert_equal_pointer(NULL, SCM_MEM_ROOT_BLOCK_NEXT(block1));
  cut_assert_equal_pointer(block3, SCM_MEM_ROOT_BLOCK_PREV(block1));

  SCM_MEM_ROOT_BLOCK_FREE(block1);
  SCM_MEM_ROOT_BLOCK_FREE(block2);
  SCM_MEM_ROOT_BLOCK_FREE(block3);
}

void
test_scm_mem_alloc_heap(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  bool in_to_heap;
  ScmMemHeapBlock *block;
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;

  /* preprocess */
  mem = scm_mem_construct();

  scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(obj));

  /* action */
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(obj));

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(obj));
  cut_assert(SCM_OBJ_IS_TYPE(obj, &type));
  cut_assert_equal_int(1, ((StubObj *)obj)->nr_call_ini_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_fin_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  in_to_heap = false;
  SCM_MEM_HEAP_FOR_EACH_BLOCK(mem->to_heap, block) {
    if (SCM_MEM_HEAP_BLOCK_IS_OBJ_IN_BLOCK(block, obj))
      in_to_heap = true;
  }

  cut_assert_true(in_to_heap);

  /* postprocess */
  scm_mem_destruct(mem);
}

void
test_scm_mem_alloc_root(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;

  /* preprocess */
  mem = scm_mem_construct();

  /* action */
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(obj));

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(obj));
  cut_assert(SCM_OBJ_IS_TYPE(obj, &type));
  cut_assert_equal_int(0, (uintptr_t)obj % SCM_MEM_ALIGN_BYTE);
  cut_assert_equal_uint(SCM_MEM_ROOT_BLOCK_OBJECT(mem->roots), obj);

  cut_assert_equal_int(1, ((StubObj *)obj)->nr_call_ini_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_fin_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func);
  cut_assert_equal_int(0, ((StubObj *)obj)->nr_call_accept_func_weak);

  scm_mem_free_root(mem, obj);

  /* postprocess */
  scm_mem_destruct(mem);
}

void
test_scm_mem_free_root(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj obj = SCM_OBJ_INIT;
  int obj_num;

  /* preprocess */
  mem = scm_mem_construct();

  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(obj));
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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__not_scavenged(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj)->obj));
  heap_obj1 = STUB_OBJ(root_obj)->obj;
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(heap_obj1)->obj));
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj1_num = STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(STUB_OBJ(root_obj)->obj));
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__scavenged(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj)->obj));
  heap_obj1 = STUB_OBJ(root_obj)->obj;
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(heap_obj1)->obj));
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj1_num = STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->obj)->obj_num;

  STUB_OBJ(heap_obj1)->obj = SCM_OBJ_NULL; /* heap_obj2 become garbage */

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(STUB_OBJ(root_obj)->obj));
  cut_assert_true(SCM_OBJ_IS_NULL(STUB_OBJ(STUB_OBJ(root_obj)->obj)->obj));

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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__referred_by_two_objects(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT, root_obj2 = SCM_OBJ_INIT;
  int root_obj1_num, root_obj2_num, heap_obj_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj1));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj1)->obj));
  root_obj1_num = STUB_OBJ(root_obj1)->obj_num;
  heap_obj_num = STUB_OBJ(STUB_OBJ(root_obj1)->obj)->obj_num;

  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj2));
  /* heap_obj is referred by root_obj1 and root_obj2 */
  STUB_OBJ(root_obj2)->obj = STUB_OBJ(root_obj1)->obj;
  root_obj2_num = STUB_OBJ(root_obj2)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  /* postcondition check */
  cut_assert_true(scm_obj_is_same_instance(STUB_OBJ(root_obj1)->obj,
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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__root_obj_referred_by_heap_obj(void)
{
  ScmTypeInfo type = {
    NULL,                     /* pp_func              */
    sizeof(StubObj),          /* obj_size             */
    stub_obj_gc_init_func,    /* gc_ini_func          */
    stub_obj_gc_fin_func,     /* gc_fin_func          */
    stub_obj_gc_accept_func,  /* gc_accept_func       */
    NULL,                     /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj = SCM_OBJ_INIT, heap_obj = SCM_OBJ_INIT;
  int root_obj_num, heap_obj_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(heap_obj));
  STUB_OBJ(root_obj)->obj = heap_obj;
  STUB_OBJ(heap_obj)->obj = root_obj;   /* heap_obj refers root_obj; */
  root_obj_num = STUB_OBJ(root_obj)->obj_num;
  heap_obj_num = STUB_OBJ(heap_obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  heap_obj = STUB_OBJ(root_obj)->obj;

  cut_assert_true(scm_obj_is_same_instance(root_obj, STUB_OBJ(heap_obj)->obj));

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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__weak_reference_refer_to_obj_not_scavenged(void)
{
  ScmTypeInfo type = {
    NULL,                         /* pp_func              */
    sizeof(StubObj),              /* obj_size             */
    stub_obj_gc_init_func,        /* gc_ini_func          */
    stub_obj_gc_fin_func,         /* gc_fin_func          */
    stub_obj_gc_accept_func,      /* gc_accept_func       */
    stub_obj_gc_accept_func_weak  /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT;
  ScmObj root_obj2 = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj1_num, root_obj2_num,  heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj1));
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj2));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj1)->obj));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj2)->obj));
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
  cut_assert_true(scm_obj_is_same_instance(STUB_OBJ(root_obj2)->obj,
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
  scm_mem_destruct(mem);
}

void
test_scm_mem_gc_start__weak_reference_refer_to_obj_scavenged(void)
{
  ScmTypeInfo type = {
    NULL,                         /* pp_func              */
    sizeof(StubObj),              /* obj_size             */
    stub_obj_gc_init_func,        /* gc_ini_func          */
    stub_obj_gc_fin_func,         /* gc_fin_func          */
    stub_obj_gc_accept_func,      /* gc_accept_func       */
    stub_obj_gc_accept_func_weak  /* gc_accpet_func_weak  */
  };
  ScmMem *mem;
  ScmObj root_obj1 = SCM_OBJ_INIT, heap_obj1 = SCM_OBJ_INIT;
  int root_obj1_num, heap_obj1_num, heap_obj2_num;

  /* preprocess */
  mem = scm_mem_construct();
  scm_mem_alloc_root(mem, &type, SCM_REF_MAKE(root_obj1));
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(root_obj1)->obj));
  heap_obj1 = STUB_OBJ(root_obj1)->obj;
  scm_mem_alloc_heap(mem, &type, SCM_REF_MAKE(STUB_OBJ(heap_obj1)->weak_obj));
  root_obj1_num = STUB_OBJ(root_obj1)->obj_num;
  heap_obj1_num = STUB_OBJ(heap_obj1)->obj_num;
  heap_obj2_num = STUB_OBJ(STUB_OBJ(heap_obj1)->weak_obj)->obj_num;

  /* action */
  scm_mem_gc_start(mem);

  heap_obj1 = STUB_OBJ(root_obj1)->obj;

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NULL(STUB_OBJ(heap_obj1)->weak_obj));

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
  scm_mem_destruct(mem);
}

