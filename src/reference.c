#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "object.h"
#include "api.h"
#include "reference.h"

ScmTypeInfo SCM_REFSTACK_TYPE_INFO = {
  .name                            = "refstack",
  .flags                           = SCM_TYPE_FLG_MMO,
  .pp_func                         = scm_ref_stack_pretty_print,
  .obj_size                        = sizeof(ScmRefStack),
  .gc_ini_func                     = scm_ref_stack_gc_initialize,
  .gc_fin_func                     = scm_ref_stack_gc_finalize,
  .gc_accept_func                  = scm_ref_stack_gc_accept,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

scm_local_inline bool
scm_ref_stack_block_full_p(ScmRefStackBlock *block)
{
  return (block->stack + block->size <= block->sp) ? true : false;
}

scm_local_inline ScmRefStackBlock *
scm_ref_stack_new_block(size_t sz)
{
  ScmRefStackBlock *block;

  scm_assert((SIZE_MAX - sizeof(ScmRefStackBlock)) / sizeof(ScmRef) >= sz);

  block = scm_capi_malloc(sizeof(ScmRefStackBlock) + sizeof(ScmRef) * sz);
  if (block == NULL) return NULL; /* [ERR]: [through] */

  block->next = NULL;
  block->prev = NULL;
  block->size = sz;
  block->sp = block->stack;

  return block;
}

scm_local_inline void
scm_ref_stack_block_push(ScmRefStackBlock *block, ScmRef ref)
{
  *(block->sp++) = ref;
}

scm_local_inline void
scm_ref_stack_block_init_sp(ScmRefStackBlock *block)
{
  block->sp = block->stack;
}

scm_local_func void
scm_ref_stack_add_block(ScmObj stack, ScmRefStackBlock *block)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  if (SCM_REFSTACK(stack)->head == NULL) {
    SCM_REFSTACK(stack)->head = block;
    SCM_REFSTACK(stack)->tail = block;
    SCM_REFSTACK(stack)->current = block;
  }
  else {
    SCM_REFSTACK(stack)->tail->next = block;
    block->next = NULL;
    block->prev = SCM_REFSTACK(stack)->tail;
    SCM_REFSTACK(stack)->tail = block;
  }
}

scm_local_func void
scm_ref_stack_decrease_block(ScmObj stack)
{
  ScmRefStackBlock *block;

  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  block = SCM_REFSTACK(stack)->tail;
  if (block != NULL) {
    SCM_REFSTACK(stack)->tail = block->prev;

    if (SCM_REFSTACK(stack)->tail == NULL)
      SCM_REFSTACK(stack)->head = SCM_REFSTACK(stack)->current = NULL;
    else
      SCM_REFSTACK(stack)->tail->next = NULL;

    if (SCM_REFSTACK(stack)->current == block)
      SCM_REFSTACK(stack)->current = SCM_REFSTACK(stack)->tail;

    scm_capi_free(block);
  }
}

scm_local_inline void
scm_ref_stack_shift_stack_block(ScmObj stack)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  SCM_REFSTACK(stack)->current = SCM_REFSTACK(stack)->current->next;
  if (SCM_REFSTACK(stack)->current != NULL)
    SCM_REFSTACK(stack)->current->sp = SCM_REFSTACK(stack)->current->stack;
}


scm_local_func int
scm_ref_stack_add_new_block(ScmObj stack, size_t size)
{
  ScmRefStackBlock *block;

  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  block = scm_ref_stack_new_block(size);
  if (block == NULL) return -1;

  scm_ref_stack_add_block(stack, block);

  return 0;
}

scm_local_func int
scm_ref_stack_growth_if_needed(ScmObj stack)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  if (scm_ref_stack_block_full_p(SCM_REFSTACK(stack)->current))
    scm_ref_stack_shift_stack_block(stack);

  if (SCM_REFSTACK(stack)->current == NULL) {
    if (scm_ref_stack_add_new_block(stack, SCM_REFSTACK(stack)->tail->size) < 0)
      return -1;

    SCM_REFSTACK(stack)->current = SCM_REFSTACK(stack)->tail;
  }

  return 0;
}

scm_local_func void
scm_ref_stack_decrease_if_possible(ScmObj stack)
{
  ;                             /* TODO: write me */
}

int
scm_ref_stack_initialize(ScmObj stack, size_t size)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  SCM_REFSTACK(stack)->head = NULL;
  SCM_REFSTACK(stack)->tail = NULL;
  SCM_REFSTACK(stack)->current = NULL;
  return scm_ref_stack_add_new_block(stack, size);
}

void
scm_ref_stack_finalize(ScmObj stack)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  while (SCM_REFSTACK(stack)->head != NULL) {
    scm_ref_stack_decrease_block(stack);
  }
}

ScmObj
scm_ref_stack_new(SCM_MEM_TYPE_T mtype, size_t size)
{
  ScmObj stack = SCM_OBJ_INIT;

  stack = scm_capi_mem_alloc(&SCM_REFSTACK_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(stack)) return SCM_OBJ_NULL;

  if (scm_ref_stack_initialize(stack, size) < 0)
    return SCM_OBJ_NULL;

  return stack;
}

int
scm_ref_stack_push_va(ScmObj stack, va_list ap)
{
  ScmRef ref;

  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  while ((ref = va_arg(ap, ScmRef)) != NULL) {
    if (scm_ref_stack_growth_if_needed(stack) < 0)
      return -1;

    scm_ref_stack_block_push(SCM_REFSTACK(stack)->current, ref);
  }

  return 0;
}

int
scm_ref_stack_push_ary(ScmObj stack, ScmObj *ary, size_t n)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(ary != NULL);

  for (size_t i = 0; i < n; i++)
    scm_ref_stack_block_push(SCM_REFSTACK(stack)->current,
                             SCM_REF_MAKE(ary[i]));

  return 0;
}

int
scm_ref_stack_push(ScmObj stack, ...)
{
  int rslt;
  va_list ap;

  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  va_start(ap, stack);
  rslt = scm_ref_stack_push_va(stack, ap);
  va_end(ap);

  return rslt;
}

void
scm_ref_stack_save(ScmObj stack, ScmRefStackInfo *info)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(info != NULL);

  info->current = SCM_REFSTACK(stack)->current;
  info->sp = SCM_REFSTACK(stack)->current->sp;
}

void
scm_ref_stack_restore(ScmObj stack, ScmRefStackInfo *info)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(info != NULL);

  SCM_REFSTACK(stack)->current = info->current;
  SCM_REFSTACK(stack)->current->sp = info->sp;

  scm_ref_stack_decrease_if_possible(stack);
}

void
scm_ref_stack_init_sp(ScmObj stack)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);

  SCM_REFSTACK(stack)->current = SCM_REFSTACK(stack)->head;
  scm_ref_stack_block_init_sp(SCM_REFSTACK(stack)->current);
}

int
scm_ref_stack_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_REFSTACK_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<refstack %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_ref_stack_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_REFSTACK_TYPE_INFO);

  SCM_REFSTACK(obj)->head = NULL;
  SCM_REFSTACK(obj)->tail = NULL;
  SCM_REFSTACK(obj)->current = NULL;
}

void
scm_ref_stack_gc_finalize(ScmObj obj)
{
  scm_ref_stack_finalize(obj);
}

int
scm_ref_stack_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmRefStackBlock *block;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  for (block = SCM_REFSTACK(obj)->head;
       block != NULL && block->prev != SCM_REFSTACK(obj)->current;
       block = block->next) {
    ScmRef *ep;

    for (ep = block->stack; ep != block->sp; ep++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_REF_DEREF(*ep), mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;
    }
  }

  return rslt;
}
