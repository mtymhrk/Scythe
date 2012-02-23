#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "object.h"
#include "vm.h"
#include "reference.h"

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

  block = malloc(sizeof(ScmRefStackBlock) + sizeof(ScmRef) * sz);
  if (block == NULL)
    return NULL;

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

scm_local_func void
scm_ref_stack_add_block(ScmRefStack *stack, ScmRefStackBlock *block)
{
  if (stack->head == NULL) {
    stack->head = stack->tail = stack->current = block;
  }
  else {
    stack->tail->next = block;
    block->next = NULL;
    block->prev = stack->tail;
    stack->tail = block;
  }
}

scm_local_func void
scm_ref_stack_decrease_block(ScmRefStack *stack)
{
  ScmRefStackBlock *block;

  block = stack->tail;
  if (block != NULL) {
    stack->tail = block->prev;

    if (stack->tail == NULL)
      stack->head = stack->current = NULL;
    else
      stack->tail->next = NULL;

    if (stack->current == block)
      stack->current = stack->tail;

    free(block);
  }
}

scm_local_inline void
scm_ref_stack_shift_stack_block(ScmRefStack *stack)
{
  stack->current = stack->current->next;
  if (stack->current != NULL)
    stack->current->sp = stack->current->stack;
}


scm_local_func ScmRefStack *
scm_ref_stack_add_new_block(ScmRefStack *stack, size_t size)
{
  ScmRefStackBlock *block;

  scm_assert(stack != NULL);

  block = scm_ref_stack_new_block(size);
  if (block == NULL) return NULL;

  scm_ref_stack_add_block(stack, block);

  return stack;
}

scm_local_func ScmRefStack *
scm_ref_stack_growth_if_needed(ScmRefStack *stack)
{
  if (scm_ref_stack_block_full_p(stack->current))
    scm_ref_stack_shift_stack_block(stack);

  if (stack->current == NULL) {
    if (scm_ref_stack_add_new_block(stack, stack->tail->size) == NULL)
      return NULL;
    stack->current = stack->tail;
  }

  return stack;
}

scm_local_func void
scm_ref_stack_decrease_if_possible(ScmRefStack *stack)
{
  ;                             /* TODO: write me */
}

ScmRefStack *
scm_ref_stack_initialize(ScmRefStack *stack, size_t size)
{
  scm_assert(stack != NULL);

  stack->head = stack->tail = stack->current = NULL;
  return scm_ref_stack_add_new_block(stack, size);
}

void
scm_ref_stack_finalize(ScmRefStack *stack)
{
  scm_assert(stack != NULL);

  while (stack->head != NULL) {
    scm_ref_stack_decrease_block(stack);
  }
}

ScmRefStack *
scm_ref_stack_new(size_t size)
{
  ScmRefStack *stack;

  stack = malloc(sizeof(ScmRefStack));
  if (stack == NULL) return NULL;

  if (scm_ref_stack_initialize(stack, size) == NULL) {
    free(stack);
    return NULL;
  }

  stack->tail = stack->current = stack->head;
  return stack;
}

void
scm_ref_stack_end(ScmRefStack *stack)
{
  scm_assert(stack != NULL);

  scm_ref_stack_finalize(stack);
  free(stack);
}

ScmRefStack *
scm_ref_stack_push_va(ScmRefStack *stack, va_list ap)
{
  ScmRef ref;

  scm_assert(stack != NULL);

  while ((ref = va_arg(ap, ScmRef)) != NULL) {
    if (scm_ref_stack_growth_if_needed(stack) == NULL)
      return NULL;

    scm_ref_stack_block_push(stack->current, ref);
  }

  return stack;
}

ScmRefStack *
scm_ref_stack_push(ScmRefStack *stack, ...)
{
  ScmRefStack *rslt;
  va_list ap;

  scm_assert(stack != NULL);

  va_start(ap, stack);
  rslt = scm_ref_stack_push_va(stack, ap);
  va_end(ap);

  return rslt;
}

extern inline void
scm_ref_stack_save(ScmRefStack *stack, ScmRefStackInfo *info)
{
  scm_assert(stack != NULL);
  scm_assert(info != NULL);

  info->current = stack->current;
  info->sp = stack->current->sp;
}

extern inline void
scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info)
{
  scm_assert(stack != NULL);
  scm_assert(info != NULL);

  stack->current = info->current;
  stack->current->sp = info->sp;

  scm_ref_stack_decrease_if_possible(stack);
}


int
scm_ref_stack_gc_accept(ScmRefStack *stack, ScmObj owner,
                        ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmRefStackBlock *block;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  for (block = stack->head;
       block != NULL && block->prev != stack->current;
       block = block->next) {
    ScmRef *ep;

    for (ep = block->stack; ep != block->sp; ep++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, SCM_REF_DEREF(*ep), mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;
    }
  }

  return rslt;
}
