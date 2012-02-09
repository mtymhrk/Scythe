#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "reference.h"
#include "memory.h"
#include "object.h"


ScmRefStack *
scm_ref_stack_add_new_block(ScmRefStack *stack, size_t size)
{
  ScmRefStackBlock *block;

  scm_assert(stack != NULL);

  block = scm_ref_stack_new_block(size);
  if (block == NULL) return NULL;

  scm_ref_stack_add_block(stack, block);

  return stack;
}

ScmRefStack *
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

  stack = scm_memory_allocate(sizeof(ScmRefStack));
  if (stack == NULL) return NULL;

  if (scm_ref_stack_initialize(stack, size) == NULL) {
    scm_memory_release(stack);
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
  scm_memory_release(stack);
}

ScmRefStack *
scm_ref_stack_push(ScmRefStack *stack, ...)
{
  va_list ap;
  ScmRef ref;

  scm_assert(stack != NULL);

  va_start(ap, stack);
  while ((ref = va_arg(ap, ScmRef)) != NULL) {
    if (scm_ref_stack_growth_if_needed(stack) == NULL)
      goto err;

    scm_ref_stack_block_push(stack->current, ref);
  }

  va_end(ap);
  return stack;

 err:
  va_end(ap);
  return NULL;
}

void
scm_ref_stack_save(ScmRefStack *stack, ScmRefStackInfo *info)
{
  scm_assert(stack != NULL);
  scm_assert(info != NULL);

  info->current = stack->current;
  info->sp = stack->current->sp;
}

void
scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info)
{
  scm_assert(stack != NULL);
  scm_assert(info != NULL);

  stack->current = info->current;
  stack->current->sp = info->sp;
}

void
scm_ref_stack_save_current_stack(ScmRefStackInfo *info)
{
  scm_ref_stack_save(scm_vm_current_ref_stack(), info);
}

void
scm_ref_stack_restore_current_stack(ScmRefStackInfo *info)
{
  scm_ref_stack_restore(scm_vm_current_ref_stack(), info);
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
