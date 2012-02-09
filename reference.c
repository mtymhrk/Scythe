#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "reference.h"
#include "memory.h"
#include "object.h"


static ScmRefStack *
scm_ref_stack_add_new_block(ScmRefStack *stack, size_t size)
{
  ScmRefStackBlock *block;

  scm_assert(stack != NULL);

  SCM_REF_STACK_NEW_BLOCK(block, size);
  if (block == NULL) return NULL;

  SCM_REF_STACK_ADD_BLOCK(stack, block);

  return stack;
}

static ScmRefStack *
scm_ref_stack_growth_if_needed(ScmRefStack *stack)
{
  if (SCM_REF_STACK_BLOCK_IS_FULL(stack->current))
    SCM_REF_STACK_SHIFT_STACK_BLOCK(stack);

  if (stack->current == NULL) {
    size_t size = SCM_REF_STACK_BLOCK_SIZE(stack->tail);
    if (scm_ref_stack_add_new_block(stack, size) == NULL)
      return NULL;
    stack->current = stack->tail;
  }

  return stack;
}


static ScmRefStack *
scm_ref_stack_initialize(ScmRefStack *stack, size_t size)
{
  scm_assert(stack != NULL);

  stack->head = stack->tail = stack->current = NULL;
  return scm_ref_stack_add_new_block(stack, size);
}

static void
scm_ref_stack_finalize(ScmRefStack *stack)
{
  scm_assert(stack != NULL);

  do {
    ScmRefStackBlock *block;
    SCM_REF_STACK_DEL_BLOCK(stack, block);
    if (block != NULL) scm_memory_release(block);
  } while(stack->head != NULL);
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
  ScmObj *ptr;

  scm_assert(stack != NULL);

  va_start(ap, stack);
  while ((ptr = va_arg(ap, ScmObj *)) != NULL) {
    if (scm_ref_stack_growth_if_needed(stack) == NULL)
      goto err;

    SCM_REF_STACK_BLOCK_PUSH(stack->current, ptr);
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
  info->sp = SCM_REF_STACK_BLOCK_SP(stack->current);
}

void
scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info)
{
  scm_assert(stack != NULL);
  scm_assert(info != NULL);

  stack->current = info->current;
  SCM_REF_STACK_BLOCK_SET_SP(stack->current, info->sp);
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
       block != NULL && SCM_REF_STACK_BLOCK_PREV(block) != stack->current;
       block = SCM_REF_STACK_BLOCK_NEXT(block)) {
    ScmRef *ep;

    for (ep = SCM_REF_STACK_BLOCK_BOTTOM(block);
         ep != SCM_REF_STACK_BLOCK_SP(block);
         ep++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, SCM_REF_DEREF(*ep), mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;
    }
  }

  return rslt;
}

