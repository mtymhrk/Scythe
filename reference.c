#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#include "reference.h"
#include "object.h"

static ScmRefStack *
scm_ref_stack_add_new_block(ScmRefStack *stack, size_t size)
{
  ScmRefStackBlock *block;

  assert(stack != NULL);

  SCM_REF_STACK_NEW_BLOCK(block, size);
  if (block == NULL) return NULL;

  SCM_REF_STACK_ADD_BLOCK(stack, block);

  return stack;
}

static ScmRefStack *
scm_ref_stack_growth_if_needed(ScmRefStack *stack)
{
  if (SCM_REF_STACK_BLOCK_IS_FULL(stack->current)) {
    if (stack->current == stack->tail) {
      size_t size = SCM_REF_STACK_BLOCK_SIZE(stack->tail);
      if (scm_ref_stack_add_new_block(stack, size) == NULL)
        return NULL;
    }
    stack->current = SCM_REF_STACK_BLOCK_NEXT(stack->current);
  }

  return stack;
}


static ScmRefStack *
scm_ref_stack_initialize(ScmRefStack *stack, size_t size)
{
  assert(stack != NULL);

  stack->head = stack->tail = stack->current = NULL;
  return scm_ref_stack_add_new_block(stack, size);
}

static void
scm_ref_stack_finalize(ScmRefStack *stack)
{
  assert(stack != NULL);

  do {
    ScmRefStackBlock *block;
    SCM_REF_STACK_DEL_BLOCK(stack, block);
    if (block != NULL) scm_memory_release(block);
  } while(stack->head != NULL);
}

ScmRefStack *
scm_ref_stack_construct(size_t size)
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
scm_ref_stack_destruct(ScmRefStack *stack)
{
  assert(stack != NULL);

  scm_ref_stack_finalize(stack);
  scm_memory_release(stack);
}

ScmRefStack *
scm_ref_stack_push(ScmRefStack *stack, ...)
{
  va_list ap;
  ScmObj *ptr;

  assert(stack != NULL);

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

ScmRef
scm_ref_stack_alloc(ScmRefStack *stack, ScmObj init)
{
  ScmRef ref;

  if (scm_ref_stack_growth_if_needed(stack) == NULL)
    return SCM_REF_NULL;

  SCM_REF_STACK_BLOCK_ALLOC(stack->current, ref, init);
  return ref;
}

void
scm_ref_stack_save(ScmRefStack *stack, ScmRefStackInfo *info)
{
  assert(stack != NULL);
  assert(info != NULL);

  info->current = stack->current;
  info->sp = SCM_REF_STACK_BLOCK_SP(stack->current);
}

void
scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info)
{
  assert(stack != NULL);
  assert(info != NULL);

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
