#ifndef INCLUDE_REFERENCE_H__
#define INCLUDE_REFERENCE_H__

#include <stdint.h>

typedef struct ScmRefStackBlockRec ScmRefStackBlock;
typedef struct ScmRefStackInfoRec ScmRefStackInfo;
typedef struct ScmRefStackRec ScmRefStack;


#include "object.h"
#include "vm.h"
#include "impl_utils.h"

struct ScmRefStackBlockRec {
  ScmRefStackBlock *next;
  ScmRefStackBlock *prev;
  size_t size;
  ScmRef *sp;
  ScmRef stack[0];
};

static inline bool
scm_ref_stack_block_full_p(ScmRefStackBlock *block)
{
  return (block->stack + block->size <= block->sp) ? true : false;
}

static inline ScmRefStackBlock *
scm_ref_stack_new_block(size_t sz)
{
  ScmRefStackBlock *block;

  block = scm_memory_allocate(sizeof(ScmRefStackBlock) + sizeof(ScmRef) * sz);
  if (block == NULL)
    return NULL;

  block->next = NULL;
  block->prev = NULL;
  block->size = sz;
  block->sp = block->stack;

  return block;
}

static inline void
scm_ref_stack_block_push(ScmRefStackBlock *block, ScmRef ref)
{
  *(block->sp++) = ref;
}



struct ScmRefStackRec {
  ScmRefStackBlock *head;
  ScmRefStackBlock *tail;
  ScmRefStackBlock *current;
};

static inline void
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

static inline void
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

    scm_memory_release(block);
  }
}

static inline void
scm_ref_stack_shift_stack_block(ScmRefStack *stack)
{
  stack->current = stack->current->next;
  if (stack->current != NULL)
    stack->current->sp = stack->current->stack;
}


struct ScmRefStackInfoRec {
  ScmRefStackBlock *current;
  ScmRef *sp;
};

ScmRefStack *scm_ref_stack_add_new_block(ScmRefStack *stack, size_t size);
ScmRefStack *scm_ref_stack_growth_if_needed(ScmRefStack *stack);
ScmRefStack *scm_ref_stack_initialize(ScmRefStack *stack, size_t size);
void scm_ref_stack_finalize(ScmRefStack *stack);
ScmRefStack *scm_ref_stack_new(size_t size);
void scm_ref_stack_end(ScmRefStack *stack);
ScmRefStack *scm_ref_stack_push(ScmRefStack *stack, ...);
ScmRef scm_ref_stack_alloc(ScmRefStack *stack, ScmObj init);
void scm_ref_stack_save(ScmRefStack *stack, ScmRefStackInfo *info);
void scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info);
void scm_ref_stack_save_current_stack(ScmRefStackInfo *info);
void scm_ref_stack_restore_current_stack(ScmRefStackInfo *info);
int scm_ref_stack_gc_accept(ScmRefStack *stack, ScmObj owner,
                            ScmObj mem, ScmGCRefHandlerFunc handler);



#define SCM_STACK_FRAME                         \
  __attribute__((__cleanup__(scm_ref_stack_restore_current_stack)))     \
  ScmRefStackInfo SCM_CONCAT_SYMBOL__(scm_ref_stack_frame__, __LINE__) \
  = { NULL, NULL };                                                     \
  scm_ref_stack_save_current_stack(&SCM_CONCAT_SYMBOL__(scm_ref_stack_frame__, __LINE__));

#define SCM_STACK_PUSH(...)                                             \
  scm_ref_stack_push(scm_vm_current_ref_stack(), __VA_ARGS__, NULL)

#define SCM_STACK_FRAME_PUSH(...) \
  SCM_STACK_FRAME; SCM_STACK_PUSH(__VA_ARGS__);

#endif /* INCLUDE_REFERENCE_H__ */
