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

#define SCM_REF_STACK_BLOCK_NEXT(block) ((block)->next)
#define SCM_REF_STACK_BLOCK_PREV(block) ((block)->prev)
#define SCM_REF_STACK_BLOCK_SIZE(block) ((block)->size)
#define SCM_REF_STACK_BLOCK_SP(block) ((block)->sp)
#define SCM_REF_STACK_BLOCK_BOTTOM(block) ((block)->stack)
#define SCM_REF_STACK_BLOCK_TOP(block)                                  \
  (SCM_REF_STACK_BLOCK_BOTTOM(block) + SCM_REF_STACK_BLOCK_SIZE(block))
#define SCM_REF_STACK_BLOCK_IS_FULL(block)                              \
  (SCM_REF_STACK_BLOCK_TOP(block) <= SCM_REF_STACK_BLOCK_SP(block))
#define SCM_REF_STACK_BLOCK_SET_NEXT(block, nxt) ((block)->next = (nxt))
#define SCM_REF_STACK_BLOCK_SET_PREV(block, prv) ((block)->prev = (prv))
#define SCM_REF_STACK_BLOCK_SET_SIZE(block, sz) ((block)->size = (sz))
#define SCM_REF_STACK_BLOCK_SET_SP(block, ptr) ((block)->sp = (ptr))

#define SCM_REF_STACK_NEW_BLOCK(block, sz)                              \
  do {                                                                  \
    (block) = scm_memory_allocate(sizeof(ScmRefStackBlock)              \
                                  + sizeof(ScmRef) * sz);               \
    if ((block) != NULL) {                                              \
      SCM_REF_STACK_BLOCK_SET_NEXT(block, NULL);                        \
      SCM_REF_STACK_BLOCK_SET_PREV(block, NULL);                        \
      SCM_REF_STACK_BLOCK_SET_SIZE(block, sz);                          \
      SCM_REF_STACK_BLOCK_SET_SP(block,                                 \
                                 SCM_REF_STACK_BLOCK_BOTTOM(block));    \
    }                                                                   \
  } while(0)

#define SCM_REF_STACK_BLOCK_PUSH(block, ref)                            \
  do {                                                                  \
    *((block)->sp++) = (ref);                                           \
  } while(0)

struct ScmRefStackRec {
  ScmRefStackBlock *head;
  ScmRefStackBlock *tail;
  ScmRefStackBlock *current;
};

#define SCM_REF_STACK_ADD_BLOCK(stack, block)                           \
  do {                                                                  \
    if ((stack)->head == NULL) {                                        \
      (stack)->head = (stack)->tail = (stack)->current = (block);       \
    }                                                                   \
    else {                                                              \
      (stack)->tail->next = (block);                                    \
      SCM_REF_STACK_BLOCK_SET_NEXT(block, NULL);                        \
      SCM_REF_STACK_BLOCK_SET_PREV(block, (stack)->tail);               \
      (stack)->tail = (block);                                          \
    }                                                                   \
  } while(0)

#define SCM_REF_STACK_DEL_BLOCK(stack, block)                        \
  do {                                                               \
    (block) = (stack)->tail;                                         \
    if ((block) != NULL) {                                           \
      (stack)->tail = SCM_REF_STACK_BLOCK_PREV(block);               \
                                                                     \
      if ((stack)->tail == NULL)                                     \
        (stack)->head = (stack)->current = NULL;                     \
      else                                                           \
        SCM_REF_STACK_BLOCK_SET_NEXT((stack)->tail, NULL);           \
                                                                     \
      if ((stack)->current == (block))                               \
        (stack)->current = (stack)->tail;                            \
    }                                                                \
  } while(0)

#define SCM_REF_STACK_SHIFT_STACK_BLOCK(stack)                          \
  do {                                                                  \
    (stack)->current = SCM_REF_STACK_BLOCK_NEXT((stack)->current);      \
    if ((stack)->current != NULL)                                       \
      SCM_REF_STACK_BLOCK_SET_SP((stack)->current,                      \
                                 SCM_REF_STACK_BLOCK_BOTTOM((stack)->current)); \
  } while(0)


struct ScmRefStackInfoRec {
  ScmRefStackBlock *current;
  ScmRef *sp;
};


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
