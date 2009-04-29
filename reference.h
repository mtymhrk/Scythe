#ifndef INCLUDE_REFERENCE_H__
#define INCLUDE_REFERENCE_H__

#include <stdint.h>

typedef struct ScmRefStackElemRec ScmRefStackElem;
typedef struct ScmRefStackBlockRec ScmRefStackBlock;
typedef struct ScmRefStackInfoRec ScmRefStackInfo;
typedef struct ScmRefStackRec ScmRefStack;

#include "memory.h"
#include "object.h"
#include "vm.h"

enum { SCM_REF_STACK_ELEM_OBJ, SCM_REF_STACK_ELEM_REF };

struct ScmRefStackElemRec {
  int type;
  union {
    ScmObj obj;
    ScmRef ref;
  } val;
};

#define SCM_REF_STACK_ELEM_TYPE(elem) ((elem)->type)

#define SCM_REF_STACK_ELEM_VAL(elem, value)                 \
  do {                                                      \
    switch ((elem)->type) {                                 \
    case SCM_REF_STACK_ELEM_OBJ:                            \
      SCM_OBJ(value) = (elem)->val.obj;                     \
      break;                                                \
    case SCM_REF_STACK_ELEM_REF:                            \
      SCM_REF_MAKE_FROM_PTR(value) = (elem)->val.ref;       \
      break;                                                \
    }                                                       \
  } while(0)

#define SCM_REF_STACK_ELEM_TO_REF(elem) (SCM_REF_MAKE((elem)->val.obj))

#define SCM_REF_STACK_ELEM_SET_VAL(elem, value)             \
  do {                                                      \
    switch ((elem)->type) {                                 \
    case SCM_REF_STACK_ELEM_OBJ:                            \
      (elem)->val.obj = SCM_OBJ((value));                   \
      break;                                                \
    case SCM_REF_STACK_ELEM_REF:                            \
      (elem)->val.ref = SCM_REF_MAKE_FROM_PTR(value);       \
      break;                                                \
    }                                                       \
  } while(0)

#define SCM_REF_STACK_ELEM_MAKE_SCM_REF(elem, ref)          \
  do { \
    switch ((elem)->type) {                                 \
    case SCM_REF_STACK_ELEM_OBJ:                            \
      (ref) = SCM_REF_MAKE((elem)->val.obj);                \
      break;                                                \
    case SCM_REF_STACK_ELEM_REF:                            \
      (ref) = (elem)->val.ref;                              \
      break;                                                \
    }                                                       \
  } while(0)

#define SCM_REF_STACK_ELEM_INIT_TO_OBJ(elem, v)             \
  do {                                                      \
    (elem)->type = SCM_REF_STACK_ELEM_OBJ;                  \
    (elem)->val.obj = SCM_OBJ(v);                           \
  } while(0)

#define SCM_REF_STACK_ELEM_INIT_TO_REF(elem, v)             \
  do {                                                      \
    (elem)->type = SCM_REF_STACK_ELEM_REF;                  \
    (elem)->val.ref = SCM_REF_MAKE_FROM_PTR(v);                          \
  } while(0)

struct ScmRefStackBlockRec {
  ScmRefStackBlock *next;
  ScmRefStackBlock *prev;
  size_t size;
  ScmRefStackElem *sp;
  ScmRefStackElem stack[0];
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
                                  + sizeof(ScmRefStackElem) * sz);      \
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
    SCM_REF_STACK_ELEM_INIT_TO_REF((block)->sp, ref);                   \
    (block)->sp++;                                                      \
  } while(0)

#define SCM_REF_STACK_BLOCK_ALLOC(block, ref, init)                     \
  do {                                                                  \
    SCM_REF_STACK_ELEM_INIT_TO_OBJ((block)->sp, init);                  \
    (ref) = SCM_REF_STACK_ELEM_TO_REF((block)->sp);                     \
    (block)->sp++;                                                      \
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

struct ScmRefStackInfoRec {
  ScmRefStackBlock *current;
  ScmRefStackElem *sp;
};

ScmRefStack *scm_ref_stack_construct(size_t size);
void scm_ref_stack_destruct(ScmRefStack *stack);
ScmRefStack *scm_ref_stack_push(ScmRefStack *stack, ...);
ScmRef scm_ref_stack_alloc(ScmRefStack *stack, ScmObj init);
void scm_ref_stack_save(ScmRefStack *stack, ScmRefStackInfo *info);
void scm_ref_stack_restore(ScmRefStack *stack, ScmRefStackInfo *info);
void scm_ref_stack_save_current_stack(ScmRefStackInfo *info);
void scm_ref_stack_restore_current_stack(ScmRefStackInfo *info);
int scm_ref_stack_gc_accept(ScmRefStack *stack, ScmObj owner,
                            ScmMem *mem, ScmGCRefHandlerFunc handler);

#define SCM_REF_STACK_CONCAT2__(x, y) x##y
#define SCM_REF_STACK_CONCAT__(x, y) SCM_REF_STACK_CONCAT2__(x, y)

#define SCM_STACK_FRAME                                                 \
  __attribute__((__cleanup__(scm_ref_stack_restore_current_stack)))     \
  ScmRefStackInfo SCM_REF_STACK_CONCAT__(scm_ref_stack_frame__, __LINE__) \
  = { NULL, NULL };                                                     \
  scm_ref_stack_save_current_stack(&SCM_REF_STACK_CONCAT__(scm_ref_stack_frame__, __LINE__));

#define SCM_STACK_PUSH(...)                                             \
  scm_ref_stack_push(scm_vm_current_ref_stack(), __VA_ARGS__, NULL)
#define SCM_STACK_ALLOC(v)                              \
  scm_ref_stack_alloc(scm_vm_current_ref_stack(), v)

#endif /* INCLUDE_REFERENCE_H__ */
