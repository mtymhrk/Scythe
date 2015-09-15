#ifndef INCLUDE_REFSTK_H__
#define INCLUDE_REFSTK_H__

#include <stdint.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/memory.h"

typedef struct ScmRefStackBlockRec ScmRefStackBlock;
typedef struct ScmRefStackRec ScmRefStack;

enum { SCM_REFSTACK_RARY, SCM_REFSTACK_ARY };

struct ScmRefStackBlockRec {
  ScmRefStackBlock *next;
  int type;
  union {
    ScmObj **rary;
    struct {
      ScmObj *head;
      size_t n;
    } ary;
  } ref;
};

struct ScmRefStackRec {
  ScmObjHeader *header;
  ScmRefStackBlock *stack;
};

#define SCM_REFSTACK(obj) ((ScmRefStack *)(obj))

extern ScmTypeInfo SCM_REFSTACK_TYPE_INFO;

int scm_ref_stack_initialize(ScmObj stack);
ScmObj scm_ref_stack_new(scm_mem_type_t mtype);
void scm_ref_stack_gc_initialize(ScmObj obj);
int scm_ref_stack_gc_accept(ScmObj obj, ScmGCRefHandler handler);


/***************************************************************************/
/*  Facade                                                                 */
/***************************************************************************/

extern ScmObj scm__current_ref_stack;

static inline ScmObj
scm_current_ref_stack(void)
{
  return scm__current_ref_stack;
}

static inline void
scm_chg_current_ref_stack(ScmObj stack)
{
  scm__current_ref_stack = stack;
}

typedef struct ScmRefStackInfoRec ScmRefStackInfo;

struct ScmRefStackInfoRec {
  ScmRefStackBlock *stack;
};

static inline void
scm_ref_stack_push(ScmRefStackBlock *block)
{
  ScmObj stack = scm_current_ref_stack();
  scm_assert(block != NULL);
  block->next = SCM_REFSTACK(stack)->stack;
  SCM_REFSTACK(stack)->stack = block;
}

static inline void
scm_ref_stack_save(ScmRefStackInfo *info)
{
  ScmObj stack = scm_current_ref_stack();
  scm_assert(info != NULL);
  info->stack = SCM_REFSTACK(stack)->stack;
}

static inline void
scm_ref_stack_restore(ScmRefStackInfo *info)
{
  ScmObj stack = scm_current_ref_stack();
  scm_assert(info != NULL);
  SCM_REFSTACK(stack)->stack = info->stack;
}

#define SCM_REFSTK_INIT                                                 \
  __attribute__((__cleanup__(scm_ref_stack_restore)))                   \
  ScmRefStackInfo SCM_CONCAT_SYMBOL__(scm_ref_stack_info__, __LINE__) = { .stack = NULL }; \
  scm_ref_stack_save(&SCM_CONCAT_SYMBOL__(scm_ref_stack_info__, __LINE__));

#define SCM_REFSTK_BLK_INIT(name, ...)                  \
  ScmRefStackBlock name = {                             \
    .next = NULL,                                       \
    .type = SCM_REFSTACK_RARY,                          \
    .ref = { .rary = (ScmObj *[]){__VA_ARGS__, NULL} }  \
  }

#define SCM_REFSTK_BLK_INIT_ARY(name, a, l)       \
  ScmRefStackBlock name = {                       \
    .next = NULL,                                 \
    .type = SCM_REFSTACK_ARY,                     \
    .ref = { .ary = { .head = (a), .n = (l)} }    \
  }

#define SCM_REFSTK_PUSH(name) scm_ref_stack_push(&(name));

#define SCM_REFSTK_REG(...)                                             \
  scm_ref_stack_push(&(ScmRefStackBlock){                               \
      .next = NULL,                                                     \
      .type = SCM_REFSTACK_RARY,                                        \
      .ref = { .rary = (ScmObj *[]){__VA_ARGS__, NULL} }                \
    })

#define SCM_REFSTK_REG_ARY(a, l)                                        \
  scm_ref_stack_push(&(ScmRefStackBlock){                               \
      .next = NULL,                                                     \
      .type = SCM_REFSTACK_ARY,                                         \
      .ref = { .ary = { .head = (a), .n = (l) } }                       \
    })

#define SCM_REFSTK_INIT_REG(...)                \
  SCM_REFSTK_INIT; SCM_REFSTK_REG(__VA_ARGS__);


#endif /* INCLUDE_REFSTK_H__ */
