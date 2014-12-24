#ifndef INCLUDE_FCD_REFERENCE_H__
#define INCLUDE_FCD_REFERENCE_H__

#include "scythe/fcd_type.h"

#define SCM_REFSTK_INIT                                                 \
  __attribute__((__cleanup__(scm_fcd_ref_stack_restore)))               \
  ScmRefStackInfo SCM_CONCAT_SYMBOL__(scm_ref_stack_info__, __LINE__) = { .stack = NULL }; \
  scm_fcd_ref_stack_save(&SCM_CONCAT_SYMBOL__(scm_ref_stack_info__, __LINE__));

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

#define SCM_REFSTK_PUSH(name) scm_fcd_ref_stack_push(&(name));

#define SCM_REFSTK_REG(...)                                             \
  scm_fcd_ref_stack_push(&(ScmRefStackBlock){                           \
      .next = NULL,                                                     \
      .type = SCM_REFSTACK_RARY,                                        \
      .ref = { .rary = (ScmObj *[]){__VA_ARGS__, NULL} }                \
    })

#define SCM_REFSTK_REG_ARY(a, l)                                        \
  scm_fcd_ref_stack_push(&(ScmRefStackBlock){                           \
      .next = NULL,                                                     \
      .type = SCM_REFSTACK_ARY,                                         \
      .ref = { .ary = { .head = (a), .n = (l) } }                       \
    })

#define SCM_REFSTK_INIT_REG(...)                \
  SCM_REFSTK_INIT; SCM_REFSTK_REG(__VA_ARGS__);

static inline void
scm_fcd_ref_stack_push(ScmRefStackBlock *block)
{
  ScmObj stack = scm_fcd_current_ref_stack();
  scm_assert(block != NULL);
  block->next = SCM_REFSTACK(stack)->stack;
  SCM_REFSTACK(stack)->stack = block;
}

static inline void
scm_fcd_ref_stack_save(ScmRefStackInfo *info)
{
  ScmObj stack = scm_fcd_current_ref_stack();
  scm_assert(info != NULL);
  info->stack = SCM_REFSTACK(stack)->stack;
}

static inline void
scm_fcd_ref_stack_restore(ScmRefStackInfo *info)
{
  ScmObj stack = scm_fcd_current_ref_stack();
  scm_assert(info != NULL);
  SCM_REFSTACK(stack)->stack = info->stack;
}


#endif /* INCLUDE_FCD_REFERENCE_H__ */
