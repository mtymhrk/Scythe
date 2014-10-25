#ifndef INCLUDE_REFERENCE_H__
#define INCLUDE_REFERENCE_H__

#include <stdint.h>
#include <stdarg.h>

typedef struct ScmRefStackRec ScmRefStack;

#define SCM_REFSTACK(obj) ((ScmRefStack *)(obj))

#include "scythe/object.h"
#include "scythe/api_type.h"
#include "scythe/impl_utils.h"

extern ScmTypeInfo SCM_REFSTACK_TYPE_INFO;

struct ScmRefStackRec {
  ScmObjHeader *header;
  ScmRefStackBlock *stack;
};


int scm_ref_stack_initialize(ScmObj stack);
ScmObj scm_ref_stack_new(SCM_MEM_TYPE_T mtyp);
void scm_ref_stack_gc_initialize(ScmObj obj, ScmObj mem);
int scm_ref_stack_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

static inline void
scm_ref_stack_push(ScmObj stack, ScmRefStackBlock *block)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(block != NULL);

  block->next = SCM_REFSTACK(stack)->stack;
  SCM_REFSTACK(stack)->stack = block;
}

static inline void
scm_ref_stack_save(ScmObj stack, ScmRefStackInfo *info)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(info != NULL);

  info->stack = SCM_REFSTACK(stack)->stack;
}

static inline void
scm_ref_stack_restore(ScmObj stack, ScmRefStackInfo *info)
{
  scm_assert_obj_type(stack, &SCM_REFSTACK_TYPE_INFO);
  scm_assert(info != NULL);

  SCM_REFSTACK(stack)->stack = info->stack;
}


#endif /* INCLUDE_REFERENCE_H__ */
