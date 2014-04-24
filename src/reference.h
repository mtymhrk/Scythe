#ifndef INCLUDE_REFERENCE_H__
#define INCLUDE_REFERENCE_H__

#include <stdint.h>
#include <stdarg.h>

typedef struct ScmRefStackRec ScmRefStack;

#define SCM_REFSTACK(obj) ((ScmRefStack *)(obj))

#include "object.h"
#include "api_enum.h"
#include "api_type.h"
#include "impl_utils.h"

extern ScmTypeInfo SCM_REFSTACK_TYPE_INFO;

struct ScmRefStackRec {
  ScmObjHeader *header;
  ScmRefStackBlock *head;
  ScmRefStackBlock *tail;
  ScmRefStackBlock *current;
};


/* private functions */

#ifdef SCM_UNIT_TEST

bool scm_ref_stack_block_full_p(ScmRefStackBlock *block);
ScmRefStackBlock *scm_ref_stack_new_block(size_t sz);
void scm_ref_stack_block_push(ScmRefStackBlock *block, ScmRef ref);
void scm_ref_stack_block_init_sp(ScmRefStackBlock *block);
void scm_ref_stack_add_block(ScmObj stack, ScmRefStackBlock *block);
void scm_ref_stack_decrease_block(ScmObj stack);
void scm_ref_stack_shift_stack_block(ScmObj stack);
int scm_ref_stack_add_new_block(ScmObj stack, size_t size);
int scm_ref_stack_growth_if_needed(ScmObj stack);
void scm_ref_stack_decrease_if_possible(ScmObj stack);

#endif

/* public functions */
int scm_ref_stack_initialize(ScmObj stack, size_t size);
void scm_ref_stack_finalize(ScmObj stack);
ScmObj scm_ref_stack_new(SCM_MEM_TYPE_T mtype, size_t size);
int scm_ref_stack_push_va(ScmObj stack, va_list ap);
int scm_ref_stack_push_ary(ScmObj stack, ScmObj *ary, size_t n);
int scm_ref_stack_push(ScmObj stack, ...);
ScmRef scm_ref_stack_alloc(ScmObj stack, ScmObj init);
void scm_ref_stack_save(ScmObj stack, ScmRefStackInfo *info);
void scm_ref_stack_restore(ScmObj stack, ScmRefStackInfo *info);
void scm_ref_stack_init_sp(ScmObj stack);
int scm_ref_stack_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_ref_stack_gc_initialize(ScmObj obj, ScmObj mem);
void scm_ref_stack_gc_finalize(ScmObj obj);
int scm_ref_stack_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_REFERENCE_H__ */
