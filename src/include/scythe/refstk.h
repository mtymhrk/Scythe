#ifndef INCLUDE_REFSTK_H__
#define INCLUDE_REFSTK_H__

#include <stdint.h>
#include <stdarg.h>


#include "scythe/object.h"
#include "scythe/api_type.h"
#include "scythe/impl_utils.h"

extern ScmTypeInfo SCM_REFSTACK_TYPE_INFO;

int scm_ref_stack_initialize(ScmObj stack);
void scm_ref_stack_gc_initialize(ScmObj obj, ScmObj mem);
int scm_ref_stack_gc_accept(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_REFSTK_H__ */
