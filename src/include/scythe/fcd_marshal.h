#ifndef INCLUDE_FCD_MARSHAL_H__
#define INCLUDE_FCD_MARSHAL_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"

bool scm_fcd_marshal_p(ScmObj obj);
ScmObj scm_fcd_marshal_new(SCM_MEM_TYPE_T mtype);
ScmObj scm_fcd_make_marshal(void);
bool scm_fcd_marshal_terminated_p(ScmObj marshal);
int scm_fcd_marshal_push(ScmObj marshal, ScmObj obj);
void *scm_fcd_marshal_terminate(ScmObj marshal, size_t *size);
bool scm_fcd_unmarshal_p(ScmObj obj);
ScmObj scm_fcd_unmarshal_new(SCM_MEM_TYPE_T mtype, const void *data);
ScmObj scm_fcd_make_unmarshal(const void *data);
size_t scm_fcd_unmarshal_num(ScmObj unmarshal);
ScmObj scm_fcd_unmarshal_ref(ScmObj unmarshal, size_t idx);
void *scm_fcd_marshal_va(size_t *size, va_list arg);
void *scm_fcd_marshal(size_t *size, ...);

#endif /* INCLUDE_FCD_MARSHAL_H__ */
