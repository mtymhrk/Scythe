#ifndef INCLUDE_FCD_EXCEPTION_H__
#define INCLUDE_FCD_EXCEPTION_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"

int scm_fcd_halt(void);
int scm_fcd_raise(ScmObj obj);
int scm_fcd_raise_continuable(ScmObj obj);
bool scm_fcd_raised_p(void);
ScmObj scm_fcd_raised_obj(void);
void scm_fcd_discard_raised_obj(void);
int scm_fcd_push_exception_handler(ScmObj handler);
int scm_fcd_pop_exception_handler(void);
void scm_fcd_disposal_unhandled_exec(void);
ScmObj scm_fcd_error_new_cv(SCM_MEM_TYPE_T mtype, ScmObj msg,
                            ScmObj type, ScmObj *irris, size_t n);
ScmObj scm_fcd_error_new_lst(SCM_MEM_TYPE_T mtype, ScmObj msg,
                             ScmObj type, ScmObj irris);
int scm_fcd_error(const char *msg, size_t n, ...);
int scm_fcd_read_error(const char *msg, size_t n, ...);
int scm_fcd_file_error(const char *msg, size_t n, ...);
ScmObj scm_fcd_error_lst(ScmObj msg, ScmObj irris);
bool scm_fcd_error_object_p(ScmObj obj);
ScmObj scm_fcd_error_object_P(ScmObj obj);
ScmObj scm_fcd_error_object_message(ScmObj obj);
ScmObj scm_fcd_error_object_irritants(ScmObj obj);
ScmObj scm_fcd_read_error_P(ScmObj obj);
ScmObj scm_fcd_file_error_P(ScmObj obj);

#endif /* INCLUDE_FCD_EXCEPTION_H__ */
