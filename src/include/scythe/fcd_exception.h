#ifndef INCLUDE_FCD_EXCEPTION_H__
#define INCLUDE_FCD_EXCEPTION_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/fcd_memory.h"

ScmObj scm_fcd_error_new_cv(scm_mem_type_t mtype, ScmObj msg,
                            ScmObj type, ScmObj *irris, size_t n);
ScmObj scm_fcd_error_new_lst(scm_mem_type_t mtype, ScmObj msg,
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
