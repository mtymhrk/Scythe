#ifndef INCLUDE_CORE_SUBR_H__
#define INCLUDE_CORE_SUBR_H__

#include "object.h"

ScmObj scm_subr_func_cons(void);
ScmObj scm_subr_func_car(void);
ScmObj scm_subr_func_cdr(void);

void scm_core_subr_system_setup(void);

#endif /* INCLUDE_CORE_SUBR_H__ */
