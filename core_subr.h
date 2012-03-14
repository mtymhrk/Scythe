#ifndef INCLUDE_CORE_SUBR_H__
#define INCLUDE_CORE_SUBR_H__

#include <stdint.h>

#include "object.h"

ScmObj scm_subr_func_cons(uint32_t argc, ScmObj *argv);
ScmObj scm_subr_func_car(uint32_t argc, ScmObj *argv);
ScmObj scm_subr_func_cdr(uint32_t argc, ScmObj *argv);

void scm_core_subr_system_setup(void);

#endif /* INCLUDE_CORE_SUBR_H__ */
