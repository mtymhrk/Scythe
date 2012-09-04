#ifndef INCLUDE_COMPILER_H__
#define INCLUDE_COMPILER_H__

#include "object.h"

ScmObj scm_cmpl_compile(ScmObj exp);

int scm_core_syntx_system_setup(void);

#ifdef SCM_UNIT_TEST

void scm_cmpl_ut_clear_label_id(void);

#endif


#endif /* INCLUDE_COMPILER_H__ */





