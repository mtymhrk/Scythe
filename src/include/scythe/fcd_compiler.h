#ifndef INCLUDE_FCD_COMPILER_H__
#define INCLUDE_FCD_COMPILER_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"

bool scm_fcd_compiler_p(ScmObj obj);
ScmObj scm_fcd_compiler_P(ScmObj obj);
ScmObj scm_fcd_compiler_new(SCM_MEM_TYPE_T mtype, ScmObj module);
ScmObj scm_fcd_make_compiler(ScmObj mod);
ScmObj scm_fcd_compiler_current_module(ScmObj cmpl);
ScmObj scm_fcd_compiler_current_expr(ScmObj cmpl);
ScmObj scm_fcd_compiler_select_module_i(ScmObj cmpl, ScmObj mod);
void scm_fcd_compiler_select_expr_i(ScmObj cmpl, ScmObj expr);

#endif /* INCLUDE_FCD_COMPILER_H__ */
