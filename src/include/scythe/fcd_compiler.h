#ifndef INCLUDE_FCD_COMPILER_H__
#define INCLUDE_FCD_COMPILER_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"


/*************************************************************************/
/* Compiler                                                              */
/*************************************************************************/

bool scm_fcd_compiler_p(ScmObj obj);
ScmObj scm_fcd_compiler_P(ScmObj obj);
ScmObj scm_fcd_compiler_new(SCM_MEM_TYPE_T mtype, ScmObj module);
ScmObj scm_fcd_make_compiler(ScmObj mod);
ScmObj scm_fcd_compiler_current_module(ScmObj cmpl);
ScmObj scm_fcd_compiler_current_expr(ScmObj cmpl);
ScmObj scm_fcd_compiler_select_module_i(ScmObj cmpl, ScmObj mod);
void scm_fcd_compiler_select_expr_i(ScmObj cmpl, ScmObj expr);


/*************************************************************************/
/* Quasiquotation                                                        */
/*************************************************************************/

bool scm_fcd_qqtmplnode_p(ScmObj obj);
ScmObj scm_fcd_qqtmplnode_new(SCM_MEM_TYPE_T mtype, int kind, ScmObj obj);

bool scm_fcd_qqtmpl_p(ScmObj obj);
ScmObj scm_fcd_qqtmpl_new(SCM_MEM_TYPE_T mtype, ScmObj tmpl);
ScmObj scm_fcd_qqtmpl_template(ScmObj qq);
size_t scm_fcd_qqtmpl_nr_unquoted_expr(ScmObj qq);
ScmObj scm_fcd_qqtmpl_unquoted_expr(ScmObj qq, size_t n);
ScmObj scm_fcd_compile_qq_template(ScmObj tmpl);
ScmObj scm_fcd_substitute_qq_template(ScmObj qq, ScmObj values);


#endif /* INCLUDE_FCD_COMPILER_H__ */
