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
ScmObj scm_fcd_compiler_new(SCM_MEM_TYPE_T mtype, ScmObj env);
ScmObj scm_fcd_make_compiler(ScmObj env);
ScmObj scm_fcd_compiler_base_env(ScmObj cmpl);
ScmObj scm_fcd_compiler_current_expr(ScmObj cmpl);
ScmObj scm_fcd_compiler_select_base_env_i(ScmObj cmpl, ScmObj env);
ScmObj scm_fcd_compiler_select_module_i(ScmObj cmpl, ScmObj env);
void scm_fcd_compiler_select_expr_i(ScmObj cmpl, ScmObj expr);


/*************************************************************************/
/* Quasiquotation                                                        */
/*************************************************************************/

bool scm_fcd_qqtmplnode_p(ScmObj obj);
ScmObj scm_fcd_qqtmplnode_new(SCM_MEM_TYPE_T mtype, int kind, ScmObj obj);
ScmObj scm_fcd_make_qqtmplnode_for_unmarshal(void);
void scm_fcd_qqtmplnode_get_contents_for_marshal(ScmObj node,
                                                 int *kind, scm_csetter_t *obj);
int scm_fcd_qqtmplnode_setup_for_unmarshal(ScmObj node, int kind, ScmObj obj);

bool scm_fcd_qqtmpl_p(ScmObj obj);
ScmObj scm_fcd_qqtmpl_new(SCM_MEM_TYPE_T mtype, ScmObj tmpl);
ScmObj scm_fcd_make_qqtmpl_for_unmarshal(void);
int scm_fcd_qqtmpl_get_contents_for_marshal(ScmObj qq,
                                            scm_csetter_t *tmpl,
                                            scm_csetter_t *compiled,
                                            scm_csetter_t *expr);
int scm_fcd_qqtmpl_setup_for_unmarshal(ScmObj qq,
                                       ScmObj tmpl, ScmObj compiled,
                                       ScmObj expr);
ScmObj scm_fcd_qqtmpl_template(ScmObj qq);
size_t scm_fcd_qqtmpl_nr_unquoted_expr(ScmObj qq);
ScmObj scm_fcd_qqtmpl_unquoted_expr(ScmObj qq, size_t n);
int scm_fcd_qqtmpl_eq(ScmObj qq1, ScmObj qq2, bool *rslt);

ScmObj scm_fcd_compile_qq_template(ScmObj tmpl);
ScmObj scm_fcd_substitute_qq_template(ScmObj qq, ScmObj values);


/*************************************************************************/
/* Identifier                                                            */
/*************************************************************************/

bool scm_fcd_identifier_p(ScmObj obj);
ScmObj scm_fcd_identifier_P(ScmObj obj);
ScmObj scm_fcd_identifier_new(SCM_MEM_TYPE_T mtype, ScmObj name, ScmObj env);
ScmObj scm_fcd_make_identifier(ScmObj name, ScmObj env);
ScmObj scm_fcd_identifier_name(ScmObj ident);
ScmObj scm_fcd_identifier_env(ScmObj ident);


#endif /* INCLUDE_FCD_COMPILER_H__ */
