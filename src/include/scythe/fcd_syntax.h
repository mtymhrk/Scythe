#ifndef INCLUDE_FCD_SYNTAX_H__
#define INCLUDE_FCD_SYNTAX_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_syntax.h"

/*************************************************************************/
/* Syntax                                                                */
/*************************************************************************/

bool scm_fcd_syntax_p(ScmObj obj);
ScmObj scm_fcd_syntax_P(ScmObj obj);
ScmObj scm_fcd_syntax_new(scm_mem_type_t mtype, ScmObj key, ScmObj handler);
ScmObj scm_fcd_make_syntax(ScmObj keyword, ScmObj handler);
ScmObj scm_fcd_syntax_keyword(ScmObj syx);
ScmObj scm_fcd_syntax_handler(ScmObj syx);


/*************************************************************************/
/* Macro                                                                 */
/*************************************************************************/

bool scm_fcd_macro_p(ScmObj obj);
ScmObj scm_fcd_macro_P(ScmObj obj);
ScmObj scm_fcd_macro_new(scm_mem_type_t mtype, ScmObj transformer, ScmObj env);
ScmObj scm_fcd_make_macro(ScmObj transformer, ScmObj env);
ScmObj scm_fcd_macro_env(ScmObj macro);
int scm_fcd_trmp_macro_transformer(ScmObj macro, ScmObj form);


#endif /* INCLUDE_FCD_SYNTAX_H__ */
