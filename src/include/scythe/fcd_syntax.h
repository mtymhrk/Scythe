#ifndef INCLUDE_FCD_SYNTAX_H__
#define INCLUDE_FCD_SYNTAX_H__

#include <stdbool.h>

#include "scythe/object.h"

bool scm_fcd_syntax_p(ScmObj obj);
ScmObj scm_fcd_syntax_P(ScmObj obj);
ScmObj scm_fcd_make_syntax(ScmObj keyword, ScmObj handler);
ScmObj scm_fcd_syntax_keyword(ScmObj syx);
ScmObj scm_fcd_syntax_handler(ScmObj syx);

#endif /* INCLUDE_FCD_SYNTAX_H__ */
