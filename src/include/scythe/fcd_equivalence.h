#ifndef INCLUDE_FCD_EQUIVALENCE_H__
#define INCLUDE_FCD_EQUIVALENCE_H__

#include <stdbool.h>

#include "scythe/object.h"

bool scm_fcd_eq_p(ScmObj obj1, ScmObj obj2);
ScmObj scm_fcd_eq_P(ScmObj obj1, ScmObj obj2);
int scm_fcd_eqv(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_fcd_eqv_P(ScmObj obj1, ScmObj obj2);
int scm_fcd_equal(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_fcd_equal_P(ScmObj obj1, ScmObj obj2);

#endif /* INCLUDE_FCD_EQUIVALENCE_H__ */
