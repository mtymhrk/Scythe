#ifndef INCLUDE_EQUIVALENCE_H__
#define INCLUDE_EQUIVALENCE_H__

#include <stdbool.h>

#include "scythe/object.h"

bool scm_eq_p(ScmObj obj1, ScmObj obj2);
ScmObj scm_eq_P(ScmObj obj1, ScmObj obj2);
int scm_eqv(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_eqv_P(ScmObj obj1, ScmObj obj2);
int scm_equal(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_equal_P(ScmObj obj1, ScmObj obj2);

#endif /* INCLUDE_EQUIVALENCE_H__ */
