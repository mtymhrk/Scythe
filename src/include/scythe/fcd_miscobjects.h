#ifndef INCLUDE_FCD_MISCOBJECTS_H__
#define FCD_MISCOBJECTS_H

#include <stdbool.h>

#include "scythe/object.h"

ScmObj scm_fcd_nil(void);
bool scm_fcd_nil_p(ScmObj obj);
ScmObj scm_fcd_nil_P(ScmObj obj);

#define SCM_NIL_OBJ scm_fcd_nil()


bool scm_fcd_boolean_p(ScmObj obj);
ScmObj scm_fcd_boolean_P(ScmObj obj);
ScmObj scm_fcd_true(void);
ScmObj scm_fcd_false(void);
bool scm_fcd_true_object_p(ScmObj obj);
bool scm_fcd_false_object_p(ScmObj obj);
bool scm_fcd_true_p(ScmObj obj);
bool scm_fcd_false_p(ScmObj obj);
ScmObj scm_fcd_not(ScmObj obj);

#define SCM_TRUE_OBJ scm_fcd_true()
#define SCM_FALSE_OBJ scm_fcd_false()


ScmObj scm_fcd_eof(void);
bool scm_fcd_eof_object_p(ScmObj obj);
ScmObj scm_fcd_eof_object_P(ScmObj obj);

#define SCM_EOF_OBJ scm_fcd_eof()


ScmObj scm_fcd_undef(void);
bool scm_fcd_undef_object_p(ScmObj obj);

#define SCM_UNDEF_OBJ scm_fcd_undef()


bool scm_fcd_landmine_object_p(ScmObj obj);


#endif /* INCLUDE_FCD_MISCOBJECTS_H__ */
