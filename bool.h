#ifndef INCLUDE_BOOL_H__
#define INCLUDE_BOOL_H__

#include <stdbool.h>

typedef struct ScmBoolRec ScmBool;

#define SCM_BOOL(obj) ((ScmBool *)(obj))

#include "object.h"

extern const ScmTypeInfo SCM_BOOL_TYPE_INFO;

ScmBool *scm_bool_construct(bool value);
bool scm_bool_value(ScmBool *boolv);
bool scm_bool_is_bool(ScmObj obj);
void scm_bool_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_BOOL_H__ */
