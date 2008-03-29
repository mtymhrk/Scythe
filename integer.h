#ifndef INCLUDE_INTEGER_H__
#define INCLUDE_INTEGER_H__

#include <stdbool.h>

typedef struct ScmIntegerRec ScmInteger;

#define SCM_INTEGER(obj) ((ScmInteger *)(obj))

ScmInteger *scm_integer_construct(long long value);
long long scm_integer_value(ScmInteger *integer);
bool scm_integer_is_integer(ScmObj obj);
ScmInteger *scm_integer_plus(ScmInteger *val1, ScmInteger *val2);
ScmInteger *scm_integer_minus(ScmInteger *val1, ScmInteger *val2);
ScmInteger *scm_integer_multiply(ScmInteger *val1, ScmInteger *val2);
ScmInteger *scm_integer_divide(ScmInteger *val1, ScmInteger *val2);
ScmInteger *scm_integer_reminder(ScmInteger *val1, ScmInteger *val2);

#endif /* INCLUDE_INTEGER_H__ */
