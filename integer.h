#ifndef INCLUDE_INTEGER_H__
#define INCLUDE_INTEGER_H__

#include <stdbool.h>

typedef struct ScmIntegerRec ScmInteger;

#define SCM_INTEGER(obj) ((ScmInteger *)(obj))

#include "object.h"

struct ScmIntegerRec {
  ScmObjHeader header;
  long long value;
};

#define SCM_INTEGER_VALUE(obj) (SCM_INTEGER(obj)->value)

extern ScmTypeInfo SCM_INTEGER_TYPE_INFO;

void scm_integer_initialize(ScmObj integer, long long value);
void scm_integer_finalize(ScmObj integer);
ScmObj scm_integer_construct(SCM_MEM_ALLOC_TYPE_T mtype, long long value);
long long scm_integer_value(ScmObj integer);
bool scm_integer_is_integer(ScmObj obj);
ScmObj scm_integer_plus(ScmObj val1, ScmObj val2);
ScmObj scm_integer_minus(ScmObj val1, ScmObj val2);
ScmObj scm_integer_multiply(ScmObj val1, ScmObj val2);
ScmObj scm_integer_divide(ScmObj val1, ScmObj val2);
ScmObj scm_integer_reminder(ScmObj val1, ScmObj val2);
void scm_integer_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_INTEGER_H__ */
