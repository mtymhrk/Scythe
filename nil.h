#ifndef INCLUDE_NIL_H__
#define INCLUDE_NIL_H__

#include <stdbool.h>

typedef struct ScmNilRec ScmNil;

extern const ScmTypeInfo SCM_NIL_TYPE_INFO;

ScmNil *scm_nil_construct(void);
ScmNil *scm_nil_instance(void);
bool scm_nil_is_nil(ScmObj obj);
void scm_nil_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /*  INCLUDE_NIL_H__ */
