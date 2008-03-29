#ifndef INCLUDE_NIL_H__
#define INCLUDE_NIL_H__

#include <stdbool.h>

typedef struct ScmNilRec ScmNil;

ScmNil *scm_nil_construct(void);
ScmNil *scm_nil_instance(void);
bool scm_nil_is_nil(ScmObj obj);

#endif /*  INCLUDE_NIL_H__ */
