#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;

#include "object.h"

ScmEOF *scm_eof_construct(void);
ScmEOF *scm_eof_instance(void);
bool scm_eof_is_eof(ScmObj obj);

#endif /*  INCLUDE_MISCOBJECTS_H__ */
