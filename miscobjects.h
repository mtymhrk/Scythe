#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;

#define SCM_EOF(obj) ((ScmEof *)(obj))

#include "object.h"

extern const ScmTypeInfo SCM_EOF_TYPE_INFO;

ScmEOF *scm_eof_construct(void);
void scm_eof_destruct(ScmEOF *eof);
ScmEOF *scm_eof_instance(void);
bool scm_eof_is_eof(ScmObj obj);
void scm_eof_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /*  INCLUDE_MISCOBJECTS_H__ */
