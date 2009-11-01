#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;

#define SCM_EOF(obj) ((ScmEof *)(obj))

#include "object.h"

struct ScmEOFRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_EOF_TYPE_INFO;

void scm_eof_initialize(ScmObj eof);
void scm_eof_finalize(ScmObj eof);
ScmObj scm_eof_construct(void);
ScmObj scm_eof_instance(void);
bool scm_eof_is_eof(ScmObj obj);
void scm_eof_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /*  INCLUDE_MISCOBJECTS_H__ */
