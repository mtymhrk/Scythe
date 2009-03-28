#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

typedef struct ScmCharRec ScmChar;

#define SCM_CHAR(obj) ((ScmChar *)(obj))

#include "object.h"
#include "encoding.h"

extern const ScmTypeInfo SCM_CHAR_TYPE_INFO;

ScmChar *scm_char_construct(scm_char_t value, SCM_ENCODING_T enc);
void scm_char_destruct(ScmChar *charv);
ScmChar *scm_char_construct_newline(SCM_ENCODING_T enc);
ScmChar *scm_char_construct_space(SCM_ENCODING_T enc);
scm_char_t scm_char_value(ScmChar *charv);
SCM_ENCODING_T scm_char_encoding(ScmChar *charv);
bool scm_char_is_char(ScmObj obj);
void scm_char_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_CHAR_H__ */
