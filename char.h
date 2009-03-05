#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

typedef struct ScmCharRec ScmChar;

#define SCM_CHAR(obj) ((ScmChar *)(obj))

#include "object.h"
#include "encoding.h"

ScmChar *scm_char_construct(scm_char_t value, SCM_ENCODING_T enc);
ScmChar *scm_char_construct_newline(SCM_ENCODING_T enc);
ScmChar *scm_char_construct_space(SCM_ENCODING_T enc);
scm_char_t scm_char_value(ScmChar *charv);
bool scm_char_is_char(ScmObj obj);


#endif /* INCLUDE_CHAR_H__ */
