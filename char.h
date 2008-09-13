#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

typedef struct ScmCharRec ScmChar;

#define SCM_CHAR(obj) ((ScmChar *)(obj))

#include "object.h"

ScmChar *scm_char_construct(unsigned int value);
unsigned int scm_char_value(ScmChar *charv);
bool scm_char_is_char(ScmObj obj);


#endif /* INCLUDE_CHAR_H__ */
