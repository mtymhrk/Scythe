#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdbool.h>

typedef struct ScmStringRec ScmString;

#define SCM_STRING(obj) ((ScmString *)(obj))

#include "object.h"

ScmString *scm_string_construct(const char *str);
char *scm_string_string(const ScmString *string);
size_t scm_string_length(const ScmString *string);
bool scm_string_is_string(ScmObj obj);

#endif /* INCLUDE_STRING_H__ */
