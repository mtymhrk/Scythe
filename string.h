#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct ScmStringRec ScmString;
typedef uint32_t scm_char_t;

typedef enum {
  //  SCM_STRING_ASCII,
  //  SCM_STRING_BINARY,
  //  SCM_STRING_UCS4,
  SCM_STRING_UTF8,
  //  SCM_STRING_EUCJP,
  //  SCM_STRING_SJIS,
  SMC_STRING_NR_ENC
} SCM_STRING_ENC_T;

#define SCM_STRING(obj) ((ScmString *)(obj))

#include "object.h"

ScmString *scm_string_construct(const char *src);
ScmString *scm_string_construct_new(const void *src,
                                    size_t size, SCM_STRING_ENC_T enc);
void scm_string_destruct(ScmString *str);
ScmString *scm_string_copy(const ScmString *src);
ScmString *scm_string_dup(ScmString *src);
size_t scm_string_length(ScmString *str);
size_t scm_string_bytesize(ScmString *str);
bool scm_string_is_equal(ScmString *str1, ScmString *str2);
ScmString *scm_string_substr(ScmString *str, unsigned int pos, size_t len);
ScmString *scm_string_push(ScmString *str, const scm_char_t c);
ScmString *scm_string_append(ScmString *str, const ScmString *append);
ScmString *scm_string_set(ScmString *str, unsigned int pos,
                          const scm_char_t c);
ScmString *scm_string_fill(ScmString *str, unsigned int pos,
                           size_t len, scm_char_t c);
int scm_string_find_chr(const ScmString *str, scm_char_t c);
int scm_string_match(const ScmString *str, const ScmString *pat);
bool scm_string_is_string(ScmObj obj);

#endif /* INCLUDE_STRING_H__ */
