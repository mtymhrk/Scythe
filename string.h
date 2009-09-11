#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct ScmStringRec ScmString;

#define SCM_STRING(obj) ((ScmString *)(obj))

#include "object.h"
#include "encoding.h"

extern ScmTypeInfo SCM_STRING_TYPE_INFO;

ScmString *scm_string_construct(const void *src,
                                size_t size, SCM_ENCODING_T enc);
void scm_string_destruct(ScmString *str);
ScmString *scm_string_copy(const ScmString *src);
ScmString *scm_string_dup(ScmString *src);
size_t scm_string_length(ScmString *str);
size_t scm_string_bytesize(ScmString *str);
bool scm_string_is_equal(ScmString *str1, ScmString *str2);
ScmString *scm_string_substr(ScmString *str, unsigned int pos, size_t len);
ScmString *scm_string_push(ScmString *str, const scm_char_t c);
ScmString *scm_string_append(ScmString *str, const ScmString *append);
scm_char_t scm_string_ref(ScmString *str, unsigned int pos);
ScmString *scm_string_set(ScmString *str, unsigned int pos,
                          const scm_char_t c);
ScmString *scm_string_fill(ScmString *str, unsigned int pos,
                           size_t len, scm_char_t c);
int scm_string_find_chr(const ScmString *str, scm_char_t c);
int scm_string_match(const ScmString *str, const ScmString *pat);
ssize_t scm_string_dump(const ScmString *str, void *buf, size_t size);
SCM_ENCODING_T scm_string_encoding(const ScmString *str);
void *scm_string_content(const ScmString *str);
bool scm_string_is_string(ScmObj obj);
void scm_string_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_string_gc_finalize(ScmObj obj);

#endif /* INCLUDE_STRING_H__ */
