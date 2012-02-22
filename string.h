#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct ScmStringRec ScmString;

#define SCM_STRING(obj) ((ScmString *)(obj))

#include "object.h"
#include "api.h"
#include "encoding.h"

extern ScmTypeInfo SCM_STRING_TYPE_INFO;

struct ScmStringRec {
  ScmObjHeader header;
  uint8_t *buffer;
  uint8_t *head;
  size_t capacity;
  size_t bytesize;
  size_t length;
  int *ref_cnt;
  SCM_ENCODING_T enc;
};

#define SCM_STRING_BUFFER(obj) (SCM_STRING(obj)->buffer)
#define SCM_STRING_HEAD(obj) (SCM_STRING(obj)->head)
#define SCM_STRING_CAPACITY(obj) (SCM_STRING(obj)->capacity)
#define SCM_STRING_BYTESIZE(obj) (SCM_STRING(obj)->bytesize)
#define SCM_STRING_LENGTH(obj) (SCM_STRING(obj)->length)
#define SCM_STRING_REF_CNT(obj) (SCM_STRING(obj)->ref_cnt)
#define SCM_STRING_ENC(obj) (SCM_STRING(obj)->enc)
#define SCM_STRING_BYTE_AT(obj, idx) (SCM_STRING(obj)->head[idx])
#define SCM_STRING_INC_REF_CNT(obj) ((*SCM_STRING(obj)->ref_cnt)++)
#define SCM_STRING_DEC_REF_CNT(obj) ((*SCM_STRING(obj)->ref_cnt)--)

void scm_string_initialize(ScmObj str,
                           const void *src, size_t size, SCM_ENCODING_T enc);
ScmObj scm_string_new(SCM_CAPI_MEM_TYPE_T mtype,
                            const void *src, size_t size, SCM_ENCODING_T enc);
ScmObj scm_string_copy(ScmObj src);
ScmObj scm_string_dup(ScmObj src);
size_t scm_string_length(ScmObj str);
size_t scm_string_bytesize(ScmObj str);
bool scm_string_is_equal(ScmObj str1, ScmObj str2);
ScmObj scm_string_substr(ScmObj str, size_t pos, size_t len);
ScmObj scm_string_push(ScmObj str, const scm_char_t c);
ScmObj scm_string_append(ScmObj str, ScmObj append);
scm_char_t scm_string_ref(ScmObj str, size_t pos);
ScmObj scm_string_set(ScmObj str, size_t pos, const scm_char_t c);
ScmObj scm_string_fill(ScmObj str, size_t pos, size_t len, scm_char_t c);
ssize_t scm_string_find_chr(ScmObj str, scm_char_t c);
ssize_t scm_string_match(ScmObj str, ScmObj pat);
ssize_t scm_string_dump(ScmObj str, void *buf, size_t size);
SCM_ENCODING_T scm_string_encoding(ScmObj str);
void *scm_string_content(ScmObj str);
void scm_string_gc_initialize(ScmObj obj, ScmObj mem);
void scm_string_gc_finalize(ScmObj obj);
size_t scm_string_hash_value(ScmObj str);

#endif /* INCLUDE_STRING_H__ */
