#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct ScmStringRec ScmString;

#define SCM_STRING(obj) ((ScmString *)(obj))

#include "object.h"
#include "api_enum.h"
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
  ScmEncoding *enc;
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

int scm_string_initialize(ScmObj str,
                          const void *src, size_t size, ScmEncoding *enc);
ScmObj scm_string_new(SCM_MEM_TYPE_T mtype,
                            const void *src, size_t size, ScmEncoding *enc);
ScmObj scm_string_copy(ScmObj src);
ScmObj scm_string_dup(ScmObj src);
size_t scm_string_length(ScmObj str);
size_t scm_string_bytesize(ScmObj str);
bool scm_string_is_equal(ScmObj str1, ScmObj str2);
ScmObj scm_string_encode(ScmObj str, ScmEncoding *enc);
ScmObj scm_string_substr(ScmObj str, size_t pos, size_t len);
int scm_string_push(ScmObj str, const scm_char_t *c);
int scm_string_append(ScmObj str, ScmObj append);
int scm_string_ref(ScmObj str, size_t pos, scm_char_t *chr);
int scm_string_set(ScmObj str, size_t pos, const scm_char_t *c);
ScmObj scm_string_downcase(ScmObj str);
ScmObj scm_string_upcase(ScmObj str);
int scm_string_cmp(ScmObj s1, ScmObj s2, int *rslt);
ssize_t scm_string_dump(ScmObj str, void *buf, size_t size);
ScmEncoding *scm_string_encoding(ScmObj str);
void *scm_string_content(ScmObj str);
scm_char_t *scm_string_to_char_ary(ScmObj str, size_t pos, ssize_t len,
                                   scm_char_t *ary);
int scm_string_obj_print(ScmObj obj, ScmObj port, bool ext_rep);
void scm_string_gc_initialize(ScmObj obj, ScmObj mem);
void scm_string_gc_finalize(ScmObj obj);
size_t scm_string_hash_value(ScmObj str);
int scm_string_inline_hex_escape(scm_char_t chr, ScmEncoding *enc, ScmObj port);

#endif /* INCLUDE_STRING_H__ */
