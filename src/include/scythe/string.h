#ifndef INCLUDE_STRING_H__
#define INCLUDE_STRING_H__

#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/memory.h"

typedef struct ScmStringRec ScmString;

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

#define SCM_STRING(obj) ((ScmString *)(obj))
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

extern ScmTypeInfo SCM_STRING_TYPE_INFO;

ScmObj scm_string_P(ScmObj obj);
int scm_string_initialize(ScmObj str,
                          const void *src, size_t size, ScmEncoding *enc);
void scm_string_finalize(ScmObj str);
ScmObj scm_string_new(scm_mem_type_t mtype,
                      const void *src, size_t size, ScmEncoding *enc);
ScmObj scm_make_string_from_cstr(const char *str, ScmEncoding *enc);
ScmObj scm_make_string_from_bin(const void *data, size_t size,
                                ScmEncoding *enc);
ScmObj scm_make_string_from_external(const void *data, size_t size,
                                     const char *enc);
ScmObj scm_string_cv(const ScmObj *chr, size_t n);
ScmObj scm_string(size_t n, ...);
ScmObj scm_string_dup(ScmObj src);
bool scm_string_equal_p(ScmObj str1, ScmObj str2);
int scm_string_cmp(ScmObj s1, ScmObj s2, int *rslt);
int scm_string_eq(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_string_eq_P(ScmObj s1, ScmObj s2);
ScmObj scm_string_eq_P_lst(ScmObj lst);
int scm_string_lt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_string_lt_P(ScmObj s1, ScmObj s2);
ScmObj scm_string_lt_P_lst(ScmObj lst);
int scm_string_gt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_string_gt_P(ScmObj s1, ScmObj s2);
ScmObj scm_string_gt_P_lst(ScmObj lst);
int scm_string_le(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_string_le_P(ScmObj s1, ScmObj s2);
ScmObj scm_string_le_P_lst(ScmObj lst);
int scm_string_ge(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_string_ge_P(ScmObj s1, ScmObj s2);
ScmObj scm_string_ge_P_lst(ScmObj lst);
ScmObj scm_string_encode(ScmObj str, ScmEncoding *enc);
ScmObj scm_string_substr(ScmObj str, size_t pos, size_t len);
int scm_string_push_cchr(ScmObj str, scm_char_t chr, ScmEncoding *enc);
int scm_string_push_string(ScmObj str, ScmObj append);
int scm_string_ref_cchr(ScmObj str, size_t pos, scm_char_t *chr);
ScmObj scm_string_ref_char(ScmObj str, size_t pos);
int scm_string_set_cchr(ScmObj str, size_t pos, scm_char_t chr, ScmEncoding *enc);
int scm_string_set_char(ScmObj str, size_t pos, ScmObj chr);
ScmObj scm_string_downcase(ScmObj str);
ScmObj scm_string_upcase(ScmObj str);
ScmObj scm_string_append_lst(ScmObj lst);
ScmObj scm_string_append_cv(ScmObj *ary, size_t n);
ScmObj scm_string_append(size_t n, ...);
ScmObj scm_string_copy(ScmObj str, ssize_t start, ssize_t end);
int scm_string_copy_i(ScmObj to, size_t at,
                      ScmObj from, ssize_t start, ssize_t end);
int scm_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end);
scm_char_t *scm_string_to_cchr_ary(ScmObj str, size_t pos, ssize_t len,
                                   scm_char_t *ary);
ScmObj scm_string_to_list(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_list_to_string(ScmObj lst);
char *scm_string_to_cstr(ScmObj str, char *cstr, size_t size);
ssize_t scm_string_to_path_cstr(ScmObj str, char *cstr, size_t sz);
int scm_string_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_string_gc_initialize(ScmObj obj);
void scm_string_gc_finalize(ScmObj obj);
size_t scm_string_hash_value(ScmObj str);
int scm_string_inline_hex_escape(scm_char_t chr, ScmEncoding *enc, ScmObj port);

static inline bool
scm_string_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_STRING_TYPE_INFO) ? true : false;
}

static inline ScmObj
scm_string_lst(ScmObj lst)
{
  return scm_list_to_string(lst);
}

static inline size_t
scm_string_length(ScmObj str)
{
  scm_assert(scm_string_p(str));
  return SCM_STRING_LENGTH(str);
}

static inline size_t
scm_string_bytesize(ScmObj str)
{
  scm_assert(scm_string_p(str));
  return SCM_STRING_BYTESIZE(str);
}

static inline ScmEncoding *
scm_string_encoding(ScmObj str)
{
  scm_assert(scm_string_p(str));
  return SCM_STRING_ENC(str);
}

static inline void *
scm_string_content(ScmObj str)
{
  scm_assert(scm_string_p(str));
  return SCM_STRING_HEAD(str);
}


#endif /* INCLUDE_STRING_H__ */
