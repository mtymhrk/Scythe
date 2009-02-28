#ifndef INCLUDED_ENCODING_H__
#define INCLUDED_ENCODING_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

typedef enum {
  //  SCM_STRING_ASCII,
  //  SCM_STRING_BINARY,
  SCM_ENCODING_UCS4,
  SCM_ENCODING_UTF8,
  //  SCM_STRING_EUCJP,
  //  SCM_STRING_SJIS,
  SMC_ENCODING_NR_ENC
} SCM_ENCODING_T;

typedef struct ScmStrItrRec {
  void *p;
  ssize_t rest;
  int (*char_width)(const void *p, size_t size);
} ScmStrItr;

typedef uint32_t scm_char_t;

#define SCM_STR_ITR_PTR(iter) ((iter)->p)
#define SCM_STR_ITR_REST(iter) ((iter)->rest)
#define SCM_STR_ITR_WIDTH(iter) \
  ((iter)->char_width(SCM_STR_ITR_PTR(iter), SCM_STR_ITR_REST(iter)))
#define SCM_STR_ITR_COPY(iter, copy) (*(copy) = *(iter))
#define SCM_STR_ITR_OFFSET(iter, head) \
  ((uint8_t *)SCM_STR_ITR_PTR(iter) -  (uint8_t *)head);
#define SCM_STR_ITR_IS_END(iter) (((iter)->rest == 0) ? true : false)
#define SCM_STR_ITR_IS_ERR(iter) (((iter)->rest < 0) ? true : false)


ScmStrItr scm_str_itr_begin(void *p, size_t size,
                             int (*char_width)(const void *p, size_t size));
ScmStrItr scm_str_itr_next(const ScmStrItr *iter);

/* UTF-8 */
int scm_enc_char_width_utf8(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_utf8(void *str, size_t size, unsigned int idx);

/* UCS4 */
int scm_enc_char_width_ucs4(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ucs4(void *str, size_t size, unsigned int idx);
ssize_t scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len,
                             uint32_t *ucs4);

#endif /* INCLUDED_ENCODING_H__ */
