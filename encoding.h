#ifndef INCLUDED_ENCODING_H__
#define INCLUDED_ENCODING_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>


typedef enum {
  SCM_ENCODING_ASCII,
  SCM_STRING_BINARY,
  SCM_ENCODING_UCS4,
  SCM_ENCODING_UTF8,
  SCM_ENCODING_EUCJP,
  SCM_ENCODING_SJIS,
  SMC_ENCODING_NR_ENC
} SCM_ENCODING_T;



typedef struct ScmStrItrRec {
  void *p;
  ssize_t rest;
  int (*char_width)(const void *p, size_t size);
} ScmStrItr;

#define SCM_STR_ITR_PTR(iter) ((iter)->p)
#define SCM_STR_ITR_REST(iter) ((iter)->rest)
#define SCM_STR_ITR_WIDTH(iter) \
  ((iter)->char_width(SCM_STR_ITR_PTR(iter), SCM_STR_ITR_REST(iter)))
#define SCM_STR_ITR_COPY(iter, copy) (*(copy) = *(iter))
#define SCM_STR_ITR_OFFSET(iter, head) \
  ((uint8_t *)SCM_STR_ITR_PTR(iter) -  (uint8_t *)head);
#define SCM_STR_ITR_IS_END(iter) (((iter)->rest == 0) ? true : false)
#define SCM_STR_ITR_IS_ERR(iter) (((iter)->rest < 0) ? true : false)



typedef uint8_t scm_char_ascii_t;
typedef uint8_t scm_char_bin_t;
typedef uint32_t scm_char_utf8_t;
typedef uint32_t scm_char_ucs4_t;
typedef uint32_t scm_char_eucjp_t;
typedef uint16_t scm_char_sjis_t;

typedef union {
  scm_char_ascii_t ascii;
  scm_char_bin_t bin;
  scm_char_utf8_t utf8;
  scm_char_ucs4_t ucs4;
  scm_char_eucjp_t eucjp;
  scm_char_sjis_t sjis;
} scm_char_t;

extern const scm_char_t SCM_CHR_ZERO;

#define SCM_CHR_IS_ZERO(c) \
  ((memcmp(&(c), &SCM_CHR_ZERO, sizeof(scm_char_t)) == 0) ? true : false)



/* encoding depending function table */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, unsigned int idx);
} ScmEncVirtualFunc;

extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_ASCII;
extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_BIN;
extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_UTF8;
extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_UCS4;
extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_EUCJP;
extern const ScmEncVirtualFunc SCM_ENCODING_VFUNC_SJIS;
extern const ScmEncVirtualFunc *SCM_ENCODING_VFUNC_TBL[];

#define SCM_ENCODING_VFUNC(enc) (SCM_ENCODING_VFUNC_TBL[enc])
#define SCM_ENCODING_VFUNC_CHAR_WIDTH(enc) (SCM_ENCODING_VFUNC(enc)->char_width)
#define SCM_ENCODING_VFUNC_INDEX2ITER(enc) (SCM_ENCODING_VFUNC(enc)->index2iter)



ScmStrItr scm_str_itr_begin(void *p, size_t size,
                             int (*char_width)(const void *p, size_t size));
ScmStrItr scm_str_itr_next(const ScmStrItr *iter);


/* ASCII */
int scm_enc_char_width_ascii(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ascii(void *str, size_t size, unsigned int idx);

/* BINARY */
int scm_enc_char_width_bin(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_bin(void *str, size_t size, unsigned int idx);

/* UTF-8 */
int scm_enc_char_width_utf8(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_utf8(void *str, size_t size, unsigned int idx);

/* UCS4 */
int scm_enc_char_width_ucs4(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ucs4(void *str, size_t size, unsigned int idx);
ssize_t scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len,
                             uint32_t *ucs4);

/* EUC-JP */
int scm_enc_char_width_eucjp(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_eucjp(void *str, size_t size, unsigned int idx);

/* SJIS */
int scm_enc_char_width_sjis(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_sjis(void *str, size_t size, unsigned int idx);



#endif /* INCLUDED_ENCODING_H__ */
