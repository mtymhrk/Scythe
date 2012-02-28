#ifndef INCLUDED_ENCODING_H__
#define INCLUDED_ENCODING_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "api_enum.h"

typedef struct ScmStrItrRec {
  void *p;
  ssize_t rest;
  int (*char_width)(const void *p, size_t size);
} ScmStrItr;

#define SCM_STR_ITR_PTR(iter) ((iter)->p)
#define SCM_STR_ITR_REST(iter) ((iter)->rest)
#define SCM_STR_ITR_WIDTH(iter) \
  ((iter)->char_width(SCM_STR_ITR_PTR(iter), (size_t)SCM_STR_ITR_REST(iter)))
#define SCM_STR_ITR_COPY(iter, copy) (*(copy) = *(iter))
#define SCM_STR_ITR_OFFSET(iter, head) \
  (size_t)((uint8_t *)SCM_STR_ITR_PTR(iter) -  (uint8_t *)head);
#define SCM_STR_ITR_IS_END(iter) (((iter)->rest == 0) ? true : false)
#define SCM_STR_ITR_IS_ERR(iter) (((iter)->rest < 0) ? true : false)



typedef uint8_t scm_char_ascii_t;
typedef uint8_t scm_char_bin_t;
typedef uint32_t scm_char_utf8_t;
typedef uint32_t scm_char_ucs4_t;
typedef uint32_t scm_char_eucjp_t;
typedef uint16_t scm_char_sjis_t;

typedef union {
  uint8_t bytes[4]; /* 4 is size of largest member in scm_char_t. */
  scm_char_ascii_t ascii;
  scm_char_bin_t bin;
  scm_char_utf8_t utf8;
  scm_char_ucs4_t ucs4;
  scm_char_eucjp_t eucjp;
  scm_char_sjis_t sjis;
} scm_char_t;

extern const scm_char_t SCM_CHR_ZERO;

#define SCM_CHR_SET_ZERO(c) ((c = SCM_CHR_ZERO))
#define SCM_CHR_SET_ASCII(c, a)                 \
  do {                                          \
    SCM_CHR_SET_ZERO(c);                        \
    (c).ascii = (a);                            \
  } while(0)

#define SCM_CHR_ASCII(c) ((int)(c).ascii)

#define SCM_CHR_IS_EQUAL(c1, c2) \
  ((memcmp(&(c1), &(c2), sizeof(scm_char_t)) == 0) ? true : false)
#define SCM_CHR_IS_ZERO(c) SCM_CHR_IS_EQUAL(c, SCM_CHR_ZERO)

typedef struct ScmEncConstantsRec {
  scm_char_t lf_char;
  scm_char_t space_char;
} ScmEncConstants;

extern const ScmEncConstants SCM_ENCODING_CONST_ASCII;
extern const ScmEncConstants SCM_ENCODING_CONST_BIN;
extern const ScmEncConstants SCM_ENCODING_CONST_UTF8;
extern const ScmEncConstants SCM_ENCODING_CONST_UCS4;
extern const ScmEncConstants SCM_ENCODING_CONST_EUCJP;
extern const ScmEncConstants SCM_ENCODING_CONST_SJIS;
extern const ScmEncConstants *SCM_ENCODING_CONST_TBL[];

#define SCM_ENCODING_CONST(enc) (SCM_ENCODING_CONST_TBL[enc])
#define SCM_ENCODING_CONST_LF_CHAR(enc) (SCM_ENCODING_CONST(enc)->lf_char)
#define SCM_ENCODING_CONST_SPACE_CHAR(enc) (SCM_ENCODING_CONST(enc)->space_char)


/* encoding depending function table */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, size_t idx);
  bool (*is_lf)(scm_char_t c);
  bool (*is_space)(scm_char_t c);
  bool (*valid_char_p)(scm_char_t c);
  int (*to_ascii)(scm_char_t c);
  ssize_t (*ascii_to)(char ascii, scm_char_t *chr);
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
#define SCM_ENCODING_VFUNC_IS_LF(enc) (SCM_ENCODING_VFUNC(enc)->is_lf)
#define SCM_ENCODING_VFUNC_IS_SPACE(enc) (SCM_ENCODING_VFUNC(enc)->is_space)
#define SCM_ENCODING_VFUNC_VALID_P(enc) (SCM_ENCODING_VFUNC(enc)->valid_char_p)


ScmStrItr scm_str_itr_begin(void *p, size_t size,
                             int (*char_width)(const void *p, size_t size));
void scm_str_itr_next(ScmStrItr *iter);


/* ASCII */
int scm_enc_char_width_ascii(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ascii(void *str, size_t size, size_t idx);
bool scm_enc_is_lf_ascii(scm_char_t c);
bool scm_enc_is_space_ascii(scm_char_t c);
bool scm_enc_valid_char_p_ascii(scm_char_t c);
int scm_enc_to_ascii_ascii(scm_char_t c);
ssize_t scm_enc_ascii_to_ascii(char ascii, scm_char_t *chr);

/* BINARY */
int scm_enc_char_width_bin(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_bin(void *str, size_t size, size_t idx);
bool scm_enc_is_lf_bin(scm_char_t c);
bool scm_enc_is_space_bin(scm_char_t c);
bool scm_enc_valid_char_p_binary(scm_char_t c);
int scm_enc_to_ascii_binary(scm_char_t c);
ssize_t scm_enc_ascii_to_binary(char ascii, scm_char_t *chr);

/* UTF-8 */
int scm_enc_char_width_utf8(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_utf8(void *str, size_t size, size_t idx);
bool scm_enc_valid_char_p_utf8(scm_char_t c);
int scm_enc_to_ascii_utf8(scm_char_t c);
ssize_t scm_enc_ascii_to_utf8(char ascii, scm_char_t *chr);

/* UCS4 */
int scm_enc_char_width_ucs4(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ucs4(void *str, size_t size, size_t idx);
ssize_t scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len,
                             uint32_t *ucs4);
bool scm_enc_is_lf_ucs4(scm_char_t c);
bool scm_enc_is_space_ucs4(scm_char_t c);
bool scm_enc_valid_char_p_ucs4(scm_char_t c);
int scm_enc_to_ascii_ucs4(scm_char_t c);
ssize_t scm_enc_ascii_to_ucs4(char ascii, scm_char_t *chr);

/* EUC-JP */
int scm_enc_char_width_eucjp(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_eucjp(void *str, size_t size, size_t idx);
bool scm_enc_valid_char_p_eucjp(scm_char_t c);
int scm_enc_to_ascii_eucjp(scm_char_t c);
ssize_t scm_enc_ascii_to_eucjp(char ascii, scm_char_t *chr);

/* SJIS */
int scm_enc_char_width_sjis(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_sjis(void *str, size_t size, size_t idx);
bool scm_enc_valid_char_p_sjis(scm_char_t c);
int scm_enc_to_ascii_sjis(scm_char_t c);
ssize_t scm_enc_ascii_to_sjis(char ascii, scm_char_t *chr);

#endif /* INCLUDED_ENCODING_H__ */
