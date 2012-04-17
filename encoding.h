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
  struct {
    scm_char_t c;
    size_t w;
  } lf;
  struct {
    scm_char_t c;
    size_t w;
  } sp;
  struct {
    scm_char_t c;
    size_t w;
  } cr;
  struct {
    scm_char_t c;
    size_t w;
  } tab;
  struct {
    scm_char_t c;
    size_t w;
  } bell;
  struct {
    scm_char_t c;
    size_t w;
  } bs;
} ScmEncConstants;

extern const ScmEncConstants SCM_ENCODING_CONST_ASCII;
extern const ScmEncConstants SCM_ENCODING_CONST_BIN;
extern const ScmEncConstants SCM_ENCODING_CONST_UTF8;
extern const ScmEncConstants SCM_ENCODING_CONST_UCS4;
extern const ScmEncConstants SCM_ENCODING_CONST_EUCJP;
extern const ScmEncConstants SCM_ENCODING_CONST_SJIS;
extern const ScmEncConstants *SCM_ENCODING_CONST_TBL[];

#define SCM_ENCODING_CONST(enc) (SCM_ENCODING_CONST_TBL[enc])
#define SCM_ENCODING_CONST_LF_CHR(enc)   (SCM_ENCODING_CONST(enc)->lf.c)
#define SCM_ENCODING_CONST_SP_CHR(enc)   (SCM_ENCODING_CONST(enc)->sp.c)
#define SCM_ENCODING_CONST_CR_CHR(enc)   (SCM_ENCODING_CONST(enc)->cr.c)
#define SCM_ENCODING_CONST_TAB_CHR(enc)  (SCM_ENCODING_CONST(enc)->tab.c)
#define SCM_ENCODING_CONST_BELL_CHR(enc) (SCM_ENCODING_CONST(enc)->bell.c)
#define SCM_ENCODING_CONST_BS_CHR(enc)   (SCM_ENCODING_CONST(enc)->bs.c)
#define SCM_ENCODING_CONST_LF_WIDTH(enc)   (SCM_ENCODING_CONST(enc)->lf.w)
#define SCM_ENCODING_CONST_SP_WIDTH(enc)   (SCM_ENCODING_CONST(enc)->sp.w)
#define SCM_ENCODING_CONST_CR_WIDTH(enc)   (SCM_ENCODING_CONST(enc)->cr.w)
#define SCM_ENCODING_CONST_TAB_WIDTH(enc)  (SCM_ENCODING_CONST(enc)->tab.w)
#define SCM_ENCODING_CONST_BELL_WIDTH(enc) (SCM_ENCODING_CONST(enc)->bell.w)
#define SCM_ENCODING_CONST_BS_WIDTH(enc)   (SCM_ENCODING_CONST(enc)->bs.w)


/* encoding depending function table */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, size_t idx);
  bool (*lf_p)(scm_char_t c);
  bool (*valid_char_p)(scm_char_t c);
  int (*to_ascii)(scm_char_t c);
  ssize_t (*ascii_to)(char ascii, scm_char_t *chr);
  long long (*to_scalar)(const void *p, size_t size);
  ssize_t (*scalar_to)(long long scalar, scm_char_t *c);
  bool (*ascii_p)(const void *p, size_t size);
  bool (*printable_p)(const void *p, size_t size);
  bool (*alarm_p)(const void *p, size_t size);
  bool (*backspace_p)(const void *p, size_t size);
  bool (*delete_p)(const void *p, size_t size);
  bool (*escape_p)(const void *p, size_t size);
  bool (*newline_p)(const void *p, size_t size);
  bool (*null_p)(const void *p, size_t size);
  bool (*return_p)(const void *p, size_t size);
  bool (*space_p)(const void *p, size_t size);
  bool (*tab_p)(const void *p, size_t size);
  bool (*doublequote_p)(const void *p, size_t size);
  bool (*backslash_p)(const void *p, size_t size);
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
#define SCM_ENCODING_VFUNC_LF_P(enc) (SCM_ENCODING_VFUNC(enc)->is_lf)
#define SCM_ENCODING_VFUNC_VALID_CHAR_P(enc) (SCM_ENCODING_VFUNC(enc)->valid_char_p)
#define SCM_ENCODING_VFUNC_TO_ASCII(enc) (SCM_ENCODING_VFUNC(enc)->to_ascii)
#define SCM_ENCODING_VFUNC_ASCII_TO(enc) (SCM_ENCODING_VFUNC(enc)->ascii_to)
#define SCM_ENCODING_VFUNC_TO_SCALAR(enc) (SCM_ENCODING_VFUNC(enc)->to_scalar)
#define SCM_ENCODING_VFUNC_SCALAR_TO(enc) (SCM_ENCODING_VFUNC(enc)->scalar_to)
#define SCM_ENCODING_VFUNC_ASCII_P(enc) (SCM_ENCODING_VFUNC(enc)->ascii_p)
#define SCM_ENCODING_VFUNC_PRINTABLE_P(enc) (SCM_ENCODING_VFUNC(enc)->printable_p)
#define SCM_ENCODING_VFUNC_ALARM_P(enc) (SCM_ENCODING_VFUNC(enc)->alarm_p)
#define SCM_ENCODING_VFUNC_BACKSPACE_P(enc) (SCM_ENCODING_VFUNC(enc)->backspace_p)
#define SCM_ENCODING_VFUNC_DELETE_P(enc) (SCM_ENCODING_VFUNC(enc)->delete_p)
#define SCM_ENCODING_VFUNC_ESCAPE_P(enc) (SCM_ENCODING_VFUNC(enc)->escape_p)
#define SCM_ENCODING_VFUNC_NEWLINE_P(enc) (SCM_ENCODING_VFUNC(enc)->newline_p)
#define SCM_ENCODING_VFUNC_NULL_P(enc) (SCM_ENCODING_VFUNC(enc)->null_p)
#define SCM_ENCODING_VFUNC_RETURN_P(enc) (SCM_ENCODING_VFUNC(enc)->return_p)
#define SCM_ENCODING_VFUNC_SPACE_P(enc) (SCM_ENCODING_VFUNC(enc)->space_p)
#define SCM_ENCODING_VFUNC_TAB_P(enc) (SCM_ENCODING_VFUNC(enc)->tab_p)
#define SCM_ENCODING_VFUNC_DOUBLEQUOTE_P(enc) (SCM_ENCODING_VFUNC(enc)->doublequote_p)
#define SCM_ENCODING_VFUNC_BACKSLASH_P(enc) (SCM_ENCODING_VFUNC(enc)->backslash_p)

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
long long scm_enc_to_scalar_ascii(const void *p, size_t size);
ssize_t scm_enc_scalar_to_ascii(long long scalar, scm_char_t *chr);
bool scm_enc_ascii_p_ascii(const void *p, size_t size);
bool scm_enc_printable_p_ascii(const void *p, size_t size);
bool scm_enc_alarm_p_ascii(const void *p, size_t size);
bool scm_enc_backspace_p_ascii(const void *p, size_t size);
bool scm_enc_delete_p_ascii(const void *p, size_t size);
bool scm_enc_escape_p_ascii(const void *p, size_t size);
bool scm_enc_newline_p_ascii(const void *p, size_t size);
bool scm_enc_null_p_ascii(const void *p, size_t size);
bool scm_enc_return_p_ascii(const void *p, size_t size);
bool scm_enc_space_p_ascii(const void *p, size_t size);
bool scm_enc_tab_p_ascii(const void *p, size_t size);
bool scm_enc_doublequote_p_ascii(const void *p, size_t size);
bool scm_enc_backslash_p_ascii(const void *p, size_t size);

/* ASCII-CMPT */
int scm_enc_char_width_ascii_cmpt(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ascii_cmpt(void *str, size_t size, size_t idx);
bool scm_enc_is_lf_ascii_cmpt(scm_char_t c);
bool scm_enc_is_space_ascii_cmpt(scm_char_t c);
bool scm_enc_valid_char_p_ascii_cmpt(scm_char_t c);
int scm_enc_to_ascii_ascii_cmpt(scm_char_t c);
ssize_t scm_enc_ascii_to_ascii_cmpt(char ascii, scm_char_t *chr);
long long scm_enc_to_scalar_ascii_cmpt(const void *p, size_t size);
bool scm_enc_printable_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_alarm_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_backspace_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_delete_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_escape_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_newline_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_null_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_return_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_space_p_ascii_cmpt(const void *p, size_t size);
bool scm_enc_tab_p_ascii_cmpt(const void *p, size_t size);

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
long long scm_enc_to_scalar_utf8(const void *p, size_t size);
ssize_t scm_enc_scalar_to_utf8(long long scalar, scm_char_t *chr);
bool scm_enc_ascii_p_utf8(const void *p, size_t size);
bool scm_enc_printable_p_utf8(const void *p, size_t size);
bool scm_enc_alarm_p_utf8(const void *p, size_t size);
bool scm_enc_backspace_p_utf8(const void *p, size_t size);
bool scm_enc_delete_p_utf8(const void *p, size_t size);
bool scm_enc_escape_p_utf8(const void *p, size_t size);
bool scm_enc_newline_p_utf8(const void *p, size_t size);
bool scm_enc_null_p_utf8(const void *p, size_t size);
bool scm_enc_return_p_utf8(const void *p, size_t size);
bool scm_enc_space_p_utf8(const void *p, size_t size);
bool scm_enc_tab_p_utf8(const void *p, size_t size);
bool scm_enc_doublequote_p_utf8(const void *p, size_t size);
bool scm_enc_backslash_p_utf8(const void *p, size_t size);

/* UCS4 */
int scm_enc_char_width_ucs4(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_ucs4(void *str, size_t size, size_t idx);
ssize_t scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len,
                             uint32_t *ucs4);
ssize_t scm_enc_ucs4_to_utf8(scm_char_ucs4_t ucs4, uint8_t *utf8, size_t sz);
bool scm_enc_is_lf_ucs4(scm_char_t c);
bool scm_enc_is_space_ucs4(scm_char_t c);
bool scm_enc_valid_char_p_ucs4(scm_char_t c);
int scm_enc_to_ascii_ucs4(scm_char_t c);
ssize_t scm_enc_ascii_to_ucs4(char ascii, scm_char_t *chr);
long long scm_enc_to_scalar_ucs4(const void *p, size_t size);
ssize_t scm_enc_scalar_to_ucs4(long long scalar, scm_char_t *chr);
bool scm_enc_ascii_p_ucs4(const void *p, size_t size);
bool scm_enc_printable_p_ucs4(const void *p, size_t size);
bool scm_enc_alarm_p_ucs4(const void *p, size_t size);
bool scm_enc_backspace_p_ucs4(const void *p, size_t size);
bool scm_enc_delete_p_ucs4(const void *p, size_t size);
bool scm_enc_escape_p_ucs4(const void *p, size_t size);
bool scm_enc_newline_p_ucs4(const void *p, size_t size);
bool scm_enc_null_p_ucs4(const void *p, size_t size);
bool scm_enc_return_p_ucs4(const void *p, size_t size);
bool scm_enc_space_p_ucs4(const void *p, size_t size);
bool scm_enc_tab_p_ucs4(const void *p, size_t size);
bool scm_enc_doublequote_p_ucs4(const void *p, size_t size);
bool scm_enc_backslash_p_ucs4(const void *p, size_t size);

/* EUC-JP */
int scm_enc_char_width_eucjp(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_eucjp(void *str, size_t size, size_t idx);
bool scm_enc_valid_char_p_eucjp(scm_char_t c);
int scm_enc_to_ascii_eucjp(scm_char_t c);
ssize_t scm_enc_ascii_to_eucjp(char ascii, scm_char_t *hr);
long long scm_enc_to_scalar_eucjp(const void *p, size_t size);
ssize_t scm_enc_scalar_to_eucjp(long long scalar, scm_char_t *chr);
bool scm_enc_ascii_p_eucjp(const void *p, size_t size);
bool scm_enc_printable_p_eucjp(const void *p, size_t size);
bool scm_enc_alarm_p_eucjp(const void *p, size_t size);
bool scm_enc_backspace_p_eucjp(const void *p, size_t size);
bool scm_enc_delete_p_eucjp(const void *p, size_t size);
bool scm_enc_escape_p_eucjp(const void *p, size_t size);
bool scm_enc_newline_p_eucjp(const void *p, size_t size);
bool scm_enc_null_p_eucjp(const void *p, size_t size);
bool scm_enc_return_p_eucjp(const void *p, size_t size);
bool scm_enc_space_p_eucjp(const void *p, size_t size);
bool scm_enc_tab_p_eucjp(const void *p, size_t size);
bool scm_enc_doublequote_p_eucjp(const void *p, size_t size);
bool scm_enc_backslash_p_eucjp(const void *p, size_t size);

/* SJIS */
int scm_enc_char_width_sjis(const void *str, size_t len);
ScmStrItr scm_enc_index2itr_sjis(void *str, size_t size, size_t idx);
bool scm_enc_valid_char_p_sjis(scm_char_t c);
int scm_enc_to_ascii_sjis(scm_char_t c);
ssize_t scm_enc_ascii_to_sjis(char ascii, scm_char_t *chr);
long long scm_enc_to_scalar_sjis(const void *p, size_t size);
ssize_t scm_enc_scalar_to_sjis(long long scalar, scm_char_t *chr);
bool scm_enc_ascii_p_sjis(const void *p, size_t size);
bool scm_enc_printable_p_sjis(const void *p, size_t size);
bool scm_enc_alarm_p_sjis(const void *p, size_t size);
bool scm_enc_backspace_p_sjis(const void *p, size_t size);
bool scm_enc_delete_p_sjis(const void *p, size_t size);
bool scm_enc_escape_p_sjis(const void *p, size_t size);
bool scm_enc_newline_p_sjis(const void *p, size_t size);
bool scm_enc_null_p_sjis(const void *p, size_t size);
bool scm_enc_return_p_sjis(const void *p, size_t size);
bool scm_enc_space_p_sjis(const void *p, size_t size);
bool scm_enc_tab_p_sjis(const void *p, size_t size);
bool scm_enc_doublequote_p_sjis(const void *p, size_t size);
bool scm_enc_backslash_p_sjis(const void *p, size_t size);

#endif /* INCLUDED_ENCODING_H__ */
