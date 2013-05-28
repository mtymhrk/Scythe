#ifndef INCLUDED_ENCODING_H__
#define INCLUDED_ENCODING_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct ScmEncodingRec ScmEncoding;
typedef struct ScmStrItrRec ScmStrItr;


/***********************************************************************/
/*   scm_char_t                                                        */
/***********************************************************************/

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


/***********************************************************************/
/*   ScmEncoding                                                       */
/***********************************************************************/

struct ScmEncodingRec {
  const char *iconv_name;
  struct {
    struct { scm_char_t c; size_t w; } lf;
    struct { scm_char_t c; size_t w; } sp;
    struct { scm_char_t c; size_t w; } cr;
    struct { scm_char_t c; size_t w; } tab;
    struct { scm_char_t c; size_t w; } bell;
    struct { scm_char_t c; size_t w; } bs;
  } cnst;
  struct {
    int (*char_width)(const void *p, size_t size);
    void (*index2itr)(void *p, size_t size, size_t idx, ScmStrItr *iter);
    bool (*valid_char_p)(const scm_char_t *c);
    int (*cnv_to_ascii)(const scm_char_t *c);
    ssize_t (*cnv_from_ascii)(char ascii, scm_char_t *chr);
    long long (*cnv_to_scalar)(const void *p, size_t size);
    ssize_t (*cnv_from_scalar)(long long scalar, scm_char_t *c);
    ssize_t (*downcase)(const void *p, size_t s, scm_char_t *c);
    ssize_t (*upcase)(const void *p, size_t s, scm_char_t *c);
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
  } func;
};

extern ScmEncoding *SCM_ENC_ASCII;
extern ScmEncoding *SCM_ENC_UTF8;
extern ScmEncoding *SCM_ENC_UCS4;
extern ScmEncoding *SCM_ENC_EUCJP;
extern ScmEncoding *SCM_ENC_SJIS;

inline void
scm_enc_chr_lf(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.lf.c;
  if (w != NULL) *w = enc->cnst.lf.w;
}

inline void
scm_enc_chr_sp(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.sp.c;
  if (w != NULL) *w = enc->cnst.sp.w;
}

inline void
scm_enc_chr_cr(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.cr.c;
  if (w != NULL) *w = enc->cnst.cr.w;
}

inline void
scm_enc_chr_tab(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.tab.c;
  if (w != NULL) *w = enc->cnst.tab.w;
}

inline void
scm_enc_chr_bell(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.bell.c;
  if (w != NULL) *w = enc->cnst.bell.w;
}

inline void
scm_enc_chr_bs(ScmEncoding *enc, scm_char_t *chr, size_t *w)
{
  assert(enc != NULL);
  if (chr != NULL) *chr = enc->cnst.bs.c;
  if (w != NULL) *w = enc->cnst.bs.w;
}

inline int
scm_enc_char_width(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.char_width != NULL);
  return enc->func.char_width(p, size);
}

inline void
scm_enc_index2itr(ScmEncoding *enc,
                  void *p, size_t size, size_t idx, ScmStrItr *iter)
{
  assert(enc != NULL); assert(enc->func.index2itr != NULL);
  return enc->func.index2itr(p, size, idx, iter);
}

inline bool
scm_enc_valid_char_p(ScmEncoding *enc, const scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.valid_char_p != NULL);
  return enc->func.valid_char_p(c);
}

inline int
scm_enc_cnv_to_ascii(ScmEncoding *enc, const scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.cnv_to_ascii != NULL);
  return enc->func.cnv_to_ascii(c);
}

inline ssize_t
scm_enc_cnv_from_ascii(ScmEncoding *enc, char ascii, scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.cnv_from_ascii != NULL);
  return enc->func.cnv_from_ascii(ascii, c);
}

inline long long
scm_enc_cnv_to_scalar(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.cnv_to_scalar != NULL);
  return enc->func.cnv_to_scalar(p, size);
}

inline ssize_t
scm_enc_cnv_from_scalar(ScmEncoding *enc, long long scalar, scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.cnv_from_scalar != NULL);
  return enc->func.cnv_from_scalar(scalar, c);
}

inline ssize_t
scm_enc_downcase(ScmEncoding *enc, const void *p, size_t s, scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.downcase != NULL);
  return enc->func.downcase(p, s, c);
}

inline ssize_t
scm_enc_upcase(ScmEncoding *enc, const void *p, size_t s, scm_char_t *c)
{
  assert(enc != NULL); assert(enc->func.upcase != NULL);
  return enc->func.upcase(p, s, c);
}

inline bool
scm_enc_ascii_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.ascii_p != NULL);
  return enc->func.ascii_p(p, size);
}

inline bool
scm_enc_printable_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.printable_p != NULL);
  return enc->func.printable_p(p, size);
}

inline bool
scm_enc_alarm_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.alarm_p != NULL);
  return enc->func.alarm_p(p, size);
}

inline bool
scm_enc_backspace_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.backspace_p != NULL);
  return enc->func.backspace_p(p, size);
}

inline bool
scm_enc_delete_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.delete_p != NULL);
  return enc->func.delete_p(p, size);
}

inline bool
scm_enc_escape_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.escape_p != NULL);
  return enc->func.escape_p(p, size);
}

inline bool
scm_enc_newline_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.newline_p != NULL);
  return enc->func.newline_p(p, size);
}

inline bool
scm_enc_null_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.null_p != NULL);
  return enc->func.null_p(p, size);
}

inline bool
scm_enc_return_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.return_p != NULL);
  return enc->func.return_p(p, size);
}

inline bool
scm_enc_space_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.space_p != NULL);
  return enc->func.space_p(p, size);
}

inline bool
scm_enc_tab_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.tab_p != NULL);
  return enc->func.tab_p(p, size);
}

inline bool
scm_enc_doublequote_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.doublequote_p != NULL);
  return enc->func.doublequote_p(p, size);
}

inline bool
scm_enc_backslash_p(ScmEncoding *enc, const void *p, size_t size)
{
  assert(enc != NULL); assert(enc->func.backslash_p != NULL);
  return enc->func.backslash_p(p, size);
}


/***********************************************************************/
/*   Encoding: ASCII                                                   */
/***********************************************************************/

int scm_enc_char_width_ascii(const void *str, size_t len);
void scm_enc_index2itr_ascii(void *str, size_t size, size_t idx,
                             ScmStrItr *iter);
bool scm_enc_valid_char_p_ascii(const scm_char_t *c);
int scm_enc_cnv_to_ascii_ascii(const scm_char_t *c);
ssize_t scm_enc_cnv_from_ascii_ascii(char ascii, scm_char_t *chr);
long long scm_enc_cnv_to_scalar_ascii(const void *p, size_t size);
ssize_t scm_enc_cnv_from_scalar_ascii(long long scalar, scm_char_t *chr);
ssize_t scm_enc_downcase_ascii(const void *p, size_t s, scm_char_t *chr);
ssize_t scm_enc_upcase_ascii(const void *p, size_t s, scm_char_t *chr);
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


/***********************************************************************/
/*   Encoding: UTF-8                                                   */
/***********************************************************************/

int scm_enc_char_width_utf8(const void *str, size_t len);
void scm_enc_index2itr_utf8(void *str, size_t size, size_t idx,
                            ScmStrItr *iter);
bool scm_enc_valid_char_p_utf8(const scm_char_t *c);
int scm_enc_cnv_to_ascii_utf8(const scm_char_t *c);
ssize_t scm_enc_cnv_from_ascii_utf8(char ascii, scm_char_t *chr);
long long scm_enc_cnv_to_scalar_utf8(const void *p, size_t size);
ssize_t scm_enc_cnv_from_scalar_utf8(long long scalar, scm_char_t *chr);
ssize_t scm_enc_downcase_utf8(const void *p, size_t s, scm_char_t *chr);
ssize_t scm_enc_upcase_utf8(const void *p, size_t s, scm_char_t *chr);
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


/***********************************************************************/
/*   Encoding: UCS4                                                    */
/***********************************************************************/

int scm_enc_char_width_ucs4(const void *str, size_t len);
void scm_enc_index2itr_ucs4(void *str, size_t size, size_t idx,
                            ScmStrItr *iter);
ssize_t scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len,
                             uint32_t *ucs4);
ssize_t scm_enc_ucs4_to_utf8(scm_char_ucs4_t ucs4, uint8_t *utf8, size_t sz);
bool scm_enc_valid_char_p_ucs4(const scm_char_t *c);
int scm_enc_cnv_to_ascii_ucs4(const scm_char_t *c);
ssize_t scm_enc_cnv_from_ascii_ucs4(char ascii, scm_char_t *chr);
long long scm_enc_cnv_to_scalar_ucs4(const void *p, size_t size);
ssize_t scm_enc_cnv_from_scalar_ucs4(long long scalar, scm_char_t *chr);
ssize_t scm_enc_downcase_ucs4(const void *p, size_t s, scm_char_t *chr);
ssize_t scm_enc_upcase_ucs4(const void *p, size_t s, scm_char_t *chr);
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


/***********************************************************************/
/*   Encoding: EUC-JP-JIS-2004                                         */
/***********************************************************************/

int scm_enc_char_width_eucjp(const void *str, size_t len);
void scm_enc_index2itr_eucjp(void *str, size_t size, size_t idx,
                             ScmStrItr *iter);
bool scm_enc_valid_char_p_eucjp(const scm_char_t *c);
int scm_enc_cnv_to_ascii_eucjp(const scm_char_t *c);
ssize_t scm_enc_cnv_from_ascii_eucjp(char ascii, scm_char_t *hr);
long long scm_enc_cnv_to_scalar_eucjp(const void *p, size_t size);
ssize_t scm_enc_cnv_from_scalar_eucjp(long long scalar, scm_char_t *chr);
ssize_t scm_enc_downcase_eucjp(const void *p, size_t s, scm_char_t *chr);
ssize_t scm_enc_upcase_eucjp(const void *p, size_t s, scm_char_t *chr);
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


/***********************************************************************/
/*   Encoding: SJIS                                                    */
/***********************************************************************/

int scm_enc_char_width_sjis(const void *str, size_t len);
void scm_enc_index2itr_sjis(void *str, size_t size, size_t idx,
                            ScmStrItr *iter);
bool scm_enc_valid_char_p_sjis(const scm_char_t *c);
int scm_enc_cnv_to_ascii_sjis(const scm_char_t *c);
ssize_t scm_enc_cnv_from_ascii_sjis(char ascii, scm_char_t *chr);
long long scm_enc_cnv_to_scalar_sjis(const void *p, size_t size);
ssize_t scm_enc_cnv_from_scalar_sjis(long long scalar, scm_char_t *chr);
ssize_t scm_enc_downcase_sjis(const void *p, size_t s, scm_char_t *chr);
ssize_t scm_enc_upcase_sjis(const void *p, size_t s, scm_char_t *chr);
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


/***********************************************************************/
/*   Iterater                                                          */
/***********************************************************************/

struct ScmStrItrRec {
  void *p;
  ssize_t rest;
  ScmEncoding *enc;
};

inline bool
scm_str_itr_end_p(const ScmStrItr *iter)
{
  assert(iter != NULL);
  return (iter->rest == 0) ? true : false;
}

inline bool
scm_str_itr_err_p(const ScmStrItr *iter)
{
  assert(iter != NULL);
  return (iter->rest < 0) ? true : false;
}

inline void *
scm_str_itr_ptr(const ScmStrItr *iter)
{
  assert(iter != NULL);
  return iter->p;
}

inline ssize_t
scm_str_itr_rest(const ScmStrItr *iter)
{
  assert(iter != NULL);
  return iter->rest;
}

inline ScmEncoding *
scm_str_itr_enc(const ScmStrItr *iter)
{
  assert(iter != NULL);
  return iter->enc;
}

inline int
scm_str_itr_width(const ScmStrItr *iter)
{
  assert(iter != NULL); assert(!scm_str_itr_err_p(iter));
  return scm_enc_char_width(iter->enc, iter->p, (size_t)iter->rest);
}

inline void
scm_str_itr_copy(const ScmStrItr *src, ScmStrItr *dst)
{
  assert(src != NULL); assert(dst != NULL);
  *dst = *src;
}

inline size_t
scm_str_itr_offset(const ScmStrItr *iter, const void *head)
{
  return (size_t)((const uint8_t *)scm_str_itr_ptr(iter)
                  - (const uint8_t *)head);
}

void scm_str_itr_begin(void *p, size_t size, ScmEncoding *enc, ScmStrItr *iter);
void scm_str_itr_next(ScmStrItr *iter);


#endif /* INCLUDED_ENCODING_H__ */
