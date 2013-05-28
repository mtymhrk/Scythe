#include <limits.h>
#include <assert.h>

#include "encoding.h"

#define SCM_STR_ITR_MAKE_ERR(iter)              \
  do {                                          \
    (iter)->p = NULL;                           \
    (iter)->rest = -1;                          \
    (iter)->enc = NULL;                         \
  } while(0)

static void
scm_enc_index2itr_fixed_width(void *str, size_t size,
                              size_t idx, size_t width, ScmEncoding *enc,
                              ScmStrItr *iter)
{
  uint8_t *p = str;
  size_t offset;

  offset = width * idx;

  if (iter == NULL) {
    return;
  }
  else if (p == NULL || offset > size) {
    SCM_STR_ITR_MAKE_ERR(iter);
    return;
  }

  scm_str_itr_begin(p + offset, size - offset, enc, iter);
}

static void
scm_enc_index2itr_variable_width(void *str,
                                 size_t size, size_t idx, ScmEncoding *enc,
                                 ScmStrItr *iter)
{
  size_t i;

  if (iter == NULL) return;

  scm_str_itr_begin(str, size, enc, iter);
  if (scm_str_itr_err_p(iter)) return;

  i = 0;
  while (!scm_str_itr_end_p(iter) && i < idx) {
    scm_str_itr_next(iter);
    if (scm_str_itr_err_p(iter)) return;
    i++;
  }
}


/***********************************************************************/
/*   Encding: ASCII                                                    */
/***********************************************************************/

static ScmEncoding SCM_ENC_ASCII__ = {
  .iconv_name = "ASCII",
  .cnst = {
    .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
    .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
    .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
    .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
  },
  .func = {
    .char_width = scm_enc_char_width_ascii,
    .index2itr = scm_enc_index2itr_ascii,
    .valid_char_p = scm_enc_valid_char_p_ascii,
    .cnv_to_ascii = scm_enc_cnv_to_ascii_ascii,
    .cnv_from_ascii = scm_enc_cnv_from_ascii_ascii,
    .cnv_to_scalar = scm_enc_cnv_to_scalar_ascii,
    .cnv_from_scalar = scm_enc_cnv_from_scalar_ascii,
    .downcase = scm_enc_downcase_ascii,
    .upcase = scm_enc_upcase_ascii,
    .ascii_p = scm_enc_ascii_p_ascii,
    .printable_p = scm_enc_printable_p_ascii,
    .alarm_p = scm_enc_alarm_p_ascii,
    .backspace_p = scm_enc_backspace_p_ascii,
    .delete_p = scm_enc_delete_p_ascii,
    .escape_p = scm_enc_escape_p_ascii,
    .newline_p = scm_enc_newline_p_ascii,
    .null_p = scm_enc_null_p_ascii,
    .return_p = scm_enc_return_p_ascii,
    .space_p = scm_enc_space_p_ascii,
    .tab_p = scm_enc_tab_p_ascii,
    .doublequote_p = scm_enc_doublequote_p_ascii,
    .backslash_p = scm_enc_backslash_p_ascii,
  },
};

ScmEncoding *SCM_ENC_ASCII = &SCM_ENC_ASCII__;


#define VALID_ASCII_P(ascii) ((ascii) <= 0x7f)

int
scm_enc_char_width_ascii(const void *str, size_t len)
{
  const scm_char_ascii_t *ascii = str;

  if (ascii == NULL)
    return -1;
  else if (len < 1)
    return 0;
  else if (VALID_ASCII_P(*ascii))
    return 1;
  else
    return -1;
}

void
scm_enc_index2itr_ascii(void *str, size_t size, size_t idx, ScmStrItr *iter)
{
  scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ascii_t),
                                SCM_ENC_ASCII, iter);
}

bool
scm_enc_valid_char_p_ascii(const scm_char_t *c)
{
  return VALID_ASCII_P(c->bytes[0]) ? true : false;
}

int
scm_enc_cnv_to_ascii_ascii(const scm_char_t *c)
{
  return VALID_ASCII_P(c->bytes[0]) ? c->bytes[0] : -1;
}

ssize_t
scm_enc_cnv_from_ascii_ascii(char ascii, scm_char_t *chr)
{
  if (VALID_ASCII_P((uint8_t)ascii)) {
    chr->ascii = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_cnv_to_scalar_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return -1;
  else
    return (long long)*(const scm_char_ascii_t *)p;
}

ssize_t
scm_enc_cnv_from_scalar_ascii(long long scalar, scm_char_t *chr)
{
  if(scalar < 0 || 0x7f < scalar)
    return -1;
  else if (chr == NULL)
    return -1;
  else {
    chr->ascii = (scm_char_ascii_t)scalar;
    return 1;
  }
}

ssize_t
scm_enc_downcase_ascii(const void *p, size_t s, scm_char_t *chr)
{
  if (p == NULL || s < 1 || chr == NULL) return -1;

  chr->ascii = *(const scm_char_ascii_t *)p;
  if (0x41 <= chr->ascii && chr->ascii <= 0x5a)
    chr->ascii = (scm_char_ascii_t)(chr->ascii + 0x20);

  return 1;
}

ssize_t
scm_enc_upcase_ascii(const void *p, size_t s, scm_char_t *chr)
{
  if (p == NULL || s < 1 || chr == NULL) return -1;

  chr->ascii = *(const scm_char_ascii_t *)p;
  if (0x61 <= chr->ascii && chr->ascii <= 0x7a)
    chr->ascii = (scm_char_ascii_t)(chr->ascii - 0x20);
  return 1;
}

bool
scm_enc_ascii_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_ASCII_P(*(const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p <= 0x1f)
    return false;
  else if (*(const scm_char_ascii_t *)p >= 0x7f)
    return false;
  else
    return true;
}

bool
scm_enc_alarm_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x07)
    return true;
  else
    return false;
}

bool
scm_enc_backspace_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x08)
    return true;
  else
    return false;
}

bool
scm_enc_delete_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x7f)
    return true;
  else
    return false;
}

bool
scm_enc_escape_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x1b)
    return true;
  else
    return false;
}

bool
scm_enc_newline_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x0a)
    return true;
  else
    return false;
}

bool
scm_enc_null_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x00)
    return true;
  else
    return false;
}

bool
scm_enc_return_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x0d)
    return true;
  else
    return false;
}

bool
scm_enc_space_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x20)
    return true;
  else
    return false;
}

bool
scm_enc_tab_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x09)
    return true;
  else
    return false;
}

bool
scm_enc_doublequote_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x22)
    return true;
  else
    return false;
}

bool
scm_enc_backslash_p_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (*(const scm_char_ascii_t *)p == 0x5c)
    return true;
  else
    return false;
}


/***********************************************************************/
/*   Encoding: UTF-8                                                   */
/***********************************************************************/

static ScmEncoding SCM_ENC_UTF8__ = {
  .iconv_name = "UTF-8",
  .cnst = {
    .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
    .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
    .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
    .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
  },
  .func = {
    .char_width = scm_enc_char_width_utf8,
    .index2itr = scm_enc_index2itr_utf8,
    .valid_char_p = scm_enc_valid_char_p_utf8,
    .cnv_to_ascii = scm_enc_cnv_to_ascii_utf8,
    .cnv_from_ascii = scm_enc_cnv_from_ascii_utf8,
    .cnv_to_scalar = scm_enc_cnv_to_scalar_utf8,
    .cnv_from_scalar = scm_enc_cnv_from_scalar_utf8,
    .downcase = scm_enc_downcase_utf8,
    .upcase = scm_enc_upcase_utf8,
    .ascii_p = scm_enc_ascii_p_utf8,
    .printable_p = scm_enc_printable_p_utf8,
    .alarm_p = scm_enc_alarm_p_utf8,
    .backspace_p = scm_enc_backspace_p_utf8,
    .delete_p = scm_enc_delete_p_utf8,
    .escape_p = scm_enc_escape_p_utf8,
    .newline_p = scm_enc_newline_p_utf8,
    .null_p = scm_enc_null_p_utf8,
    .return_p = scm_enc_return_p_utf8,
    .space_p = scm_enc_space_p_utf8,
    .tab_p = scm_enc_tab_p_utf8,
    .doublequote_p = scm_enc_doublequote_p_utf8,
    .backslash_p = scm_enc_backslash_p_utf8,
  },
};

ScmEncoding *SCM_ENC_UTF8 = &SCM_ENC_UTF8__;


#define VALID_UTF8_1_P(utf8)                    \
  (/*0x00 <= (utf8)[0] && */(utf8)[0] <= 0x7f)
#define VALID_UTF8_2_P(utf8)                                            \
  ((0xc2 <= (utf8)[0] && (utf8)[0] <= 0xdf) && VALID_UTF8_TAIL_P((utf8)[1]))
#define VALID_UTF8_3_P(utf8)                                            \
  ((((utf8)[0] == 0xe0 && (0xa0 <= (utf8)[1] && (utf8)[1] <= 0xbf))     \
    || ((0xe1 <= (utf8)[0] && (utf8)[0] <= 0xec)                        \
        && VALID_UTF8_TAIL_P((utf8)[1]))                                \
    || ((utf8)[0] == 0xed && (0x80 <= (utf8)[1] && (utf8)[1] <= 0x9f))  \
    || ((0xee <= (utf8)[0] && (utf8)[0] <= 0xef)                        \
        && VALID_UTF8_TAIL_P((utf8)[1])))                               \
   && VALID_UTF8_TAIL_P((utf8)[2]))
#define VALID_UTF8_4_P(utf8)                                            \
  ((((utf8)[0] == 0xf0 && (0x90 <= (utf8)[1] && (utf8)[1] <= 0xbf))     \
    || ((0xf1 <= (utf8)[0] && (utf8)[0] <= 0xf3)                        \
        && VALID_UTF8_TAIL_P((utf8)[1]))                                \
    || ((utf8)[0] == 0xf4 && (0x80 <= (utf8)[1] && (utf8)[1] <= 0x8f))) \
   && VALID_UTF8_TAIL_P((utf8)[2]) && VALID_UTF8_TAIL_P((utf8)[3]))
#define VALID_UTF8_TAIL_P(utf8chr)              \
  (0x80 <= (utf8chr) && (utf8chr) <= 0xbf)


int
scm_enc_char_width_utf8(const void *str, size_t len)
{
  const uint8_t *utf8 = str;

  if (utf8 == NULL) {
    return -1;
  }
  else if (len < 1)
    return 0;
  else if ((utf8[0] & 0x80) == 0x00) {
    return VALID_UTF8_1_P(utf8) ? 1 : -1;
  }
  else if ((utf8[0] & 0xe0) == 0xc0) {
    if (len < 2)
      return 0;
    else if (VALID_UTF8_2_P(utf8))
      return 2;
    else
      return -1;
  }
  else if ((utf8[0] & 0xf0) == 0xe0) {
    if (len < 3)
      return 0;
    else if (VALID_UTF8_3_P(utf8))
      return 3;
    else
      return -1;
  }
  else if ((utf8[0] & 0xf8) == 0xf0) {
    if (len < 4)
      return 0;
    else if (VALID_UTF8_4_P(utf8))
      return 4;
    else
      return -1;
  }
  else {
    return -1;
  }
}

void
scm_enc_index2itr_utf8(void *str, size_t size, size_t idx, ScmStrItr *iter)
{
  scm_enc_index2itr_variable_width(str, size, idx, SCM_ENC_UTF8, iter);
}

bool
scm_enc_valid_char_p_utf8(const scm_char_t *c)
{
  return ((VALID_UTF8_1_P(c->bytes)
           || VALID_UTF8_2_P(c->bytes)
           || VALID_UTF8_3_P(c->bytes)
           || VALID_UTF8_4_P(c->bytes)) ?
          true : false);
}

int
scm_enc_cnv_to_ascii_utf8(const scm_char_t *c)
{
  return VALID_UTF8_1_P(c->bytes) ? c->bytes[0] : -1;
}

ssize_t
scm_enc_cnv_from_ascii_utf8(char ascii, scm_char_t *chr)
{
  if (VALID_ASCII_P((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_cnv_to_scalar_utf8(const void *p, size_t size)
{
  scm_char_ucs4_t ucs4;

  if (scm_enc_utf8_to_ucs4(p, size, &ucs4) < 1) return -1;
  return scm_enc_cnv_to_scalar_ucs4(&ucs4, sizeof(ucs4));
}

ssize_t
scm_enc_cnv_from_scalar_utf8(long long scalar, scm_char_t *chr)
{
  scm_char_t ucs4;

  if (chr == NULL) return -1;

  if (scm_enc_cnv_from_scalar_ucs4(scalar, &ucs4) < 0) return -1;

  return scm_enc_ucs4_to_utf8(ucs4.ucs4, chr->bytes, sizeof(*chr));
}

ssize_t
scm_enc_downcase_utf8(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_utf8(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_downcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

ssize_t
scm_enc_upcase_utf8(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_utf8(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_upcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

bool
scm_enc_ascii_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_utf8(const void *p, size_t size)
{
  scm_char_ucs4_t ucs4;

  if (scm_enc_utf8_to_ucs4(p, size, &ucs4) < 1) return false;
  return scm_enc_printable_p_ucs4(&ucs4, sizeof(ucs4));
}

bool
scm_enc_alarm_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backspace_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_delete_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_escape_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_newline_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_null_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_return_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_space_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_tab_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_doublequote_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backslash_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_UTF8_1_P((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;
}


/***********************************************************************/
/*   Encoding: UCS4                                                    */
/***********************************************************************/

static ScmEncoding SCM_ENC_UCS4__ = {
  .iconv_name = "UCS4",
  .cnst = {
    .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
    .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
    .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
    .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
  },
  .func = {
    .char_width = scm_enc_char_width_ucs4,
    .index2itr = scm_enc_index2itr_ucs4,
    .valid_char_p = scm_enc_valid_char_p_ucs4,
    .cnv_to_ascii = scm_enc_cnv_to_ascii_ucs4,
    .cnv_from_ascii = scm_enc_cnv_from_ascii_ucs4,
    .cnv_to_scalar = scm_enc_cnv_to_scalar_ucs4,
    .cnv_from_scalar = scm_enc_cnv_from_scalar_ucs4,
    .downcase = scm_enc_downcase_ucs4,
    .upcase = scm_enc_upcase_ucs4,
    .ascii_p = scm_enc_ascii_p_ucs4,
    .printable_p = scm_enc_printable_p_ucs4,
    .alarm_p = scm_enc_alarm_p_ucs4,
    .backspace_p = scm_enc_backspace_p_ucs4,
    .delete_p = scm_enc_delete_p_ucs4,
    .escape_p = scm_enc_escape_p_ucs4,
    .newline_p = scm_enc_newline_p_ucs4,
    .null_p = scm_enc_null_p_ucs4,
    .return_p = scm_enc_return_p_ucs4,
    .space_p = scm_enc_space_p_ucs4,
    .tab_p = scm_enc_tab_p_ucs4,
    .doublequote_p = scm_enc_doublequote_p_ucs4,
    .backslash_p = scm_enc_backslash_p_ucs4,
  },
};

ScmEncoding *SCM_ENC_UCS4 = &SCM_ENC_UCS4__;

/* XXX: is this correct ? */
#define VALID_UCS4_P(ucs4) \
  (ucs4 <= 0xd7ff || (0xe000 <= ucs4 && ucs4 <= 0xfffd) \
   || (0x10000 <= ucs4 && ucs4 <= 0x10ffff))

int
scm_enc_char_width_ucs4(const void *str, size_t len)
{
  const uint32_t *ucs4 = str;

  if (ucs4 == NULL)
    return -1;
  else if (len < 4)
    return 0;
  else if (VALID_UCS4_P(*ucs4))
    return 4;
  else
    return -1;
}

void
scm_enc_index2itr_ucs4(void *str, size_t size, size_t idx, ScmStrItr *iter)
{
  scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ucs4_t),
                                SCM_ENC_UCS4, iter);
}

#define UCS4CHR(c) ((uint32_t)(c))

ssize_t
scm_enc_utf8_to_ucs4(const uint8_t *utf8, size_t utf8_len, uint32_t *ucs4)
{
  if (utf8 == NULL) return -1;

  if (utf8_len == 0) {
    *ucs4 = 0;
    return 0;
  }
  else if ((utf8[0] & 0x80) == 0x00) {
    *ucs4 = UCS4CHR(utf8[0]);
    return 1;
  }
  else if ((utf8[0] & 0xe0) == 0xc0) {
    if (utf8_len < 2 || !VALID_UTF8_2_P(utf8)) return -1;
    *ucs4 = UCS4CHR(utf8[0] & 0x1f) << 6;
    *ucs4 |= UCS4CHR(utf8[1] & 0x3f);
    return 2;
  }
  else if ((utf8[0] & 0xf0) == 0xe0) {
    if (utf8_len < 3 || !VALID_UTF8_3_P(utf8)) return -1;
    *ucs4 = UCS4CHR(utf8[0] & 0x0f) << 12;
    *ucs4 |= UCS4CHR(utf8[1] & 0x3f) << 6;
    *ucs4 |= UCS4CHR(utf8[2] & 0x3f);
    return 3;
  }
  else if ((utf8[0] & 0xf8) == 0xf0) {
    if (utf8_len < 4 || !VALID_UTF8_4_P(utf8)) return -1;
    *ucs4 = UCS4CHR(utf8[0] & 0x07) << 18;
    *ucs4 |= UCS4CHR(utf8[1] & 0x3f) << 12;
    *ucs4 |= UCS4CHR(utf8[2] & 0x3f) << 6;
    *ucs4 |= UCS4CHR(utf8[3] & 0x3f);
    return 4;
  }
  else {
    return -1;
  }
}

ssize_t
scm_enc_ucs4_to_utf8(scm_char_ucs4_t ucs4, uint8_t *utf8, size_t sz)
{
  if (utf8 == NULL)
    return -1;
  else if (ucs4 <= 0x7f) {
    if (sz < 1) return -1;
    utf8[0] = (uint8_t)ucs4;
    return 1;
  }
  else if (ucs4 <= 0x07ff) {
    if (sz < 2) return -1;
    utf8[0] = (uint8_t)(0xc0 | (ucs4 >> 6));
    utf8[1] = (uint8_t)(0x80 | (ucs4 & 0x3f));
    return 2;
  }
  else if (0xd7ff < ucs4 && ucs4 < 0xe000) {
    return -1;
  }
  else if (ucs4 <= 0xfffd) {
    if (sz < 3) return -1;
    utf8[0] = (uint8_t)(0xe0 | (ucs4 >> 12));
    utf8[1] = (uint8_t)(0x80 | ((ucs4 >> 6) & 0x3f));
    utf8[2] = (uint8_t)(0x80 | (ucs4 & 0x3f));
    return 3;
  }
  else if (ucs4 < 0x10000) {
    return -1;
  }
  else if (ucs4 <= 0x10ffff) {
    if (sz < 4) return -1;
    utf8[0] = (uint8_t)(0xf0 | (ucs4 >> 18));
    utf8[1] = (uint8_t)(0x80 | ((ucs4 >> 12) & 0x3f));
    utf8[2] = (uint8_t)(0x80 | ((ucs4 >> 6) & 0x3f));
    utf8[3] = (uint8_t)(0x80 | (ucs4 & 0x3f));
    return 4;
  }
  else
    return -1;
}

bool
scm_enc_valid_char_p_ucs4(const scm_char_t *c)
{
  return VALID_UCS4_P(c->ucs4) ? true : false;
}

int
scm_enc_cnv_to_ascii_ucs4(const scm_char_t *c)
{
  return (/* 0x00 <= c->ucs4 && */ c->ucs4 <= 0x7f) ? (int)c->ucs4 : -1;
}

ssize_t
scm_enc_cnv_from_ascii_ucs4(char ascii, scm_char_t *chr)
{
  if (VALID_ASCII_P((uint8_t)ascii)) {
    chr->ucs4 = (scm_char_ucs4_t)ascii;
    return 4;
  }
  else {
    return -1;
  }
}

long long
scm_enc_cnv_to_scalar_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return -1;
  else
    return (long long)*(const scm_char_ucs4_t *)p;
}

ssize_t
scm_enc_cnv_from_scalar_ucs4(long long scalar, scm_char_t *chr)
{
  if (chr == NULL)
    return -1;
  else if (VALID_UCS4_P(scalar)) {
    chr->ucs4 = (scm_char_ucs4_t)scalar;
    return 4;
  }
  else
    return -1;
}

ssize_t
scm_enc_downcase_ucs4(const void *p, size_t s, scm_char_t *chr)
{
  scm_char_ucs4_t ucs4 = *(const scm_char_ucs4_t *)p;

  if (p == NULL || s < 4 || chr == NULL) return -1;

  if (0x41 <= ucs4 && ucs4 <= 0x5a)
    ucs4 += 0x20;

  memcpy(chr->bytes, &ucs4, 4);
  return 4;
}

ssize_t
scm_enc_upcase_ucs4(const void *p, size_t s, scm_char_t *chr)
{
  scm_char_ucs4_t ucs4 = *(const scm_char_ucs4_t *)p;

  if (p == NULL || s < 4 || chr == NULL) return -1;

  if (0x61 <= ucs4 && ucs4 <= 0x7a)
    ucs4 -= 0x20;

  memcpy(chr->bytes, &ucs4, 4);
  return 4;
}

bool
scm_enc_ascii_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p <= 0x7f)
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p <= 0x1f)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x7f)
    return false;
  else
    return true;
}

bool
scm_enc_alarm_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x07)
    return true;
  else
    return false;
}

bool
scm_enc_backspace_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x08)
    return true;
  else
    return false;
}

bool
scm_enc_delete_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x7f)
    return true;
  else
    return false;
}

bool
scm_enc_escape_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x1b)
    return true;
  else
    return false;
}

bool
scm_enc_newline_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x0a)
    return true;
  else
    return false;
}

bool
scm_enc_null_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x00)
    return true;
  else
    return false;
}

bool
scm_enc_return_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x0d)
    return true;
  else
    return false;
}

bool
scm_enc_space_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x20)
    return true;
  else
    return false;
}

bool
scm_enc_tab_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x09)
    return true;
  else
    return false;
}

bool
scm_enc_doublequote_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x22)
    return true;
  else
    return false;
}

bool
scm_enc_backslash_p_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return false;
  else if (*(const scm_char_ucs4_t *)p == 0x5c)
    return true;
  else
    return false;
}


/***********************************************************************/
/*   Encoding: EUC-JP-JIS-2004                                         */
/***********************************************************************/

static ScmEncoding SCM_ENC_EUCJP__ = {
  .iconv_name = "EUCJP-JISX0213",
  .cnst = {
    .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
    .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
    .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
    .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
  },
  .func = {
    .char_width = scm_enc_char_width_eucjp,
    .index2itr = scm_enc_index2itr_eucjp,
    .valid_char_p = scm_enc_valid_char_p_eucjp,
    .cnv_to_ascii = scm_enc_cnv_to_ascii_eucjp,
    .cnv_from_ascii = scm_enc_cnv_from_ascii_eucjp,
    .cnv_to_scalar = scm_enc_cnv_to_scalar_eucjp,
    .cnv_from_scalar = scm_enc_cnv_from_scalar_eucjp,
    .downcase = scm_enc_downcase_eucjp,
    .upcase = scm_enc_upcase_eucjp,
    .ascii_p = scm_enc_ascii_p_eucjp,
    .printable_p = scm_enc_printable_p_eucjp,
    .alarm_p = scm_enc_alarm_p_eucjp,
    .backspace_p = scm_enc_backspace_p_eucjp,
    .delete_p = scm_enc_delete_p_eucjp,
    .escape_p = scm_enc_escape_p_eucjp,
    .newline_p = scm_enc_newline_p_eucjp,
    .null_p = scm_enc_null_p_eucjp,
    .return_p = scm_enc_return_p_eucjp,
    .space_p = scm_enc_space_p_eucjp,
    .tab_p = scm_enc_tab_p_eucjp,
    .doublequote_p = scm_enc_doublequote_p_eucjp,
    .backslash_p = scm_enc_backslash_p_eucjp,
  },
};

ScmEncoding *SCM_ENC_EUCJP = &SCM_ENC_EUCJP__;

/* XXX: inexact? */
#define VALID_EUC_JP_ASCII_P(euc)               \
  (/* 0x00 <= (euc)[0] && */(euc)[0] <= 0x7f)
#define EUC_JP_SS2_P(byte) ((byte) == 0x8e)
#define EUC_JP_SS3_P(byte) ((byte) == 0x8f)
#define VALID_EUC_JP_JIS_X_0201_P(euc)                                  \
  (EUC_JP_SS2_P(euc[0]) && (0xa1 <= (euc)[1] && (euc)[1] <= 0xdf))
#define VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE_P(euc) \
  (0xa1 <= (euc)[0] && (euc)[0] <= 0xfe)
#define VALID_EUC_JP_JIS_X_0213_1_P(euc)         \
  (VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE_P(euc)   \
   && (0xa1 <= (euc)[1] && (euc)[1] <= 0xfe))
#define VALID_EUC_JP_JIS_X_0213_2_P(euc)         \
  (EUC_JP_SS3_P(euc[0])                          \
   && (0xa1 <= (euc)[1] && (euc)[1] <= 0xfe)     \
   && (0xa1 <= (euc)[2] && (euc)[2] <= 0xfe))

int
scm_enc_char_width_eucjp(const void *str, size_t len)
{
  const uint8_t *euc = str;

  if (euc == NULL) {
    return -1;
  }
  else if (len < 1)
    return 0;
  else if (VALID_EUC_JP_ASCII_P(euc)) {
    return 1;
  }
  else if (VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE_P(euc)) {
    if (len < 2)
      return 0;
    else if (VALID_EUC_JP_JIS_X_0213_1_P(euc))
      return 2;
    else
      return -1;
  }
  else if (EUC_JP_SS2_P(euc[0])) {
    if (len < 2)
      return 0;
    else if (VALID_EUC_JP_JIS_X_0201_P(euc))
      return 2;
    else
      return -1;
  }
  else if (EUC_JP_SS3_P(euc[0])) {
    if (len < 3)
      return 0;
    else if (VALID_EUC_JP_JIS_X_0213_2_P(euc))
      return 3;
    else
      return -1;
  }
  else {
    return -1;
  }
}

void
scm_enc_index2itr_eucjp(void *str, size_t size, size_t idx, ScmStrItr *iter)
{
  scm_enc_index2itr_variable_width(str, size, idx, SCM_ENC_EUCJP, iter);
}

bool
scm_enc_valid_char_p_eucjp(const scm_char_t *c)
{
  return ((VALID_EUC_JP_ASCII_P(c->bytes)
           || VALID_EUC_JP_JIS_X_0201_P(c->bytes)
           || VALID_EUC_JP_JIS_X_0213_1_P(c->bytes)
           || VALID_EUC_JP_JIS_X_0213_2_P(c->bytes)) ?
          true : false);
}

int
scm_enc_cnv_to_ascii_eucjp(const scm_char_t *c)
{
  return VALID_EUC_JP_ASCII_P(c->bytes) ? c->bytes[0] : -1;
}

ssize_t
scm_enc_cnv_from_ascii_eucjp(char ascii, scm_char_t *chr)
{
  if (VALID_ASCII_P((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_cnv_to_scalar_eucjp(const void *p, size_t size)
{
  ssize_t w;
  long long s;

  w = scm_enc_char_width_eucjp(p, size);
  if (w < 1) return -1;

  switch (w) {
  case 1:
    s = ((const uint8_t *)p)[0];
    break;
  case 2:
    s = ((const uint8_t *)p)[0];
    s = s << 8 | ((const uint8_t *)p)[1];
    break;
  case 3:
    s = ((const uint8_t *)p)[0];
    s = s << 8 | ((const uint8_t *)p)[1];
    s = s << 8 | ((const uint8_t *)p)[2];
    break;
  default:
    s = -1;
    break;
  }

  return s;
}

ssize_t
scm_enc_cnv_from_scalar_eucjp(long long scalar, scm_char_t *chr)
{
  if (chr == NULL)
    return -1;

  if (scalar <= 0xff)
    chr->bytes[0] = (uint8_t)scalar;
  else if (scalar <= 0xffff) {
    chr->bytes[0] = (uint8_t)(scalar >> 8);
    chr->bytes[1] = (uint8_t)(scalar & 0xff);
  }
  else if (scalar <= 0xffffff) {
    chr->bytes[0] = (uint8_t)(scalar >> 16);
    chr->bytes[1] = (uint8_t)((scalar >> 8) & 0xff);
    chr->bytes[2] = (uint8_t)(scalar & 0xff);
  }
  else
    return -1;

  return scm_enc_char_width_eucjp(chr->bytes, sizeof(*chr));
}

ssize_t
scm_enc_downcase_eucjp(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_eucjp(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_downcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

ssize_t
scm_enc_upcase_eucjp(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_eucjp(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_upcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

bool
scm_enc_ascii_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_printable_p_ascii(p, size);
  else
    return true;
}

bool
scm_enc_alarm_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_backspace_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_delete_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_escape_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_newline_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_null_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_return_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_space_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_tab_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_doublequote_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_backslash_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_EUC_JP_ASCII_P((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;;
}


/***********************************************************************/
/*   Encoding: SJIS                                                    */
/***********************************************************************/

static ScmEncoding SCM_ENC_SJIS__ = {
  .iconv_name = "SHIFT_JISx0213",
  .cnst = {
    .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
    .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
    .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
    .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
    .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
  },
  .func = {
    .char_width = scm_enc_char_width_sjis,
    .index2itr = scm_enc_index2itr_sjis,
    .valid_char_p = scm_enc_valid_char_p_sjis,
    .cnv_to_ascii = scm_enc_cnv_to_ascii_sjis,
    .cnv_from_ascii = scm_enc_cnv_from_ascii_sjis,
    .cnv_to_scalar = scm_enc_cnv_to_scalar_sjis,
    .cnv_from_scalar = scm_enc_cnv_from_scalar_sjis,
    .downcase = scm_enc_downcase_sjis,
    .upcase = scm_enc_upcase_sjis,
    .ascii_p = scm_enc_ascii_p_sjis,
    .printable_p = scm_enc_printable_p_sjis,
    .alarm_p = scm_enc_alarm_p_sjis,
    .backspace_p = scm_enc_backspace_p_sjis,
    .delete_p = scm_enc_delete_p_sjis,
    .escape_p = scm_enc_escape_p_sjis,
    .newline_p = scm_enc_newline_p_sjis,
    .null_p = scm_enc_null_p_sjis,
    .return_p = scm_enc_return_p_sjis,
    .space_p = scm_enc_space_p_sjis,
    .tab_p = scm_enc_tab_p_sjis,
    .doublequote_p = scm_enc_doublequote_p_sjis,
    .backslash_p = scm_enc_backslash_p_sjis,
  },
};

ScmEncoding *SCM_ENC_SJIS = &SCM_ENC_SJIS__;

/* XXX: inexact? */
#define VALID_SJIS_JIS_X_0201_LATIN_P(sjis)     \
  (/* 0x00 <= (euc)[0] && */(sjis)[0] <= 0x7f)
#define VALID_SJIS_JIS_X_0201_KATAKANA_P(sjis)  \
  (0xa1 <= (sjis)[0] && (sjis)[0] <= 0xdf)
#define VALID_SJIS_2_FIRST_BYTE_P(sjis)                 \
  ((0x81 <= (sjis)[0] && (sjis)[0] <= 0x9f)             \
   || (0xe0 <= (sjis)[0] && (sjis)[0] <= 0xef))
#define VALID_SJIS_2_P(sjis)                              \
  (VALID_SJIS_2_FIRST_BYTE_P(sjis)                        \
   && ((0x40 <= (sjis)[1] && (sjis)[1] <= 0x7e)           \
       || (0x80 <= (sjis)[1] && (sjis)[1] <= 0xfc)))

int
scm_enc_char_width_sjis(const void *str, size_t len)
{
  const uint8_t *sjis = str;

  if (sjis == NULL) {
    return -1;
  }
  else if (len < 1) {
    return 0;
  }
  else if (VALID_SJIS_JIS_X_0201_LATIN_P(sjis)) {
    return 1;
  }
  else if (VALID_SJIS_JIS_X_0201_KATAKANA_P(sjis)) {
    return 1;
  }
  else if (VALID_SJIS_2_FIRST_BYTE_P(sjis)) {
    if (len < 2)
      return 0;
    else if (VALID_SJIS_2_P(sjis))
      return 2;
    else
      return 2;
  }
  else {
    return -1;
  }
}

void
scm_enc_index2itr_sjis(void *str, size_t size, size_t idx, ScmStrItr *iter)
{
  scm_enc_index2itr_variable_width(str, size, idx, SCM_ENC_SJIS, iter);
}

bool
scm_enc_valid_char_p_sjis(const scm_char_t *c)
{
  return ((VALID_SJIS_JIS_X_0201_LATIN_P(c->bytes)
           || VALID_SJIS_JIS_X_0201_KATAKANA_P(c->bytes)
           || VALID_SJIS_2_P(c->bytes)) ?
          true : false);
}

int
scm_enc_cnv_to_ascii_sjis(const scm_char_t *c)
{
  return (VALID_SJIS_JIS_X_0201_LATIN_P(c->bytes)) ? c->bytes[0] : -1;
}

ssize_t
scm_enc_cnv_from_ascii_sjis(char ascii, scm_char_t *chr)
{
  if (VALID_ASCII_P((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_cnv_to_scalar_sjis(const void *p, size_t size)
{
  ssize_t w;
  long long s;

  w = scm_enc_char_width_sjis(p, size);
  if (w < 1) return -1;

  switch (w) {
  case 1:
    s = ((const uint8_t *)p)[0];
    break;
  case 2:
    s = ((const uint8_t *)p)[0];
    s = s << 8 | ((const uint8_t *)p)[1];
    break;
  default:
    s = -1;
    break;
  }

  return s;
}

ssize_t
scm_enc_cnv_from_scalar_sjis(long long scalar, scm_char_t *chr)
{
  if (chr == NULL)
    return -1;
  else if (scalar <= 0xff)
    chr->bytes[0] = (uint8_t)scalar;
  else if (scalar <= 0xffff) {
    chr->bytes[0] = (uint8_t)(scalar >> 8);
    chr->bytes[1] = (uint8_t)(scalar & 0xff);
  }
  else
    return -1;

  return scm_enc_char_width_sjis(chr->bytes, sizeof(*chr));
}

ssize_t
scm_enc_downcase_sjis(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_sjis(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_downcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

ssize_t
scm_enc_upcase_sjis(const void *p, size_t s, scm_char_t *chr)
{
  int w;

  if (p == NULL || chr == NULL) return -1;

  w = scm_enc_char_width_sjis(p, s);
  if (w < 0) return -1;

  if (w == 1) {
    return scm_enc_upcase_ascii(p, s, chr);
  }
  else {
    memcpy(chr->bytes, p, (size_t)w);
    return w;
  }
}

bool
scm_enc_ascii_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_printable_p_ascii(p, size);
  else
    return true;
}

bool
scm_enc_alarm_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backspace_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_delete_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_escape_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_newline_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_null_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_return_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_space_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_tab_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_doublequote_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backslash_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (VALID_SJIS_JIS_X_0201_LATIN_P((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;
}


/***********************************************************************/
/*   Iterater                                                          */
/***********************************************************************/

void
scm_str_itr_begin(void *p, size_t size, ScmEncoding *enc, ScmStrItr *iter)
{
  assert(size <= SSIZE_MAX);

  if (iter == NULL) {
    return;
  }
  else if (p == NULL || enc == NULL) {
    SCM_STR_ITR_MAKE_ERR(iter);
    return;
  }

  iter->p = p;
  iter->rest = (ssize_t)size;
  iter->enc = enc;
}

void
scm_str_itr_next(ScmStrItr *iter)
{
  ssize_t w;

  if (iter == NULL) return;
  if (iter->rest <= 0) return;

  w = scm_str_itr_width(iter);
  if (w <= 0) {
    iter->p = NULL;
    iter->rest = -1;
    iter->enc = NULL;
    return;
  }

  iter->p = (uint8_t *)iter->p + w;
  iter->rest = iter->rest - w;
}
