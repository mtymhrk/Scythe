#include <limits.h>
#include <assert.h>

#include "encoding.h"


const scm_char_t SCM_CHR_ZERO = {{ 0x00, 0x00, 0x00, 0x00 }};

/* ASCII */
const ScmEncConstants SCM_ENCODING_CONST_ASCII = {
  .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
  .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
  .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
  .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
};

const ScmEncVirtualFunc SCM_ENCODING_VFUNC_ASCII = {
  scm_enc_char_width_ascii, scm_enc_index2itr_ascii, scm_enc_is_lf_ascii,
  scm_enc_valid_char_p_ascii, scm_enc_to_ascii_ascii, scm_enc_ascii_to_ascii,
  scm_enc_to_scalar_ascii, scm_enc_scalar_to_ascii, scm_enc_downcase_ascii,
  scm_enc_upcase_ascii, scm_enc_ascii_p_ascii, scm_enc_printable_p_ascii,
  scm_enc_alarm_p_ascii, scm_enc_backspace_p_ascii, scm_enc_delete_p_ascii,
  scm_enc_escape_p_ascii, scm_enc_newline_p_ascii, scm_enc_null_p_ascii,
  scm_enc_return_p_ascii, scm_enc_space_p_ascii, scm_enc_tab_p_ascii,
  scm_enc_doublequote_p_ascii, scm_enc_backslash_p_ascii
};

/* UTF-8 */
const ScmEncConstants SCM_ENCODING_CONST_UTF8 = {
  .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
  .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
  .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
  .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
};

const ScmEncVirtualFunc SCM_ENCODING_VFUNC_UTF8 = {
  scm_enc_char_width_utf8, scm_enc_index2itr_utf8, scm_enc_is_lf_ascii,
  scm_enc_valid_char_p_utf8, scm_enc_to_ascii_utf8, scm_enc_ascii_to_utf8,
  scm_enc_to_scalar_utf8, scm_enc_scalar_to_utf8, scm_enc_downcase_utf8,
  scm_enc_upcase_utf8, scm_enc_ascii_p_utf8, scm_enc_printable_p_utf8,
  scm_enc_alarm_p_utf8, scm_enc_backspace_p_utf8, scm_enc_delete_p_utf8,
  scm_enc_escape_p_utf8, scm_enc_newline_p_utf8, scm_enc_null_p_utf8,
  scm_enc_return_p_utf8, scm_enc_space_p_utf8, scm_enc_tab_p_utf8,
  scm_enc_doublequote_p_utf8, scm_enc_backslash_p_utf8
};


/* UCS4 */
const ScmEncConstants SCM_ENCODING_CONST_UCS4 = {
  .lf   = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x0a }}, .w = 4 },
  .sp   = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x20 }}, .w = 4 },
  .cr   = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x0d }}, .w = 4 },
  .tab  = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x09 }}, .w = 4 },
  .bell = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x07 }}, .w = 4 },
  .bs   = { .c = { .bytes = { 0x00, 0x00, 0x00, 0x08 }}, .w = 4 },
};

const ScmEncVirtualFunc SCM_ENCODING_VFUNC_UCS4 = {
  scm_enc_char_width_ucs4, scm_enc_index2itr_ucs4, scm_enc_is_lf_ucs4,
  scm_enc_valid_char_p_ucs4, scm_enc_to_ascii_ucs4, scm_enc_ascii_to_ucs4,
  scm_enc_to_scalar_ucs4, scm_enc_scalar_to_ucs4, scm_enc_downcase_ucs4,
  scm_enc_upcase_ucs4, scm_enc_ascii_p_ucs4, scm_enc_printable_p_ucs4,
  scm_enc_alarm_p_ucs4, scm_enc_backspace_p_ucs4, scm_enc_delete_p_ucs4,
  scm_enc_escape_p_ucs4, scm_enc_newline_p_ucs4, scm_enc_null_p_ucs4,
  scm_enc_return_p_ucs4, scm_enc_space_p_ucs4, scm_enc_tab_p_ucs4,
  scm_enc_doublequote_p_ucs4, scm_enc_backslash_p_ucs4
};


/* EUC-JP */
const ScmEncConstants SCM_ENCODING_CONST_EUCJP = {
  .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
  .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
  .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
  .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
};

const ScmEncVirtualFunc SCM_ENCODING_VFUNC_EUCJP = {
  scm_enc_char_width_eucjp, scm_enc_index2itr_eucjp, scm_enc_is_lf_ascii,
  scm_enc_valid_char_p_eucjp, scm_enc_to_ascii_eucjp, scm_enc_ascii_to_eucjp,
  scm_enc_to_scalar_eucjp, scm_enc_scalar_to_eucjp, scm_enc_downcase_eucjp,
  scm_enc_upcase_eucjp, scm_enc_ascii_p_eucjp, scm_enc_printable_p_eucjp,
  scm_enc_alarm_p_eucjp, scm_enc_backspace_p_eucjp, scm_enc_delete_p_eucjp,
  scm_enc_escape_p_eucjp, scm_enc_newline_p_eucjp, scm_enc_null_p_eucjp,
  scm_enc_return_p_eucjp, scm_enc_space_p_eucjp, scm_enc_tab_p_eucjp,
  scm_enc_doublequote_p_eucjp, scm_enc_backslash_p_eucjp
};


/* SJIS */
const ScmEncConstants SCM_ENCODING_CONST_SJIS = {
  .lf   = { .c = { .bytes = { 0x0a, 0x00, 0x00, 0x00 }}, .w = 1 },
  .sp   = { .c = { .bytes = { 0x20, 0x00, 0x00, 0x00 }}, .w = 1 },
  .cr   = { .c = { .bytes = { 0x0d, 0x00, 0x00, 0x00 }}, .w = 1 },
  .tab  = { .c = { .bytes = { 0x09, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bell = { .c = { .bytes = { 0x07, 0x00, 0x00, 0x00 }}, .w = 1 },
  .bs   = { .c = { .bytes = { 0x08, 0x00, 0x00, 0x00 }}, .w = 1 },
};

const ScmEncVirtualFunc SCM_ENCODING_VFUNC_SJIS = {
  scm_enc_char_width_sjis, scm_enc_index2itr_sjis, scm_enc_is_lf_ascii,
  scm_enc_valid_char_p_sjis, scm_enc_to_ascii_sjis, scm_enc_ascii_to_sjis,
  scm_enc_to_scalar_sjis, scm_enc_scalar_to_sjis, scm_enc_downcase_sjis,
  scm_enc_upcase_sjis, scm_enc_ascii_p_sjis, scm_enc_printable_p_sjis,
  scm_enc_alarm_p_sjis, scm_enc_backspace_p_sjis, scm_enc_delete_p_sjis,
  scm_enc_escape_p_sjis, scm_enc_newline_p_sjis, scm_enc_null_p_sjis,
  scm_enc_return_p_sjis, scm_enc_space_p_sjis, scm_enc_tab_p_sjis,
  scm_enc_doublequote_p_sjis, scm_enc_backslash_p_sjis
};


const ScmEncConstants *SCM_ENCODING_CONST_TBL[] =
  { &SCM_ENCODING_CONST_ASCII,
    &SCM_ENCODING_CONST_UCS4,
    &SCM_ENCODING_CONST_UTF8,
    &SCM_ENCODING_CONST_EUCJP,
    &SCM_ENCODING_CONST_SJIS };

const ScmEncVirtualFunc *SCM_ENCODING_VFUNC_TBL[] =
  { &SCM_ENCODING_VFUNC_ASCII,
    &SCM_ENCODING_VFUNC_UCS4,
    &SCM_ENCODING_VFUNC_UTF8,
    &SCM_ENCODING_VFUNC_EUCJP,
    &SCM_ENCODING_VFUNC_SJIS };

#define SCM_STR_ITR_MAKE_ERR(iter)              \
  do {                                          \
    (iter)->p = NULL;                           \
    (iter)->rest = -1;                          \
    (iter)->char_width = NULL;                  \
  } while(0)

ScmStrItr
scm_str_itr_begin(void *p, size_t size,
                  int (*char_width)(const void *p, size_t size))
{
  ScmStrItr iter;

  assert(size <= SSIZE_MAX);

  iter.p = p;
  iter.rest = (ssize_t)size;

  if (p == NULL || char_width == NULL) {
    iter.char_width = NULL;
    iter.rest = -1;
    return iter;
  }
  else {
    iter.char_width = char_width;
  }

  return iter;
}

void
scm_str_itr_next(ScmStrItr *iter)
{
  ssize_t w;

  if (iter == NULL) return;
  if (iter->rest <= 0) return;

  w = SCM_STR_ITR_WIDTH(iter);
  if (w <= 0) {
    iter->p = NULL;
    iter->rest = -1;
    iter->char_width = NULL;
    return;
  }

  iter->p = (uint8_t *)iter->p + w;
  iter->rest = iter->rest - w;
}

static ScmStrItr
scm_enc_index2itr_fixed_width(void *str, size_t size,
                              size_t idx, size_t width,
                              int (*char_width)(const void *str, size_t len))
{
  ScmStrItr iter;
  uint8_t *p = str;
  size_t offset;

  offset = width * idx;

  if (p == NULL) {
    SCM_STR_ITR_MAKE_ERR(&iter);
    return iter;
  }
  else if (offset > size) {
    SCM_STR_ITR_MAKE_ERR(&iter);
    return iter;
  }

  return scm_str_itr_begin(p + offset, size - offset, char_width);
}

static ScmStrItr
scm_enc_index2itr_variable_width(void *str, size_t size,
                                 size_t idx,
                                 int (*char_width)(const void *str, size_t len))
{
  ScmStrItr iter;
  size_t i;

  iter = scm_str_itr_begin(str, size, char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return iter;

  i = 0;
  while (!SCM_STR_ITR_IS_END(&iter) && i < idx) {
    scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return iter;
    i++;
  }

  return iter;
}


/***********************************************************************/
/*   ASCII                                                             */
/***********************************************************************/

#define IS_VALID_ASCII(ascii) ((ascii) <= 0x7f)

int
scm_enc_char_width_ascii(const void *str, size_t len)
{
  const scm_char_ascii_t *ascii = str;

  if (ascii == NULL)
    return -1;
  else if (len < 1)
    return 0;
  else if (IS_VALID_ASCII(*ascii))
    return 1;
  else
    return -1;
}

ScmStrItr
scm_enc_index2itr_ascii(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ascii_t),
                                       scm_enc_char_width_ascii);
}

bool
scm_enc_is_lf_ascii(scm_char_t c)
{
  return (c.ascii == '\n') ? true : false;
}

bool
scm_enc_back_slash_p_ascii(scm_char_t c)
{
  return (c.ascii == '\\') ? true : false;
}

bool
scm_enc_valid_char_p_ascii(scm_char_t c)
{
  return IS_VALID_ASCII(c.bytes[0]) ? true : false;
}

int
scm_enc_to_ascii_ascii(scm_char_t c)
{
  return IS_VALID_ASCII(c.bytes[0]) ? c.bytes[0] : -1;
}

ssize_t
scm_enc_ascii_to_ascii(char ascii, scm_char_t *chr)
{
  if (IS_VALID_ASCII((uint8_t)ascii)) {
    chr->ascii = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_to_scalar_ascii(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return -1;
  else
    return (long long)*(const scm_char_ascii_t *)p;
}

ssize_t
scm_enc_scalar_to_ascii(long long scalar, scm_char_t *chr)
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
  else if (IS_VALID_ASCII(*(const uint8_t *)p))
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
/*   ASCII-CMPT                                                        */
/***********************************************************************/

int
scm_enc_char_width_ascii_cmpt(const void *str, size_t len)
{
  const scm_char_ascii_t *ascii = str;

  if (ascii == NULL)
    return -1;
  else if (len < 1)
    return 0;
  else
    return 1;
}

ScmStrItr
scm_enc_index2itr_ascii_cmpt(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ascii_t),
                                       scm_enc_char_width_ascii);
}

bool
scm_enc_is_lf_ascii_cmpt(scm_char_t c)
{
  return (c.ascii == '\n') ? true : false;
}

bool
scm_enc_is_space_ascii_cmpt(scm_char_t c)
{
  return (c.ascii == ' ') ? true : false;
}

bool
scm_enc_valid_char_p_ascii_cmpt(scm_char_t c)
{
  return true;
}

int
scm_enc_to_ascii_ascii_cmpt(scm_char_t c)
{
  return IS_VALID_ASCII(c.bytes[0]) ? c.bytes[0] : -1;
}

ssize_t
scm_enc_ascii_to_ascii_cmpt(char ascii, scm_char_t *chr)
{
  chr->ascii = (uint8_t)ascii;
  return 1;
}

long long
scm_enc_to_scalar_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_to_scalar_ascii(p, size);
}

bool
scm_enc_printable_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_printable_p_ascii(p, size);
}

bool
scm_enc_alarm_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_alarm_p_ascii(p, size);
}

bool
scm_enc_backspace_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_backspace_p_ascii(p, size);
}

bool
scm_enc_delete_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_delete_p_ascii(p, size);
}

bool
scm_enc_escape_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_escape_p_ascii(p, size);
}

bool
scm_enc_newline_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_newline_p_ascii(p, size);
}

bool
scm_enc_null_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_null_p_ascii(p, size);
}

bool
scm_enc_return_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_return_p_ascii(p, size);
}

bool
scm_enc_space_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_space_p_ascii(p, size);
}

bool
scm_enc_tab_p_ascii_cmpt(const void *p, size_t size)
{
  return scm_enc_tab_p_ascii(p, size);
}


/***********************************************************************/
/*   BINARY                                                            */
/***********************************************************************/

int
scm_enc_char_width_bin(const void *str, size_t len)
{
  const scm_char_bin_t *ascii = str;

  if (ascii == NULL)
    return -1;
  else if (len < 1)
    return 0;
  else
    return 1;
}

ScmStrItr
scm_enc_index2itr_bin(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_bin_t),
                                       scm_enc_char_width_ascii);
}

bool
scm_enc_is_lf_bin(scm_char_t c)
{
  return false;
}

bool
scm_enc_is_space_bin(scm_char_t c)
{
  return false;
}

bool
scm_enc_valid_char_p_binary(scm_char_t c)
{
  return true;
}

int
scm_enc_to_ascii_binary(scm_char_t c)
{
  return -1;
}

ssize_t
scm_enc_ascii_to_binary(char ascii, scm_char_t *chr)
{
  chr->bin = (uint8_t)ascii;
  return 1;
}


/***********************************************************************/
/*   UTF-8                                                             */
/***********************************************************************/

#define IS_VALID_UTF8_1(utf8)                   \
  (/*0x00 <= (utf8)[0] && */(utf8)[0] <= 0x7f)
#define IS_VALID_UTF8_2(utf8)                                           \
  ((0xc2 <= (utf8)[0] && (utf8)[0] <= 0xdf) && IS_VALID_UTF8_TAIL((utf8)[1]))
#define IS_VALID_UTF8_3(utf8)                                           \
  ((((utf8)[0] == 0xe0 && (0xa0 <= (utf8)[1] && (utf8)[1] <= 0xbf))     \
    || ((0xe1 <= (utf8)[0] && (utf8)[0] <= 0xec)                        \
        && IS_VALID_UTF8_TAIL((utf8)[1]))                               \
    || ((utf8)[0] == 0xed && (0x80 <= (utf8)[1] && (utf8)[1] <= 0x9f))  \
    || ((0xee <= (utf8)[0] && (utf8)[0] <= 0xef)                        \
        && IS_VALID_UTF8_TAIL((utf8)[1])))                              \
   && IS_VALID_UTF8_TAIL((utf8)[2]))
#define IS_VALID_UTF8_4(utf8)                                           \
  ((((utf8)[0] == 0xf0 && (0x90 <= (utf8)[1] && (utf8)[1] <= 0xbf))     \
    || ((0xf1 <= (utf8)[0] && (utf8)[0] <= 0xf3)                        \
        && IS_VALID_UTF8_TAIL((utf8)[1]))                               \
    || ((utf8)[0] == 0xf4 && (0x80 <= (utf8)[1] && (utf8)[1] <= 0x8f))) \
   && IS_VALID_UTF8_TAIL((utf8)[2]) && IS_VALID_UTF8_TAIL((utf8)[3]))
#define IS_VALID_UTF8_TAIL(utf8chr)             \
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
    return IS_VALID_UTF8_1(utf8) ? 1 : -1;
  }
  else if ((utf8[0] & 0xe0) == 0xc0) {
    if (len < 2)
      return 0;
    else if (IS_VALID_UTF8_2(utf8))
      return 2;
    else
      return -1;
  }
  else if ((utf8[0] & 0xf0) == 0xe0) {
    if (len < 3)
      return 0;
    else if (IS_VALID_UTF8_3(utf8))
      return 3;
    else
      return -1;
  }
  else if ((utf8[0] & 0xf8) == 0xf0) {
    if (len < 4)
      return 0;
    else if (IS_VALID_UTF8_4(utf8))
      return 4;
    else
      return -1;
  }
  else {
    return -1;
  }
}

ScmStrItr
scm_enc_index2itr_utf8(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_variable_width(str, size, idx,
                                          scm_enc_char_width_utf8);
}

bool
scm_enc_valid_char_p_utf8(scm_char_t c)
{
  return ((IS_VALID_UTF8_1(c.bytes)
           || IS_VALID_UTF8_2(c.bytes)
           || IS_VALID_UTF8_3(c.bytes)
           || IS_VALID_UTF8_4(c.bytes)) ?
          true : false);
}

int
scm_enc_to_ascii_utf8(scm_char_t c)
{
  return IS_VALID_UTF8_1(c.bytes) ? c.bytes[0] : -1;
}

ssize_t
scm_enc_ascii_to_utf8(char ascii, scm_char_t *chr)
{
  if (IS_VALID_ASCII((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_to_scalar_utf8(const void *p, size_t size)
{
  scm_char_ucs4_t ucs4;

  if (scm_enc_utf8_to_ucs4(p, size, &ucs4) < 1) return -1;
  return scm_enc_to_scalar_ucs4(&ucs4, sizeof(ucs4));
}

ssize_t
scm_enc_scalar_to_utf8(long long scalar, scm_char_t *chr)
{
  scm_char_t ucs4;

  if (chr == NULL) return -1;

  if (scm_enc_scalar_to_ucs4(scalar, &ucs4) < 0) return -1;

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
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
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
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backspace_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_delete_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_escape_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_newline_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_null_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_return_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_space_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_tab_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_doublequote_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backslash_p_utf8(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_UTF8_1((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;
}


/***********************************************************************/
/*   UCS4                                                              */
/***********************************************************************/

/* XXX: is this correct ? */
#define IS_VALID_UCS4(ucs4) \
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
  else if (IS_VALID_UCS4(*ucs4))
    return 4;
  else
    return -1;
}

ScmStrItr
scm_enc_index2itr_ucs4(void *str, size_t size, size_t idx)
{

  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ucs4_t),
                                       scm_enc_char_width_ucs4);
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
    if (utf8_len < 2 || !IS_VALID_UTF8_2(utf8)) return -1;
    *ucs4 = UCS4CHR(utf8[0] & 0x1f) << 6;
    *ucs4 |= UCS4CHR(utf8[1] & 0x3f);
    return 2;
  }
  else if ((utf8[0] & 0xf0) == 0xe0) {
    if (utf8_len < 3 || !IS_VALID_UTF8_3(utf8)) return -1;
    *ucs4 = UCS4CHR(utf8[0] & 0x0f) << 12;
    *ucs4 |= UCS4CHR(utf8[1] & 0x3f) << 6;
    *ucs4 |= UCS4CHR(utf8[2] & 0x3f);
    return 3;
  }
  else if ((utf8[0] & 0xf8) == 0xf0) {
    if (utf8_len < 4 || !IS_VALID_UTF8_4(utf8)) return -1;
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
scm_enc_is_lf_ucs4(scm_char_t c)
{
  return (c.ucs4 == '\n') ? true : false;
}

bool
scm_enc_is_space_ucs4(scm_char_t c)
{
  return (c.ucs4 == ' ') ? true : false;
}

bool
scm_enc_valid_char_p_ucs4(scm_char_t c)
{
  return IS_VALID_UCS4(c.ucs4) ? true : false;
}

int
scm_enc_to_ascii_ucs4(scm_char_t c)
{
  return (/* 0x00 <= c.ucs4 && */ c.ucs4 <= 0x7f) ? (int)c.ucs4 : -1;
}

ssize_t
scm_enc_ascii_to_ucs4(char ascii, scm_char_t *chr)
{
  if (IS_VALID_ASCII((uint8_t)ascii)) {
    chr->ucs4 = (scm_char_ucs4_t)ascii;
    return 4;
  }
  else {
    return -1;
  }
}

long long
scm_enc_to_scalar_ucs4(const void *p, size_t size)
{
  if (p == NULL || size < 4)
    return -1;
  else
    return (long long)*(const scm_char_ucs4_t *)p;
}

ssize_t
scm_enc_scalar_to_ucs4(long long scalar, scm_char_t *chr)
{
  if (chr == NULL)
    return -1;
  else if (IS_VALID_UCS4(scalar)) {
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
/*   EUC-JP-JIS-2004                                                   */
/***********************************************************************/

/* XXX: inexact? */
#define IS_VALID_EUC_JP_ASCII(euc)              \
  (/* 0x00 <= (euc)[0] && */(euc)[0] <= 0x7f)
#define IS_EUC_JP_SS2(byte) ((byte) == 0x8e)
#define IS_EUC_JP_SS3(byte) ((byte) == 0x8f)
#define IS_VALID_EUC_JP_JIS_X_0201(euc)                                 \
  (IS_EUC_JP_SS2(euc[0]) && (0xa1 <= (euc)[1] && (euc)[1] <= 0xdf))
#define IS_VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE(euc) \
  (0xa1 <= (euc)[0] && (euc)[0] <= 0xfe)
#define IS_VALID_EUC_JP_JIS_X_0213_1(euc)         \
  (IS_VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE(euc)   \
   && (0xa1 <= (euc)[1] && (euc)[1] <= 0xfe))
#define IS_VALID_EUC_JP_JIS_X_0213_2(euc)         \
  (IS_EUC_JP_SS3(euc[0])                        \
   && (0xa1 <= (euc)[1] && (euc)[1] <= 0xfe)    \
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
  else if (IS_VALID_EUC_JP_ASCII(euc)) {
    return 1;
  }
  else if (IS_VALID_EUC_JP_JIS_X_0213_1_FIRST_BYTE(euc)) {
    if (len < 2)
      return 0;
    else if (IS_VALID_EUC_JP_JIS_X_0213_1(euc))
      return 2;
    else
      return -1;
  }
  else if (IS_EUC_JP_SS2(euc[0])) {
    if (len < 2)
      return 0;
    else if (IS_VALID_EUC_JP_JIS_X_0201(euc))
      return 2;
    else
      return -1;
  }
  else if (IS_EUC_JP_SS3(euc[0])) {
    if (len < 3)
      return 0;
    else if (IS_VALID_EUC_JP_JIS_X_0213_2(euc))
      return 3;
    else
      return -1;
  }
  else {
    return -1;
  }
}

ScmStrItr
scm_enc_index2itr_eucjp(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_variable_width(str, size, idx,
                                          scm_enc_char_width_eucjp);
}

bool
scm_enc_valid_char_p_eucjp(scm_char_t c)
{
  return ((IS_VALID_EUC_JP_ASCII(c.bytes)
           || IS_VALID_EUC_JP_JIS_X_0201(c.bytes)
           || IS_VALID_EUC_JP_JIS_X_0213_1(c.bytes)
           || IS_VALID_EUC_JP_JIS_X_0213_2(c.bytes)) ?
          true : false);
}

int
scm_enc_to_ascii_eucjp(scm_char_t c)
{
  return IS_VALID_EUC_JP_ASCII(c.bytes) ? c.bytes[0] : -1;
}

ssize_t
scm_enc_ascii_to_eucjp(char ascii, scm_char_t *chr)
{
  if (IS_VALID_ASCII((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_to_scalar_eucjp(const void *p, size_t size)
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
scm_enc_scalar_to_eucjp(long long scalar, scm_char_t *chr)
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
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_printable_p_ascii(p, size);
  else
    return true;
}

bool
scm_enc_alarm_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_backspace_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_delete_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_escape_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_newline_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_null_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_return_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_space_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_tab_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_doublequote_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;;
}

bool
scm_enc_backslash_p_eucjp(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_EUC_JP_ASCII((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;;
}


/***********************************************************************/
/*   SJIS                                                              */
/***********************************************************************/

/* XXX: inexact? */
#define IS_VALID_SJIS_JIS_X_0201_LATIN(sjis)     \
  (/* 0x00 <= (euc)[0] && */(sjis)[0] <= 0x7f)
#define IS_VALID_SJIS_JIS_X_0201_KATAKANA(sjis) \
  (0xa1 <= (sjis)[0] && (sjis)[0] <= 0xdf)
#define IS_VALID_SJIS_2_FIRST_BYTE(sjis)                \
  ((0x81 <= (sjis)[0] && (sjis)[0] <= 0x9f)             \
   || (0xe0 <= (sjis)[0] && (sjis)[0] <= 0xef))
#define IS_VALID_SJIS_2(sjis)                             \
  (IS_VALID_SJIS_2_FIRST_BYTE(sjis)                       \
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
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN(sjis)) {
    return 1;
  }
  else if (IS_VALID_SJIS_JIS_X_0201_KATAKANA(sjis)) {
    return 1;
  }
  else if (IS_VALID_SJIS_2_FIRST_BYTE(sjis)) {
    if (len < 2)
      return 0;
    else if (IS_VALID_SJIS_2(sjis))
      return 2;
    else
      return 2;
  }
  else {
    return -1;
  }
}

ScmStrItr
scm_enc_index2itr_sjis(void *str, size_t size, size_t idx)
{
  return scm_enc_index2itr_variable_width(str, size, idx,
                                          scm_enc_char_width_sjis);
}

bool
scm_enc_valid_char_p_sjis(scm_char_t c)
{
  return ((IS_VALID_SJIS_JIS_X_0201_LATIN(c.bytes)
           || IS_VALID_SJIS_JIS_X_0201_KATAKANA(c.bytes)
           || IS_VALID_SJIS_2(c.bytes)) ?
          true : false);
}

int
scm_enc_to_ascii_sjis(scm_char_t c)
{
  return (IS_VALID_SJIS_JIS_X_0201_LATIN(c.bytes)) ? c.bytes[0] : -1;
}

ssize_t
scm_enc_ascii_to_sjis(char ascii, scm_char_t *chr)
{
  if (IS_VALID_ASCII((uint8_t)ascii)) {
    chr->bytes[0] = (uint8_t)ascii;
    return 1;
  }
  else {
    return -1;
  }
}

long long
scm_enc_to_scalar_sjis(const void *p, size_t size)
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
scm_enc_scalar_to_sjis(long long scalar, scm_char_t *chr)
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
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return true;
  else
    return false;
}

bool
scm_enc_printable_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_printable_p_ascii(p, size);
  else
    return true;
}

bool
scm_enc_alarm_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_alarm_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backspace_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_backspace_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_delete_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_delete_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_escape_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_escape_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_newline_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_newline_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_null_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_null_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_return_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_return_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_space_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_space_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_tab_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_tab_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_doublequote_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_doublequote_p_ascii(p, size);
  else
    return false;
}

bool
scm_enc_backslash_p_sjis(const void *p, size_t size)
{
  if (p == NULL || size < 1)
    return false;
  else if (IS_VALID_SJIS_JIS_X_0201_LATIN((const uint8_t *)p))
    return scm_enc_backslash_p_ascii(p, size);
  else
    return false;
}
