#include <stddef.h>
#include <ctype.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/earray.h"
#include "scythe/impl_utils.h"
#include "scythe/bignum.h"
#include "scythe/number.h"
#include "scythe/exception.h"
#include "scythe/port.h"
#include "scythe/number_parser.h"

#define PARSE_RET_INVALID   -1
#define PARSE_RET_INTERNAL_ERR   -2

#define DELIMITER " \t\r\n|()[]\";"
#define DIGIT_2_CHARS "01"
#define DIGIT_8_CHARS "01234567"
#define DIGIT_10_CHARS "0123456789"
#define DIGIT_16_CHARS "0123456789aAbBcCdDeEfF"

typedef struct ScmNumParseItrRec {
  ScmStrItr itr;
  ScmObj port;
  ScmEncoding *enc;
} ScmNumParseItr;

static void
scm_num_itr_init(ScmNumParseItr *itr, ScmObj port,
                 const char *str, ScmEncoding *enc)
{
  scm_assert(itr != NULL);
  scm_assert((scm_obj_not_null_p(port) && str == NULL)
             || (str != NULL && scm_obj_null_p(port)));
  scm_assert(enc != NULL);

  if (str != NULL)
    scm_str_itr_begin(str, strlen(str), enc, &itr->itr);

  itr->port = port;
  itr->enc = enc;
}

static inline ScmEncoding *
itr_enc(ScmNumParseItr *itr)
{
  scm_assert(itr != NULL);
  return itr->enc;
}

static inline int
peek_chr(ScmNumParseItr *itr, scm_char_t *chr)
{
  int w;

  if (scm_obj_null_p(itr->port)) {
    w = scm_str_itr_chr(&itr->itr, chr);
    if (w < 0) {
      scm_error("failed to parse number literal: "
                     "invalid byte sequence", 0);
      return PARSE_RET_INTERNAL_ERR;
    }
  }
  else {
    w = (int)scm_peek_cchr(chr, itr->port);
  }

  return w;
}

static inline int
shift_chr(ScmNumParseItr *itr, scm_char_t *chr, EArray *str)
{
  int e;
  if (scm_obj_null_p(itr->port)) {
    scm_str_itr_next(&itr->itr);
    if (scm_str_itr_err_p(&itr->itr)) {
      scm_error("failed to parse number literal: "
                     "invalid byte sequence", 0);
      return -1;
    }
  }
  else {
    ssize_t w = scm_read_cchr(chr, itr->port);
    if (w < 0) return -1;
  }
  EARY_PUSH(str, scm_char_t, *chr, e);
  return e;
}

static inline bool
chr_same_p(scm_char_t c1, char c2, bool c_sensitive, ScmEncoding *enc)
{
  if (c_sensitive)
    return scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), c2);
  else
    return (scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), toupper(c2))
            || scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), tolower(c2)));
}

static inline const char *
chr_find(const char *str, scm_char_t c, ScmEncoding *enc)
{
  for (const char *p = str; *p != '\0'; p++)
    if (scm_enc_same_char_p(enc, c.bytes, sizeof(c), *p))
      return p;
  return NULL;
}


static int
scm_num_parse_prefix(ScmNumParseItr *itr, EArray *str, ScmNumParseData *data)
{
  const char * const radix_chars = "bBoOdDxX";
  const char * const exact_chars = "iIeE";
  scm_char_t current;
  int width;
  const char *p;
  char radix, exact;
  int e;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  radix = exact = '\0';

  while (radix == '\0' || exact == '\0') {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (!chr_same_p(current, '#', true, itr_enc(itr)))
      break;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (radix == '\0' &&
        (p = chr_find(radix_chars, current, itr_enc(itr))) != NULL)
      radix = tolower(*p);
    else if (exact == '\0' &&
             (p = chr_find(exact_chars, current, itr_enc(itr))) != NULL)
      exact = tolower(*p);
    else
      return PARSE_RET_INVALID;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }

  data->radix = (char)((radix == '\0') ? 'd' : radix);
  data->exact = exact;

  return 0;
}

static int
scm_num_parse_suffix(ScmNumParseItr *itr, EArray *str, ScmNumParseRealData *data)
{
  scm_char_t current;
  int width;
  int e;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;
  else if (width == 0) return PARSE_RET_INVALID;

  if (chr_find(DIGIT_10_CHARS, current, itr_enc(itr)) != NULL) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->rat.num.s_sign = '+';
    data->rat.num.s_head = EARY_SIZE(str) - 1;
  }
  else {
    if (chr_same_p(current, '+', true, itr_enc(itr)))
      data->rat.num.s_sign = '+';
    else if (chr_same_p(current, '-', true, itr_enc(itr)))
      data->rat.num.s_sign = '+';
    else
      return PARSE_RET_INVALID;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (chr_find(DIGIT_10_CHARS, current, itr_enc(itr)) != NULL) {
      e = shift_chr(itr, &current, str);
      if (e < 0) return PARSE_RET_INTERNAL_ERR;

      data->rat.num.s_head = EARY_SIZE(str) - 1;
    }
    else {
      return PARSE_RET_INVALID;
    }
  }

  while (true) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;

    if (width == 0
        || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL) {
      data->rat.num.s_len = EARY_SIZE(str) - data->rat.num.s_head;
      return 0;
    }
    else if (chr_find(DIGIT_10_CHARS, current, itr_enc(itr)) == NULL) {
      return PARSE_RET_INVALID;
    }

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }
}

static int
scm_num_parse_decimal(ScmNumParseItr *itr, EArray *str, ScmNumParseRealData *data)
{
  scm_char_t current;
  int width;
  int e;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (width == 0 || chr_find(DIGIT_10_CHARS, current, itr_enc(itr)) == NULL)
    return PARSE_RET_INVALID;

  e = shift_chr(itr, &current, str);
  if (e < 0) return PARSE_RET_INTERNAL_ERR;

  while (true) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;

    if (width == 0
        || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL) {
      data->rat.num.len = EARY_SIZE(str) - data->rat.num.head;
      data->rat.num.s_sign = '\0';
      return 0;
    }
    else if (chr_same_p(current, 'e', true, itr_enc(itr))) {
      e = shift_chr(itr, &current, str);
      if (e < 0) return PARSE_RET_INTERNAL_ERR;

      data->rat.num.len = EARY_SIZE(str) - data->rat.num.head - 1;

      if (data->rat.num.len == 1)
        return PARSE_RET_INVALID;

      return scm_num_parse_suffix(itr, str, data);
    }
    else if (chr_find(DIGIT_10_CHARS, current, itr_enc(itr)) == NULL) {
      return PARSE_RET_INVALID;
    }

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }
}

static int
scm_num_parse_denominator(ScmNumParseItr *itr, char radix,
                          EArray *str, ScmNumParseRealData *data)
{
  const char *digit_chars;
  scm_char_t current;
  int width;
  int e;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  switch (radix) {
  case 'b': digit_chars = DIGIT_2_CHARS; break;
  case 'o': digit_chars = DIGIT_8_CHARS; break;
  case 'd': digit_chars = DIGIT_10_CHARS; break;
  case 'x': digit_chars = DIGIT_16_CHARS; break;
  default: scm_assert(false); break; /* must not happen */
  }

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;
  else if (width == 0) return PARSE_RET_INVALID;

  if (chr_find(digit_chars, current, itr_enc(itr)) != NULL) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->rat.den.head = EARY_SIZE(str) - 1;
  }
  else {
    return PARSE_RET_INVALID;
  }

  while (true) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;

    if (width == 0
        || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL) {
      data->rat.den.len = EARY_SIZE(str) - data->rat.den.head;
      data->rat.den.s_sign = '\0';
      return 0;
    }
    else if (chr_find(digit_chars, current, itr_enc(itr)) == NULL) {
      return PARSE_RET_INVALID;
    }

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }
}

static int
scm_num_parse_ureal(ScmNumParseItr *itr, char radix,
                    EArray *str,ScmNumParseRealData *data)
{
  const char *digit_chars;
  scm_char_t current;
  int width;
  int e;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  switch (radix) {
  case 'b': digit_chars = DIGIT_2_CHARS; break;
  case 'o': digit_chars = DIGIT_8_CHARS; break;
  case 'd': digit_chars = DIGIT_10_CHARS; break;
  case 'x': digit_chars = DIGIT_16_CHARS; break;
  default: scm_assert(false); break; /* must not happen */
  }

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;
  else if (width == 0) return PARSE_RET_INVALID;

  if (radix == 'd' && chr_same_p(current, '.', true, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->type = 'i';
    data->rat.num.head = EARY_SIZE(str) - 1;
    data->rat.num.point = (ssize_t)EARY_SIZE(str) - 1;

    return scm_num_parse_decimal(itr, str, data);
  }
  else if (chr_find(digit_chars, current, itr_enc(itr)) != NULL) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->type = 'i';
    data->rat.num.head = EARY_SIZE(str) - 1;
  }
  else {
    return PARSE_RET_INVALID;
  }

  while (true) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;

    if (width == 0
        || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL) {
      data->rat.num.len = EARY_SIZE(str) - data->rat.num.head;
      data->rat.num.point = -1;
      data->rat.num.s_sign = '\0';
      return 0;
    }
    else if (radix == 'd' && chr_same_p(current, '.', true, itr_enc(itr))) {
      e = shift_chr(itr, &current, str);
      if (e < 0) return PARSE_RET_INTERNAL_ERR;

      data->rat.num.point = (ssize_t)EARY_SIZE(str) - 1;

      return scm_num_parse_decimal(itr, str, data);
    }
    else if (radix == 'd' && chr_same_p(current, 'e', true, itr_enc(itr))) {
      e = shift_chr(itr, &current, str);
      if (e < 0) return PARSE_RET_INTERNAL_ERR;

      data->rat.num.len = EARY_SIZE(str) - data->rat.num.head - 1;
      data->rat.num.point = -1;
      return scm_num_parse_suffix(itr, str, data);
    }
    else if (chr_same_p(current, '/', true, itr_enc(itr))) {
      e = shift_chr(itr, &current, str);
      if (e < 0) return PARSE_RET_INTERNAL_ERR;

      data->type = 'r';
      data->rat.num.len = EARY_SIZE(str) - data->rat.num.head - 1;
      data->rat.num.point = -1;
      return scm_num_parse_denominator(itr, radix, str, data);
    }
    else if (chr_find(digit_chars, current, itr_enc(itr)) == NULL) {
      return PARSE_RET_INVALID;
    }

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }
}

static int
scm_num_parse_inf_img(ScmNumParseItr *itr, EArray *str, ScmNumParseRealData *data)
{
  const char * const exp = "nf.0";
  scm_char_t current;
  int width;
  int e, i;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (width == 0 || chr_find(DELIMITER, current, itr_enc(itr)) != NULL) {
    data->type = '\0';
    return 0;
  }

  i = 0;
  if (chr_same_p(current, exp[i], false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }

  for (i = 1; exp[i] != '0'; i++) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (!chr_same_p(current, exp[i], false, itr_enc(itr)))
      return PARSE_RET_INVALID;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }

  data->type = 'I';

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (width == 0 || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL)
    return 0;
  else
    return PARSE_RET_INVALID;
}

static int
scm_num_parse_nan(ScmNumParseItr *itr, EArray *str, ScmNumParseRealData *data)
{
  const char * const exp = "an.0";
  scm_char_t current;
  int width;
  int e, i;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  for (i = 0; exp[i] != '0'; i++) {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (!chr_same_p(current, exp[i], false, itr_enc(itr)))
      return PARSE_RET_INVALID;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }

  data->type = 'N';

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (width == 0 || chr_find("iI@+-" DELIMITER, current, itr_enc(itr)) != NULL)
    return 0;
  else
    return PARSE_RET_INVALID;
}

static int
scm_num_parse_complex(ScmNumParseItr *itr, EArray *str, ScmNumParseData *data)
{
  scm_char_t current;
  int width;
  int e, r;

  scm_assert(itr != NULL);
  scm_assert(str != NULL);
  scm_assert(data != NULL);

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;
  else if (width == 0) return PARSE_RET_INVALID;

  if (chr_same_p(current, '+', true, itr_enc(itr)))
    data->fir.sign = '+';
  else if (chr_same_p(current, '-', true, itr_enc(itr)))
    data->fir.sign = '-';
  else
    data->fir.sign = '\0';

  if (data->fir.sign != '\0') {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;
  }

  if (chr_same_p(current, 'i', false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    r = scm_num_parse_inf_img(itr, str, &data->fir);
  }
  else if (chr_same_p(current, 'n', false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    r = scm_num_parse_nan(itr, str, &data->fir);
  }
  else {
    r = scm_num_parse_ureal(itr, data->radix, str, &data->fir);
  }

  if (r < 0) return r;

  if (data->fir.type == '\0') {
    data->complex = 'O';
    data->sec = data->fir;
    data->fir.type = '\0';
    return 0;
  }

  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (width == 0 || chr_find(DELIMITER, current, itr_enc(itr)) != NULL) {
    data->complex = 'o';
    data->sec.sign = data->sec.type = '\0';
    return 0;
  }
  else if (chr_same_p(current, 'i', false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->complex = 'O';
    data->sec = data->fir;
    data->fir.type = '\0';
    goto last;
  }
  else if (chr_same_p(current, '@', true, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->complex = '@';
  }
  else if (chr_same_p(current, '+', true, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->complex = 'O';
    data->sec.sign = '+';
  }
  else if (chr_same_p(current, '-', true, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    data->complex = 'O';
    data->sec.sign = '-';
  }
  else {
    return PARSE_RET_INVALID;
  }

  if (data->complex == '@') {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;

    if (chr_same_p(current, '+', true, itr_enc(itr)))
      data->sec.sign = '+';
    else if (chr_same_p(current, '-', true, itr_enc(itr)))
      data->sec.sign = '-';
    else
      data->sec.sign = '\0';
  }

  if (data->sec.sign != '\0') {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;
    else if (width == 0) return PARSE_RET_INVALID;
  }

  if (chr_same_p(current, 'i', false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    r = scm_num_parse_inf_img(itr, str, &data->sec);
  }
  else if (chr_same_p(current, 'n', false, itr_enc(itr))) {
    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;

    r = scm_num_parse_nan(itr, str, &data->sec);
  }
  else {
    r = scm_num_parse_ureal(itr, data->radix, str, &data->sec);
  }

  if (r < 0) return r;

  if (data->sec.type != '\0')
    return 0;

  if (data->complex == 'O') {
    width = peek_chr(itr, &current);
    if (width < 0) return PARSE_RET_INTERNAL_ERR;

    if (width == 0 || !chr_same_p(current, 'i', false, itr_enc(itr)))
      return PARSE_RET_INVALID;

    e = shift_chr(itr, &current, str);
    if (e < 0) return PARSE_RET_INTERNAL_ERR;
  }

 last:
  width = peek_chr(itr, &current);
  if (width < 0) return PARSE_RET_INTERNAL_ERR;

  if (chr_find(DELIMITER, current, itr_enc(itr)) != NULL)
    return 0;
  else
    return PARSE_RET_INVALID;
}

static ScmNumParseData *
scm_num_parse_internal(ScmNumParseItr *itr, EArray *str, ScmNumParseData *data)
{
  ScmNumParseData *dat;
  int r;

  scm_assert(itr != NULL);

  dat = data;
  if (data == NULL) {
    dat = scm_malloc(sizeof(*data));
    if (dat == NULL) return NULL;
  }

  r = scm_num_parse_prefix(itr, str, dat);
  if (r < 0) goto err;

  r = scm_num_parse_complex(itr, str, dat);
  if (r < 0) goto err;

  dat->rslt = SCM_NUM_PARSE_SUCCESS;
  return dat;

 err:
  switch (r) {
  case PARSE_RET_INVALID:
    dat->rslt = SCM_NUM_PARSE_INVALID;
    break;
  case PARSE_RET_INTERNAL_ERR:
    dat->rslt = SCM_NUM_PARSE_INTERNAL_ERR;
    break;
  default:
    scm_assert(false);
    break;
  }
  return dat;
}

ScmNumParseData *
scm_num_parse(ScmObj port, EArray *str, ScmNumParseData *data)
{
  ScmNumParseItr itr;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&port,
                      &itr.port);

  scm_assert(scm_input_port_p(port));

  enc = scm_port_internal_enc(port);
  scm_num_itr_init(&itr, port, NULL, enc);
  return scm_num_parse_internal(&itr, str, data);
}



static int
scm_num_chr_to_int(scm_char_t chr, ScmEncoding *enc)
{
  const char *digits = "0123456789abcdefABCDEF";
  const char *p;
  int v;

  p = chr_find(digits, chr, enc);
  if (p == NULL) {
    scm_error("failed to parse number literal: invalid format", 0);
    return -1;
  }

  v = (int)(p - digits);
  if (v > 15) v -= 6;

  return v;
}

static ssize_t
scm_num_calc_len_of_ary_of_sword(size_t str_size, int radix)
{
  scm_bignum_c_t base;
  int place, rslt;

  scm_assert(str_size <= SSIZE_MAX);

  rslt = scm_bignum_calc_base_and_place_for_ary_of_digits(radix,
                                                          &base, &place);
  if (rslt < 0) return -1;

  return ((ssize_t)str_size / place
          + (((ssize_t)str_size % place) ? 1 : 0));
}

static scm_bignum_sc_t
scm_num_str_to_ary_of_sword(int radix, const scm_char_t *str, size_t len,
                            ScmEncoding *enc,
                            scm_bignum_d_t *ary, size_t ary_sz)
{
  scm_bignum_c_t base;
  int place, rslt;

  scm_assert(str != NULL);
  scm_assert(ary != NULL);
  scm_assert(0 < ary_sz && ary_sz <= SSIZE_MAX);

  rslt = scm_bignum_calc_base_and_place_for_ary_of_digits(radix, &base, &place);

  if (rslt < 0) return -1;

  for (ssize_t i = (ssize_t)len, idx = 0;
       i > 0;
       i -= place, idx++) {
    ary[idx] = 0;

    for (int j = (i >= place) ? -place : (int)-i; j < 0; j++) {
      int v = scm_num_chr_to_int(str[i + j], enc);
      if (v < 0) return -1;

      ary[idx] = ary[idx] * (scm_bignum_d_t)radix + (scm_bignum_d_t)v;
    }
  }

  return (scm_bignum_sc_t)base;
}

static ScmObj
scm_num_make_inf(char sing)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_nan(char sing)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_integer(const scm_char_t *str, ScmEncoding *enc,
                     const ScmNumParseData *data, char sign,
                     const ScmNumParseIntDecData *idata)
{
  ScmObj num = SCM_OBJ_INIT;

  ssize_t ary_sz;
  int radix;

  scm_assert(str != NULL);
  scm_assert(enc != NULL);
  scm_assert(data != NULL && data->rslt == SCM_NUM_PARSE_SUCCESS);
  scm_assert(sign == '+' || sign == '-');
  scm_assert(idata != NULL);

  SCM_REFSTK_INIT_REG(&num);

  switch (data->radix) {
  case 'b': radix = 2; break;
  case 'o': radix = 8; break;
  case 'd': radix = 10; break;
  case 'x': radix = 16; break;
  default:
    scm_error("invalid parsing data", 0);
    return SCM_OBJ_NULL;
  }

  ary_sz = scm_num_calc_len_of_ary_of_sword(idata->len, radix);
  if (ary_sz < 0) return 0;

  {
    scm_bignum_d_t ary[ary_sz];
    scm_bignum_sc_t base;

    base = scm_num_str_to_ary_of_sword(radix, str + idata->head, idata->len,
                                       enc, ary, (size_t)ary_sz);
    if (base < 0) return SCM_OBJ_NULL;

    num = scm_bignum_make_int_from_ary(sign, ary, (size_t)ary_sz,
                                       (scm_bignum_c_t)base);
  }

  if (idata->s_sign != '\0') {
    scm_error("failed to parse number literal: unsupported format", 0);
    return SCM_OBJ_NULL;
  }

  return num;
}

static ScmObj
scm_num_make_float(const scm_char_t *str, ScmEncoding *enc,
                   const ScmNumParseData *data, char sign,
                   const ScmNumParseIntDecData *idata)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_float_from_rat(const scm_char_t *str, ScmEncoding *enc,
                            const ScmNumParseData *data, char sign,
                            const ScmNumParseRatData *rdata)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_rational(const scm_char_t *str, ScmEncoding *enc,
                      const ScmNumParseData *data, char sign,
                      const ScmNumParseRatData *rdata)
{
  ScmObj num = SCM_OBJ_INIT, den = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num, &den);

  scm_assert(str != NULL);
  scm_assert(enc != NULL);
  scm_assert(data != NULL && data->rslt == SCM_NUM_PARSE_SUCCESS);
  scm_assert(sign == '+' || sign == '-');
  scm_assert(rdata != NULL);

  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;

  num = scm_num_make_integer(str, enc, data, '+', &rdata->num);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  den = scm_num_make_integer(str, enc, data, '+', &rdata->den);
  if (scm_obj_null_p(den)) return SCM_OBJ_NULL;

  /* TODO: write a code that makes rational number object */
}

static ScmObj
scm_num_make_rational_from_dec(const scm_char_t *str, ScmEncoding *enc,
                               const ScmNumParseData *data, char sign,
                               const ScmNumParseIntDecData *idata)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_real(const scm_char_t *str, ScmEncoding *enc,
                  const ScmNumParseData *data,
                  const ScmNumParseRealData *rdata)
{
  char sign;

  scm_assert(str != NULL);
  scm_assert(enc != NULL);
  scm_assert(data != NULL && data->rslt == SCM_NUM_PARSE_SUCCESS);
  scm_assert(rdata != NULL && rdata->type != '\0');

  sign = (char)((rdata->sign == '\0') ? '+' : rdata->sign);

  switch (rdata->type) {
  case 'I':
    return scm_num_make_inf(rdata->sign);
    break;
  case 'N':
    return scm_num_make_nan(rdata->sign);
    break;
  case 'i':
    if (rdata->rat.num.point < 0) {
      if (data->exact == 'e')
        return scm_num_make_integer(str, enc, data, sign, &rdata->rat.num);
      else if (data->exact == 'i')
        /* inexact な整数の表記は float オブジェクトを生成 */
        return scm_num_make_float(str, enc, data, sign, &rdata->rat.num);
      else if (rdata->rat.num.s_sign == '\0')
        return scm_num_make_integer(str, enc, data, sign, &rdata->rat.num);
      else
        /* 正確性未指定で suffix 有りな整数の表記は float オブジェクトを生成 */
        return scm_num_make_float(str, enc, data, sign, &rdata->rat.num);
    }
    else if (data->exact == 'e')
      /* exact な小数点数の表記は rational オブジェクトを生成 */
      return scm_num_make_rational_from_dec(str, enc, data, sign, &rdata->rat.num);
    else
      return scm_num_make_float(str, enc, data, sign, &rdata->rat.num);
    break;
  case 'r':
    if (data->exact == 'e' && data->exact == '\0')
      return scm_num_make_rational(str, enc, data, sign, &rdata->rat);
    else
      /* inexact な分数の表記は float オブジェクトを生成 */
      return scm_num_make_float_from_rat(str, enc, data, sign, &rdata->rat);
    break;
  default:
    scm_error("invalid parsing data", 0);
    return SCM_OBJ_NULL;
  }
}

static ScmObj
scm_num_make_complex_polar(const ScmNumParseData *data, ScmObj rad, ScmObj arg)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_num_make_complex_orth(const ScmNumParseData *data, ScmObj real, ScmObj img)
{
  scm_error("failed to parse number literal: unsupported format", 0);
  return SCM_OBJ_NULL;
}

ScmObj
scm_num_make_from_parsedata(const scm_char_t *str, ScmEncoding *enc,
                            const ScmNumParseData *data)
{
  ScmObj real1 = SCM_OBJ_INIT, real2 = SCM_OBJ_INIT;

  scm_assert(str != NULL);
  scm_assert(enc != NULL);
  scm_assert(data != NULL && data->rslt == SCM_NUM_PARSE_SUCCESS);


  SCM_REFSTK_INIT_REG(&real1, &real2);

  if (data->fir.type != '\0')
    real1 = scm_num_make_real(str, enc, data, &data->fir);
  else
    real1 = scm_make_number_from_sword(0);

  if (scm_obj_null_p(real1)) return SCM_OBJ_NULL;

  if (data->sec.type != '\0')
    real2 = scm_num_make_real(str, enc, data, &data->sec);
  else if (data->complex == 'O')
    real2 = scm_make_number_from_sword((data->sec.sign == '+') ? 1 : -1);
  else
    return real1;

  if (scm_obj_null_p(real2)) return SCM_OBJ_NULL;

  if (data->complex == '@')
    return scm_num_make_complex_polar(data, real1, real2);
  else
    return scm_num_make_complex_orth(data, real1, real2);
}

ScmObj
scm_num_make_from_literal(const char *literal, ScmEncoding *enc)
{
  ScmObj num = SCM_OBJ_INIT;
  ScmNumParseItr itr;
  ScmNumParseData data, *p;
  EArray str;
  int r;

  scm_assert(literal != NULL);
  scm_assert(enc != NULL);

  SCM_REFSTK_INIT_REG(&num);

  scm_num_itr_init(&itr, SCM_OBJ_NULL, literal, enc);

  r = eary_init(&str, sizeof(scm_char_t), 0);
  if (r < 0) return SCM_OBJ_NULL;

  num = SCM_OBJ_NULL;

  p = scm_num_parse_internal(&itr, &str, &data);
  if (p == NULL) goto ret;

  if (data.rslt != SCM_NUM_PARSE_SUCCESS) {
    scm_error("failed to parse number literal: invalid format", 0);
    goto ret;
  }

  num = scm_num_make_from_parsedata(EARY_HEAD(&str), enc, &data);

 ret:
  eary_fin(&str);
  return num;
}
