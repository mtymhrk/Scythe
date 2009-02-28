#include "encoding.h"

scm_char_t SCM_CHR_ZERO;

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

  iter.p = p;
  iter.rest = size;

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

ScmStrItr
scm_str_itr_next(const ScmStrItr *iter)
{
  ScmStrItr next;
  int w;

  next.p = NULL;
  next.rest = -1;
  next.char_width = NULL;

  if (iter == NULL) return next;
  if (iter->rest <= 0) return *iter;

  w = SCM_STR_ITR_WIDTH(iter);
  if (w < 0) return next;

  next.p = (uint8_t *)iter->p + w;
  next.rest = iter->rest - w;
  next.char_width = iter->char_width;

  return next;
}

static ScmStrItr
scm_enc_index2itr_fixed_width(void *str, size_t size,
                              unsigned int idx, size_t width,
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
                                 unsigned int idx, 
                                 int (*char_width)(const void *str, size_t len))
{
  ScmStrItr iter;
  int i;

  iter = scm_str_itr_begin(str, size, char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return iter;

  i = 0;
  while (!SCM_STR_ITR_IS_END(&iter) && i < idx) {
    iter = scm_str_itr_next(&iter);
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
  else if (len >= 1 && IS_VALID_ASCII(*ascii))
    return 1;
  else
    return -1;
}



ScmStrItr
scm_enc_index2itr_ascii(void *str, size_t size, unsigned int idx)
{
  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ascii_t),
                                       scm_enc_char_width_ascii);
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
  else if ((utf8[0] & 0x80) == 0x00) {
    if (len < 1 || !IS_VALID_UTF8_1(utf8)) return -1;
    return 1;
  }
  else if ((utf8[0] & 0xe0) == 0xc0) {
    if (len < 2 || !IS_VALID_UTF8_2(utf8)) return -1;
    return 2;
  }
  else if ((utf8[0] & 0xf0) == 0xe0) {
    if (len < 3 || !IS_VALID_UTF8_3(utf8)) return -1;
    return 3;
  }
  else if ((utf8[0] & 0xf8) == 0xf0) {
    if (len < 4 || !IS_VALID_UTF8_4(utf8)) return -1;
    return 4;
  }
  else {
    return -1;
  }    
}

ScmStrItr 
scm_enc_index2itr_utf8(void *str, size_t size, unsigned int idx)
{
  return scm_enc_index2itr_variable_width(str, size, idx,
                                          scm_enc_char_width_utf8);
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
  else if (len >= 4 && IS_VALID_UCS4(*ucs4))
    return 4;
  else
    return -1;
}

ScmStrItr
scm_enc_index2itr_ucs4(void *str, size_t size, unsigned int idx)
{

  return scm_enc_index2itr_fixed_width(str, size, idx, sizeof(scm_char_ucs4_t),
                                       scm_enc_char_width_ucs4);


  /* ScmStrItr iter; */
  /* uint32_t *ucs4 = str; */
  /* size_t offset; */

  /* offset = sizeof(*ucs4) * idx; */

  /* if (ucs4 == NULL) { */
  /*   SCM_STR_ITR_MAKE_ERR(&iter);   */
  /*   return iter; */
  /* } */
  /* else if (offset > size) { */
  /*   SCM_STR_ITR_MAKE_ERR(&iter);   */
  /*   return iter; */
  /* } */
  
  /* return scm_str_itr_begin(ucs4 + idx, size - offset, scm_enc_char_width_ucs4); */
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
