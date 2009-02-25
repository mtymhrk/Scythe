#include "encoding.h"

ScmStrItr 
scm_str_itr_begin(void *p, size_t size,
                  int (*char_width)(const void *p, size_t size))
{
  ScmStrItr iter;

  iter.p = p;
  iter.rest = size;

  if (char_width == NULL) {
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
  next.p = (uint8_t *)iter->p + w;
  next.rest = iter->rest - w;
  next.char_width = iter->char_width;

  return next;
}


/***********************************************************************/
/*   UTF-8                                                             */
/***********************************************************************/

#define IS_VALID_UTF8_1(utf8)                   \
  (0x00 <= (utf8)[0] && (utf8)[0] <= 0x7f)
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
    return 0;
  }
  else if ((utf8[0] & 0x80) == 0x00) {
    return (len >= 1) ? 1 : -1;
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
  ScmStrItr iter;
  int i;

  iter = scm_str_itr_begin(str, size, scm_enc_char_width_utf8);
  if (SCM_STR_ITR_IS_ERR(&iter)) return iter;

  i = 0;
  while (!SCM_STR_ITR_IS_END(&iter) && i < idx) {
    iter = scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return iter;
    i++;
  }

  return iter;
}
