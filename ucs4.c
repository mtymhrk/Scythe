#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "ucs4.h"

#define UCS4_STR_BLOCK_SIZE 64

struct Ucs4StringRec {
  ucs4chr_t *buffer;
  ucs4chr_t *head;
  size_t capacity;
  size_t length;
  int *ref_cnt;
};

#define CAPACITY(str) ((str)->capacity - ((str)->head - (str)->buffer))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - (str)->length)

#define IS_VALID_ASCII(ascii) \
  (0x00 <= (unsigned int)(ascii) && (unsigned int)(ascii) <= 0x7f)

/* XXX: is this correct ? */
#define IS_VALID_UCS4(ucs4) \
  (ucs4 <= 0xd7ff || (0xe000 <= ucs4 && ucs4 <= 0xfffd) \
   || (0x10000 <= ucs4 && ucs4 <= 0x10ffff))

/* from RFC3629 */
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


static ssize_t
utf8chr_to_ucs4chr(const utf8_t *utf8, size_t utf8_len, ucs4chr_t *ucs4)
{
  if (utf8 == NULL) return -1;
  if (utf8_len == 0) return 0;

  if ((utf8[0] & 0x80) == 0x00) {
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

static int
ucs4chr_to_utf8chr(ucs4chr_t ucs4chr, utf8_t *utf8, size_t size)
{
  if (ucs4chr <= 0x7f) {
    if (size < 2) return -1;
    utf8[0] = (utf8_t)ucs4chr & 0x7f;
    utf8[1] = '\0';
    return 1;
  }
  else if (ucs4chr <= 0x7ff) {
    if (size < 3) return -1;
    utf8[0] = 0xc0 | ((ucs4chr & 0x7c0) >> 6);
    utf8[1] = 0x80 | (ucs4chr & 0x3f);
    utf8[2] = '\0';
    return 2;
  }
  else if (ucs4chr <= 0xffff) {
    if (size < 4) return -1;
    utf8[0] = 0xe0 | ((ucs4chr & 0xf000) >> 12);
    utf8[1] = 0x80 | ((ucs4chr & 0xfc0) >> 6);
    utf8[2] = 0x80 | (ucs4chr & 0x3f);
    utf8[3] = '\0';
    return 3;
  }
  else if (ucs4chr <= 0x10ffff) {
    if (size < 5) return -1;
    utf8[0] = 0xf0 | ((ucs4chr & 0x1c0000) >> 18);
    utf8[1] = 0x80 | ((ucs4chr & 0x3f000) >> 12);
    utf8[2] = 0x80 | ((ucs4chr & 0xfc0) >> 6);
    utf8[3] = 0x80 | (ucs4chr & 0x3f);
    utf8[4] = '\0';
    return 4;
  }
  else {
    return -1;
  }
}

static Ucs4String *
ucs4str(const void *src, size_t len)
{
  Ucs4String *str;

  str = malloc(sizeof(*str));
  if (str == NULL) return NULL;
  str->buffer = NULL;
  str->ref_cnt = NULL;

  for (str->capacity = UCS4_STR_BLOCK_SIZE;
       str->capacity < len;
       str->capacity *= 2)
    ;

  str->head = str->buffer = malloc(sizeof(ucs4chr_t) * str->capacity);
  if (str->buffer == NULL) goto err;

  str->ref_cnt = malloc(sizeof(*str->ref_cnt));
  if (str->ref_cnt == NULL) goto err;
  *str->ref_cnt = 1;

  if (src != NULL) {
    memcpy(str->buffer, src, len * sizeof(ucs4chr_t));
    str->length = len;
  }
  else {
    str->length = 0;
  }
  
  return str;

 err:
  free(str->buffer);
  free(str->ref_cnt);
  free(str);

  return NULL;
}

void
ucs4str_destruct(Ucs4String *str)
{
  if (str == NULL) return;

  if (*str->ref_cnt > 1) {
    (*str->ref_cnt)--;
  }
  else {
    free(str->buffer);
    free(str->ref_cnt);
  }

  free(str);
}

Ucs4String *
ucs4str_from_ascii(const char *ascii)
{
  Ucs4String *str;

  if (ascii != NULL)
    str = ucs4str(NULL, strlen(ascii));
  else
    str = ucs4str(NULL, UCS4_STR_BLOCK_SIZE);

  if (str == NULL) return NULL;

  if (ascii != NULL) {
    size_t i;
    str->length = strlen(ascii);
    for (i = 0; i < str->length; i++) {
      if (!IS_VALID_ASCII(ascii[i])) goto err;
      str->head[i] = UCS4CHR(ascii[i]);
    }
  }

  return str;

 err:
  ucs4str_destruct(str);
  return NULL;
}

Ucs4String *
ucs4str_from_utf8(const utf8_t *utf8)
{
  Ucs4String *str;
  int utf8_len;

  utf8_len = 0;
  if (utf8 != NULL) {
    utf8_len = strlen((const char *)utf8);
    str = ucs4str(NULL,  utf8_len);
  }
  else
    str = ucs4str(NULL, UCS4_STR_BLOCK_SIZE);

  if (str == NULL) return NULL;

  if (utf8 != NULL) {
    int utf8_idx;

    utf8_idx = 0;
    str->length = 0;
    while (utf8_idx < utf8_len) {
      int n;

      if (str->length >= CAPACITY(str)) goto err;
      n = utf8chr_to_ucs4chr(utf8 + utf8_idx, utf8_len - utf8_idx,
                             str->head + str->length);
      if (n < 0) goto err;
      utf8_idx += n;
      str->length++;
    }
  }

  return str;

 err:
  ucs4str_destruct(str);
  return NULL;
}

static Ucs4String *
ucs4str_copy_and_expand(const Ucs4String *src, size_t size)
{
  Ucs4String *str;

  if (src == NULL) return NULL;

  str = ucs4str(NULL, size);
  if (str == NULL) return NULL;

  str->length = (size < src->length) ? size : src->length;
  memcpy(str->head, src->head, str->length * sizeof(ucs4chr_t));

  return str;
}

static void
ucs4str_replace_contents(Ucs4String *target, Ucs4String *src)
{
  assert(target != NULL);
  assert(target != NULL);

  if (*target->ref_cnt > 1) {
    (*target->ref_cnt)--;
  }
  else {
    free(target->buffer);
    free(target->ref_cnt);
  }

  *target = *src;
  (*target->ref_cnt)++;
}

Ucs4String *
ucs4str_copy(const Ucs4String *src)
{
  Ucs4String *str;

  if (src == NULL) return NULL;

  str = ucs4str(src->head, src->length);
  return (str == NULL) ? NULL : str;
}

Ucs4String *
ucs4str_dup(Ucs4String *src)
{
  Ucs4String *str;

  if (src == NULL) return NULL;

  str = malloc(sizeof(*str));
  if (str == NULL) return NULL;

  *str = *src;
  (*str->ref_cnt)++;

  return str;
}

ssize_t
ucs4str_length(Ucs4String *str)
{
  if (str == NULL) return -1;
  return str->length;
}

bool
ucs4str_is_equal(Ucs4String *str1, Ucs4String *str2)
{
  if (str1 == NULL && str2 == NULL) return true;
  else if (str1 == NULL || str2 == NULL) return false;

  if (str1->length != str2->length) return false;
  if (str1->head == str2->head) return true;
  return (memcmp(str1->head, str2->head, str1->length) == 0);
}

Ucs4String *
ucs4str_substr(Ucs4String *str, unsigned int pos, size_t len)
{
  Ucs4String *substr;

  if (str == NULL) return NULL;
  if (pos + len > str->length) return NULL;

  substr = ucs4str_dup(str);
  substr->head = substr->buffer + pos;
  substr->length = len;

  return substr;
}

Ucs4String *
ucs4str_push(Ucs4String *str, ucs4chr_t c)
{
  if (str == NULL) return NULL;
  if (!IS_VALID_UCS4(c)) return NULL;

  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) >= 1) {
    Ucs4String *tmp =
      ucs4str_copy_and_expand(str, str->length + 1);
    if (tmp == NULL) return NULL;
    ucs4str_replace_contents(str, tmp);
    ucs4str_destruct(tmp);
  }

  str->head[str->length++] = c;

  return str;
}

Ucs4String *
ucs4str_append(Ucs4String *str, const Ucs4String *append)
{
  if (str == NULL) return NULL;
  if (append == NULL) return str;

  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) < append->length) {
    Ucs4String *tmp = ucs4str_copy_and_expand(str,
                                              str->length + append->length);
    if (tmp == NULL) return NULL;
    ucs4str_replace_contents(str, tmp);
    ucs4str_destruct(tmp);
  }

  memcpy(str->head + str->length, append->head,
         append->length * sizeof(ucs4chr_t));
  str->length += append->length;

  return str;
}

Ucs4String *
ucs4str_append_ascii(Ucs4String *str, const char *append)
{
  Ucs4String *apnd, *rslt;

  if (str == NULL) return NULL;
  if (append == NULL) return str;

  apnd = ucs4str_from_ascii(append);
  if (apnd == NULL) return NULL;

  rslt = ucs4str_append(str, apnd);

  ucs4str_destruct(apnd);

  return rslt;
}

Ucs4String *
ucs4str_append_utf8(Ucs4String *str, const utf8_t *append)
{
  Ucs4String *apnd, *rslt;

  if (str == NULL) return NULL;
  if (append == NULL) return str;

  apnd = ucs4str_from_utf8(append);
  if (apnd == NULL) return NULL;

  rslt = ucs4str_append(str, apnd);

  ucs4str_destruct(apnd);

  return rslt;
}

ucs4chr_t
ucs4str_get(const Ucs4String *str, unsigned int pos)
{
  if (str == NULL) return UCS4EOF;
  if (pos >= str->length) return UCS4EOF;

  return str->head[pos];
}

Ucs4String *
ucs4str_set(Ucs4String *str, unsigned int pos, ucs4chr_t c)
{
  if (str == NULL) return NULL;
  if (pos >= str->length) return NULL;

  if (*str->ref_cnt > 1) {
    Ucs4String *tmp = ucs4str_copy(str);
    if (tmp == NULL) return NULL;
    ucs4str_replace_contents(str, tmp);
    ucs4str_destruct(tmp);
  }

  str->head[pos] = c;
  return str;
}

Ucs4String *
ucs4str_fill(Ucs4String *str, unsigned int pos, size_t len, ucs4chr_t c)
{
  unsigned int i;
  size_t length;

  if (str == NULL) return NULL;
  if (pos > str->length) return NULL; /* permit append to tail */

  length = (str->length > pos + len) ? str->length : pos + len;
  if ((*str->ref_cnt > 1) || (CAPACITY(str) < length)) {
    Ucs4String *tmp = ucs4str_copy_and_expand(str, length);
    if (tmp == NULL) return NULL;
    ucs4str_replace_contents(str, tmp);
    ucs4str_destruct(tmp);
  }

  for (i = 0; i < len; i++)
    str->head[pos + i] = c;
  str->length = length;

  return str;
}

int
ucs4str_find_chr(const Ucs4String *str, ucs4chr_t c)
{
  int i;

  if (str == NULL) return -1;

  for (i = 0; i < str->length; i++)
    if (str->head[i] == c)
      return i;

  return -1;
}

int
ucs4str_match(const Ucs4String *str, const Ucs4String *pat)
{
  int i, j, n;

  if (str == NULL || pat == NULL) return -1;

  n = str->length - pat->length + 1;
  for (i = 0; i < n; i++)
    for (j = 0; j < pat->length; j++)
      if (str->head[i + j] == pat->head[j])
        return i;

  return -1;
}

int
ucs4str_match_ascii(const Ucs4String *str, const char *ascii_pat)
{
  Ucs4String *pat;
  int rslt;

  if (str == NULL || ascii_pat == NULL) return -1;

  pat = ucs4str_from_ascii(ascii_pat);
  if (pat == NULL) return -1;
  rslt = ucs4str_match(str, pat);
  ucs4str_destruct(pat);
  return rslt;
}

int
ucs4str_match_utf8(const Ucs4String *str, const utf8_t *utf8_pat)
{
  Ucs4String *pat;
  int rslt;

  if (str == NULL || utf8_pat == NULL) return -1;

  pat = ucs4str_from_utf8(utf8_pat);
  if (pat == NULL) return -1;
  rslt = ucs4str_match(str, pat);
  ucs4str_destruct(pat);
  return rslt;
}

ssize_t
ucs4str_to_ascii(const Ucs4String *str, char *ascii, size_t size)
{
  char *p;
  int i, rest;
  
  if (str == NULL || ascii == NULL) return -1;

  rest = size;
  p = ascii;
  for (i = 0; i < str->length; i++) {
    if (rest <= 1) break;
    if (!IS_VALID_ASCII(str->head[i])) break;
    *(p++) = str->head[i];
    rest--;
  }

  *p = '\0';

  return size - rest;
}

ssize_t
ucs4str_to_utf8(const Ucs4String *str, utf8_t *utf8, size_t size)
{
  utf8_t *p;
  int i, rest;
  
  if (str == NULL || utf8 == NULL) return -1;

  rest = size;
  p = utf8;
  for (i = 0; i < str->length; i++) {
    int n;

    n = ucs4chr_to_utf8chr(str->head[i], p, rest);
    if (n < 0) break;

    p += n;
    rest -= n;
  }

  return size - rest;
}

ssize_t
ucs4str_dump(const Ucs4String *str, void *buf, size_t size)
{
  ssize_t len;

  if (str == NULL || buf == NULL) return -1;

  len = str->length * sizeof(ucs4chr_t);
  if (size < len) len = size;

  memcpy(buf, str->head, len);

  return len;
}
