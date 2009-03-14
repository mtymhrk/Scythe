#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "utf8.h"

#define UTF8_STR_BLOCK_SIZE 64


typedef struct StrIterRec {
  void *p;
  size_t rest;
  int (*char_width)(const void *p, size_t size);
} ScmStrIter;

#define ITER_PTR(iter) ((iter)->p)
#define ITER_REST(iter) ((iter)->rest)
#define ITER_WIDTH(iter) ((iter)->char_width(ITER_PTR(iter), ITER_REST(iter)))
#define ITER_COPY(iter, copy) (*(copy) = *(iter))
#define ITER_OFFSET(iter, head) ((uint8_t *)ITER_PTR(iter) -  (uint8_t *)head);

/* encoding depending function table */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  int (*index2iter)(ScmStrIter *iter,
                    void *p, size_t size, unsigned int idx);
} ScmStrVirtualFunc;

struct Utf8StringRec {
  utf8chr_t *buffer;
  utf8chr_t *head;
  size_t capacity;
  size_t bytesize;
  size_t length;
  int *ref_cnt;
};

#define CAPACITY(str) \
  ((str)->capacity - ((uint8_t *)(str)->head - (uint8_t *)(str)->buffer))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - (str)->bytesize)

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


static ScmStrIter 
scm_str_iter_begin(void *p, size_t size,
                   int (*char_width)(const void *p, size_t size))
{
  ScmStrIter iter;

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

static ScmStrIter
scm_str_iter_next(const ScmStrIter *iter)
{
  ScmStrIter next;
  int w;

  next.p = NULL;
  next.rest = -1;
  next.char_width = NULL;

  if (iter == NULL) return next;
  if (iter->rest <= 0) return *iter;

  w = ITER_WIDTH(iter);
  next.p = (uint8_t *)iter->p + w;
  next.rest = iter->rest - w;
  next.char_width = iter->char_width;

  return next;
}

static bool
scm_str_iter_is_end(const ScmStrIter *iter)
{
  if (iter == NULL) return true;

  return (iter->rest == 0) ? true : false;
}

static bool
scm_str_iter_is_error(const ScmStrIter *iter)
{
  if (iter == NULL) return true;

  return (iter->rest < 0) ? true : false;
}

static ssize_t
scm_str_check_bytes(const void *str, size_t size, ScmStrVirtualFunc *vf)
{
  ScmStrIter iter;
  ssize_t len;

  if (str == NULL || vf == NULL) return -1;

  iter = scm_str_iter_begin((void *)str, size, vf->char_width);
  if (scm_str_iter_is_error(&iter)) return -1;

  len = 0;
  while (!scm_str_iter_is_end(&iter)) {
    len++;
    iter = scm_str_iter_next(&iter);
    if (scm_str_iter_is_error(&iter)) return -1;
  }

  return len;
}

static ssize_t
scm_str_copy_bytes_with_checking_validity(void *dst,
                                          const void *src, size_t size,
                                          ScmStrVirtualFunc *vf)
{
  ssize_t len;

  if (dst == NULL || src == NULL || vf == NULL) return -1;

  len = scm_str_check_bytes(src, size, vf);
  if (len < 0) return -1;

  memcpy(dst, src, size);
  return len;
}

static int
utf8str_char_width(const void *str, size_t len)
{
  const utf8chr_t *utf8 = str;

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

static int
utf8str_index2iter(ScmStrIter *iter, void *str, size_t size, unsigned int idx)
{
  int i;

  if (iter == NULL) return -1;

  *iter = scm_str_iter_begin(str, size, utf8str_char_width);
  if (scm_str_iter_is_error(iter)) return -1;

  i = 0;
  while (!scm_str_iter_is_end(iter) && i < idx) {
    *iter = scm_str_iter_next(iter);
    if (scm_str_iter_is_error(iter)) return -1;
    i++;
  }

  if (scm_str_iter_is_end(iter)) {
    return (i == idx) ? 0 : -1;
  }
  else
    return 0;
}

ScmStrVirtualFunc UTF8VFUNC = { utf8str_char_width, utf8str_index2iter };

Utf8String *
utf8str(const void *src, size_t len)
{
  Utf8String *str;

  str = malloc(sizeof(*str));
  if (str == NULL) return NULL;
  str->buffer = NULL;
  str->ref_cnt = NULL;

  for (str->capacity = UTF8_STR_BLOCK_SIZE;
       str->capacity < len;
       str->capacity *= 2)
    ;

  str->head = str->buffer = malloc(str->capacity);
  if (str->buffer == NULL) goto err;

  str->ref_cnt = malloc(sizeof(*str->ref_cnt));
  if (str->ref_cnt == NULL) goto err;
  *str->ref_cnt = 1;

  if (src != NULL) {
    str->length = scm_str_copy_bytes_with_checking_validity(str->buffer,
                                                            src, len,
                                                            &UTF8VFUNC);
    if (str->length < 0) goto err;
    str->bytesize = len;
  }
  else {
    str->length = 0;
    str->bytesize = 0;
  }
  
  return str;

 err:
  free(str->buffer);
  free(str->ref_cnt);
  free(str);

  return NULL;
}

void
utf8str_destruct(Utf8String *str)
{
  if (str == NULL) return;

  if (*str->ref_cnt > 1)
    (*str->ref_cnt)--;
  else {
    free(str->buffer);
    free(str->ref_cnt);
  }

  free(str);
}

static Utf8String *
utf8str_copy_and_expand(const Utf8String *src, size_t size)
{
  Utf8String *str;

  if (src == NULL) return NULL;

  str = utf8str(NULL, size);
  if (str == NULL) return NULL;

  str->bytesize = (size < src->bytesize) ? size : src->bytesize;
  str->length = scm_str_copy_bytes_with_checking_validity(str->buffer,
                                                          src->head,
                                                          src->bytesize,
                                                          &UTF8VFUNC);
  if (str->length < 0) {
    utf8str_destruct(str);
    return NULL;
  }

  return str;
}

static void
utf8str_replace_contents(Utf8String *target, Utf8String *src)
{
  if (target == NULL || src == NULL) return;

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

Utf8String *
utf8str_copy(const Utf8String *src)
{
  if (src == NULL) return NULL;
  
  return utf8str(src->head, src->bytesize);
}

Utf8String *
utf8str_dup(Utf8String *src)
{
  Utf8String *str;

  if (src == NULL) return NULL;

  str = malloc(sizeof(*str));
  if (str == NULL) return NULL;

  *str = *src;
  (*str->ref_cnt)++;

  return str;
}

ssize_t
utf8str_length(Utf8String *str)
{
  if (str == NULL) return -1;
  return str->length;
}

ssize_t
utf8str_bytesize(Utf8String *str)
{
  if (str == NULL) return -1;
  return str->bytesize;
}


bool
utf8str_is_equal(Utf8String *str1, Utf8String *str2)
{
  if (str1 == NULL && str2 == NULL) return true;
  else if (str1 == NULL || str2 == NULL) return false;

  if (str1->length != str2->length) return false;
  if (str1->bytesize != str2->bytesize) return false;
  if (str1->head == str2->head) return true;
  return (memcmp(str1->head, str2->head, str1->bytesize) == 0);
}

Utf8String *
utf8str_substr(Utf8String *str, unsigned int pos, size_t len)
{
  Utf8String *substr;
  ScmStrIter head, tail;

  if (str == NULL) return NULL;
  if (pos + len > str->length) return NULL;

  utf8str_index2iter(&head, str->head, str->bytesize, pos);
  utf8str_index2iter(&tail, str->head, str->bytesize, pos + len);

  if (scm_str_iter_is_error(&head) || scm_str_iter_is_error(&tail))
    return NULL;

  substr = utf8str_dup(str);
  substr->head = (utf8chr_t *)ITER_PTR(&head);
  substr->length = len;
  substr->bytesize = (uint8_t *)ITER_PTR(&tail) - (uint8_t *)ITER_PTR(&head);

  return substr;
}

Utf8String *
utf8str_push(Utf8String *str, const Utf8Chr *c)
{
  int width;

  if (str == NULL) return NULL;
  if (c == NULL) return NULL;

  width = UTF8VFUNC.char_width(c->chr, sizeof(*c));
  if (width < 0) return NULL;

  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) >= width) {
    Utf8String *tmp =
      utf8str_copy_and_expand(str, str->bytesize + width);
    if (tmp == NULL) return NULL;
    utf8str_replace_contents(str, tmp);
    utf8str_destruct(tmp);
  }

  memcpy(str->head + str->bytesize, c->chr, width);
  str->length += 1;
  str->bytesize += width;

  return str;
}

Utf8String *
utf8str_append(Utf8String *str, const Utf8String *append)
{
  if (str == NULL) return NULL;
  if (append == NULL) return str;
  
  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) < append->bytesize) {
    Utf8String *tmp = utf8str_copy_and_expand(str,
                                              str->bytesize + append->bytesize);
    if (tmp == NULL) return NULL;
    utf8str_replace_contents(str, tmp);
    utf8str_destruct(tmp);
  }

  memcpy(str->head + str->bytesize, append->head, append->bytesize);
  str->length += append->length;
  str->bytesize += append->bytesize;

  return str;
}

Utf8Chr
utf8str_get(Utf8String *str, unsigned int pos)
{
  ScmStrIter iter;
  Utf8Chr c;

  memset(&c, 0, sizeof(c));

  if (str == NULL) return c;
  if (pos >= str->length) return c;

  utf8str_index2iter(&iter, str->head, str->bytesize, pos);
  if (scm_str_iter_is_end(&iter)) return c;

  memcpy(c.chr, ITER_PTR(&iter), ITER_WIDTH(&iter));

  return c;
}

static Utf8String*
utf8str_set_less_or_same_width_char(Utf8String *str,
                                    const ScmStrIter *iter, const Utf8Chr *c)
{
  size_t offset;
  size_t rest;
  int iw, cw;

  assert(str != NULL);
  assert(iter != NULL);
  assert(c != NULL);

  offset = ITER_OFFSET(iter, str->head);
  rest = ITER_REST(iter);
  iw = ITER_WIDTH(iter);
  cw = UTF8VFUNC.char_width(c->chr, sizeof(*c));

  if (*str->ref_cnt > 1) {
    Utf8String *tmp = utf8str_copy(str);
    if (tmp == NULL) return NULL;
    utf8str_replace_contents(str, tmp);
    utf8str_destruct(tmp);    
  }

  memcpy(str->head + offset, c->chr, cw);
  if (cw < iw) {
    memmove(str->head + offset + cw, str->head + offset + iw, rest - iw);
    str->bytesize -= iw - cw;
  }

  return str;
}



Utf8String *
utf8str_set(Utf8String *str, unsigned int pos, const Utf8Chr *c)
{
  ScmStrIter iter;
  Utf8String *rslt, *front, *rear;
  int cw;

  if (str == NULL) return NULL;
  if (c == NULL) return NULL;
  if (pos >= str->length) return NULL;

  UTF8VFUNC.index2iter(&iter, str->head, str->bytesize, pos);
  if (scm_str_iter_is_error(&iter)) return NULL;

  cw = UTF8VFUNC.char_width(c->chr, sizeof(*c));

  if (cw <= ITER_WIDTH(&iter))
    return utf8str_set_less_or_same_width_char(str, &iter, c);

  rslt = front = rear = NULL;

  front = utf8str_substr(str, 0, pos);
  rear = utf8str_substr(str, pos + 1, str->length - pos - 1);
  
  if (front == NULL || rear == NULL) goto end;
  
  if (utf8str_push(front, c) == NULL) goto end;
  if (utf8str_append(front, rear) == NULL) goto end;
  
  utf8str_replace_contents(str, front);
  rslt = str;
  
 end:
  utf8str_destruct(front);
  utf8str_destruct(rear);
  return rslt;
}

Utf8String *
utf8str_fill(Utf8String *str, unsigned int pos, size_t len, const Utf8Chr *c)
{
  int filledsize;
  size_t i;
  Utf8String *rslt, *front, *rear;

  rslt = front = rear = NULL;

  if (str == NULL) return NULL;
  if (c == NULL) return NULL;
  if (pos > str->length) return NULL;

  filledsize = UTF8VFUNC.char_width(c->chr, sizeof(Utf8Chr)) * len;
  if (filledsize < 0) return 0;

  front = utf8str_substr(str, 0, pos);
  if (pos + len < str->length) {
    rear = utf8str_substr(str, pos + len, str->length - pos - len);
    if (rear == NULL) goto end;
  }

  for (i = 0; i < len; i++)
    if (utf8str_push(front, c) == NULL)
      goto end;

  if (rear != NULL)
    if (utf8str_append(front, rear) == NULL)
      goto end;

  utf8str_replace_contents(str, front);
  rslt = str;

 end:
  utf8str_destruct(front);
  utf8str_destruct(rear);
  return rslt;
}

int
utf8str_find_chr(const Utf8String *str, const Utf8Chr *c)
{
  size_t i;
  int cw;
  utf8chr_t *p;

  if (str == NULL) return -1;
  if (c == NULL) return -1;

  cw = UTF8VFUNC.char_width(c->chr, sizeof(Utf8Chr));
  if (cw < 0) return -1;

  p = str->head;
  for (i = 0; i < str->length; i++) {
    int w = UTF8VFUNC.char_width(p, str->bytesize - (p - str->head));
    if (w < 0) return -1;

    if (cw == w && (memcmp(c->chr, p, cw) == 0))
      return i;
    p += w;
  }

  return -1;
}

int
utf8str_match(const Utf8String *str, const Utf8String *pat)
{
  int (* const char_width)(const void *p, size_t size) = UTF8VFUNC.char_width;
  ScmStrIter iter_str_ext, iter_pat;
  int pos;

  pos = 0;

  iter_str_ext = scm_str_iter_begin(str->head, str->bytesize, char_width);
  if (scm_str_iter_is_error(&iter_str_ext)) return -1;

  while (!scm_str_iter_is_end(&iter_str_ext)) {
    ScmStrIter iter_str_inn;

    ITER_COPY(&iter_str_ext, &iter_str_inn);

    iter_pat = scm_str_iter_begin(pat->head, pat->bytesize, char_width);
    if (scm_str_iter_is_error(&iter_pat)) return -1;

    if (ITER_REST(&iter_str_ext) < ITER_REST(&iter_pat))
      return -1;

    while (!scm_str_iter_is_end(&iter_str_inn)
           && !scm_str_iter_is_end(&iter_pat)) {

      if (ITER_WIDTH(&iter_str_inn) != ITER_WIDTH(&iter_pat)) break;
      if (memcmp(ITER_PTR(&iter_str_inn), ITER_PTR(&iter_pat),
                 ITER_WIDTH(&iter_str_inn)) != 0)
        break;

      iter_str_inn = scm_str_iter_next(&iter_str_inn);
      if (scm_str_iter_is_error(&iter_str_inn)) return -1;

      iter_pat = scm_str_iter_next(&iter_pat);
      if (scm_str_iter_is_error(&iter_pat)) return -1;
    }

    if (scm_str_iter_is_end(&iter_pat))
      return pos;

    iter_str_ext = scm_str_iter_next(&iter_str_ext);
    if (scm_str_iter_is_error(&iter_str_ext)) return -1;;

    pos++;
  }

  return -1;
}

ssize_t
utf8str_dump(const Utf8String *str, void *buf, size_t size)
{
  ssize_t len;

  if (str == NULL || buf == NULL) return -1;

  len = (size < str->bytesize) ? size : str->bytesize;
  memcpy(buf, str->head, len); // XXX

  return len;
}

