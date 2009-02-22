#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "string.h"

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
/* TODO: change index2iter return value to ScmStrIter */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  int (*index2iter)(ScmStrIter *iter,
                    void *p, size_t size, unsigned int idx);
} ScmStrVirtualFunc;

struct ScmStringRec {
  ScmObjHeader header;
  uint8_t *buffer;
  uint8_t *head;
  size_t capacity;
  size_t bytesize;
  size_t length;
  int *ref_cnt;
  SCM_STRING_ENC_T enc;
};

#define SCM_STRING_BLOCK_SIZE  64
#define CAPACITY(str)((str)->capacity - ((str)->head - (str)->buffer))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - (str)->bytesize)

// TODO: change to be encdoing depende function
static bool
scm_string_is_char_to_be_escaped(char c)
{
  return (strchr("\\\"", c) != NULL);
}

// TODO: change to be encdoing depende function
static void
scm_string_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmString *string = NULL;
  uint8_t *p;

  assert(obj != NULL); assert(scm_string_is_string(obj));
  assert(obuffer != NULL);

  string = SCM_STRING(obj);

  scm_obuffer_concatenate_char(obuffer, '"');
  for (p = string->head; *p != '\0'; p++) {
    if (scm_string_is_char_to_be_escaped(*p))
      scm_obuffer_concatenate_char(obuffer, '\\');
    scm_obuffer_concatenate_char(obuffer, *p);
  }
  scm_obuffer_concatenate_char(obuffer, '"');
}

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


/***********************************************************************
 * encoding depende functions
 ***********************************************************************/

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

static int
utf8str_char_width(const void *str, size_t len)
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

static ScmStrVirtualFunc SCM_STRING_VFUNC_UTF8 =
  { utf8str_char_width, utf8str_index2iter };

static ScmStrVirtualFunc *SCM_STRING_VFUNC_TBL[] =
  { &SCM_STRING_VFUNC_UTF8 };

#define SCM_STRING_VFUNC(enc) (SCM_STRING_VFUNC_TBL[enc])
#define SCM_STRING_VFUNC_CHAR_WIDTH(enc) (SCM_STRING_VFUNC(enc)->char_width)
#define SCM_STRING_VFUNC_INDEX2ITER(enc) (SCM_STRING_VFUNC(enc)->index2iter)

static ScmString *
scm_string_copy_and_expand(const ScmString *src, size_t size)
{
  ScmString *str;

  assert(src != NULL);

  str = scm_string_construct(NULL, size, src->enc);
  if (str == NULL) return NULL;

  str->bytesize = (size < src->bytesize) ? size : src->bytesize;
  str->length
    = scm_str_copy_bytes_with_checking_validity(str->buffer,
                                                src->head,
                                                src->bytesize,
                                                SCM_STRING_VFUNC(src->enc));
  if (str->length < 0) {
    scm_string_destruct(str);
    return NULL;
  }

  return str;
}

static void
scm_string_replace_contents(ScmString *target, ScmString *src)
{
  if (target == NULL || src == NULL) return;

  if (*target->ref_cnt > 1) {
    (*target->ref_cnt)--;
  }
  else {
    scm_memory_release(target->buffer);
    scm_memory_release(target->ref_cnt);
  }

  *target = *src;
  (*target->ref_cnt)++;
}

ScmString *
scm_string_construct(const void *src, size_t size, SCM_STRING_ENC_T enc)
{
  ScmString *str = NULL;

  assert(0 < enc && enc < SMC_STRING_NR_ENC);

  str = (ScmString *)scm_memory_allocate(sizeof(ScmString));
  scm_obj_init(SCM_OBJ(str), SCM_OBJ_TYPE_STRING, scm_string_pretty_print);

  str->buffer = NULL;
  str->ref_cnt = NULL;
  str->enc = enc;

  for (str->capacity = SCM_STRING_BLOCK_SIZE;
       str->capacity < size;
       str->capacity *= 2)
    ;

  str->head = str->buffer = scm_memory_allocate(str->capacity);
  if (str->buffer == NULL) goto err;

  str->ref_cnt = scm_memory_allocate(sizeof(*str->ref_cnt));
  if (str->ref_cnt == NULL) goto err;
  *str->ref_cnt = 1;

  if (src != NULL) {
    str->length
      = scm_str_copy_bytes_with_checking_validity(str->buffer,
                                                  src, size,
                                                  SCM_STRING_VFUNC(enc));
    if (str->length < 0) goto err;
    str->bytesize = size;
  }
  else {
    str->length = 0;
    str->bytesize = 0;
  }
  
  return str;

 err:
  scm_memory_release(str->buffer);
  scm_memory_release(str->ref_cnt);
  scm_memory_release(str);
  
  return NULL;
}

void
scm_string_destruct(ScmString *str)
{
  assert(str != NULL);

  if (*str->ref_cnt > 1)
    (*str->ref_cnt)--;
  else {
    scm_memory_release(str->buffer);
    scm_memory_release(str->ref_cnt);
  }

  scm_memory_release(str);
}

ScmString *
scm_string_copy(const ScmString *src)
{
  assert(src != NULL);
  
  return scm_string_construct(src->head, src->bytesize, src->enc);
}

ScmString *
scm_string_dup(ScmString *src)
{
  ScmString *str;

  assert(src != NULL);

  str = (ScmString *)scm_memory_allocate(sizeof(ScmString));
  scm_obj_init(SCM_OBJ(str), SCM_OBJ_TYPE_STRING, scm_string_pretty_print);
  
  str->buffer = src->buffer;
  str->head = src->head;
  str->capacity = src->capacity;
  str->bytesize = src->bytesize;
  str->length = src->length;
  str->ref_cnt = src->ref_cnt;
  str->enc = src->enc;

  (*str->ref_cnt)++;

  return str;
}

size_t
scm_string_length(ScmString *str)
{
  assert(str != NULL);
  return str->length;
}

size_t
scm_string_bytesize(ScmString *str)
{
  assert(str != NULL);
  return str->bytesize;
}

bool
scm_string_is_equal(ScmString *str1, ScmString *str2)
{
  assert(str1 != NULL); assert(str2 != NULL);

  if (str1->length != str2->length) return false;
  if (str1->bytesize != str2->bytesize) return false;
  if (str1->head == str2->head) return true;
  return (memcmp(str1->head, str2->head, str1->bytesize) == 0);
}

ScmString *
scm_string_substr(ScmString *str, unsigned int pos, size_t len)
{
  ScmString *substr;
  ScmStrIter head, tail;
  int (*index2iter)(ScmStrIter *iter,
                    void *p, size_t size, unsigned int idx);

  assert(str != NULL);

  if (pos + len > str->length) return NULL;

  index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc);
  index2iter(&head, str->head, str->bytesize, pos);
  index2iter(&tail, str->head, str->bytesize, pos + len);

  if (scm_str_iter_is_error(&head) || scm_str_iter_is_error(&tail))
    return NULL;

  substr = scm_string_dup(str);
  substr->head = (uint8_t *)ITER_PTR(&head);
  substr->length = len;
  substr->bytesize = (uint8_t *)ITER_PTR(&tail) - (uint8_t *)ITER_PTR(&head);

  return substr;
}

ScmString *
scm_string_push(ScmString *str, const void *c, size_t csize)
{
  int (*char_width)(const void *p, size_t size);
  int width;

  assert(str != NULL);
  assert(c != NULL);

  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);
  width = char_width(c, csize);
  if (width < 0 || width != csize) return NULL;

  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) >= width) {
    ScmString *tmp
      = scm_string_copy_and_expand(str, str->bytesize + width);
    if (tmp == NULL) return NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_destruct(tmp);
  }

  memcpy(str->head + str->bytesize, c, csize);
  str->length += 1;
  str->bytesize += csize;

  return str;
}

ScmString *
scm_string_append(ScmString *str, const ScmString *append)
{
  assert(str != NULL);
  assert(append != NULL);
  
  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) < append->bytesize) {
    ScmString *tmp
      = scm_string_copy_and_expand(str, str->bytesize + append->bytesize);
    if (tmp == NULL) return NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_destruct(tmp);
  }

  memcpy(str->head + str->bytesize, append->head, append->bytesize);
  str->length += append->length;
  str->bytesize += append->bytesize;

  return str;
}

/* TODO: define object for Charactor */
/* ScmChar */
/* scm_string_ref(Utf8String *str, unsigned int pos) */
/* { */
/*   ScmStrIter iter; */
/*   ScmChar c; */
/*   int (*index2iter)(ScmStrIter *iter, */
/*                     void *p, size_t size, unsigned int idx); */

/*   assert(str != NUL); */

/*   if (pos >= str->length) return c; */

/*   index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc); */
/*   index2iter(&iter, str->head, str->bytesize, pos); */
/*   if (scm_str_iter_is_end(&iter)) return c; */

/*   memcpy(c.chr, ITER_PTR(&iter), ITER_WIDTH(&iter)); */

/*   return c; */
/* } */

static ScmString*
scm_string_set_less_or_same_width_char(ScmString *str,
                                       const ScmStrIter *iter,
                                       const void *c, size_t cw)
{
  size_t offset;
  size_t rest;
  int iw;

  assert(str != NULL);
  assert(iter != NULL);
  assert(c != NULL);

  offset = ITER_OFFSET(iter, str->head);
  rest = ITER_REST(iter);
  iw = ITER_WIDTH(iter);

  if (*str->ref_cnt > 1) {
    ScmString *tmp = scm_string_copy(str);
    if (tmp == NULL) return NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_destruct(tmp);    
  }

  memcpy(str->head + offset, c, cw);
  if (cw < iw) {
    memmove(str->head + offset + cw, str->head + offset + iw, rest - iw);
    str->bytesize -= iw - cw;
  }

  return str;
}

ScmString *
scm_string_set(ScmString *str, unsigned int pos, const void *c, size_t cw)
{
  int (*char_width)(const void *p, size_t size);
  int (*index2iter)(ScmStrIter *iter,
                    void *p, size_t size, unsigned int idx);
  ScmStrIter iter;
  ScmString *rslt, *front, *rear;
  int w;

  assert(str != NULL);
  assert(c != NULL);

  if (pos >= str->length) return NULL;

  index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc);
  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);

  index2iter(&iter, str->head, str->bytesize, pos);
  if (scm_str_iter_is_error(&iter)) return NULL;

  w = char_width(c, cw);
  if (w != cw) return NULL;

  if (cw <= ITER_WIDTH(&iter))
    return scm_string_set_less_or_same_width_char(str, &iter, c, cw);

  rslt = front = rear = NULL;

  front = scm_string_substr(str, 0, pos);
  rear = scm_string_substr(str, pos + 1, str->length - pos - 1);
  
  if (front == NULL || rear == NULL) goto end;
  
  if (scm_string_push(front, c, cw) == NULL) goto end;
  if (scm_string_append(front, rear) == NULL) goto end;
  
  scm_string_replace_contents(str, front);
  rslt = str;
  
 end:
  scm_string_destruct(front);
  scm_string_destruct(rear);
  return rslt;
}



/* XXX: old implementation */
/* char * */
/* scm_string_string(const ScmString *string) */
/* { */
/*   assert(string != NULL); */

/*   return string->string; */
/* } */

/* size_t */
/* scm_string_length(const ScmString *string) */
/* { */
/*   assert(string != NULL); */

/*   return string->length; */
/* } */

bool
scm_string_is_string(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_STRING);
}
