#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "encoding.h"
#include "string.h"

/* encoding depending function table */
typedef struct ScmStrVirtualFunc {
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, unsigned int idx);
} ScmStrVirtualFunc;

struct ScmStringRec {
  ScmObjHeader header;
  uint8_t *buffer;
  uint8_t *head;
  size_t capacity;
  size_t bytesize;
  size_t length;
  int *ref_cnt;
  SCM_ENCODING_T enc;
};

#define SCM_STRING_BLOCK_SIZE  64
#define CAPACITY(str)((str)->capacity - ((str)->head - (str)->buffer))
#define ROOM_FOR_APPEND(str) (CAPACITY(str) - (str)->bytesize)

static ScmStrVirtualFunc SCM_STRING_VFUNC_ASCII =
  { scm_enc_char_width_ascii, scm_enc_index2itr_ascii };

static ScmStrVirtualFunc SCM_STRING_VFUNC_BIN =
  { scm_enc_char_width_bin, scm_enc_index2itr_bin };

static ScmStrVirtualFunc SCM_STRING_VFUNC_UTF8 =
  { scm_enc_char_width_utf8, scm_enc_index2itr_utf8 };

static ScmStrVirtualFunc SCM_STRING_VFUNC_UCS4 =
  { scm_enc_char_width_ucs4, scm_enc_index2itr_ucs4 };

static ScmStrVirtualFunc SCM_STRING_VFUNC_EUCJP =
  { scm_enc_char_width_eucjp, scm_enc_index2itr_eucjp };

static ScmStrVirtualFunc SCM_STRING_VFUNC_SJIS =
  { scm_enc_char_width_sjis, scm_enc_index2itr_sjis };

static ScmStrVirtualFunc *SCM_STRING_VFUNC_TBL[] =
  { &SCM_STRING_VFUNC_ASCII,
    &SCM_STRING_VFUNC_BIN,
    &SCM_STRING_VFUNC_UCS4,
    &SCM_STRING_VFUNC_UTF8,
    &SCM_STRING_VFUNC_EUCJP,
    &SCM_STRING_VFUNC_SJIS };

#define SCM_STRING_VFUNC(enc) (SCM_STRING_VFUNC_TBL[enc])
#define SCM_STRING_VFUNC_CHAR_WIDTH(enc) (SCM_STRING_VFUNC(enc)->char_width)
#define SCM_STRING_VFUNC_INDEX2ITER(enc) (SCM_STRING_VFUNC(enc)->index2iter)


static ssize_t
scm_string_check_bytes(const void *str, size_t size, ScmStrVirtualFunc *vf)
{
  ScmStrItr iter;
  ssize_t len;

  if (str == NULL || vf == NULL) return -1;

  iter = scm_str_itr_begin((void *)str, size, vf->char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  len = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    len++;
    iter = scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return -1;
  }

  return len;
}

static ssize_t
scm_string_copy_bytes_with_check(void *dst, const void *src, size_t size,
                                 ScmStrVirtualFunc *vf)
{
  ssize_t len;

  if (dst == NULL || src == NULL || vf == NULL) return -1;

  len = scm_string_check_bytes(src, size, vf);
  if (len < 0) return -1;

  memcpy(dst, src, size);
  return len;
}

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
  int (*char_width)(const void *p, size_t size);
  ScmString *str = NULL;
  ScmStrItr iter;

  assert(obj != NULL); assert(scm_string_is_string(obj));
  assert(obuffer != NULL);

  str = SCM_STRING(obj);

  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);
  iter = scm_str_itr_begin(str->head, str->bytesize, char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return;

  scm_obuffer_concatenate_char(obuffer, '"');
  while (!SCM_STR_ITR_IS_END(&iter)) {
    int i, w = SCM_STR_ITR_WIDTH(&iter);

    if (w == 1
        && scm_string_is_char_to_be_escaped(((char *)SCM_STR_ITR_PTR(&iter))[0]))
      scm_obuffer_concatenate_char(obuffer, '\\');
    
    for (i = 0; i < w; i++)
      scm_obuffer_concatenate_char(obuffer,
                                   ((char *)SCM_STR_ITR_PTR(&iter))[i]);

    iter = scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return;
  }

  scm_obuffer_concatenate_char(obuffer, '"');
}

static ScmString *
scm_string_copy_and_expand(const ScmString *src, size_t size)
{
  ScmString *str;
  ssize_t len;

  assert(src != NULL);

  str = scm_string_construct(NULL, size, src->enc);
  if (str == NULL) return NULL;

  str->bytesize = (size < src->bytesize) ? size : src->bytesize;
  len = scm_string_copy_bytes_with_check(str->buffer, src->head, src->bytesize,
                                         SCM_STRING_VFUNC(src->enc));
  if (len < 0) {
    scm_string_destruct(str);
    return NULL;
  }

  str->length = len;

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
scm_string_construct(const void *src, size_t size, SCM_ENCODING_T enc)
{
  ScmString *str = NULL;

  assert(0 <= enc && enc < SMC_ENCODING_NR_ENC);

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
    ssize_t len = scm_string_copy_bytes_with_check(str->buffer, src, size,
                                                   SCM_STRING_VFUNC(enc));
    if (len < 0) goto err;
    str->length = len;
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
  ScmStrItr head, tail;
  ScmStrItr (*index2iter)(void *p, size_t size, unsigned int idx);

  assert(str != NULL);

  if (pos + len > str->length) return NULL;

  index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc);
  head = index2iter(str->head, str->bytesize, pos);
  tail = index2iter(str->head, str->bytesize, pos + len);

  if (SCM_STR_ITR_IS_ERR(&head) || SCM_STR_ITR_IS_ERR(&tail))
    return NULL;

  substr = scm_string_dup(str);
  substr->head = (uint8_t *)SCM_STR_ITR_PTR(&head);
  substr->length = len;
  substr->bytesize
    = (uint8_t *)SCM_STR_ITR_PTR(&tail) - (uint8_t *)SCM_STR_ITR_PTR(&head);

  return substr;
}

ScmString *
scm_string_push(ScmString *str, const scm_char_t c)
{
  int (*char_width)(const void *p, size_t size);
  int width;

  assert(str != NULL);

  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);
  width = char_width(&c, sizeof(c));
  if (width < 0) return NULL;

  if ((*str->ref_cnt > 1) || ROOM_FOR_APPEND(str) < width) {
    ScmString *tmp
      = scm_string_copy_and_expand(str, str->bytesize + width);
    if (tmp == NULL) return NULL;
    scm_string_replace_contents(str, tmp);
    scm_string_destruct(tmp);
  }

  memcpy(str->head + str->bytesize, &c, width);
  str->length += 1;
  str->bytesize += width;

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

scm_char_t
scm_string_ref(ScmString *str, unsigned int pos)
{
  ScmStrItr iter;
  scm_char_t c;
  ScmStrItr (*index2iter)(void *p, size_t size, unsigned int idx);

  assert(str != NULL);

  c = SCM_CHR_ZERO;
  if (pos >= str->length) return c;

  index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc);
  iter = index2iter(str->head, str->bytesize, pos);
  if (SCM_STR_ITR_IS_ERR(&iter)) return c;
  if (SCM_STR_ITR_IS_END(&iter)) return c;
  
  memcpy(&c, SCM_STR_ITR_PTR(&iter), SCM_STR_ITR_WIDTH(&iter));

  return c;
}

ScmString *
scm_string_set(ScmString *str, unsigned int pos, const scm_char_t c)
{
  int (*char_width)(const void *p, size_t size);
  ScmStrItr (*index2iter)(void *p, size_t size, unsigned int idx);
  ScmStrItr iter;
  int cw, iw;

  assert(str != NULL);

  if (pos >= str->length) return NULL;

  index2iter = SCM_STRING_VFUNC_INDEX2ITER(str->enc);
  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);

  iter = index2iter(str->head, str->bytesize, pos);
  if (SCM_STR_ITR_IS_ERR(&iter)) return NULL;

  cw = char_width(&c, sizeof(c));
  if (cw < 0) return NULL;

  iw = SCM_STR_ITR_WIDTH(&iter);
  if (iw < 0) return NULL;

  if (*str->ref_cnt == 1 && ROOM_FOR_APPEND(str) >= (cw - iw)) {
    size_t rest = SCM_STR_ITR_REST(&iter);
    size_t offset = SCM_STR_ITR_OFFSET(&iter, str->head);

    if (cw != iw) {
      memmove(str->head + offset + cw, str->head + offset + iw, rest - iw);
      str->bytesize += cw - iw;
    }
    memcpy(str->head + offset, &c, cw);

    return str;
  }
  else if (cw == iw) {
    ScmString *tmp = scm_string_copy(str);
    size_t offset = SCM_STR_ITR_OFFSET(&iter, str->head);

    if (tmp == NULL) return NULL;
    memcpy(str->head + offset, &c, cw);
    scm_string_replace_contents(str, tmp);

    return str;
  }
  else {
    ScmString *rslt, *front, *rear, *tmp;

    rslt = front = rear = tmp = NULL;

    front = scm_string_substr(str, 0, pos);
    rear = scm_string_substr(str, pos + 1, str->length - pos - 1);
  
    if (front == NULL || rear == NULL) goto end;

    tmp = scm_string_copy_and_expand(front,
                                     front->bytesize + cw + rear->bytesize);
    if (tmp == NULL) goto end;
    if (scm_string_push(tmp, c) == NULL) goto end;
    if (scm_string_append(tmp, rear) == NULL) goto end;
  
    scm_string_replace_contents(str, tmp);
    rslt = str;
  
  end:
    if (front != NULL) scm_string_destruct(front);
    if (rear != NULL) scm_string_destruct(rear);
    if (tmp != NULL) scm_string_destruct(tmp);
    return rslt;
  }
}

/* TODO: optimize */
ScmString *
scm_string_fill(ScmString *str, unsigned int pos, size_t len, scm_char_t c)
{
  int (*char_width)(const void *p, size_t size);
  int filledsize;
  size_t i;
  ScmString *rslt, *front, *rear, *tmp;

  assert(str != NULL);

  if (pos > str->length) return NULL;

  rslt = front = rear = tmp = NULL;

  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);

  filledsize = char_width(&c, sizeof(c)) * len;
  if (filledsize < 0) return 0;

  front = scm_string_substr(str, 0, pos);
  if (front == NULL) goto end;

  if (pos + len < str->length) {
    rear = scm_string_substr(str, pos + len, str->length - pos - len);
    if (rear == NULL) goto end;
  }

  tmp = scm_string_copy_and_expand(front,
                                   front->bytesize + len
                                   + ((rear == NULL)? 0 : rear->bytesize));
  if (tmp == NULL) goto end;

  for (i = 0; i < len; i++)
    if (scm_string_push(tmp, c) == NULL)
      goto end;

  if (rear != NULL)
    if (scm_string_append(tmp, rear) == NULL)
      goto end;

  scm_string_replace_contents(str, tmp);
  rslt = str;

 end:
  if (front != NULL) scm_string_destruct(front);
  if (rear != NULL) scm_string_destruct(rear);
  if (tmp != NULL) scm_string_destruct(tmp);
  return rslt;
}

int
scm_string_find_chr(const ScmString *str, scm_char_t c)
{
  int (*char_width)(const void *p, size_t size);
  ScmStrItr iter;
  int cw, pos;

  assert(str != NULL);

  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);
  cw = char_width(&c, sizeof(c));
  if (cw < 0) return -1;

  iter = scm_str_itr_begin(str->head, str->bytesize, char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  pos = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    int w = SCM_STR_ITR_WIDTH(&iter);
    if (w < 0) return -1;
    if ((cw == w) && (memcmp(&c, SCM_STR_ITR_PTR(&iter), cw) == 0))
      return pos;

    iter = scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

    pos++;
  }

  return -1;
}

int
scm_string_match(const ScmString *str, const ScmString *pat)
{
  int (*char_width)(const void *p, size_t size);
  ScmStrItr iter_str_ext, iter_pat;
  int pos;

  pos = 0;

  assert(str != NULL);
  assert(pat != NULL);

  if (str->enc != pat->enc)
    return -1;
  
  char_width = SCM_STRING_VFUNC_CHAR_WIDTH(str->enc);

  iter_str_ext = scm_str_itr_begin(str->head, str->bytesize, char_width);
  if (SCM_STR_ITR_IS_ERR(&iter_str_ext)) return -1;

  while (!SCM_STR_ITR_IS_END(&iter_str_ext)) {
    ScmStrItr iter_str_inn;

    SCM_STR_ITR_COPY(&iter_str_ext, &iter_str_inn);

    iter_pat = scm_str_itr_begin(pat->head, pat->bytesize, char_width);
    if (SCM_STR_ITR_IS_ERR(&iter_pat)) return -1;

    if (SCM_STR_ITR_REST(&iter_str_ext) < SCM_STR_ITR_REST(&iter_pat))
      return -1;

    while (!SCM_STR_ITR_IS_END(&iter_str_inn)
           && !SCM_STR_ITR_IS_END(&iter_pat)) {

      if (SCM_STR_ITR_WIDTH(&iter_str_inn) != SCM_STR_ITR_WIDTH(&iter_pat))
        break;

      if (memcmp(SCM_STR_ITR_PTR(&iter_str_inn), SCM_STR_ITR_PTR(&iter_pat),
                 SCM_STR_ITR_WIDTH(&iter_str_inn)) != 0)
        break;

      iter_str_inn = scm_str_itr_next(&iter_str_inn);
      if (SCM_STR_ITR_IS_ERR(&iter_str_inn)) return -1;

      iter_pat = scm_str_itr_next(&iter_pat);
      if (SCM_STR_ITR_IS_ERR(&iter_pat)) return -1;
    }

    if (SCM_STR_ITR_IS_END(&iter_pat))
      return pos;

    iter_str_ext = scm_str_itr_next(&iter_str_ext);
    if (SCM_STR_ITR_IS_ERR(&iter_str_ext)) return -1;;

    pos++;
  }

  return -1;
}

ssize_t
scm_string_dump(const ScmString *str, void *buf, size_t size)
{
  ssize_t len;

  assert(str != NULL);
  assert(buf != NULL);

  len = (size < str->bytesize) ? size : str->bytesize;
  memcpy(buf, str->head, len); // XXX

  return len;
}

SCM_ENCODING_T
scm_string_encoding(const ScmString *str)
{
  assert(str != NULL);

  return str->enc;
}

void *
scm_string_content(const ScmString *str)
{
  assert(str != NULL);
  return str->head;
}

bool
scm_string_is_string(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_STRING);
}
