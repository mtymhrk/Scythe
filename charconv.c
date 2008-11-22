#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <iconv.h>
#include <assert.h>

#include "memory.h"
#include "charconv.h"

#define INIT_BUF_SIZE 64

struct ScmCharConvRec {
  iconv_t icd;
  SCM_CHARCONV_TYPE_T type;
  char *src_encode;
  char *dst_encode;
  char *converted;
  char *unconverted;
  size_t cnv_capacity;
  size_t uncnv_capacity;
  size_t cnv_len;
  size_t uncnv_len;
  int error;
};


ScmCharConv *
scm_charconv_construct(const char *from, const char* to,
                       SCM_CHARCONV_TYPE_T type)
{
  ScmCharConv *conv;
  
  assert(from != NULL);
  assert(to != NULL);

  conv = scm_memory_allocate(sizeof(ScmCharConv));
  conv->icd = iconv_open(to, from);
  if (conv->icd == (void *)-1) {
    scm_memory_release(conv);
    return NULL;
  }

  conv->type = type;
  conv->src_encode = scm_memory_allocate(strlen(from) + 1);
  strncpy(conv->src_encode, from, strlen(from) + 1);
  conv->dst_encode = scm_memory_allocate(strlen(to) + 1);
  strncpy(conv->dst_encode, to, strlen(to) + 1);
  conv->converted = scm_memory_allocate(INIT_BUF_SIZE);
  conv->unconverted = scm_memory_allocate(INIT_BUF_SIZE);
  conv->cnv_capacity = INIT_BUF_SIZE;
  conv->uncnv_capacity = INIT_BUF_SIZE;
  conv->cnv_len = 0;
  conv->uncnv_len = 0;
  conv->error = 0;

  return conv;
}

void
scm_charconv_destruct(ScmCharConv *conv)
{
  assert(conv != NULL);

  iconv_close(conv->icd);

  scm_memory_release(conv->src_encode);
  scm_memory_release(conv->dst_encode);
  scm_memory_release(conv->converted);
  scm_memory_release(conv->unconverted);
  scm_memory_release(conv);
}

const char *
scm_charconv_src_encoding(ScmCharConv *conv)
{
  assert(conv != NULL);
  return conv->src_encode;
}

const char *
scm_charconv_dst_encoding(ScmCharConv *conv)
{
  assert(conv != NULL);
  return conv->dst_encode;  
}


static size_t
scm_charconv_put_in_unconverted(ScmCharConv *conv,
                                const void *input, size_t size)
{
  size_t len;
  size_t space;

  assert(conv != NULL);
  assert(input != NULL);

  space = conv->uncnv_capacity - conv->uncnv_len;
  len = (size < space) ? size : space;
  memcpy(conv->unconverted + conv->uncnv_len, input, len);
  conv->uncnv_len += len;

  return len;
}

static size_t
scm_charconv_shift_unconverted(ScmCharConv *conv, size_t nshift)
{
  assert(conv != NULL);

  memmove(conv->unconverted, conv->unconverted + nshift,
          conv->uncnv_len - nshift);
  conv->uncnv_len -= nshift;

  return nshift;
}

static size_t
scm_charconv_put_out_converted(ScmCharConv *conv, void *out, size_t size)
{
  size_t len;

  assert(conv != NULL);

  if (out == NULL) return 0;

  len = (size < conv->cnv_len) ? size : conv->cnv_len;
  memcpy(out, conv->converted, len);
  memmove(conv->converted, conv->converted + len, conv->cnv_len - len);
  conv->cnv_len -= len;

  return len;
}

static int
scm_charconv_convert_aux(ScmCharConv *conv, bool terminate)
{
  size_t ret;
  char *inbuf, *outbuf;
  size_t inbytesleft, outbytesleft;
  int result;

  assert(conv != NULL);

  inbuf = terminate ? NULL : conv->unconverted;
  outbuf = conv->converted + conv->cnv_len;
  inbytesleft = conv->uncnv_len;
  outbytesleft = conv->cnv_capacity - conv->cnv_len;

  ret = iconv(conv->icd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
  result = (ret == (size_t)-1) ? errno : 0;

  conv->cnv_len = conv->cnv_capacity - outbytesleft;

  if (!terminate)
    scm_charconv_shift_unconverted(conv, conv->uncnv_len - inbytesleft);

  return result;
}

static void
scm_charconv_expand_converted(ScmCharConv *conv)
{
  size_t new_capacity;
  void *new_buf;

  assert(conv != NULL);

  new_capacity = conv->cnv_capacity * 2;
  new_buf = scm_memory_allocate(new_capacity);
  memcpy(new_buf, conv->converted, conv->cnv_len);
  scm_memory_release(conv->converted);
  conv->converted = new_buf;
  conv->cnv_capacity = new_capacity;
}

ssize_t
scm_charconv_convert(ScmCharConv *conv,
                     const void *input, size_t in_size,
                     void *output, size_t out_size)
{
  const bool DONT_TERMINATE = false;
  const void *inp;
  void *outp;
  size_t rest, room;
  bool retry;

  assert(conv != NULL);

  if (input == NULL || in_size == 0)
    return scm_charconv_put_out_converted(conv, output, out_size);

  inp = input; outp = output;
  rest = in_size; room = out_size;
  retry = false;

  do {
    size_t ret;
    int error_no;

    ret = scm_charconv_put_in_unconverted(conv, inp, rest);
    rest -= ret;
    inp += ret;

    error_no = scm_charconv_convert_aux(conv, DONT_TERMINATE);

    if (error_no == EILSEQ) {
      switch (conv->type) {
      case SCM_CHARCONV_OMIT:
        retry = true;
        scm_charconv_shift_unconverted(conv, 1);
        break;
      case SCM_CHARCONV_ERROR:
        retry = false;
        conv->error = error_no;
        break;
      }
    }
    else if (conv->uncnv_len > 0 && error_no != EINVAL) {
      /* absence of converted buffer size */
      retry = true;
      if (room == 0)
        scm_charconv_expand_converted(conv);
    }
    else if (rest > 0)
      retry = true;
    else
      retry = false;

    ret = scm_charconv_put_out_converted(conv, outp, room);
    room -= ret;
    outp += ret;

  } while (retry);

  return out_size - room;
}

void
scm_charconv_put(ScmCharConv *conv, const void *input, size_t size)
{
  assert(conv != NULL);

  scm_charconv_convert(conv, input, size, NULL, 0);
}

ssize_t
scm_charconv_get(ScmCharConv *conv, void *output, size_t size)
{
  assert(conv != NULL);

  return scm_charconv_convert(conv, NULL, 0, output, size);
}

ssize_t
scm_charconv_terminate(ScmCharConv *conv, void *output, size_t out_size)
{
  const bool TERMINATE = true;
  int error_no;

  assert(conv != NULL);
  assert(output != NULL);

  conv->uncnv_len = 0;
  conv->error = 0;
  while (1) {
    error_no = scm_charconv_convert_aux(conv, TERMINATE);
    if (error_no != E2BIG) break;
    scm_charconv_expand_converted(conv);
  }

  return scm_charconv_put_out_converted(conv, output, out_size);
}

bool
scm_charconv_ready(ScmCharConv *conv)
{
  assert(conv != NULL);

  return (conv->cnv_len > 0);
}

bool
scm_charconv_has_error(ScmCharConv *conv)
{
  assert(conv != NULL);

  return (conv->error != 0);
}

int
scm_charconv_errorno(ScmCharConv *conv)
{
  assert(conv != NULL);

  return conv->error;
}
