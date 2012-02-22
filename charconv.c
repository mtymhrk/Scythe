#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <iconv.h>
#include <limits.h>
#include <assert.h>

#include "api.h"
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
scm_charconv_new(const char *from, const char* to,
                       SCM_CHARCONV_TYPE_T type)
{
  ScmCharConv *conv;

  assert(from != NULL);
  assert(to != NULL);

  conv = scm_capi_malloc(sizeof(ScmCharConv));
  if (conv == NULL) return NULL;

  conv->icd = (void *)-1;
  conv->src_encode  = NULL;
  conv->dst_encode  = NULL;
  conv->converted   = NULL;
  conv->unconverted = NULL;

  conv->icd = iconv_open(to, from);
  if (conv->icd == (void *)-1) goto err;

  conv->type = type;
  conv->src_encode = scm_capi_malloc(strlen(from) + 1);
  if (conv->src_encode == NULL) goto err;
  strncpy(conv->src_encode, from, strlen(from) + 1);

  conv->dst_encode = scm_capi_malloc(strlen(to) + 1);
  if (conv->dst_encode == NULL) goto err;
  strncpy(conv->dst_encode, to, strlen(to) + 1);

  conv->converted = scm_capi_malloc(INIT_BUF_SIZE);
  if (conv->converted == NULL) goto err;

  conv->unconverted = scm_capi_malloc(INIT_BUF_SIZE);
  if (conv->unconverted == NULL) goto err;

  conv->cnv_capacity = INIT_BUF_SIZE;
  conv->uncnv_capacity = INIT_BUF_SIZE;
  conv->cnv_len = 0;
  conv->uncnv_len = 0;
  conv->error = 0;

  return conv;

 err:
  scm_capi_free(conv->unconverted);
  scm_capi_free(conv->converted);
  scm_capi_free(conv->dst_encode);
  scm_capi_free(conv->src_encode);
  if (conv->icd != (void *)-1) iconv_close(conv->icd);
  scm_capi_free(conv);

  return NULL;
}

void
scm_charconv_end(ScmCharConv *conv)
{
  scm_assert(conv != NULL);

  iconv_close(conv->icd);

  scm_capi_free(conv->src_encode);
  scm_capi_free(conv->dst_encode);
  scm_capi_free(conv->converted);
  scm_capi_free(conv->unconverted);
  scm_capi_free(conv);
}

const char *
scm_charconv_src_encoding(ScmCharConv *conv)
{
  scm_assert(conv != NULL);
  return conv->src_encode;
}

const char *
scm_charconv_dst_encoding(ScmCharConv *conv)
{
  scm_assert(conv != NULL);
  return conv->dst_encode;
}


static size_t
scm_charconv_put_in_unconverted(ScmCharConv *conv,
                                const void *input, size_t size)
{
  size_t len;
  size_t space;

  scm_assert(conv != NULL);
  scm_assert(input != NULL);

  space = conv->uncnv_capacity - conv->uncnv_len;
  len = (size < space) ? size : space;
  memcpy(conv->unconverted + conv->uncnv_len, input, len);
  conv->uncnv_len += len;

  return len;
}

static size_t
scm_charconv_shift_unconverted(ScmCharConv *conv, size_t nshift)
{
  scm_assert(conv != NULL);

  memmove(conv->unconverted, conv->unconverted + nshift,
          conv->uncnv_len - nshift);
  conv->uncnv_len -= nshift;

  return nshift;
}

static size_t
scm_charconv_put_out_converted(ScmCharConv *conv, void *out, size_t size)
{
  size_t len;

  scm_assert(conv != NULL);

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

  scm_assert(conv != NULL);

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

static int
scm_charconv_expand_converted(ScmCharConv *conv)
{
  size_t new_capacity;
  void *new_buf;

  assert(conv != NULL);

  new_capacity = conv->cnv_capacity * 2;
  new_buf = scm_capi_realloc(conv->converted, new_capacity);
  if (new_buf == NULL) return -1;

  conv->converted = new_buf;
  conv->cnv_capacity = new_capacity;

  return 0;
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

  scm_assert(conv != NULL);
  scm_assert(in_size <= SSIZE_MAX);
  scm_assert(out_size <= SSIZE_MAX);

  if (input == NULL || in_size == 0)
    return (ssize_t)scm_charconv_put_out_converted(conv, output, out_size);

  inp = input; outp = output;
  rest = in_size; room = out_size;
  retry = false;

  do {
    size_t ret;
    int error_no;

    ret = scm_charconv_put_in_unconverted(conv, inp, rest);
    rest -= ret;
    inp = (const uint8_t *)inp + ret;

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
        if (scm_charconv_expand_converted(conv) < 0)
          return -1;
    }
    else if (rest > 0)
      retry = true;
    else
      retry = false;

    ret = scm_charconv_put_out_converted(conv, outp, room);
    room -= ret;
    outp = (uint8_t *)outp + ret;

  } while (retry);

  return (ssize_t)(out_size - room);
}

ssize_t
scm_charconv_put(ScmCharConv *conv, const void *input, size_t size)
{
  ssize_t rslt;

  scm_assert(conv != NULL);
  scm_assert(size <= SSIZE_MAX);

  rslt = scm_charconv_convert(conv, input, size, NULL, 0);
  if (rslt < 0) return -1;

  return (ssize_t)size;
}

ssize_t
scm_charconv_get(ScmCharConv *conv, void *output, size_t size)
{
  assert(conv != NULL);
  assert(size <= SSIZE_MAX);

  return scm_charconv_convert(conv, NULL, 0, output, size);
}

ssize_t
scm_charconv_terminate(ScmCharConv *conv, void *output, size_t out_size)
{
  const bool TERMINATE = true;
  int error_no;

  assert(conv != NULL);
  assert(output != NULL);
  assert(out_size <= SSIZE_MAX);

  conv->uncnv_len = 0;
  conv->error = 0;
  while (1) {
    error_no = scm_charconv_convert_aux(conv, TERMINATE);
    if (error_no != E2BIG) break;
    scm_charconv_expand_converted(conv);
  }

  return (ssize_t)scm_charconv_put_out_converted(conv, output, out_size);
}

bool
scm_charconv_is_ready(ScmCharConv *conv)
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

void
scm_charconv_clear(ScmCharConv *conv)
{
  assert(conv != NULL);

  iconv(conv->icd, NULL, NULL, NULL, NULL);
  conv->cnv_len = 0;
  conv->uncnv_len = 0;
  conv->error = 0;
}
