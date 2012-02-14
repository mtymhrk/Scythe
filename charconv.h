#ifndef INCLUDE_CHARCONV_H__
#define INCLUDE_CHARCONV_H__

#include <stdbool.h>
#include <iconv.h>
#include <errno.h>

typedef struct ScmCharConvRec ScmCharConv;

typedef enum {
  SCM_CHARCONV_OMIT,
  //  SCM_CHARCONV_DONOT_CONV,
  SCM_CHARCONV_ERROR
} SCM_CHARCONV_TYPE_T;

ScmCharConv * scm_charconv_new(const char *from, const char* to,
                                     SCM_CHARCONV_TYPE_T type);
void scm_charconv_end(ScmCharConv *conv);
const char *scm_charconv_src_encoding(ScmCharConv *conv);
const char *scm_charconv_dst_encoding(ScmCharConv *conv);
ssize_t scm_charconv_convert(ScmCharConv *conv,
                             const void *input, size_t in_size,
                             void *output, size_t out_size);
ssize_t scm_charconv_put(ScmCharConv *conv, const void *input, size_t size);
ssize_t scm_charconv_get(ScmCharConv *conv, void *output, size_t size);
ssize_t scm_charconv_terminate(ScmCharConv *conv,
                               void *output, size_t out_size);
bool scm_charconv_is_ready(ScmCharConv *conv);
bool scm_charconv_has_error(ScmCharConv *conv);
int scm_charconv_errorno(ScmCharConv *conv);
void scm_charconv_clear(ScmCharConv *conv);

#endif /* INCLUDE_CHARCONV_H__ */
