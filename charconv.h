#ifndef INCLUDE_CHARCONV_H__
#define INCLUDE_CHARCONV_H__

#include <stdbool.h>
#include <iconv.h>

typedef struct ScmCharConvRec ScmCharConv;

ScmCharConv *scm_charconv_construct(const char *from, const char* to);
void scm_charconv_destruct(ScmCharConv *conv);
ssize_t scm_charconv_convert(ScmCharConv *conv,
                             const void *input, size_t in_size,
                             void *output, size_t out_size);
ssize_t scm_charconv_terminate(ScmCharConv *conv,
                               void *output, size_t out_size);
bool scm_charconv_ready(ScmCharConv *conv);
bool scm_charconv_has_error(ScmCharConv *conv);
int scm_charconv_errorno(ScmCharConv *conv);

#endif /* INCLUDE_CHARCONV_H__ */
