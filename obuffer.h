#ifndef INCLUDE_OBUFFER_H__
#define INCLUDE_OBUFFER_H__

#include <stdio.h>

typedef struct ScmOBufferRec ScmOBuffer;

typedef enum {
  SCM_OBUFFER_MODE_CLEAR = 0x01,
  SCM_OBUFFER_MODE_FLUSH = 0x02
} SCM_OBUFFER_MODE_T;

#include "object.h"

void scm_obuffer_concatenate_string(ScmOBuffer *obuffer, const char *str);
void scm_obuffer_concatenate_char(ScmOBuffer *obuffer, int c);
void scm_obuffer_truncate_buffer(ScmOBuffer *obuffer, int size);
void scm_obuffer_clear(ScmOBuffer *obuffer);
void scm_obuffer_flush(ScmOBuffer *obuffer);
void scm_obuffer_line_feed(ScmOBuffer *obuffer);
void scm_obuffer_pretty_print_scm_obj(ScmOBuffer *obuffer,
				      ScmObj obj, unsigned int mode);
size_t scm_obuffer_length(ScmOBuffer *obuffer);
void scm_obuffer_copy_buffer(ScmOBuffer *obuffer, char *dst, size_t len);
char *scm_obuffer_buffer(ScmOBuffer *obuffer);
ScmOBuffer *scm_obuffer_construct(FILE *output);
void scm_obuffer_destruct(ScmOBuffer *obuffer);

#endif /* INCLUDE_OBUFFER_H__ */
