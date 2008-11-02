#ifndef INCLUDE_IBUFFER_H__
#define INCLUDE_IBUFFER_H__

#include <stdio.h>
#include <stdbool.h>

#include "port.h"

typedef struct ScmIBufferRec ScmIBuffer;

ScmIBuffer *scm_ibuffer_construct(ScmPort *port);
void scm_ibuffer_destruct(ScmIBuffer *ibuffer);
int scm_ibuffer_head_char(ScmIBuffer *ibuffer);
int scm_ibuffer_forecast(ScmIBuffer *ibuffer, size_t look_ahead);
void scm_ibuffer_shift_char(ScmIBuffer *ibuffer);
bool scm_ibuffer_is_eof(ScmIBuffer *ibuffer);
int scm_ibuffer_current_line_num(ScmIBuffer *ibuffer);
int scm_ibuffer_current_column_num(ScmIBuffer *ibuffer);

#endif /* INCLUDE_IBUFFER_H__ */
