#ifndef INCLUDE_READER_H__
#define INCLUDE_READER_H__

#include <stdbool.h>

typedef struct ScmReaderRec ScmReader;

ScmReader *scm_reader_construct(FILE *input);
int scm_reader_head_char(ScmReader *reader);
int scm_reader_forecast(ScmReader *reader, size_t look_ahead);
void scm_reader_shift_char(ScmReader *reader);
bool scm_reader_is_eof(ScmReader *reader);

#endif /* INCLUDE_READER_H__ */
