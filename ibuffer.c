#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "ibuffer.h"

#define BUFFER_SIZE 128

#define IS_NEWLINE(c) ((c) == '\n')

typedef int (*ReadLineFunc)(ScmIBuffer *ibuffer, char *buffer, size_t size);
typedef int (*ReadCharFunc)(ScmIBuffer *ibuffer);
typedef bool (*IsEOFFunc)(ScmIBuffer *ibuffer);

struct ScmIBufferRec {
  unsigned char *buffer;
  size_t size;
  size_t head;
  size_t used;
  int line;
  int column;
  ReadLineFunc readline;
  ReadCharFunc readchar;
  IsEOFFunc    is_eof;
};

typedef struct {
  ScmIBuffer base;
  FILE *input;
} ScmFileIBuffer;

typedef struct {
  ScmIBuffer base;
  const char *string;
  size_t len;
  const char *ptr;
} ScmStringIBuffer;



static int
read_line_from_file(ScmIBuffer *ibuffer, char *buffer, size_t size)
{
  ScmFileIBuffer *file_ibuffer = (ScmFileIBuffer *)ibuffer;
  char *p;

  assert(file_ibuffer != NULL);

  p = fgets(buffer, size, file_ibuffer->input);
  if (p == NULL)
    return 0;
  else
    return strlen(p);
}

static int
read_char_from_file(ScmIBuffer *ibuffer)
{
  ScmFileIBuffer *file_ibuffer = (ScmFileIBuffer *)ibuffer;
  assert(file_ibuffer != NULL);
  return fgetc(file_ibuffer->input);
}

static bool
is_file_eof(ScmIBuffer *ibuffer)
{
  ScmFileIBuffer *file_ibuffer = (ScmFileIBuffer *)ibuffer;
  assert(file_ibuffer != NULL);
  return (feof(file_ibuffer->input) ? true : false);
}

static int
read_line_from_string(ScmIBuffer *ibuffer, char *buffer, size_t size)
{
  ScmStringIBuffer *str_ibuffer = (ScmStringIBuffer *)ibuffer;
  size_t i;

  assert(str_ibuffer != NULL);

  for (i = 0; i < size - 1; i++) {
    if (str_ibuffer->ptr[i] != '\n') break;
    if (str_ibuffer->ptr[i] != '\0') break;

     buffer[i] = str_ibuffer->ptr[i];
  }

  if (i < size - 1) {
    buffer[i] = str_ibuffer->ptr[i];
    i++;
  }

  buffer[i] = '\0';
  str_ibuffer->ptr += i;

  return i;
}

static int
read_char_from_string(ScmIBuffer *ibuffer)
{
  ScmStringIBuffer *str_ibuffer = (ScmStringIBuffer *)ibuffer;

  assert(str_ibuffer != NULL);

  if (str_ibuffer->ptr - str_ibuffer->string >= str_ibuffer->len)
    return EOF;

  return (int)(*str_ibuffer->ptr++);
}

static bool
is_string_eof(ScmIBuffer *ibuffer)
{
  ScmStringIBuffer *str_ibuffer = (ScmStringIBuffer *)ibuffer;
  assert(str_ibuffer != NULL);
  return (str_ibuffer->ptr - str_ibuffer->string >= str_ibuffer->len) ? true : false;
}

static void
scm_ibuffer_normalize_buffer(ScmIBuffer *ibuffer, size_t needed_size)
{
  size_t new_size;
  unsigned char *new_buffer;

  assert(ibuffer != NULL);

  for (new_size = ibuffer->size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > ibuffer->size) {
    new_buffer = (unsigned char *)(scm_memory_allocate(new_size));
    if (ibuffer->head < ibuffer->used)
      memcpy(new_buffer,
             ibuffer->buffer + ibuffer->head,
             ibuffer->used + 1 - ibuffer->head);
    scm_memory_release(ibuffer->buffer);
    ibuffer->buffer = new_buffer;
    ibuffer->size = new_size;
  }
  else if (ibuffer->head < ibuffer->used) {
    memmove(ibuffer->buffer,
            ibuffer->buffer + ibuffer->head,
            ibuffer->used + 1 - ibuffer->head);
  }

  if (ibuffer->head < ibuffer->used) {
    ibuffer->used -= ibuffer->head;
  }
  else {
    ibuffer->used = 0;
    ibuffer->buffer[0] = '\0';
  }
  ibuffer->head = 0;
}

static void
scm_ibuffer_chuck_off_char(ScmIBuffer* ibuffer)
{
  assert(ibuffer != NULL);

  if (ibuffer->head > ibuffer->used) {
    size_t chuck_off = ibuffer->head - ibuffer->used;
    while (chuck_off-- > 0) ibuffer->readchar(ibuffer);
  }
}

static int
scm_ibuffer_access(ScmIBuffer *ibuffer, size_t index)
{
  assert(ibuffer != NULL);

  while (ibuffer->head + index >= ibuffer->used) {
    int len;

    if (ibuffer->is_eof(ibuffer)) return EOF;

    scm_ibuffer_chuck_off_char(ibuffer); /* This call is unnecessary */
    scm_ibuffer_normalize_buffer(ibuffer, index + 1);
    len = ibuffer->readline(ibuffer, (char *)ibuffer->buffer + ibuffer->used,
                      ibuffer->size - ibuffer->used);
    ibuffer->used += len;
  }

  return ibuffer->buffer[ibuffer->head + index];
}

static void
scm_initialize_base(ScmIBuffer *ibuffer,
                    ReadLineFunc readline,
                    ReadCharFunc readchar,
                    IsEOFFunc is_eof)
{
  assert(ibuffer != NULL);

  ibuffer->buffer = scm_memory_allocate(BUFFER_SIZE);
  ibuffer->size = BUFFER_SIZE;
  ibuffer->buffer[0] = '\0';
  ibuffer->head = 0;
  ibuffer->used = 0;
  ibuffer->line = 1;
  ibuffer->column = 1;
  ibuffer->readline = readline;
  ibuffer->readchar = readchar;
  ibuffer->is_eof = is_eof;
}

ScmIBuffer *
scm_ibuffer_construct(FILE *input)
{
  ScmFileIBuffer *ibuffer;

  assert(input != NULL);

  ibuffer = scm_memory_allocate(sizeof(ScmFileIBuffer));
  scm_initialize_base(&ibuffer->base,
                      read_line_from_file,
                      read_char_from_file,
                      is_file_eof);
  ibuffer->input = input;
  return (ScmIBuffer *)ibuffer;
}

void
scm_ibuffer_destruct(ScmIBuffer *ibuffer)
{
  assert(ibuffer != ibuffer);

  scm_memory_release(ibuffer->buffer);
  scm_memory_release(ibuffer);
}

ScmIBuffer *
scm_ibuffer_construct_from_string(const char *string)
{
  ScmStringIBuffer *ibuffer;

  assert(string != NULL);

  ibuffer = scm_memory_allocate(sizeof(ScmStringIBuffer));
  scm_initialize_base(&ibuffer->base,
                      read_line_from_string,
                      read_char_from_string,
                      is_string_eof);
  ibuffer->string = ibuffer->ptr = string;
  ibuffer->len = strlen(string);

  return (ScmIBuffer *)ibuffer;
}

int
scm_ibuffer_head_char(ScmIBuffer *ibuffer)
{
  assert(ibuffer != NULL);
  return scm_ibuffer_access(ibuffer, 0);
}

int
scm_ibuffer_forecast(ScmIBuffer *ibuffer, size_t look_ahead)
{
  assert(ibuffer != NULL);
  return scm_ibuffer_access(ibuffer, look_ahead);
}

void
scm_ibuffer_shift_char(ScmIBuffer *ibuffer)
{
  int c;

  assert(ibuffer != NULL);

  c = scm_ibuffer_head_char(ibuffer);
  if (IS_NEWLINE(c)) {
    ibuffer->line++;
    ibuffer->column = 1;
  }
  else if (c != EOF) {
    ibuffer->column++;
  }

  ibuffer->head++;
}

bool
scm_ibuffer_is_eof(ScmIBuffer *ibuffer)
{
  assert(ibuffer != NULL);
  return (scm_ibuffer_access(ibuffer, 0) == EOF) ? true : false;
}

int
scm_ibuffer_current_line_num(ScmIBuffer *ibuffer)
{
  assert(ibuffer != NULL);
  return ibuffer->line;
}

int
scm_ibuffer_current_column_num(ScmIBuffer *ibuffer)
{
  assert(ibuffer != NULL);
  return ibuffer->column;
}
