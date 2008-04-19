#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "reader.h"

#define BUFFER_SIZE 128

typedef int (*ReadLineFunc)(ScmReader *reader, char *buffer, size_t size);
typedef int (*ReadCharFunc)(ScmReader *reader);
typedef bool (*IsEOFFunc)(ScmReader *reader);

struct ScmReaderRec {
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
  ScmReader base;
  FILE *input;
} ScmFileReader;

typedef struct {
  ScmReader base;
  const char *string;
  size_t len;
  const char *ptr;
} ScmStringReader;


static int
read_line_from_file(ScmReader *reader, char *buffer, size_t size)
{
  ScmFileReader *file_reader = (ScmFileReader *)reader;
  char *p;

  assert(file_reader != NULL);

  p = fgets(buffer, size, file_reader->input);
  if (p == NULL)
    return 0;
  else
    return strlen(p);
}

static int
read_char_from_file(ScmReader *reader)
{
  ScmFileReader *file_reader = (ScmFileReader *)reader;
  assert(file_reader != NULL);
  return fgetc(file_reader->input);
}

static bool
is_file_eof(ScmReader *reader)
{
  ScmFileReader *file_reader = (ScmFileReader *)reader;
  assert(file_reader != NULL);
  return (feof(file_reader->input) ? true : false);
}

static int
read_line_from_string(ScmReader *reader, char *buffer, size_t size)
{
  ScmStringReader *str_reader = (ScmStringReader *)reader;
  const char *tail;
  size_t readlen;

  assert(str_reader != NULL);

  tail = strchr(str_reader->ptr, '\n');
  if (tail == NULL)
    tail = str_reader->string + str_reader->len;
  else
    tail++;

  readlen = str_reader->ptr - tail;
  if (readlen >= size)
    readlen = size - 1;

  strncpy(buffer, str_reader->ptr, readlen);
  buffer[readlen] = '\0';
  str_reader->ptr += readlen;

  return readlen;
}

static int
read_char_from_string(ScmReader *reader)
{
  ScmStringReader *str_reader = (ScmStringReader *)reader;

  assert(str_reader != NULL);

  if (str_reader->string - str_reader->ptr >= str_reader->len)
    return EOF;

  return (int)(*str_reader->ptr++);
}

static bool
is_string_eof(ScmReader *reader)
{
  ScmStringReader *str_reader = (ScmStringReader *)reader;
  assert(str_reader != NULL);
  return (str_reader->string - str_reader->ptr >= str_reader->len) ? true : false;
}

static void
scm_reader_normalize_buffer(ScmReader *reader, size_t needed_size)
{
  size_t new_size;
  unsigned char *new_buffer;

  assert(reader != NULL);

  for (new_size = reader->size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > reader->size) {
    new_buffer = (unsigned char *)(scm_memory_allocate(new_size));
    if (reader->head < reader->used)
      memcpy(new_buffer,
             reader->buffer + reader->head,
             reader->used + 1 - reader->head);
    scm_memory_release(reader->buffer);
    reader->buffer = new_buffer;
    reader->size = new_size;
  }
  else if (reader->head < reader->used) {
    memmove(reader->buffer,
            reader->buffer + reader->head,
            reader->used + 1 - reader->head);
  }

  if (reader->head < reader->used) {
    reader->used -= reader->head;
  }
  else {
    reader->used = 0;
    reader->buffer[0] = '\0';
  }
  reader->head = 0;
}

static void
scm_reader_chuck_off_char(ScmReader* reader)
{
  assert(reader != NULL);

  if (reader->head > reader->used) {
    size_t chuck_off = reader->head - reader->used;
    while (chuck_off-- > 0) reader->readchar(reader);
  }
}

static int
scm_reader_access(ScmReader *reader, size_t index)
{
  size_t offset;

  assert(reader != NULL);

  if (reader->head + index >= reader->used) {
    if (reader->is_eof(reader)) return EOF;

    scm_reader_chuck_off_char(reader);
    scm_reader_normalize_buffer(reader, index + 1);
    reader->readline(reader, (char *)reader->buffer + reader->used,
                     reader->size - reader->used);
    reader->used = strlen((char *)reader->buffer);
  }

  offset = reader->head + index;
  if (offset >= reader->used)
    return EOF;
  else
    return reader->buffer[offset];
}

static void
scm_initialize_base(ScmReader *reader,
                    ReadLineFunc readline,
                    ReadCharFunc readchar,
                    IsEOFFunc is_eof)
{
  assert(reader != NULL);

  reader->buffer = scm_memory_allocate(BUFFER_SIZE);
  reader->size = BUFFER_SIZE;
  reader->buffer[0] = '\0';
  reader->head = 0;
  reader->used = 0;
  reader->readline = readline;
  reader->readchar = readchar;
  reader->is_eof = is_eof;
}

ScmReader *
scm_reader_construct(FILE *input)
{
  ScmFileReader *reader;

  assert(input != NULL);

  reader = scm_memory_allocate(sizeof(ScmFileReader));
  scm_initialize_base(&reader->base,
                      read_line_from_file,
                      read_char_from_file,
                      is_file_eof);
  reader->input = input;
  return (ScmReader *)reader;
}

ScmReader *
scm_reader_construct_from_string(const char *string)
{
  ScmStringReader *reader;

  assert(string != NULL);

  reader = scm_memory_allocate(sizeof(ScmStringReader));
  scm_initialize_base(&reader->base,
                      read_line_from_string,
                      read_char_from_string,
                      is_string_eof);
  reader->string = reader->ptr = string;
  reader->len = strlen(string);

  return (ScmReader *)reader;
}

int
scm_reader_head_char(ScmReader *reader)
{
  assert(reader != NULL);
  return scm_reader_access(reader, 0);
}

int
scm_reader_forecast(ScmReader *reader, size_t look_ahead)
{
  assert(reader != NULL);
  return scm_reader_access(reader, look_ahead);
}

void
scm_reader_shift_char(ScmReader *reader)
{
  assert(reader != NULL);
  reader->head++;
}

bool
scm_reader_is_eof(ScmReader *reader)
{
  assert(reader != NULL);
  return (scm_reader_access(reader, 0) == EOF) ? true : false;
}
