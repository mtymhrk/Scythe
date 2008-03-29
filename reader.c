#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "reader.h"

#define BUFFER_SIZE 256

struct ScmReaderRec {
  FILE *input;
  unsigned char *buffer;
  size_t size;
  size_t head;
  int line;
  int column;
};

static bool
scm_reader_buffer_is_empty(ScmReader *reader)
{
  assert(reader != NULL);
  return (reader->buffer[reader->head] == '\0') ? true : false;
}

ScmReader *
scm_reader_construct(FILE *input)
{
  ScmReader *reader;

  assert(input != NULL);

  reader = scm_memory_allocate(sizeof(ScmReader));
  reader->input = input;
  reader->buffer = scm_memory_allocate(BUFFER_SIZE);
  reader->size = BUFFER_SIZE;
  reader->buffer[0] = '\0';
  reader->head = 0;

  return reader;
}

int
scm_reader_head_char(ScmReader *reader)
{
  assert(reader != NULL);

  if (scm_reader_buffer_is_empty(reader)) {
    if (fgets((char *)reader->buffer, reader->size, reader->input) == NULL)
      return EOF;
    reader->head = 0;
  }

  return reader->buffer[reader->head];
}

void
scm_reader_pop_char(ScmReader *reader)
{
  assert(reader != NULL);

  if ( ! scm_reader_buffer_is_empty(reader))
    reader->head++;
}

bool
scm_reader_is_eof(ScmReader *reader)
{
  assert(reader != NULL);

  if (scm_reader_buffer_is_empty(reader))
    return feof(reader->input) ? true : false;

  return false;
}
