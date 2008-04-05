#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "reader.h"

#define BUFFER_SIZE 128

struct ScmReaderRec {
  FILE *input;
  unsigned char *buffer;
  size_t size;
  size_t head;
  size_t used;
  int line;
  int column;
};


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
    while (chuck_off-- > 0) fgetc(reader->input);
  }
}

static int
scm_reader_access(ScmReader *reader, size_t index)
{
  size_t offset;

  assert(reader != NULL);

  if (reader->head + index >= reader->used) {
    if (feof(reader->input)) return EOF;

    scm_reader_chuck_off_char(reader);
    scm_reader_normalize_buffer(reader, index + 1);
    fgets((char *)reader->buffer + reader->used,
          reader->size - reader->used, reader->input);
    reader->used = strlen((char *)reader->buffer);
  }

  offset = reader->head + index;
  if (offset >= reader->used)
    return EOF;
  else
    return reader->buffer[offset];
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
  reader->used = 0;
  return reader;
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
