#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"

#define INITIAL_BUFFER_SIZE 256

struct ScmOBufferRec {
  FILE *output;
  char *buffer;
  size_t size;
  size_t used;
};

static void
expand_buffer_if_needed(ScmOBuffer *obuffer, size_t needed_size)
{
  size_t new_size;
  char *new_buffer = NULL;

  for (new_size = obuffer->size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > obuffer->size) {
    new_buffer = (char *)scm_memory_allocate(new_size);
    memcpy(new_buffer, obuffer->buffer, obuffer->used);
    scm_memory_release(obuffer->buffer);
    obuffer->buffer = new_buffer;
    obuffer->size = new_size;
  }
}


void
scm_obuffer_concatenate_string(ScmOBuffer *obuffer, const char *str)
{
  size_t len = 0;

  assert(obuffer != NULL); assert(str != NULL);

  len = strlen(str);
  expand_buffer_if_needed(obuffer, obuffer->used + len);
  memcpy(obuffer->buffer + obuffer->used - 1, str, len + 1);
  obuffer->used += len;
}

void
scm_obuffer_concatenate_char(ScmOBuffer *obuffer, char c)
{
  assert(obuffer != NULL);

  expand_buffer_if_needed(obuffer, obuffer->used + 1);
  obuffer->buffer[obuffer->used - 1] = c;
  obuffer->buffer[obuffer->used] = '\0';
  obuffer->used += 1;
}

void
scm_obuffer_truncate_buffer(ScmOBuffer *obuffer, int size)
{
  assert(obuffer != NULL);

  if (size >= 0) {
    obuffer->size = size + 1;
    obuffer->buffer[size] = '\0';
  } 
  else {
    obuffer->size -= size;
    if (obuffer->size < 1) obuffer->size = 1;
    obuffer->buffer[size - 1] = '\0';
  }
}

void
scm_obuffer_clear(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);

  obuffer->buffer[0] = '\0';
  obuffer->used = 1;
}

void
scm_obuffer_flush(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);

  fputs(obuffer->buffer, obuffer->output);
}

void
scm_obuffer_line_feed(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);
  scm_obuffer_concatenate_char(obuffer, '\n');
}

void
scm_obuffer_pretty_print_scm_obj(ScmOBuffer *obuffer,
				 ScmObj obj, unsigned int mode)
{
  assert(obuffer != NULL); assert(obj != NULL);

  if ((mode & SCM_OBUFFER_MODE_CLEAR) != 0)
    scm_obuffer_clear(obuffer);

  scm_obj_pretty_print(obj, obuffer);

  if ((mode & SCM_OBUFFER_MODE_FLUSH) != 0)
    scm_obuffer_flush(obuffer);
}

size_t
scm_obuffer_length(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);

  return obuffer->used - 1;
}

void
scm_obuffer_copy_buffer(ScmOBuffer *obuffer, char *dst, size_t len)
{
  assert(obuffer != NULL);

  strncpy(dst, obuffer->buffer, len);
  dst[len - 1] = '\n';
}

char *
scm_obuffer_buffer(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);

  return obuffer->buffer;
}

ScmOBuffer *
scm_obuffer_construct(FILE *output)
{
  ScmOBuffer *obuffer = NULL;

  assert(output != NULL);

  obuffer = scm_memory_allocate(sizeof(ScmOBuffer));
  obuffer->output = output;
  obuffer->buffer = scm_memory_allocate(INITIAL_BUFFER_SIZE);
  obuffer->buffer[0] = '\0';
  obuffer->size = INITIAL_BUFFER_SIZE;
  obuffer->used = 1; // Include NULL character;

  return obuffer;
}

void
scm_obuffer_destruct(ScmOBuffer *obuffer)
{
  assert(obuffer != NULL);

  scm_memory_release(obuffer->buffer);
  scm_memory_release(obuffer);
}
