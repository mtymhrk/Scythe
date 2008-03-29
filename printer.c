#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "printer.h"

#define INITIAL_BUFFER_SIZE 256

struct ScmPrinterRec {
  FILE *output;
  char *buffer;
  size_t size;
  size_t used;
};

static void
expand_buffer_if_needed(ScmPrinter *printer, size_t needed_size)
{
  size_t new_size;
  char *new_buffer = NULL;

  for (new_size = printer->size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > printer->size) {
    new_buffer = (char *)scm_memory_allocate(new_size);
    memcpy(new_buffer, printer->buffer, printer->used);
    scm_memory_release(printer->buffer);
    printer->buffer = new_buffer;
    printer->size = new_size;
  }
}


void
scm_printer_concatenate_string(ScmPrinter *printer, const char *str)
{
  size_t len = 0;

  assert(printer != NULL); assert(str != NULL);

  len = strlen(str);
  expand_buffer_if_needed(printer, printer->used + len);
  memcpy(printer->buffer + printer->used - 1, str, len + 1);
  printer->used += len;
}

void
scm_printer_concatenate_char(ScmPrinter *printer, char c)
{
  assert(printer != NULL);

  expand_buffer_if_needed(printer, printer->used + 1);
  printer->buffer[printer->used - 1] = c;
  printer->buffer[printer->used] = '\0';
  printer->used += 1;
}

void
scm_printer_truncate_buffer(ScmPrinter *printer, int size)
{
  assert(printer != NULL);

  if (size >= 0) {
    printer->size = size + 1;
    printer->buffer[size] = '\0';
  } 
  else {
    printer->size -= size;
    if (printer->size < 1) printer->size = 1;
    printer->buffer[size - 1] = '\0';
  }
}

void
scm_printer_clear(ScmPrinter *printer)
{
  assert(printer != NULL);

  printer->buffer[0] = '\0';
  printer->size = 1;
}

void
scm_printer_flush(ScmPrinter *printer)
{
  assert(printer != NULL);

  fputs(printer->buffer, printer->output);
}

void
scm_printer_line_feed(ScmPrinter *printer)
{
  assert(printer != NULL);
  scm_printer_concatenate_char(printer, '\n');
}

void
scm_printer_pretty_print_scm_obj(ScmPrinter *printer,
				 ScmObj obj, unsigned int mode)
{
  assert(printer != NULL); assert(obj != NULL);

  if ((mode & SCM_PRINTER_MODE_CLEAR) != 0)
    scm_printer_clear(printer);

  scm_obj_pretty_print(obj, printer);

  if ((mode & SCM_PRINTER_MODE_FLUSH) != 0)
    scm_printer_flush(printer);
}

size_t
scm_printer_length(ScmPrinter *printer)
{
  assert(printer != NULL);

  return printer->used - 1;
}

void
scm_printer_copy_buffer(ScmPrinter *printer, char *dst, size_t len)
{
  assert(printer != NULL);

  strncpy(dst, printer->buffer, len);
  dst[len - 1] = '\n';
}

char *
scm_printer_buffer(ScmPrinter *printer)
{
  assert(printer != NULL);

  return printer->buffer;
}

ScmPrinter *
scm_printer_construct(FILE *output)
{
  ScmPrinter *printer = NULL;

  assert(output != NULL);

  printer = scm_memory_allocate(sizeof(ScmPrinter));
  printer->output = output;
  printer->buffer = scm_memory_allocate(INITIAL_BUFFER_SIZE);
  printer->buffer[0] = '\0';
  printer->size = INITIAL_BUFFER_SIZE;
  printer->used = 1; // Include NULL character;

  return printer;
}
