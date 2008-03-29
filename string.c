#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "printer.h"
#include "string.h"


struct ScmStringRec {
  ScmObjHeader header;
  char *string;
  size_t length;
};

static bool
scm_string_is_char_to_be_escaped(char c)
{
  return (strchr("\\\"", c) != NULL);
}

static void
scm_string_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  ScmString *string = NULL;
  char *p;

  assert(obj != NULL); assert(scm_string_is_string(obj));
  assert(printer != NULL);

  string = SCM_STRING(obj);

  scm_printer_concatenate_char(printer, '"');
  for (p = string->string; *p != '\0'; p++) {
    if (scm_string_is_char_to_be_escaped(*p))
      scm_printer_concatenate_char(printer, '\\');
    scm_printer_concatenate_char(printer, *p);
  }
  scm_printer_concatenate_char(printer, '"');
}

ScmString *
scm_string_construct(const char *str)
{
  ScmString *string = NULL;

  assert(str != NULL);

  string = (ScmString *)scm_memory_allocate(sizeof(ScmString));
  scm_obj_init(SCM_OBJ(string), SCM_OBJ_TYPE_STRING, scm_string_pretty_print);
  string->length = strlen(str);
  string->string = (char *)scm_memory_allocate(string->length + 1);
  strncpy(string->string, str, string->length + 1);

  return string;
}

char *
scm_string_string(const ScmString *string)
{
  assert(string != NULL);

  return string->string;
}

size_t
scm_string_length(const ScmString *string)
{
  assert(string != NULL);

  return string->length;
}

bool
scm_string_is_string(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_STRING);
}
