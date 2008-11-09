#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "char.h"

struct ScmCharRec {
  ScmObjHeader header;
  unsigned int value;
};

static void
scm_char_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  char str[32];
  ScmChar *charv;

  assert(obj != NULL); assert(scm_char_is_char(obj));
  assert(obuffer != NULL);

  charv = SCM_CHAR(obj);

  if (charv->value == '\n')
    strncpy(str, "#\\newline", sizeof(str));
  else if (charv->value == ' ')
    strncpy(str, "#\\space", sizeof(str));
  else if (isprint(charv->value))
    snprintf(str, sizeof(str), "#\\%c", charv->value);
  else if (charv->value <= 0xff)
    snprintf(str, sizeof(str), "#\\%#02x", charv->value);
  else if (charv->value <= 0xffff)
    snprintf(str, sizeof(str), "#\\%#04x", charv->value);
  else
    snprintf(str, sizeof(str), "#\\%#08x", charv->value);

  scm_obuffer_concatenate_string(obuffer, str);
}

ScmChar *
scm_char_construct(unsigned int value)
{
  ScmChar *charv;

  charv = scm_memory_allocate(sizeof(ScmChar));
  scm_obj_init(SCM_OBJ(charv), SCM_OBJ_TYPE_CHAR, scm_char_pretty_print);
  charv->value = value;

  return charv;
}

unsigned int
scm_char_value(ScmChar *charv)
{
  assert(charv != NULL);
  return charv->value;
}

bool
scm_char_is_char(ScmObj obj)
{
  assert(obj != NULL);
  return (scm_obj_type(obj) == SCM_OBJ_TYPE_CHAR);
}
