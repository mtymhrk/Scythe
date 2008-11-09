#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "obuffer.h"
#include "memory.h"
#include "miscobjects.h"

struct ScmEOFRec {
  ScmObjHeader header;
};

static ScmEOF *eof_instance = NULL;

static void
scm_eof_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  assert(obj != NULL); assert(scm_eof_is_eof(obj));
  assert(obuffer != NULL);

  scm_obuffer_concatenate_string(obuffer, "#<eof>");
}

ScmEOF *
scm_eof_construct(void)
{
  ScmEOF *eof;

  eof = scm_memory_allocate(sizeof(ScmEOF));
  scm_obj_init(SCM_OBJ(eof), SCM_OBJ_TYPE_EOF, scm_eof_pretty_print);

  return eof;
}

ScmEOF *
scm_eof_instance(void)
{
  if (eof_instance == NULL)
    eof_instance = scm_eof_construct();

  return eof_instance;
}

bool
scm_eof_is_eof(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_EOF);
}
