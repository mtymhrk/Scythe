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

const ScmTypeInfo SCM_EOF_TYPE_INFO = {
  SCM_OBJ_TYPE_EOF,          /* type            */
  scm_eof_pretty_print,      /* pp_func         */
  sizeof(ScmEOF),            /* obj_size        */
  NULL,                      /* gc_ini_func     */
  NULL,                      /* gc_fin_func     */
  NULL                       /* gc_ref_itr_func */
};


ScmEOF *
scm_eof_construct(void)
{
  ScmEOF *eof;

  eof = scm_memory_allocate(sizeof(ScmEOF));
  scm_obj_init(SCM_OBJ(eof), SCM_OBJ_TYPE_EOF);

  return eof;
}

void
scm_eof_destruct(ScmEOF *eof)
{
  assert(eof != NULL);
  scm_memory_release(eof);
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

void
scm_eof_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  assert(obj != NULL); assert(scm_eof_is_eof(obj));
  assert(obuffer != NULL);

  scm_obuffer_concatenate_string(obuffer, "#<eof>");
}

