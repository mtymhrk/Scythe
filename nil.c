#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "nil.h"

#define SCM_NIL(obj) ((ScmNil *)(obj))

struct ScmNilRec {
  ScmObjHeader header;
};

static ScmNil *nil_instance = NULL;

static void
scm_nil_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  scm_obuffer_concatenate_string(obuffer, "()");
}

ScmNil *
scm_nil_construct(void)
{
  ScmNil *nil = scm_memory_allocate(sizeof(ScmNil));
  scm_obj_init(SCM_OBJ(nil), SCM_OBJ_TYPE_NIL, scm_nil_pretty_print);
  return nil;
}

ScmNil *
scm_nil_instance(void)
{
  if (nil_instance == NULL)
    nil_instance = scm_nil_construct();

  return nil_instance;
}

bool
scm_nil_is_nil(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_NIL);
}

