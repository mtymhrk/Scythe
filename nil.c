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

ScmTypeInfo SCM_NIL_TYPE_INFO = {
  scm_nil_pretty_print,      /* pp_func              */
  sizeof(ScmNil),            /* obj_size             */
  NULL,                      /* gc_ini_func          */
  NULL,                      /* gc_fin_func          */
  NULL,                      /* gc_accept_func       */
  NULL,                      /* gc_accpet_func_weak  */
};


static ScmNil *nil_instance = NULL;

ScmNil *
scm_nil_construct(void)
{
  ScmNil *nil = scm_memory_allocate(sizeof(ScmNil));
  scm_obj_init(SCM_OBJ(nil), &SCM_NIL_TYPE_INFO);
  return nil;
}

void
scm_nil_destruct(ScmNil *nil)
{
  assert(nil != NULL);
  scm_memory_release(nil);
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

  return SCM_OBJ_IS_TYPE(obj, &SCM_NIL_TYPE_INFO);
}

void
scm_nil_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  scm_obuffer_concatenate_string(obuffer, "()");
}

