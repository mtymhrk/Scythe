#include <cutter.h>
#include <stdbool.h>

#include "object.h"
#include "vm.h"
#include "api.h"
#include "miscobjects.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  vm = scm_vm_new();
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}


void
test_scm_bool_new(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP, true);

  cut_assert(scm_obj_not_null_p(bl));
  cut_assert(scm_obj_type_p(SCM_OBJ(bl), &SCM_BOOL_TYPE_INFO));
}

void
test_scm_bool_value_true(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP,true);

  cut_assert_true(scm_bool_value(bl));
}

void
test_scm_bool_value_false(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  bl = scm_bool_new(SCM_MEM_HEAP,false);

  cut_assert_false(scm_bool_value(bl));
}
