#include <cutter.h>

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
test_scm_nil_new(void)
{
  ScmObj nil = scm_nil_new(SCM_CAPI_MEM_HEAP);

  cut_assert_true(scm_obj_not_null_p(nil));
}
