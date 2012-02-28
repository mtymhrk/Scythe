#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
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
test_scm_eof_new(void)
{
  ScmObj eof = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&eof);

  eof = scm_eof_new(SCM_MEM_HEAP);

  cut_assert_true(scm_obj_not_null_p(eof));
  cut_assert_true(scm_obj_type_p(eof, &SCM_EOF_TYPE_INFO));
}
