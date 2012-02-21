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
  ScmObj eof1 = SCM_OBJ_INIT, eof2 = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&eof1, eof2);

  eof1 = scm_eof_instance();
  eof2 = scm_eof_instance();

  cut_assert_true(scm_obj_not_null_p(eof1));
  cut_assert_true(scm_obj_not_null_p(eof2));

  cut_assert_true(scm_obj_same_instance_p(SCM_OBJ(eof1), SCM_OBJ(eof2)));
}

void
test_scm_eof_is_eof(void)
{
  cut_assert_true(scm_eof_is_eof(SCM_OBJ(scm_eof_instance())));
  cut_assert_false(scm_eof_is_eof(SCM_OBJ(scm_nil_instance())));
}
