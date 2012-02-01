#include <cutter.h>
#include <stdbool.h>

#include "object.h"
#include "vm.h"
#include "miscobjects.h"
#include "char.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
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

  SCM_SETQ(bl, scm_bool_new(SCM_MEM_ALLOC_HEAP, true));

  cut_assert(SCM_OBJ_IS_NOT_NULL(bl));
  cut_assert(SCM_OBJ_IS_TYPE(SCM_OBJ(bl), &SCM_BOOL_TYPE_INFO));
}

void
test_scm_bool_value_true(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_SETQ(bl, scm_bool_new(SCM_MEM_ALLOC_HEAP,true));

  cut_assert_true(scm_bool_value(bl));
}

void
test_scm_bool_value_false(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_SETQ(bl, scm_bool_new(SCM_MEM_ALLOC_HEAP,false));

  cut_assert_false(scm_bool_value(bl));
}

void
test_scm_bool_is_bool_true(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_SETQ(bl, scm_bool_new(SCM_MEM_ALLOC_HEAP,true));

  cut_assert_true(scm_bool_is_bool(bl));
}

void
test_scm_bool_is_bool_false(void)
{
  ScmObj bl = SCM_OBJ_INIT;

  SCM_SETQ(bl, scm_bool_new(SCM_MEM_ALLOC_HEAP,false));

  cut_assert_true(scm_bool_is_bool(bl));
}

void
test_scm_bool_is_bool_not_bool(void)
{
  ScmObj nil = scm_nil_instance();

  cut_assert_false(scm_bool_is_bool(nil));
}
