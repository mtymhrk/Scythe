#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "pair.h"
#include "miscobjects.h"

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
test_scm_nil_new(void)
{
  ScmObj nil = scm_nil_new(SCM_MEM_ALLOC_HEAP);

  cut_assert_true(SCM_OBJ_NOT_NULL_P(nil));
}

void
test_scm_nil_instance(void)
{
  ScmObj nil1 = SCM_OBJ_INIT, nil2 = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&nil1, &nil2);

  SCM_SETQ(nil1, scm_nil_instance());
  SCM_SETQ(nil2, scm_nil_instance());

  cut_assert_true(SCM_OBJ_NOT_NULL_P(nil1));
  cut_assert_true(SCM_OBJ_NOT_NULL_P(nil2));
  cut_assert_equal_uint(nil1, nil2);
}

void
test_scm_nil_is_nil(void)
{
  ScmObj nil = SCM_OBJ_INIT, pair = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&nil, &pair);

  SCM_SETQ(nil, scm_nil_new(SCM_MEM_ALLOC_HEAP));
  SCM_SETQ(pair, scm_pair_new(SCM_MEM_ALLOC_HEAP, nil, nil));

  cut_assert_true(scm_nil_is_nil(nil));
  cut_assert_false(scm_nil_is_nil(pair));
}
