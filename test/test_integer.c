#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "miscobjects.h"
#include "integer.h"

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
test_scm_integer_new(void)
{
  ScmObj integer = SCM_OBJ_INIT;

  SCM_SETQ(integer, scm_integer_new(SCM_MEM_ALLOC_HEAP, 0LL));

  cut_assert_true(scm_obj_not_null_p(integer));
}

void
test_scm_integer_is_integer_pass(void)
{
  ScmObj integer = SCM_OBJ_INIT;

  SCM_SETQ(integer, scm_integer_new(SCM_MEM_ALLOC_HEAP, 0LL));

  cut_assert_true(scm_integer_is_integer(integer));
}

void
test_scm_integer_is_integer_failure(void)
{
  ScmObj nil = scm_nil_instance();

  cut_assert_false(scm_integer_is_integer(nil));
}

void
test_scm_integer_value(void)
{
  ScmObj integer = SCM_OBJ_INIT;

  SCM_SETQ(integer, scm_integer_new(SCM_MEM_ALLOC_HEAP, 100LL));

  cut_assert_equal_int(100, (int)scm_integer_value(integer));
}

void
test_scm_integer_plus(void)
{
  ScmObj integer1 = SCM_OBJ_INIT, integer2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer1, &integer2);

  SCM_SETQ(integer1, scm_integer_new(SCM_MEM_ALLOC_HEAP, 200LL));
  SCM_SETQ(integer2, scm_integer_new(SCM_MEM_ALLOC_HEAP, 100LL));

  cut_assert_equal_int(300,
                       (int)scm_integer_value(scm_integer_plus(integer1,
                                                               integer2)));
}

void
test_scm_integer_minus(void)
{
  ScmObj integer1 = SCM_OBJ_INIT, integer2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer1, &integer2);

  SCM_SETQ(integer1, scm_integer_new(SCM_MEM_ALLOC_HEAP, 200LL));
  SCM_SETQ(integer2, scm_integer_new(SCM_MEM_ALLOC_HEAP, 100LL));

  cut_assert_equal_int(100,
                       (int)scm_integer_value(scm_integer_minus(integer1,
                                                                integer2)));
}

void
test_scm_integer_multiple(void)
{
  ScmObj integer1 = SCM_OBJ_INIT, integer2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer1, &integer2);

  SCM_SETQ(integer1, scm_integer_new(SCM_MEM_ALLOC_HEAP, 200LL));
  SCM_SETQ(integer2, scm_integer_new(SCM_MEM_ALLOC_HEAP, 100LL));

  cut_assert_equal_int(20000,
                       (int)scm_integer_value(scm_integer_multiply(integer1,
                                                                   integer2)));
}

void
test_scm_integer_divide(void)
{
  ScmObj integer1 = SCM_OBJ_INIT, integer2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer1, &integer2);

  SCM_SETQ(integer1, scm_integer_new(SCM_MEM_ALLOC_HEAP, 200LL));
  SCM_SETQ(integer2, scm_integer_new(SCM_MEM_ALLOC_HEAP, 100LL));

  cut_assert_equal_int(2,
                       (int)scm_integer_value(scm_integer_divide(integer1,
                                                                 integer2)));
}

void
test_scm_integer_reminder(void)
{
  ScmObj integer1 = SCM_OBJ_INIT, integer2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&integer1, &integer2);

  SCM_SETQ(integer1, scm_integer_new(SCM_MEM_ALLOC_HEAP, 200LL));
  SCM_SETQ(integer2, scm_integer_new(SCM_MEM_ALLOC_HEAP, 3LL));

  cut_assert_equal_int(2,
                       (int)scm_integer_value(scm_integer_reminder(integer1,
                                                                   integer2)));
}
