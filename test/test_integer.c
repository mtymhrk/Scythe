#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "integer.h"

void
test_scm_integer_construct(void)
{
  ScmInteger *integer = scm_integer_construct(0);

  cut_assert_not_null(integer);
}

void
test_scm_integer_is_integer_pass(void)
{
  ScmInteger *integer = scm_integer_construct(0);

  cut_assert_true(scm_integer_is_integer(SCM_OBJ(integer)));
}

void
test_scm_integer_is_integer_failure(void)
{
  ScmNil *nil = scm_nil_instance();

  cut_assert_false(scm_integer_is_integer(SCM_OBJ(nil)));
}

void
test_scm_integer_value(void)
{
  ScmInteger *integer = scm_integer_construct(100);

  cut_assert_equal_int(100, scm_integer_value(integer));
}

void
test_scm_integer_plus(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  cut_assert_equal_int(300,
                       scm_integer_value(scm_integer_plus(integer1, integer2)));
}

void
test_scm_integer_minus(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  cut_assert_equal_int(100,
                       scm_integer_value(scm_integer_minus(integer1,
                                                           integer2)));
}

void
test_scm_integer_multiple(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  cut_assert_equal_int(20000,
                       scm_integer_value(scm_integer_multiply(integer1,
                                                              integer2)));
}

void
test_scm_integer_divide(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  cut_assert_equal_int(2,
                       scm_integer_value(scm_integer_divide(integer1,
                                                            integer2)));
}

void
test_scm_integer_reminder(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(3);

  cut_assert_equal_int(2,
                       scm_integer_value(scm_integer_reminder(integer1,
                                                              integer2)));
}
