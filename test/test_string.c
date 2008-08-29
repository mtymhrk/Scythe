#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "string.h"

void
test_scm_string_construct(void)
{
  ScmString *string = scm_string_construct("foo");

  cut_assert_not_null(string);
}

void
test_scm_string_is_string(void)
{
  ScmString *string = scm_string_construct("foo");
  ScmNil *nil = scm_nil_construct();

  cut_assert_true(scm_string_is_string(SCM_OBJ(string)));
  cut_assert_false(scm_string_is_string(SCM_OBJ(nil)));
}

void
test_scm_string_string(void)
{
  ScmString *string = scm_string_construct("foo");

  cut_assert_equal_string("foo", scm_string_string(string));
}

void
test_scm_string_length(void)
{
  ScmString *string = scm_string_construct("foo");

  cut_assert_equal_int(3, scm_string_length(string));
}
