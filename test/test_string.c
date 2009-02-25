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
