#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "char.h"

void
test_scm_char_construct(void)
{
  ScmChar *charv = scm_char_construct('a');

  cut_assert_not_null(charv);
  cut_assert_equal_int(SCM_OBJ_TYPE_CHAR,
                       scm_obj_type(SCM_OBJ(charv)));
}

void
test_scm_char_value_a(void)
{
  ScmChar *charv = scm_char_construct('a');

  cut_assert_not_null(charv);
  cut_assert_equal_int('a',
                       scm_char_value(charv));

}

void
test_scm_char_value_integer(void)
{
  ScmChar *charv = scm_char_construct(123);

  cut_assert_not_null(charv);
  cut_assert_equal_int(123,
                       scm_char_value(charv));

}

void
test_scm_char_is_char(void)
{
  ScmChar *charv = scm_char_construct('a');

  cut_assert_true(scm_char_is_char(SCM_OBJ(charv)));
}

void
test_scm_char_is_char_not_char(void)
{
  ScmObj nil = SCM_OBJ(scm_nil_instance());

  cut_assert_false(scm_char_is_char(nil));
}
