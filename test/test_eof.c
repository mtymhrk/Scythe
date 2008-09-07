#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "miscobjects.h"

void
test_scm_eof_construct(void)
{
  ScmEOF *eof1 = scm_eof_instance();
  ScmEOF *eof2 = scm_eof_instance();

  cut_assert_not_null(eof1);
  cut_assert_not_null(eof2);

  cut_assert_true(scm_obj_is_same_instance(SCM_OBJ(eof1), SCM_OBJ(eof2)));
}

void
test_scm_eof_is_eof(void)
{
  cut_assert_true(scm_eof_is_eof(SCM_OBJ(scm_eof_instance())));
  cut_assert_false(scm_eof_is_eof(SCM_OBJ(scm_nil_instance())));
}
