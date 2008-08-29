#include <cutter.h>

#include "object.h"
#include "pair.h"
#include "nil.h"

void
test_scm_nil_construct(void)
{
  ScmNil *nil = scm_nil_construct();

  cut_assert_not_null(nil);
}

void
test_scm_nil_instance(void)
{
  ScmNil *nil1 = scm_nil_instance();
  ScmNil *nil2 = scm_nil_instance();

  cut_assert_not_null(nil1);
  cut_assert_not_null(nil2);
  cut_assert_equal_pointer(nil1, nil2);
}

void
test_scm_nil_is_nil(void)
{
  ScmNil *nil = scm_nil_construct();
  ScmPair *pair = scm_pair_construct(SCM_OBJ(nil), SCM_OBJ(nil));

  cut_assert_true(scm_nil_is_nil(SCM_OBJ(nil)));
  cut_assert_false(scm_nil_is_nil(SCM_OBJ(pair)));
}
