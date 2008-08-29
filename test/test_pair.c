#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "pair.h"

void
test_scm_pair_construct(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  cut_assert_not_null(pair);
}

void
test_scm_pair_is_pair(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  cut_assert_true(scm_pair_is_pair(SCM_OBJ(pair)));
  cut_assert_false(scm_pair_is_pair(SCM_OBJ(car)));
}

void
test_scm_pair_car(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  cut_assert_equal_pointer(car, scm_pair_car(pair));
}

void
test_scm_pair_cdr(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  cut_assert_equal_pointer(cdr, scm_pair_cdr(pair));
}
