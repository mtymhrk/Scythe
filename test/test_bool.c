#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "char.h"
#include "bool.h"

void
test_scm_bool_construct(void)
{
  ScmBool *boolt = scm_bool_construct(true);

  cut_assert_not_null(boolt);
  cut_assert_equal_int(SCM_OBJ_TYPE_BOOL,
                       scm_obj_type(SCM_OBJ(boolt)));
}

void
test_scm_bool_value_true(void)
{
  ScmBool *boolt = scm_bool_construct(true);
  
  cut_assert_true(scm_bool_value(boolt));
}

void
test_scm_bool_value_false(void)
{
  ScmBool *boolf = scm_bool_construct(false);
  
  cut_assert_false(scm_bool_value(boolf));
}

void
test_scm_bool_is_bool_true(void)
{
  ScmBool *boolt = scm_bool_construct(true);
  
  cut_assert_true(scm_bool_is_bool(SCM_OBJ(boolt)));
}

void
test_scm_bool_is_bool_false(void)
{
  ScmBool *boolf = scm_bool_construct(false);
  
  cut_assert_true(scm_bool_is_bool(SCM_OBJ(boolf)));
}

void
test_scm_bool_is_bool_not_bool(void)
{
  ScmObj nil = SCM_OBJ(scm_nil_instance());

  cut_assert_false(scm_char_is_char(nil));
}
