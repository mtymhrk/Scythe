#include <cutter.h>


#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "vector.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_vector_new(void)
{
  ScmObj vector = SCM_OBJ_INIT;
  ScmObj expected = SCM_OBJ_NULL, actual = SCM_OBJ_NULL;
  size_t i;

  SCM_STACK_FRAME_PUSH(&vector, &expected, &actual);

  vector = scm_vector_new(SCM_MEM_HEAP, 5, SCM_NIL_OBJ);

  cut_assert_true(scm_obj_not_null_p(vector));
  cut_assert_true(scm_obj_type_p(vector, &SCM_VECTOR_TYPE_INFO));
  cut_assert_equal_uint(5, scm_vector_length(vector));

  expected = SCM_NIL_OBJ;
  for (i = 0; i < 5u; i++) {
    actual = scm_vector_ref(vector, i);
    cut_assert_true(scm_capi_eq_p(expected, actual));
  }
}

void
test_scm_vector_set_and_ref(void)
{
  ScmObj vector = SCM_OBJ_INIT;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  ScmObj str3 = SCM_OBJ_INIT, str4 = SCM_OBJ_INIT, str5 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vector, &str1, &str2, &str3, &str4, &str5);

  vector = scm_vector_new(SCM_MEM_HEAP, 5, SCM_NIL_OBJ);
  str1 = scm_capi_make_string_from_cstr("str1", SCM_ENC_ASCII);
  str2 = scm_capi_make_string_from_cstr("str2", SCM_ENC_ASCII);
  str3 = scm_capi_make_string_from_cstr("str3", SCM_ENC_ASCII);
  str4 = scm_capi_make_string_from_cstr("str4", SCM_ENC_ASCII);
  str5 = scm_capi_make_string_from_cstr("str5", SCM_ENC_ASCII);

  scm_vector_set(vector, 0, str1);
  scm_vector_set(vector, 1, str2);
  scm_vector_set(vector, 2, str3);
  scm_vector_set(vector, 3, str4);
  scm_vector_set(vector, 4, str5);

  cut_assert(scm_obj_same_instance_p(str1, scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_same_instance_p(str2, scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_same_instance_p(str3, scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_same_instance_p(str4, scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_same_instance_p(str5, scm_vector_ref(vector, 4)));

  scm_vector_set(vector, 0, str5);
  scm_vector_set(vector, 1, str4);
  scm_vector_set(vector, 2, str3);
  scm_vector_set(vector, 3, str2);
  scm_vector_set(vector, 4, str1);

  cut_assert(scm_obj_same_instance_p(str5, scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_same_instance_p(str4, scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_same_instance_p(str3, scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_same_instance_p(str2, scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_same_instance_p(str1, scm_vector_ref(vector, 4)));
}
