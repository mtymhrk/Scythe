#include <cutter.h>


#include "vector.h"
#include "object.h"
#include "string.h"
#include "nil.h"

void
test_scm_vector_construct(void)
{
  ScmVector *vector = scm_vector_construct(5);
  int i;

  cut_assert_not_null(vector);
  cut_assert(scm_vector_is_vector(SCM_OBJ(vector)));
  cut_assert_equal_int(5, scm_vector_length(vector));

  for (i = 0; i < 5; i++)
    cut_assert(scm_nil_is_nil(scm_vector_ref(vector, i)));
}

void
test_scm_vector_set_and_ref(void)
{
  ScmVector *vector = scm_vector_construct(5);
  ScmString *str1 = scm_string_construct("str1", 4, SCM_ENCODING_ASCII);
  ScmString *str2 = scm_string_construct("str2", 4, SCM_ENCODING_ASCII);
  ScmString *str3 = scm_string_construct("str3", 4, SCM_ENCODING_ASCII);
  ScmString *str4 = scm_string_construct("str4", 4, SCM_ENCODING_ASCII);
  ScmString *str5 = scm_string_construct("str5", 4, SCM_ENCODING_ASCII);

  scm_vector_set(vector, 0, SCM_OBJ(str1));
  scm_vector_set(vector, 1, SCM_OBJ(str2));
  scm_vector_set(vector, 2, SCM_OBJ(str3));
  scm_vector_set(vector, 3, SCM_OBJ(str4));
  scm_vector_set(vector, 4, SCM_OBJ(str5));

  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str1),
                                      scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str2),
                                      scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str3),
                                      scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str4),
                                      scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str5),
                                      scm_vector_ref(vector, 4)));
                                   

  scm_vector_set(vector, 0, SCM_OBJ(str5));
  scm_vector_set(vector, 1, SCM_OBJ(str4));
  scm_vector_set(vector, 2, SCM_OBJ(str3));
  scm_vector_set(vector, 3, SCM_OBJ(str2));
  scm_vector_set(vector, 4, SCM_OBJ(str1));

  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str5),
                                      scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str4),
                                      scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str3),
                                      scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str2),
                                      scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_is_same_instance(SCM_OBJ(str1),
                                      scm_vector_ref(vector, 4)));

}
