#include <cutter.h>


#include "object.h"
#include "vm.h"
#include "string.h"
#include "miscobjects.h"
#include "vector.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}

void
test_scm_vector_new(void)
{
  ScmObj vector = SCM_OBJ_INIT;
  size_t i;

  SCM_STACK_FRAME_PUSH(&vector);

  SCM_SETQ(vector, scm_vector_new(SCM_MEM_ALLOC_HEAP, 5));

  cut_assert_true(SCM_OBJ_IS_NOT_NULL(vector));
  cut_assert(scm_vector_is_vector(vector));
  cut_assert_equal_uint(5, scm_vector_length(vector));

  for (i = 0; i < 5u; i++)
    cut_assert(scm_nil_is_nil(scm_vector_ref(vector, i)));
}

void
test_scm_vector_set_and_ref(void)
{
  ScmObj vector = SCM_OBJ_INIT;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  ScmObj str3 = SCM_OBJ_INIT, str4 = SCM_OBJ_INIT, str5 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vector, &str1, &str2, &str3, &str4, &str5);

  SCM_SETQ(vector, scm_vector_new(SCM_MEM_ALLOC_HEAP, 5));
  SCM_SETQ(str1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "str1", 4, SCM_ENCODING_ASCII));
  SCM_SETQ(str2, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "str2", 4, SCM_ENCODING_ASCII));
  SCM_SETQ(str3, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "str3", 4, SCM_ENCODING_ASCII));
  SCM_SETQ(str4, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "str4", 4, SCM_ENCODING_ASCII));
  SCM_SETQ(str5, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "str5", 4, SCM_ENCODING_ASCII));

  scm_vector_set(vector, 0, str1);
  scm_vector_set(vector, 1, str2);
  scm_vector_set(vector, 2, str3);
  scm_vector_set(vector, 3, str4);
  scm_vector_set(vector, 4, str5);

  cut_assert(scm_obj_is_same_instance(str1, scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_is_same_instance(str2, scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_is_same_instance(str3, scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_is_same_instance(str4, scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_is_same_instance(str5, scm_vector_ref(vector, 4)));

  scm_vector_set(vector, 0, str5);
  scm_vector_set(vector, 1, str4);
  scm_vector_set(vector, 2, str3);
  scm_vector_set(vector, 3, str2);
  scm_vector_set(vector, 4, str1);

  cut_assert(scm_obj_is_same_instance(str5, scm_vector_ref(vector, 0)));
  cut_assert(scm_obj_is_same_instance(str4, scm_vector_ref(vector, 1)));
  cut_assert(scm_obj_is_same_instance(str3, scm_vector_ref(vector, 2)));
  cut_assert(scm_obj_is_same_instance(str2, scm_vector_ref(vector, 3)));
  cut_assert(scm_obj_is_same_instance(str1, scm_vector_ref(vector, 4)));
}
