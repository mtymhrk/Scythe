#include <CUnit/Basic.h>

CU_ErrorCode register_test_case(void);

int
main()
{
  CU_initialize_registry();
  register_test_case();
  CU_basic_set_mode(CU_BRM_VERBOSE);
  CU_basic_run_tests();
  CU_cleanup_registry();
  return(0);
}
