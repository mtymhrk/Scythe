#include "scythe/api.h"

#include "test.h"

ScmObj
read_cstr(const char *str)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_capi_open_input_string_cstr(str, SCM_ENC_NAME_SRC);
  return scm_api_read(port);
}

