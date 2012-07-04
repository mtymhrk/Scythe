#include <string.h>

#include "object.h"
#include "api.h"
#include "syntax.h"

ScmTypeInfo SCM_SYNTAX_TYPE_INFO = {
  .name                = "syntax",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_syntax_pretty_print,
  .obj_size            = sizeof(ScmSyntax),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_syntax_initialize(ScmObj syx, int id, const char *keyword)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);

  SCM_SYNTAX(syx)->id = id;
  strncpy(SCM_SYNTAX(syx)->keyword, keyword, SCM_SYNTAX_KEYWORD_LEN_MAX);
  SCM_SYNTAX(syx)->keyword[SCM_SYNTAX_KEYWORD_LEN_MAX - 1] = '\0';

  return 0;
}

ScmObj
scm_syntax_new(SCM_MEM_TYPE_T mtype, int id, const char *keyword)
{
  ScmObj syx;
  int rslt;

  syx = scm_capi_mem_alloc(&SCM_SYNTAX_TYPE_INFO, mtype);
  if (scm_obj_null_p(syx)) return SCM_OBJ_NULL;

  rslt = scm_syntax_initialize(syx, id, keyword);
  if (rslt < 0) return SCM_OBJ_NULL;

  return syx;
}

bool
scm_syntax_id_eq_p(ScmObj syx, int id)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);

  return (SCM_SYNTAX(syx)->id == id) ? true : false;
}

int
scm_syntax_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char str[SCM_SYNTAX_KEYWORD_LEN_MAX + 32];
  int rslt;

  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  snprintf(str, sizeof(str), "#<%s %s>",
           scm_obj_type_name(obj), SCM_SYNTAX(obj)->keyword);

  rslt = scm_capi_write_cstr(str, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}
