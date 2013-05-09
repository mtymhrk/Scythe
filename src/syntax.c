#include <string.h>

#include "object.h"
#include "api.h"
#include "syntax.h"

ScmTypeInfo SCM_SYNTAX_TYPE_INFO = {
  .name                = "syntax",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_syntax_pretty_print,
  .obj_size            = sizeof(ScmSyntax),
  .gc_ini_func         = scm_syntax_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_syntax_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_syntax_initialize(ScmObj syx, ScmObj key, ScmSyntaxHandlerFunc handler)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(key));
  scm_assert(handler != NULL);

  SCM_SLOT_SETQ(ScmSyntax, syx, keyword, key);
  SCM_SYNTAX(syx)->handler = handler;

  return 0;
}

ScmObj
scm_syntax_new(SCM_MEM_TYPE_T mtype, ScmObj key, ScmSyntaxHandlerFunc handler)
{
  ScmObj syx;
  int rslt;

  scm_assert(scm_capi_symbol_p(key));
  scm_assert(handler != NULL);

  syx = scm_capi_mem_alloc(&SCM_SYNTAX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(syx)) return SCM_OBJ_NULL;

  rslt = scm_syntax_initialize(syx, key, handler);
  if (rslt < 0) return SCM_OBJ_NULL;

  return syx;
}


int
scm_syntax_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj o = SCM_OBJ_INIT;
  char str[SCM_SYNTAX_KEYWORD_LEN_MAX + 32];
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port,
                       &o);

  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  snprintf(str, sizeof(str), "#<%s ", scm_obj_type_name(obj));

  rslt = scm_capi_write_cstr(str, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  o = scm_api_display(SCM_SYNTAX(obj)->keyword, port);
  if (scm_obj_null_p(o)) return -1;

  rslt = scm_capi_write_cstr(">", SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_syntax_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  SCM_SYNTAX(obj)->keyword = SCM_OBJ_NULL;
}

int
scm_syntax_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYNTAX(obj)->keyword, mem);
}
