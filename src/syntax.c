#include <string.h>

#include "object.h"
#include "api.h"
#include "syntax.h"

ScmTypeInfo SCM_SYNTAX_TYPE_INFO = {
  .name                = "syntax",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_syntax_obj_print,
  .obj_size            = sizeof(ScmSyntax),
  .gc_ini_func         = scm_syntax_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_syntax_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_syntax_initialize(ScmObj syx, ScmObj key, ScmObj handler)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);
  scm_assert(scm_capi_symbol_p(key));
  scm_assert(scm_obj_not_null_p(handler));

  SCM_SLOT_SETQ(ScmSyntax, syx, keyword, key);
  SCM_SLOT_SETQ(ScmSyntax, syx, handler, handler);

  return 0;
}

ScmObj
scm_syntax_new(SCM_MEM_TYPE_T mtype, ScmObj key, ScmObj handler)
{
  ScmObj syx = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&key, &handler,
                       &syx);

  scm_assert(scm_capi_symbol_p(key));
  scm_assert(scm_obj_not_null_p(handler));

  syx = scm_capi_mem_alloc(&SCM_SYNTAX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(syx)) return SCM_OBJ_NULL;

  rslt = scm_syntax_initialize(syx, key, handler);
  if (rslt < 0) return SCM_OBJ_NULL;

  return syx;
}


int
scm_syntax_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  return scm_capi_pformat_cstr(port, "#<syntax ~a>",
                               SCM_SYNTAX(obj)->keyword, SCM_OBJ_NULL);
}

void
scm_syntax_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);

  SCM_SYNTAX(obj)->keyword = SCM_OBJ_NULL;
  SCM_SYNTAX(obj)->handler = SCM_OBJ_NULL;
}

int
scm_syntax_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_SYNTAX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYNTAX(obj)->keyword, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYNTAX(obj)->handler, mem);
}
