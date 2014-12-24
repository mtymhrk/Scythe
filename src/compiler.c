#include <stdio.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/assembler.h"
#include "scythe/compiler.h"

ScmTypeInfo SCM_COMPILER_TYPE_INFO = {
  .name                            = "compiler",
  .flags                           = SCM_TYPE_FLG_MMO,
  .obj_print_func                  = NULL,
  .obj_size                        = sizeof(ScmCompiler),
  .gc_ini_func                     = scm_cmpl_gc_initialize,
  .gc_fin_func                     = NULL,
  .gc_accept_func                  = scm_cmpl_gc_accept,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

int
scm_cmpl_initialize(ScmObj cmpl, ScmObj module)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_fcd_module_p(module));

  SCM_COMPILER(cmpl)->label_id = 0;
  SCM_SLOT_SETQ(ScmCompiler, cmpl, module, module);
  SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, SCM_NIL_OBJ);

  return 0;
}

void
scm_cmpl_set_module(ScmObj cmpl, ScmObj module)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_fcd_module_p(module));

  SCM_SLOT_SETQ(ScmCompiler, cmpl, module, module);
}

void
scm_cmpl_set_expr(ScmObj cmpl, ScmObj expr)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  if (scm_obj_null_p(expr))
    SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, SCM_NIL_OBJ);
  else
    SCM_SLOT_SETQ(ScmCompiler, cmpl, expr, expr);
}

int
scm_cmpl_assign_label_id(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  return SCM_COMPILER(cmpl)->label_id++;
}

void
scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(obj)->module = SCM_OBJ_NULL;
  SCM_COMPILER(obj)->expr = SCM_OBJ_NULL;
}

int
scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->module, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->expr, mem);
}
