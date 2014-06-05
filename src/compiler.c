#include <stdio.h>
#include <stdbool.h>

#include "object.h"
#include "api.h"
#include "assembler.h"
#include "compiler.h"

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
scm_cmpl_initialize(ScmObj cmpl)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(cmpl)->label_id = 0;
  SCM_COMPILER(cmpl)->module = SCM_OBJ_NULL;

  return 0;
}

ScmObj
scm_cmpl_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj cmpl = SCM_OBJ_INIT;

  cmpl = scm_capi_mem_alloc(&SCM_COMPILER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cmpl)) return SCM_OBJ_NULL;

  if (scm_cmpl_initialize(cmpl) < 0)
    return SCM_OBJ_NULL;

  return cmpl;
}

void
scm_cmpl_select_module(ScmObj cmpl, ScmObj module)
{
  scm_assert_obj_type(cmpl, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_capi_module_p(module));

  SCM_SLOT_SETQ(ScmCompiler, cmpl, module, module);
}

void
scm_cmpl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);

  SCM_COMPILER(obj)->module = SCM_OBJ_NULL;
}

int
scm_cmpl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_COMPILER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_COMPILER(obj)->module, mem);
}
