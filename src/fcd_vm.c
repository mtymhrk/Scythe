#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/vm.h"
#include "scythe/module.h"

ScmBedrock *scm_fcd__current_br = NULL;
ScmObj scm_fcd__current_vm = SCM_OBJ_INIT;
ScmObj scm_fcd__current_ref_stack = SCM_OBJ_INIT;

void
scm_fcd_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_fcd_current_br(), msg);
}

extern inline void
scm_fcd_fatalf(const char *fmt, ...)
{
}

extern inline bool
scm_fcd_fatal_p(void)
{
  return scm_bedrock_fatal_p(scm_fcd_current_br());
}

int
scm_fcd_halt(void)
{
  return scm_vm_setup_stat_halt(scm_fcd_current_vm());
}

int
scm_fcd_cached_global_var_ref(int kind, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&gloc, &v);

  r = scm_bedrock_cached_gv(scm_fcd_current_br(), kind, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_not_null_p(gloc))
    v = scm_gloc_value(gloc);
  else
    v = SCM_OBJ_NULL;

  if (val != NULL)
    scm_csetter_setq(val, v);

  return 0;
}

extern inline bool
scm_fcd_vm_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_VM_TYPE_INFO);
}

ScmObj
scm_fcd_vm_new()
{
  return scm_vm_new();
}

void
scm_fcd_vm_end(ScmObj vm)
{
  scm_assert(scm_fcd_vm_p(vm));
  scm_vm_end(vm);
}

ScmObj
scm_fcd_vm_apply(ScmObj vm, ScmObj proc, ScmObj args)
{
  scm_assert(scm_fcd_vm_p(vm));
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));
  return scm_vm_apply(vm, proc, args);
}

ScmObj
scm_fcd_vm_run_cloned(ScmObj vm, ScmObj iseq)
{
  scm_assert(scm_fcd_vm_p(vm));
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_vm_run_cloned(vm, iseq);
}

void
scm_fcd_vm_disposal_unhandled_exc(ScmObj vm)
{
  scm_assert(scm_fcd_vm_p(vm));
  return scm_vm_disposal_unhandled_exc(vm);
}

int
scm_fcd_return_val(const ScmObj *val, int vc)
{
  scm_assert(vc >= 0);
  scm_assert(vc == 0 || val != NULL);
  return scm_vm_set_val_reg(scm_fcd_current_vm(), val, vc);
}

int
scm_fcd_trampolining(ScmObj proc, ScmObj args,
                     ScmObj postproc, ScmObj handover)
{
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));
  scm_assert(scm_obj_null_p(postproc) || scm_fcd_procedure_p(postproc));

  return scm_vm_setup_stat_trmp(scm_fcd_current_vm(), proc, args,
                                postproc, handover, true);
}

void
scm_fcd_exit(ScmObj obj)
{
  /* TODO: obj の内容に応じた VM の終了ステータスの設定*/

  scm_vm_setup_stat_halt(scm_fcd_current_vm());
}

ScmEncoding *
scm_fcd_system_encoding(void)
{
  return scm_bedrock_encoding(scm_fcd_current_br());
}

int
scm_fcd_load_iseq(ScmObj iseq)
{
  ScmObj o = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&iseq,
                      &o);

  scm_assert(scm_fcd_iseq_p(iseq));

  rslt = scm_fcd_iseq_push_inst(iseq, SCM_OPCODE_HALT);
  if (rslt < 0) return -1;

  o = scm_fcd_vm_run_cloned(scm_fcd_current_vm(), iseq);
  if (scm_obj_null_p(o)) return -1;

  return 0;
}
