#ifndef INCLUDE_FCD_VM_H__
#define INCLUDE_FCD_VM_H__

#include "scythe/object.h"
#include "scythe/encoding.h"

typedef struct ScmBedrockRec ScmBedrock;

/* vm.c の外部が scm__current_XXX を直接参照するのは禁止。
   scm_fcd_current_XXX() 経由で取得すること。 */
extern ScmBedrock *scm_fcd__current_br;
extern ScmObj scm_fcd__current_vm;
extern ScmObj scm_fcd__current_ref_stack;

static inline ScmBedrock *
scm_fcd_current_br(void)
{
  return scm_fcd__current_br;
}

static inline ScmObj
scm_fcd_current_vm(void)
{
  return scm_fcd__current_vm;
}

static inline ScmObj
scm_fcd_current_ref_stack(void)
{
  return scm_fcd__current_ref_stack;
}

static inline void
scm_fcd_chg_current_br(ScmBedrock *br)
{
  scm_fcd__current_br = br;
}

static inline void
scm_fcd_chg_current_vm(ScmObj vm)
{
  scm_fcd__current_vm = vm;
}

static inline void
scm_fcd_chg_current_ref_stack(ScmObj stack)
{
  scm_fcd__current_ref_stack = stack;
}

void scm_fcd_fatal(const char *msg);
void scm_fcd_fatalf(const char *fmt, ...);
bool scm_fcd_fatal_p(void);

int scm_fcd_cached_global_var_ref(int kind, scm_csetter_t *val);

bool scm_fcd_vm_p(ScmObj obj);
ScmObj scm_fcd_vm_new();
void scm_fcd_vm_end(ScmObj vm);
ScmObj scm_fcd_vm_apply(ScmObj vm, ScmObj proc, ScmObj args);
ScmObj scm_fcd_vm_run_cloned(ScmObj vm, ScmObj iseq);
void scm_fcd_vm_disposal_unhandled_exc(ScmObj vm);

int scm_fcd_return_val(const ScmObj *val, int vc);

int scm_fcd_trampolining(ScmObj proc, ScmObj args,
                         ScmObj postproc, ScmObj handover);

void scm_fcd_exit(ScmObj obj);

ScmEncoding *scm_fcd_system_encoding(void);

int scm_fcd_load_iseq(ScmObj iseq);

#endif  /* INCLUDE_FCD_VM_H__ */
