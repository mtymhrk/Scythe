#ifndef INCLUDE_FCD_VM_H__
#define INCLUDE_FCD_VM_H__

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd_type.h"


void scm_fcd_fatal(const char *msg);
void scm_fcd_fatalf(const char *fmt, ...);
bool scm_fcd_fatal_p(void);

int scm_fcd_cached_global_var_ref(int kind, scm_csetter_t *val);
ScmObj scm_fcd_cached_symbol(int kind);

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
