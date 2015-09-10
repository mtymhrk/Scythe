#ifndef INCLUDE_FCD_SCYTHE_H__
#define INCLUDE_FCD_SCYTHE_H__

typedef struct ScmScytheRec ScmScythe;

ScmScythe *scm_fcd_scythe_new();
void scm_fcd_scythe_end(ScmScythe *scy);
void scm_fcd_scythe_enable(ScmScythe *scy);
void scm_fcd_scythe_disable(ScmScythe *scy);
int scm_fcd_scythe_bootup(ScmScythe *scy);
void scm_fcd_scythe_shutdown(ScmScythe *scy);
int scm_fcd_scythe_load_core(ScmScythe *scy);
int scm_fcd_scythe_add_load_path(ScmScythe *scy, const char *path);
void scm_fcd_scythe_clear_load_path(ScmScythe *scy);

int scm_fcd_scythe_run_repl(ScmScythe *scy);
int scm_fcd_scythe_exec_file(ScmScythe *scy, const char *path);
int scm_fcd_scythe_exec_cstr(ScmScythe *scy, const char *expr);
int scm_fcd_scythe_compile_file(ScmScythe *scy, const char *path);

#endif  /* INCLUDE_FCD_SCYTHE_H__ */
