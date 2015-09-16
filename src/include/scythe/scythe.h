#ifndef INCLUDE_SCYTHE_H__
#define INCLUDE_SCYTHE_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/earray.h"
#include "scythe/bedrock.h"

typedef struct ScmScytheRec ScmScythe;

enum { SCM_SCYTHE_S_DOWN, SCM_SCYTHE_S_UP };

struct ScmScytheRec {
  int stat;
  ScmBedrock *bedrock;
  ScmObj vm;
  ScmObj refstack;

  struct {
    EArray load_path;
    ScmGlobalConf gconf;
    /* int argc; */
    /* char **argv; */
  } conf;
};

int scm_scythe_initialize(ScmScythe *scy);
void scm_scythe_finalize(ScmScythe *scy);
ScmScythe *scm_scythe_new();
void scm_scythe_end(ScmScythe *scy);
void scm_scythe_switch(ScmScythe *scy);
int scm_scythe_bootup(ScmScythe *scy);
void scm_scythe_shutdown(ScmScythe *scy);
int scm_scythe_add_load_path(ScmScythe *scy, const char *path);
void scm_scythe_clear_load_path(ScmScythe *scy);
int scm_scythe_set_system_encoding(ScmScythe *scy, const char *enc);
void scm_scythe_clear_system_encoding(ScmScythe *scy);
int scm_scythe_set_external_encoding(ScmScythe *scy, const char *enc);
void scm_scythe_clear_external_encoding(ScmScythe *scy);
/* int scm_scythe_set_arguments(ScmScythe *scy, int argc, const char **argv); */
/* void scm_scythe_clear_arguments(ScmScythe *scy); */
int scm_scythe_update_load_path_variable(ScmScythe *scy);
int scm_scythe_load_core(ScmScythe *scy);
int scm_scythe_run_repl(ScmScythe *scy);
int scm_scythe_exec_file(ScmScythe *scy, const char *path);
int scm_scythe_exec_cstr(ScmScythe *scy, const char *expr);
int scm_scythe_compile_file(ScmScythe *scy, const char *path);
int scm_prepare_scythe(void);

static inline void
scm_scythe_enable(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  scm_scythe_switch(scy);
}

static inline void
scm_scythe_disable(ScmScythe *scy)
{
  scm_scythe_switch(NULL);
}

static inline bool
scm_scythe_conf_modifiable_p(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  return ((scy->stat == SCM_SCYTHE_S_DOWN) ? true : false);
}



#endif  /* INCLUDE_SCYTHE_H__ */
