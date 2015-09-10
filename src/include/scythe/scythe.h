#ifndef INCLUDE_SCYTHE_H__
#define INCLUDE_SCYTHE_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/earray.h"

enum { SCM_SCYTHE_S_DOWN, SCM_SCYTHE_S_UP };

struct ScmScytheRec {
  int stat;
  ScmBedrock *bedrock;
  ScmObj vm;
  ScmObj refstack;

  struct {
    EArray load_path;
    /* char *system_encoding; */
    /* char *external_encoding; */
    /* int argc; */
    /* char **argv; */
  } conf;
};

int scm_scythe_initialize(ScmScythe *scy);
void scm_scythe_finalize(ScmScythe *scy);
void scm_scythe_switch(ScmScythe *scy);
int scm_scythe_bootup(ScmScythe *scy);
void scm_scythe_shutdown(ScmScythe *scy);
int scm_scythe_add_load_path(ScmScythe *scy, const char *path);
void scm_scythe_clear_load_path(ScmScythe *scy);
/* int scm_scythe_set_system_encoding(ScmScythe *scy, const char *enc); */
/* void scm_scythe_clear_system_encoding(ScmScythe *scy); */
/* int scm_scythe_set_external_encoding(ScmScythe *scy, const char *enc); */
/* void scm_scythe_clear_external_encoding(ScmScythe *scy); */
/* int scm_scythe_set_arguments(ScmScythe *scy, int argc, const char **argv); */
/* void scm_scythe_clear_arguments(ScmScythe *scy); */
int scm_scythe_update_load_path_variable(ScmScythe *scy);

static inline bool
scm_scythe_conf_modifiable_p(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  return ((scy->stat == SCM_SCYTHE_S_DOWN) ? true : false);
}

#endif  /* INCLUDE_SCYTHE_H__ */
