#ifndef INCLUDE_FILE_H__
#define INCLUDE_FILE_H__

#include <stdbool.h>

#include "scythe/object.h"

#define SCM_LOAD_PATH_VARIABLE_NAME "*load-path*"

int scm_add_load_path(ScmObj dir);
ScmObj scm_search_load_file(ScmObj name);
int scm_file_exists(ScmObj path, bool *rslt);
int scm_delete_file(ScmObj path);

#endif /* INCLUDE_FILE_H__ */
