#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/impl_utils.h"

int
scm_fcd_file_exists(ScmObj path, bool *rslt)
{
  char path_cstr[PATH_MAX];
  struct stat st;
  ssize_t s;
  int r;

  scm_assert(scm_fcd_string_p(path));

  s = scm_fcd_string_to_path_cstr(path, path_cstr, sizeof(path_cstr));
  if (s < 0) return -1;

  SCM_SYSCALL(r, stat(path_cstr, &st));
  if (r < 0 && errno != ENOENT) {
    /* TODO; change error message */
    scm_fcd_error("system call error: stat", 0);
    return -1;
  }

  if (rslt != NULL)
    *rslt = (r == 0);

  return 0;
}

int
scm_fcd_delete_file(ScmObj path)
{
  char path_cstr[PATH_MAX];
  ssize_t s;
  int r;

  scm_assert(scm_fcd_string_p(path));

  s = scm_fcd_string_to_path_cstr(path, path_cstr, sizeof(path_cstr));
  if (s < 0) return -1;

  SCM_SYSCALL(r, unlink(path_cstr));
  if (r < 0) {
    /* TODO; change error message */
    scm_fcd_file_error("system call error: unlink", 0);
    return -1;
  }

  return 0;
}
