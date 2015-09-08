#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/impl_utils.h"

static ScmObj
scm_string_char_eq_P(ScmObj str, size_t idx, int ascii)
{
  ScmObj chr = SCM_OBJ_INIT;
  ScmEncoding *enc;
  scm_char_t c;

  scm_assert(scm_fcd_string_p(str));

  chr = scm_fcd_string_ref(str, idx);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  scm_fcd_char_to_cchr(chr, &c);
  enc = scm_fcd_char_encoding(chr);

  if (scm_enc_cnv_to_ascii(enc, &c) == ascii)
    return SCM_TRUE_OBJ;
  else
    return SCM_FALSE_OBJ;
}

static ScmObj
scm_absolute_path_P(ScmObj path)
{
  return scm_string_char_eq_P(path, 0, '/');
}

static ScmObj
scm_starts_with_pairent_dir_or_current_dir_P(ScmObj path)
{
  ScmObj b = SCM_OBJ_INIT;

  b = scm_string_char_eq_P(path, 0, '.');
  if (scm_obj_null_p(b)) return SCM_OBJ_NULL;

  if (scm_fcd_false_p(b))
    return b;

  b = scm_string_char_eq_P(path, 1, '.');
  if (scm_obj_null_p(b)) return SCM_OBJ_NULL;

  return scm_string_char_eq_P(path, scm_fcd_true_p(b) ? 2 : 1, '/');
}

static ScmObj
scm_file_path_join(ScmObj path1, ScmObj path2)
{
  ScmObj b;
  size_t l;

  scm_assert(scm_fcd_string_p(path1));
  scm_assert(scm_fcd_string_p(path2));

  l = scm_fcd_string_length(path1);
  if (l == 0)
    return path2;

  b = scm_string_char_eq_P(path1, l - 1, '/');
  if (scm_obj_null_p(b)) return SCM_OBJ_NULL;

  if (scm_fcd_true_p(b)) {
    return scm_fcd_format_cstr("~a~a", path1, path2);
  }
  else {
    return scm_fcd_format_cstr("~a/~a", path1, path2);
  }
}

static ScmObj
scm_make_file_path_if_exists(ScmObj dir, ScmObj name)
{
  const char const * const tbl[] = { "~a.scm", NULL};
  ScmObj base = SCM_OBJ_INIT, p = SCM_OBJ_INIT;
  bool exists;
  int r;

  SCM_REFSTK_INIT_REG(&dir, &name,
                      &base, &p);

  scm_assert(scm_obj_null_p(dir) || scm_fcd_string_p(dir));
  scm_assert(scm_fcd_string_p(name));

  if (scm_obj_null_p(dir)) {
    base = name;
  }
  else {
    base = scm_file_path_join(dir, name);
    if (scm_obj_null_p(base)) return SCM_OBJ_NULL;
  }

  for (size_t i = 0; tbl[i] != NULL; i++) {
    p = scm_fcd_format_cstr(tbl[i], base);
    if (scm_obj_null_p(p)) return SCM_OBJ_NULL;

    r = scm_fcd_file_exists(p, &exists);
    if (r < 0) return SCM_OBJ_NULL;

    if (exists)
      return p;
  }

  r = scm_fcd_file_exists(base, &exists);
  if (r < 0) return SCM_OBJ_NULL;

  return (exists ? base : SCM_FALSE_OBJ);
}

static ScmObj
scm_search_load_file(ScmObj name, ScmObj paths)
{
  ScmObj lst = SCM_OBJ_INIT, p = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name, &paths,
                      &lst, &p);

  scm_assert(scm_fcd_string_p(name));
  scm_assert(scm_fcd_nil_p(paths) || scm_fcd_pair_p(paths));

  for (lst = paths; scm_fcd_pair_p(lst); lst = scm_fcd_cdr(lst)) {
    p = scm_make_file_path_if_exists(scm_fcd_car(lst), name);
    if (scm_obj_null_p(p)) return SCM_OBJ_NULL;

    if (scm_fcd_true_p(p))
      return p;
  }

  if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;

  return SCM_FALSE_OBJ;
}

static ScmObj
scm_use_load_path_P(ScmObj name)
{
  ScmObj (*tbl[])(ScmObj) = {
    scm_absolute_path_P, scm_starts_with_pairent_dir_or_current_dir_P,
    NULL,
  };
  ScmObj b = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &b);

  scm_assert(scm_fcd_string_p(name));

  for (size_t i = 0; tbl[i] != NULL; i++) {
    b = tbl[i](name);
    if (scm_obj_null_p(b)) return SCM_OBJ_NULL;

    if (scm_fcd_true_p(b))
      return SCM_FALSE_OBJ;
  }

  return SCM_TRUE_OBJ;
}

ScmObj
scm_fcd_search_load_file(ScmObj name)
{
  ScmObj paths = SCM_OBJ_INIT, b = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&name,
                      &paths, &b);

  scm_assert(scm_fcd_string_p(name));

  if (scm_fcd_string_length(name) == 0)
    return SCM_FALSE_OBJ;

  b = scm_use_load_path_P(name);
  if (scm_obj_null_p(b)) return SCM_OBJ_NULL;

  if (scm_fcd_false_p(b))
    return scm_make_file_path_if_exists(SCM_OBJ_NULL, name);

  r = scm_fcd_cached_global_var_ref(SCM_CACHED_GV_LOAD_PATH,
                                    SCM_CSETTER_L(paths));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(paths)) {
    scm_fcd_error("unbound variable: " SCM_LOAD_PATH_VARIABLE_NAME, 0);
    return SCM_OBJ_NULL;
  }

  return scm_search_load_file(name, paths);
}

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
