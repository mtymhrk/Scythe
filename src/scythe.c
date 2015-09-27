#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/vm.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/assembler.h"
#include "scythe/file.h"
#include "scythe/exception.h"
#include "scythe/module.h"
#include "scythe/pair.h"
#include "scythe/string.h"
#include "scythe/encoding.h"
#include "scythe/symbol.h"
#include "scythe/vector.h"
#include "scythe/core_modules.h"
#include "scythe/earray.h"
#include "scythe/config.h"
#include "scythe/scythe.h"

#define DEFAULT_SYS_ENC SCM_ENC_UTF8

static char default_ext_enc[64] = "";

int
scm_scythe_initialize(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  scy->stat = SCM_SCYTHE_S_DOWN;
  scy->bedrock = NULL;
  scy->vm = SCM_OBJ_NULL;
  scy->refstack = SCM_OBJ_NULL;

  eary_init(&scy->conf.gconf.load_path, sizeof(char *), 0);
  eary_init(&scy->conf.gconf.load_suffixes, sizeof(char *), 0);

  scy->conf.gconf.system_encoding = DEFAULT_SYS_ENC;
  scy->conf.gconf.external_encoding = default_ext_enc;

  return 0;
}

void
scm_scythe_finalize(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  scm_scythe_shutdown(scy);

  scm_scythe_clear_load_path(scy);
  eary_fin(&scy->conf.gconf.load_path);

  scm_scythe_clear_load_suffix(scy);
  eary_fin(&scy->conf.gconf.load_suffixes);

  scm_scythe_clear_system_encoding(scy);
  scm_scythe_clear_external_encoding(scy);
}

ScmScythe *
scm_scythe_new()
{
  ScmScythe *scy;
  int r;

  scy = malloc(sizeof(ScmScythe));
  if (scy == NULL) return NULL;

  r = scm_scythe_initialize(scy);
  if (r < 0) return NULL;

  return scy;
}

void
scm_scythe_end(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  scm_scythe_finalize(scy);
  free(scy);
}

void
scm_scythe_switch(ScmScythe *scy)
{
  if (scy == NULL) {
    scm_chg_current_vm(SCM_OBJ_NULL);
    scm_chg_current_ref_stack(SCM_OBJ_NULL);
    scm_chg_current_br(NULL);
  }
  else {
    scm_chg_current_vm(scy->vm);
    scm_chg_current_ref_stack(scy->refstack);
    scm_chg_current_br(scy->bedrock);
  }
}

#define WITH_SCYTHE(scy)                                              \
  do {                                                                \
    ScmObj save_current__v = scm_current_vm();                        \
    ScmObj save_current__s = scm_current_ref_stack();                 \
    ScmBedrock *save_current__b = scm_current_br();                   \
    scm_scythe_switch(scy);                                           \
    do

#define WITH_SCYTHE_END                                         \
    while(0);                                                   \
    scm_chg_current_vm(save_current__v);                        \
    scm_chg_current_ref_stack(save_current__s);                 \
    scm_chg_current_br(save_current__b);                        \
  } while(0)

int
scm_scythe_bootup(ScmScythe *scy)
{
  int r, retval;

  scm_assert(scy != NULL);

  if (scy->stat == SCM_SCYTHE_S_UP)
    return 0;

  retval = -1;
  WITH_SCYTHE(NULL) {
    scy->stat = SCM_SCYTHE_S_UP;

    scy->bedrock = scm_bedrock_new(stderr, &scy->conf.gconf);
    if (scy->bedrock == NULL) break;

    scm_chg_current_br(scy->bedrock);

    r = scm_bedrock_create_mem(scy->bedrock);
    if (r < 0) break;

    scy->refstack = scm_ref_stack_new(SCM_MEM_ROOT);
    if (scm_obj_null_p(scy->refstack)) break;

    scm_chg_current_ref_stack(scy->refstack);

    r = scm_bedrock_setup(scy->bedrock);
    if (r < 0) break;

    scy->vm = scm_vm_new();
    if (scm_obj_null_p(scy->vm)) break;

    retval = 0;
  } WITH_SCYTHE_END;

  return retval;
}

void
scm_scythe_shutdown(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  if (scy->stat != SCM_SCYTHE_S_UP)
    return;

  WITH_SCYTHE(scy) {
    scm_gc_start();

    if (scm_obj_not_null_p(scy->vm)) {
      scm_chg_current_vm(SCM_OBJ_NULL);
      scm_vm_end(scy->vm);
      scy->vm = SCM_OBJ_NULL;
    }

    if (scy->bedrock != NULL)
      scm_bedrock_cleanup(scy->bedrock);

    if (scm_obj_not_null_p(scy->refstack)) {
      scm_chg_current_ref_stack(SCM_OBJ_NULL);
      scm_free_root(scy->refstack);
      scy->refstack = SCM_OBJ_NULL;
    }

    if (scy->bedrock != NULL) {
      scm_bedrock_delete_mem(scy->bedrock);

      scm_chg_current_br(NULL);
      scm_bedrock_end(scy->bedrock);
      scy->bedrock = NULL;
    }

    scy->stat = SCM_SCYTHE_S_DOWN;
  } WITH_SCYTHE_END;
}

int
scm_scythe_add_load_path(ScmScythe *scy, const char *path)
{
  char *p;
  int r;

  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return 0;

  p = strdup(path);
  if (p == NULL) return -1;

  EARY_PUSH(&scy->conf.gconf.load_path, char *, p, r);
  if (r < 0) return -1;

  return 0;
}

void
scm_scythe_clear_load_path(ScmScythe *scy)
{
  size_t idx;
  char **ptr;

  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return;

  EARY_FOR_EACH(&scy->conf.gconf.load_path, idx, ptr)
    free(*ptr);

  eary_truncate(&scy->conf.gconf.load_path);
}

int
scm_scythe_add_load_suffix(ScmScythe *scy, const char *suffix)
{
  char *p;
  int r;

  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return 0;

  p = strdup(suffix);
  if (p == NULL) return -1;

  EARY_PUSH(&scy->conf.gconf.load_suffixes, char *, p, r);
  if (r < 0) return -1;

  return 0;
}

void
scm_scythe_clear_load_suffix(ScmScythe *scy)
{
  size_t idx;
  char **ptr;

  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return;

  EARY_FOR_EACH(&scy->conf.gconf.load_suffixes, idx, ptr)
    free(*ptr);

  eary_truncate(&scy->conf.gconf.load_suffixes);
}

int
scm_scythe_set_system_encoding(ScmScythe *scy, const char *enc)
{
  ScmEncoding *e;

  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return 0;

  e = scm_enc_find_enc(enc);
  if (e == NULL) return -1;

  scy->conf.gconf.system_encoding = e;
  return 0;
}

void
scm_scythe_clear_system_encoding(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return;

  scy->conf.gconf.system_encoding = DEFAULT_SYS_ENC;
}

int
scm_scythe_set_external_encoding(ScmScythe *scy, const char *enc)
{
  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return 0;

  scm_scythe_clear_external_encoding(scy);
  scy->conf.gconf.external_encoding = strdup(enc);
  if (scy->conf.gconf.external_encoding == NULL)
    return -1;

  return 0;
}

void
scm_scythe_clear_external_encoding(ScmScythe *scy)
{
  scm_assert(scy != NULL);

  if (!scm_scythe_conf_modifiable_p(scy))
    return;

  if (scy->conf.gconf.external_encoding == NULL
      || scy->conf.gconf.external_encoding == default_ext_enc)
    return;

  free(scy->conf.gconf.external_encoding);
  scy->conf.gconf.external_encoding = default_ext_enc;
}

int
scm_scythe_default_setup(ScmScythe *scy)
{
  static const char *sfx[] = { ".scm", ".sld", NULL };
  int r;

  scm_assert(scy != NULL);

  scm_scythe_clear_load_path(scy);
  scm_scythe_clear_load_suffix(scy);
  scm_scythe_clear_system_encoding(scy);
  scm_scythe_clear_external_encoding(scy);

  r = scm_scythe_add_load_path(scy, SCYTHE_LIB_DIR);
  if (r < 0) return -1;

  for (const char **p = sfx; *p != NULL; p++) {
    r = scm_scythe_add_load_suffix(scy, *p);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_scythe_load_core(ScmScythe *scy)
{
  int r, retval;

  scm_assert(scy != NULL);

  retval = -1;
  WITH_SCYTHE(scy) {
    r = scm_load_core_modules();
    if (r < 0) goto dsp;

    retval = 0;
    break;

  dsp:
    scm_vm_disposal_unhandled_exc(scy->vm);
  } WITH_SCYTHE_END;

  return retval;
}

static ScmObj
get_proc(const char *name, const char * const *module, size_t n)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, mod_name = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&sym, &mod, &mod_name,
                      &proc, &o);

  mod_name = SCM_NIL_OBJ;
  for (size_t i = n; i > 0; i--) {
    o = scm_make_symbol_from_cstr(module[i - 1], SCM_ENC_SRC);
    if (scm_obj_null_p(o)) return SCM_OBJ_NULL;

    mod_name = scm_cons(o, mod_name);
    if (scm_obj_null_p(mod_name)) return SCM_OBJ_NULL;
  }

  sym = scm_make_symbol_from_cstr(name, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  r = scm_find_module(mod_name, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_error("failed to find module", 1, mod_name);
    return SCM_OBJ_NULL;
  }

  r = scm_refer_global_var_cstr(module, n, name, SCM_CSETTER_L(proc));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(proc)) {
    scm_error("unbund variable", 1, sym);
    return SCM_OBJ_NULL;
  }

  return proc;
}

int
scm_scythe_apply(ScmScythe *scy,
                 const char *cmd, const char * const *args, size_t n)
{
  ScmObj proc = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  scm_assert(scy != NULL);
  scm_assert(cmd != NULL);
  scm_assert(n == 0 || args != NULL);

  WITH_SCYTHE(scy) {
    SCM_REFSTK_INIT_REG(&proc, &lst, &str);

    lst = SCM_NIL_OBJ;
    for (size_t i = n; i > 0; i--) {
      str = scm_make_string_from_external(args[i - 1],
                                          strlen(args[i - 1]), NULL);
      if (scm_obj_null_p(str)) goto dsp;

      lst = scm_cons(str, lst);
      if (scm_obj_null_p(str)) goto dsp;
    }

    proc = get_proc(cmd, (const char *[]){"scythe", "internal", "command"}, 3);
    if(scm_obj_null_p(proc)) goto dsp;

    scm_vm_apply(scy->vm, proc, lst);

  dsp:
    scm_vm_disposal_unhandled_exc(scy->vm);

  } WITH_SCYTHE_END;

  return 0;
}

int
scm_prepare_scythe(void)
{
  static bool prepared = false;

  if (prepared)
    return 0;

  prepared = true;
  scm_enc_locale_to_enc_name(default_ext_enc, sizeof(default_ext_enc));
  scm_prepare_vm();

  return 0;
}
