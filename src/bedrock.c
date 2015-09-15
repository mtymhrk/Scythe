#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/core_subr.h"
#include "scythe/exception.h"
#include "scythe/file.h"
#include "scythe/miscobjects.h"
#include "scythe/module.h"
#include "scythe/pair.h"
#include "scythe/procedure.h"
#include "scythe/symbol.h"
#include "scythe/vm.h"
#include "scythe/bedrock.h"

#define SCM_BEDROCK_ERR_MSG_SIZE 256

static void
scm_bedrock_print_msg(ScmBedrock *br, const char *msg)
{
  size_t len;

  scm_assert(br != NULL);
  scm_assert(msg != NULL);

  fputs(msg, br->output);

  len = strlen(msg);
  if (msg[len - 1] != '\n')
    fputc('\n', br->output);
}

static int
scm_bedrock_make_premade_proc(ScmBedrock *br)
{
  static const struct {
    ScmSubrFunc func;
    int arity;
    unsigned int flags;
  } tbl[] = {
    { scm_vm_subr_exc_hndlr_caller, -2, 0 },
    { scm_vm_subr_exc_hndlr_caller_cont, 1, 0 },
    { scm_vm_subr_exc_hndlr_caller_post, -2, SCM_PROC_ADJ_UNWISHED },
    { scm_vm_subr_trmp_apply, -3 , 0 },
    { scm_subr_func_eval__post_compile,
      SCM_SUBR_ARITY_EVAL__POST_COMPILE, SCM_SUBR_FLAG_EVAL__POST_COMPILE },
    { scm_subr_func_eval_file__loop,
      SCM_SUBR_ARITY_EVAL_FILE__LOOP, SCM_SUBR_FLAG_EVAL_FILE__LOOP },
  };

  scm_assert(br != NULL);

  for (size_t i = 0; i < SCM_PREMADE_PROC_NR; i++) {
    br->proc[i] = scm_subrutine_new (SCM_MEM_ROOT,
                                     tbl[i].func, SCM_OBJ_NULL,
                                     tbl[i].arity, tbl[i].flags, SCM_OBJ_NULL);
    if (scm_obj_null_p(br->proc[i])) return -1;
  }

  return 0;
}

int
scm_bedrock_setup(ScmBedrock *br)
{
  int r;

  scm_assert(br != NULL);

  br->cnsts.nil = scm_nil_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.nil)) return -1;

  br->cnsts.eof = scm_eof_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.eof)) return -1;

  br->cnsts.b_true = scm_bool_new(SCM_MEM_ROOT, true);
  if (scm_obj_null_p(br->cnsts.b_true)) return -1;

  br->cnsts.b_false = scm_bool_new(SCM_MEM_ROOT, false);
  if (scm_obj_null_p(br->cnsts.b_false)) return -1;

  br->cnsts.undef = scm_undef_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.undef)) return -1;

  br->cnsts.landmine = scm_landmine_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.landmine)) return -1;

  br->symtbl = scm_symtbl_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->symtbl)) return -1;

  br->modtree = scm_moduletree_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->modtree)) return -1;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    scm_register_extra_rfrn(SCM_REF_MAKE(br->gv[i]));

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    scm_register_extra_rfrn(SCM_REF_MAKE(br->sym[i]));

  r = scm_bedrock_make_premade_proc(br);
  if (r < 0) return -1;

  return 0;
}

void
scm_bedrock_cleanup(ScmBedrock *br)
{
  scm_assert(br != NULL);

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    br->sym[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    br->gv[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_PREMADE_PROC_NR; i++) {
    if (scm_obj_not_null_p(br->proc[i])) {
      scm_free_root(br->proc[i]);
      br->proc[i] = SCM_OBJ_NULL;
    }
  }

  if (scm_obj_not_null_p(br->modtree)) {
    scm_free_root(br->modtree);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->symtbl)) {
    scm_free_root(br->symtbl);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.landmine)) {
    scm_free_root(br->cnsts.landmine);
    br->cnsts.landmine = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.undef)) {
    scm_free_root(br->cnsts.undef);
    br->cnsts.undef = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_false)) {
    scm_free_root(br->cnsts.b_false);
    br->cnsts.b_false = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_true)) {
    scm_free_root(br->cnsts.b_true);
    br->cnsts.b_true = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.eof)) {
    scm_free_root(br->cnsts.eof);
    br->cnsts.eof = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.nil)) {
    scm_free_root(br->cnsts.nil);
    br->cnsts.nil = SCM_OBJ_NULL;
  }
}

int
scm_bedrock_create_mem(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if (br->mem != NULL)
    return 0;

  br->mem = scm_mem_new();
  if (br->mem == NULL) return -1;

  return 0;
}

void
scm_bedrock_delete_mem(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if (br->mem == NULL)
    return;

  scm_mem_end(br->mem);
  br->mem = NULL;

  return;
}

int
scm_bedrock_initialize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  br->output = stderr;
  br->err_type = SCM_BEDROCK_ERR_NONE;
  br->exit_stat = 0;

  br->mem = NULL;

  br->cnsts.nil = SCM_OBJ_NULL;
  br->cnsts.eof = SCM_OBJ_NULL;
  br->cnsts.b_true = SCM_OBJ_NULL;
  br->cnsts.b_false = SCM_OBJ_NULL;
  br->cnsts.undef = SCM_OBJ_NULL;
  br->cnsts.landmine = SCM_OBJ_NULL;

  br->symtbl = SCM_OBJ_NULL;
  br->modtree = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_PREMADE_PROC_NR; i++)
    br->proc[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    br->gv[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    br->sym[i] = SCM_OBJ_NULL;

  br->encoding = SCM_ENC_UTF8;

  return 0;
}

void
scm_bedrock_finalize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if (br->mem != NULL)
    scm_bedrock_delete_mem(br);
}

ScmBedrock *
scm_bedrock_new(void)
{
  int rslt;
  ScmBedrock *br;

  br = malloc(sizeof(*br));
  if (br == NULL) return NULL;

  rslt = scm_bedrock_initialize(br);
  if (rslt < 0) {
    free(br);
    return NULL;
  }

  return br;
}

void
scm_bedrock_end(ScmBedrock *br)
{
  scm_assert(br != NULL);

  scm_bedrock_finalize(br);
  free(br);
}


void
scm_bedrock_fatal(ScmBedrock *br, const char *msg)
{
  scm_assert(br != NULL);

  br->err_type = SCM_BEDROCK_ERR_FATAL;

  if (msg != NULL)
    scm_bedrock_print_msg(br, msg);

  if (scm_obj_not_null_p(scm_current_vm()))
    scm_vm_setup_stat_halt(scm_current_vm());
}

void
scm_bedrock_error(ScmBedrock *br, const char *msg)
{
  scm_assert(br != NULL);

  if (br->err_type != SCM_BEDROCK_ERR_FATAL)
    br->err_type = SCM_BEDROCK_ERR_ERROR;

  if (msg != NULL)
    scm_bedrock_print_msg(br, msg);
}

bool
scm_bedrock_fatal_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  return (br->err_type == SCM_BEDROCK_ERR_FATAL) ? true : false;
}

bool
scm_bedrock_error_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if ((br->err_type == SCM_BEDROCK_ERR_FATAL
       || br->err_type == SCM_BEDROCK_ERR_ERROR))
    return true;
  else
    return false;
}

static int
scm_bedrock_search_gv(const char *sym_str, const char * const *name_str, size_t n,
                      scm_csetter_t *gloc)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, name = SCM_OBJ_INIT;
  ScmObj name_sym[n];
  int r;

  for (size_t i = 0; i < n; i++) name_sym[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&sym, &mod, &name);
  SCM_REFSTK_REG_ARY(name_sym, n);

  scm_assert(sym_str != NULL);
  scm_assert(name_str != NULL);
  scm_assert(gloc != NULL);

  for (size_t i = 0; i < n; i++) {
    name_sym[i] = scm_make_symbol_from_cstr(name_str[i], SCM_ENC_SRC);
    if (scm_obj_null_p(name_sym[i])) return -1;
  }

  sym = scm_make_symbol_from_cstr(sym_str, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return -1;

  name = scm_list_cv(name_sym, n);
  if (scm_obj_null_p(name)) return -1;

  r = scm_find_module(name, SCM_CSETTER_L(mod));
  if (r < 0) return -1;

  if (scm_obj_null_p(mod)) {
    scm_csetter_setq(gloc, SCM_OBJ_NULL);
    return 0;
  }

  return scm_module_find_gloc(mod, sym, gloc);
}

static int
scm_bedrock_cached_gv_aux(ScmRef holder,
                          const char *sym, const char * const *name, size_t n,
                          scm_csetter_t *gloc)
{
  int r;

  scm_assert(holder != SCM_REF_NULL);
  scm_assert(sym != NULL);
  scm_assert(name != NULL);
  scm_assert(gloc != NULL);

  if (scm_obj_not_null_p(SCM_REF_DEREF(holder))) {
    scm_csetter_setq(gloc, SCM_REF_DEREF(holder));
    return 0;
  }

  r = scm_bedrock_search_gv(sym, name, n, gloc);
  if (r < 0) return -1;

  SCM_REF_UPDATE(holder, scm_csetter_val(gloc));
  return 0;
}

int
scm_bedrock_cached_gv(ScmBedrock *br, int kind, scm_csetter_t *gloc)
{
  static const struct {
    const char *sym_name;    const char *mod_name[3]; size_t n;
  } tbl[] = {
    { "compile",             { "scythe", "internal", "compile" }, 3 },
    { "eval",                { "scythe", "base" }, 2 },
    { "current-input-port",  { "scythe", "base" }, 2 },
    { "current-output-port", { "scythe", "base" }, 2 },
    { SCM_LOAD_PATH_VARIABLE_NAME, { "scheme", "load" }, 2 },
  };

  scm_assert(br != NULL);
  scm_assert(gloc != NULL);
  scm_assert(kind < SCM_CACHED_GV_NR);

  return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv[kind]),
                                   tbl[kind].sym_name,
                                   tbl[kind].mod_name,
                                   tbl[kind].n,
                                   gloc);
}

ScmObj
scm_bedrock_cached_sym(ScmBedrock *br, int kind)
{
  static const char *tbl[] = {
    "quote", "quasiquote", "unquote", "unquote-splicing",
  };

  scm_assert(br != NULL);
  scm_assert(kind < SCM_CACHED_SYM_NR);

  if (scm_obj_not_null_p(br->sym[kind]))
    return br->sym[kind];

  br->sym[kind] = scm_make_symbol_from_cstr(tbl[kind], SCM_ENC_SRC);
  return br->sym[kind];
}


/***************************************************************************/
/*  Facade                                                                 */
/***************************************************************************/

ScmBedrock *scm__current_br = NULL;

int
scm_cached_global_var_ref(int kind, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&gloc, &v);

  r = scm_bedrock_cached_gv(scm_current_br(), kind, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_gloc_variable_value(gloc);
    if (scm_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else
    v = SCM_OBJ_NULL;

  if (val != NULL)
    scm_csetter_setq(val, v);

  return 0;
}

int
scm_cached_global_var_set(int kind, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&gloc, &v);

  scm_assert(scm_obj_not_null_p(val) && !scm_landmine_object_p(val));

  r = scm_bedrock_cached_gv(scm_current_br(), kind, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_null_p(gloc)
      || scm_landmine_p(scm_gloc_variable_value(gloc))) {
    scm_error("failed to update cached global variable value", 0);
    return -1;
  }

  scm_gloc_bind_variable(gloc, val);
  return 0;
}
