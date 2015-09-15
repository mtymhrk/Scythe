#ifndef INCLUDE_BEDROCK_H__
#define INCLUDE_BEDROCK_H__

#include <stdio.h>

#include <scythe/object.h>
#include <scythe/memory.h>
#include <scythe/encoding.h>

typedef struct ScmBedrockRec ScmBedrock;
typedef enum scm_bedrock_err_type scm_bedrock_err_type_t;

enum scm_bedrock_err_type {
  SCM_BEDROCK_ERR_NONE,
  SCM_BEDROCK_ERR_FATAL,
  SCM_BEDROCK_ERR_ERROR,
};

enum {
  SCM_PREMADE_PROC_EXC_HANDLER_CALLER = 0,
  SCM_PREMADE_PROC_EXC_HANDLER_CALLER_CONT,
  SCM_PREMADE_PROC_EXC_HANDLER_CALLER_POST,
  SCM_PREMADE_PROC_TRMP_APPLY,
  SCM_PREMADE_PROC_EVAL__POST_COMPILE,

  SCM_PREMADE_PROC_NR
};

enum {
  SCM_CACHED_GV_COMPILE = 0,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
  SCM_CACHED_GV_LOAD_PATH,

  SCM_CACHED_GV_NR
};

enum {
  SCM_CACHED_SYM_QUOTE = 0,
  SCM_CACHED_SYM_QUASIQUOTE,
  SCM_CACHED_SYM_UNQUOTE,
  SCM_CACHED_SYM_UNQUOTE_SPLICING,

  SCM_CACHED_SYM_NR
};

struct ScmBedrockRec {
  FILE *output;

  /*** Error Status ***/
  scm_bedrock_err_type_t err_type;

  /* Exit Status */
  int exit_stat;

  /*** Constant Values ***/
  struct {
    ScmObj nil;
    ScmObj eof;
    ScmObj b_true;
    ScmObj b_false;
    ScmObj undef;
    ScmObj landmine;
  } cnsts;

  ScmMem *mem;
  ScmObj symtbl;
  ScmObj modtree;
  ScmObj proc[SCM_PREMADE_PROC_NR];
  ScmObj gv[SCM_CACHED_GV_NR];
  ScmObj sym[SCM_CACHED_SYM_NR];

  /*** Configurations ***/
  ScmEncoding *encoding;
};

int scm_bedrock_setup(ScmBedrock *br);
void scm_bedrock_cleanup(ScmBedrock *br);
int scm_bedrock_create_mem(ScmBedrock *br);
void scm_bedrock_delete_mem(ScmBedrock *br);
int scm_bedrock_initialize(ScmBedrock *br);
void scm_bedrock_finalize(ScmBedrock *br);
ScmBedrock *scm_bedrock_new(void);
void scm_bedrock_end(ScmBedrock *br);
void scm_bedrock_fatal(ScmBedrock *br, const char *msg);
void scm_bedrock_error(ScmBedrock *br, const char *msg);
bool scm_bedrock_fatal_p(ScmBedrock *br);
bool scm_bedrock_error_p(ScmBedrock *br);
int scm_bedrock_cached_gv(ScmBedrock *br, int kind, scm_csetter_t *gloc);
ScmObj scm_bedrock_cached_sym(ScmBedrock *br, int kind);

static inline ScmObj
scm_bedrock_nil(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.nil;
}

static inline ScmObj
scm_bedrock_eof(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.eof;
}

static inline ScmObj
scm_bedrock_true(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_true;
}

static inline ScmObj
scm_bedrock_false(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_false;
}

static inline ScmObj
scm_bedrock_undef(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.undef;
}

static inline ScmObj
scm_bedrock_landmine(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.landmine;
}

static inline ScmMem *
scm_bedrock_mem(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->mem;
}

static inline ScmObj
scm_bedrock_symtbl(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->symtbl;
}

static inline ScmObj
scm_bedrock_modtree(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->modtree;
}

static inline ScmObj
scm_bedrock_premade_proc(ScmBedrock *br, int kind)
{
  scm_assert(br != NULL);
  scm_assert(0 <= kind && kind < SCM_PREMADE_PROC_NR);
  return br->proc[kind];
}

static inline ScmEncoding *
scm_bedrock_encoding(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->encoding;
}


/***************************************************************************/
/*  Facade                                                                 */
/***************************************************************************/

extern ScmBedrock *scm__current_br;

static inline ScmBedrock *
scm_current_br(void)
{
  return scm__current_br;
}

static inline void
scm_chg_current_br(ScmBedrock *br)
{
  scm__current_br = br;
}

#define SCM_NIL_OBJ (scm_bedrock_nil(scm_current_br()))
#define SCM_TRUE_OBJ (scm_bedrock_true(scm_current_br()))
#define SCM_FALSE_OBJ (scm_bedrock_false(scm_current_br()))
#define SCM_EOF_OBJ (scm_bedrock_eof(scm_current_br()))
#define SCM_UNDEF_OBJ (scm_bedrock_undef(scm_current_br()))
#define SCM_LANDMINE_OBJ (scm_bedrock_landmine(scm_current_br()))
#define SCM_UNINIT_OBJ SCM_LANDMINE_OBJ

int scm_cached_global_var_ref(int kind, scm_csetter_t *val);
int scm_cached_global_var_set(int kind, ScmObj val);

static inline void
scm_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_current_br(), msg);
}

static inline ScmObj
scm_cached_symbol(int kind)
{
  return scm_bedrock_cached_sym(scm_current_br(), kind);
}

static inline ScmEncoding *
scm_system_encoding(void)
{
  return scm_bedrock_encoding(scm_current_br());
}

static inline void *
scm_current_memory_manager(void)
{
  return scm_bedrock_mem(scm_current_br());
}

static inline ScmObj
scm_current_symbol_table(void)
{
  return scm_bedrock_symtbl(scm_current_br());
}

static inline ScmObj
scm_current_module_tree(void)
{
  return scm_bedrock_modtree(scm_current_br());
}

static inline ScmObj
scm_premade_procedure(int kind)
{
  return scm_bedrock_premade_proc(scm_current_br(), kind);
}


#endif  /* INCLUDE_BEDROCK_H__ */
