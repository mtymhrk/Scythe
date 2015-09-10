#ifndef INCLUDE_FCD_VM_H__
#define INCLUDE_FCD_VM_H__

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/vminst.h"

typedef struct ScmBedrockRec ScmBedrock;

extern ScmBedrock *scm_fcd__current_br;
extern ScmObj scm_fcd__current_vm;
extern ScmObj scm_fcd__current_ref_stack;

static inline ScmBedrock *
scm_fcd_current_br(void)
{
  return scm_fcd__current_br;
}

static inline ScmObj
scm_fcd_current_vm(void)
{
  return scm_fcd__current_vm;
}

static inline ScmObj
scm_fcd_current_ref_stack(void)
{
  return scm_fcd__current_ref_stack;
}

static inline void
scm_fcd_chg_current_br(ScmBedrock *br)
{
  scm_fcd__current_br = br;
}

static inline void
scm_fcd_chg_current_vm(ScmObj vm)
{
  scm_fcd__current_vm = vm;
}

static inline void
scm_fcd_chg_current_ref_stack(ScmObj stack)
{
  scm_fcd__current_ref_stack = stack;
}

ScmBedrock *scm_fcd_bedrock_new(void);
void scm_fcd_bedrock_end(ScmBedrock *br);
int scm_fcd_bedrock_create_mem(ScmBedrock *br);
void scm_fcd_bedrock_delete_mem(ScmBedrock *br);
int scm_fcd_bedrock_setup(ScmBedrock *br);
void scm_fcd_bedrock_cleanup(ScmBedrock *br);

ScmObj scm_fcd_nil(void);
ScmObj scm_fcd_true(void);
ScmObj scm_fcd_false(void);
ScmObj scm_fcd_eof(void);
ScmObj scm_fcd_undef(void);
ScmObj scm_fcd_landmine(void);

#define SCM_NIL_OBJ scm_fcd_nil()
#define SCM_TRUE_OBJ scm_fcd_true()
#define SCM_FALSE_OBJ scm_fcd_false()
#define SCM_EOF_OBJ scm_fcd_eof()
#define SCM_UNDEF_OBJ scm_fcd_undef()
#define SCM_LANDMINE_OBJ scm_fcd_landmine()
#define SCM_UNINIT_OBJ scm_fcd_landmine()

void scm_fcd_fatal(const char *msg);
void scm_fcd_fatalf(const char *fmt, ...);
bool scm_fcd_fatal_p(void);

int scm_fcd_halt(void);

enum {
  SCM_CACHED_GV_COMPILE = 0,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
  SCM_CACHED_GV_LOAD_PATH,
};

#define SCM_CACHED_GV_NR 5

enum {
  SCM_CACHED_SYM_QUOTE = 0,
  SCM_CACHED_SYM_QUASIQUOTE,
  SCM_CACHED_SYM_UNQUOTE,
  SCM_CACHED_SYM_UNQUOTE_SPLICING,
};

#define SCM_CACHED_SYM_NR 4

int scm_fcd_cached_global_var_ref(int kind, scm_csetter_t *val);
int scm_fcd_cached_global_var_set(int kind, ScmObj val);
ScmObj scm_fcd_cached_symbol(int kind);

ScmEncoding *scm_fcd_system_encoding(void);

void *scm_fcd_current_memory_manager(void);
ScmObj scm_fcd_current_symbol_table(void);
ScmObj scm_fcd_current_module_tree(void);

bool scm_fcd_vm_p(ScmObj obj);
ScmObj scm_fcd_vm_new();
void scm_fcd_vm_end(ScmObj vm);
ScmObj scm_fcd_vm_apply(ScmObj vm, ScmObj proc, ScmObj args);
ScmObj scm_fcd_vm_run_cloned(ScmObj vm, ScmObj iseq);
void scm_fcd_vm_disposal_unhandled_exc(ScmObj vm);

int scm_fcd_return_val(const ScmObj *val, int vc);

ScmObj scm_fcd_capture_continuation(void);
int scm_fcd_reinstantemnet_continuation(ScmObj cc);
int scm_fcd_push_dynamic_bindings(ScmObj alist);
void scm_fcd_pop_dynamic_bindings(void);
ScmObj scm_fcd_parameter_value(ScmObj var);

int scm_fcd_trampolining(ScmObj proc, ScmObj args,
                         ScmObj postproc, ScmObj handover);

void scm_fcd_exit(ScmObj obj);

int scm_fcd_raise(ScmObj obj);
int scm_fcd_raise_continuable(ScmObj obj);
bool scm_fcd_raised_p(void);
ScmObj scm_fcd_raised_obj(void);
void scm_fcd_discard_raised_obj(void);
int scm_fcd_push_exception_handler(ScmObj handler);
int scm_fcd_pop_exception_handler(void);
void scm_fcd_disposal_unhandled_exec(void);
int scm_fcd_push_dynamic_wind_handler(ScmObj before, ScmObj after);
int scm_fcd_pop_dynamic_wind_handler(void);
ScmObj scm_fcd_collect_dynamic_wind_handler(ScmObj contcap);

scm_opcode_t scm_fcd_internal_opcode(scm_opcode_t op);
scm_opcode_t scm_fcd_external_opcode(scm_opcode_t op);

int scm_fcd_load_iseq(ScmObj iseq);

#endif  /* INCLUDE_FCD_VM_H__ */
