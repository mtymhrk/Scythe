#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

typedef struct ScmProcedureRec ScmProcedure;

enum {
  SCM_PROC_ADJ_UNWISHED = 0x0001,
};

struct ScmProcedureRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj env;
  int arity;
  unsigned int flags;
};

#define SCM_PROCEDURE(obj) ((ScmProcedure *)(obj))

ScmObj scm_procedure_P(ScmObj proc);
int scm_proc_initialize(ScmObj proc, ScmObj name,
                        int arity, unsigned int flags, ScmObj env);
void scm_proc_gc_initialize(ScmObj obj);
int scm_proc_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_procedure_p(ScmObj proc)
{
  return scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC);
}

static inline int
scm_proc_arity(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->arity;
}

static inline ScmObj
scm_proc_name(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->name;
}

static inline ScmObj
scm_proc_env(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->env;
}

static inline ScmObj
scm_proc_flg_set_p(ScmObj proc, unsigned int flg)
{
  return (SCM_PROCEDURE(proc)->flags & flg) ? true : false;
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

typedef struct ScmSubrutineRec ScmSubrutine;
typedef int (*ScmSubrFunc)(ScmObj subr, int argc, const ScmObj *argv);

struct ScmSubrutineRec {
  ScmProcedure proc;
  ScmSubrFunc subr_func;
};

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

int scm_subrutine_initialize(ScmObj subr,  ScmSubrFunc func,
                             ScmObj name, int arity, unsigned int flags,
                             ScmObj env);
ScmObj scm_subrutine_new(scm_mem_type_t mtype,
                         ScmSubrFunc func, ScmObj name, int arity,
                         unsigned int flags, ScmObj env);
int scm_subrutine_obj_print(ScmObj obj, ScmObj port, int kind,
                            ScmObjPrintHandler handler);
void scm_subrutine_gc_initialize(ScmObj obj);
int scm_subrutine_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_subrutine_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_SUBRUTINE_TYPE_INFO);
}

static inline ScmObj
scm_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags, ScmObj env)
{
  scm_assert(func != NULL);

  return scm_subrutine_new(SCM_MEM_HEAP, func, SCM_OBJ_NULL, arity, flags, env);
}

static inline ScmSubrFunc
scm_subrutine_func(ScmObj subr)
{
  scm_assert(scm_subrutine_p(subr));
  return SCM_SUBRUTINE(subr)->subr_func;
}

static inline ScmObj
scm_subrutine_env(ScmObj subr)
{
  scm_assert(scm_subrutine_p(subr));
  return scm_proc_env(subr);
}

static inline int
scm_subrutine_call(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_assert(scm_subrutine_p(subr));
  return SCM_SUBRUTINE(subr)->subr_func(subr, argc, argv);
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

typedef struct ScmClosureRec ScmClosure;

struct ScmClosureRec {
  ScmProcedure proc;
  ScmObj iseq;
};

#define SCM_CLOSURE(obj) ((ScmClosure *)(obj))

extern ScmTypeInfo SCM_CLOSURE_TYPE_INFO;

int scm_closure_initialize(ScmObj clsr,
                           ScmObj iseq, ScmObj env, ScmObj name, int arity);
ScmObj scm_closure_new(scm_mem_type_t mtype,
                       ScmObj iseq, ScmObj env, ScmObj name, int arity);
scm_byte_t *scm_closure_to_ip(ScmObj clsr);
void scm_closure_gc_initialize(ScmObj obj);
int scm_closure_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_closure_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_CLOSURE_TYPE_INFO);
}

static inline ScmObj
scm_make_closure(ScmObj iseq, ScmObj env, int arity)
{
  return scm_closure_new(SCM_MEM_HEAP, iseq, env, SCM_OBJ_NULL, arity);
}

static inline ScmObj
scm_closure_iseq(ScmObj clsr)
{
  scm_assert(scm_closure_p(clsr));
  return SCM_CLOSURE(clsr)->iseq;
}

static inline ScmObj
scm_closure_env(ScmObj clsr)
{
  scm_assert(scm_closure_p(clsr));
  return scm_proc_env(clsr);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

typedef struct ScmDWHCallerEnvRec ScmDWHCallerEnv;

struct ScmDWHCallerEnvRec {
  ScmObjHeader header;
  ScmObj cont;
  size_t vc;
  ScmObj *val;
};

#define SCM_DWHCALLERENV(obj) ((ScmDWHCallerEnv *)(obj))

extern ScmTypeInfo SCM_DWHCALLERENV_TYPE_INFO;

int scm_dwhcallerenv_initialize(ScmObj dwhce,
                                ScmObj cont, const ScmObj *val, size_t vc);
void scm_dwhcallerenv_finalize(ScmObj dwhce);
ScmObj scm_dwhcallerenv_new(scm_mem_type_t mtype,
                            ScmObj cont, const ScmObj *val, size_t vc);
void scm_dwhcallerenv_gc_initialize(ScmObj obj);
void scm_dwhcallerenv_gc_finalize(ScmObj obj);
int scm_dwhcallerenv_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_dwhcallerenv_cont(ScmObj dwhce)
{
  scm_assert_obj_type(dwhce, &SCM_DWHCALLERENV_TYPE_INFO);

  return SCM_DWHCALLERENV(dwhce)->cont;
}

static inline size_t
scm_dwhcallerenv_vc(ScmObj dwhce)
{
  scm_assert_obj_type(dwhce, &SCM_DWHCALLERENV_TYPE_INFO);

  return SCM_DWHCALLERENV(dwhce)->vc;
}

static inline const ScmObj *
scm_dwhcallerenv_val(ScmObj dwhce)
{
  scm_assert_obj_type(dwhce, &SCM_DWHCALLERENV_TYPE_INFO);

  return SCM_DWHCALLERENV(dwhce)->val;
}

int scm_subr_func_continuation(ScmObj subr, int argc, const ScmObj *argv);
ScmObj scm_continuation_new(scm_mem_type_t mtype, ScmObj contcap);
ScmObj scm_make_continuation(void);

static inline bool
scm_continuation_p(ScmObj obj)
{
  return (scm_subrutine_p(obj)
          && scm_subrutine_func(obj) == scm_subr_func_continuation);
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

int scm_subr_func_parameter(ScmObj subr, int argc, const ScmObj *argv);
ScmObj scm_parameter_new(scm_mem_type_t mtype, ScmObj init, ScmObj conv);
ScmObj scm_parameter_init_val(ScmObj prm);
ScmObj scm_parameter_converter(ScmObj prm);
void scm_parameter_set_init_val(ScmObj prm, ScmObj val);

static inline bool
scm_parameter_p(ScmObj obj)
{
  return (scm_subrutine_p(obj)
          && scm_subrutine_func(obj) == scm_subr_func_parameter);
}

static inline ScmObj
scm_make_parameter(ScmObj init, ScmObj conv)
{
  return scm_parameter_new(SCM_MEM_HEAP, init, conv);
}


#endif /* INCLUDE_PROCEDURE_H__ */
