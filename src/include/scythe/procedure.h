#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmProcedureRec ScmProcedure;
typedef struct ScmSubrutineRec ScmSubrutine;
typedef struct ScmClosureRec ScmClosure;
typedef struct ScmContinuationRec ScmContinuation;
typedef struct ScmParameterRec ScmParameter;

#define SCM_PROCEDURE(obj) ((ScmProcedure *)(obj))
#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))
#define SCM_CLOSURE(obj) ((ScmClosure *)(obj))
#define SCM_CONT(obj) ((ScmContinuation *)(obj))
#define SCM_PARAMETER(obj) ((ScmParameter *)(obj))


#include "scythe/object.h"
#include "scythe/api_type.h"


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

struct ScmProcedureRec {
  ScmObjHeader header;
  ScmObj name;
  int arity;
  unsigned int flags;
};

int scm_proc_initialize(ScmObj proc,
                        ScmObj name, int arity, unsigned int flags);
void scm_proc_gc_initialize(ScmObj obj, ScmObj mem);
int scm_proc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

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
scm_proc_flg_set_p(ScmObj proc, SCM_PROC_FLG_T flg)
{
  return (SCM_PROCEDURE(proc)->flags & flg) ? true : false;
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmProcedure proc;
  ScmSubrFunc subr_func;
  ScmObj module;
};

int scm_subrutine_initialize(ScmObj subr,  ScmSubrFunc func,
                             ScmObj name, int arity, unsigned int flags,
                             ScmObj module);
ScmObj scm_subrutine_new(SCM_MEM_TYPE_T mtype, ScmSubrFunc func,
                         ScmObj name, int arity, unsigned int flags,
                         ScmObj module);
int scm_subrutine_obj_print(ScmObj obj, ScmObj port, bool ext_rep);
void scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem);
int scm_subrutine_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

static inline int
scm_subrutine_call(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->subr_func(subr, argc, argv);
}

static inline ScmObj
scm_subrutine_module(ScmObj subr)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->module;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

extern ScmTypeInfo SCM_CLOSURE_TYPE_INFO;

struct ScmClosureRec {
  ScmProcedure proc;
  ScmObj iseq;
  ScmObj env;
};

int scm_closure_initialize(ScmObj clsr,
                           ScmObj iseq, ScmObj env, ScmObj name, int arity);
ScmObj scm_closure_new(SCM_MEM_TYPE_T mtype,
                       ScmObj iseq, ScmObj env, ScmObj name, int arity);
void scm_closure_gc_initialize(ScmObj obj, ScmObj mem);
int scm_closure_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmObj
scm_closure_body(ScmObj clsr)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);

  return SCM_CLOSURE(clsr)->iseq;
}

static inline ScmObj
scm_closure_env(ScmObj clsr)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);

  return SCM_CLOSURE(clsr)->env;
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

extern ScmTypeInfo SCM_CONTINUATION_TYPE_INFO;

struct ScmContinuationRec {
  ScmProcedure proc;
  ScmObj contcap;
};

int scm_cont_initialize(ScmObj cont, ScmObj contcap);
ScmObj scm_cont_new(SCM_MEM_TYPE_T mtype, ScmObj contcap);
void scm_cont_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cont_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmObj
scm_cont_content(ScmObj cont)
{
  scm_assert_obj_type(cont, &SCM_CONTINUATION_TYPE_INFO);

  return SCM_CONT(cont)->contcap;
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

extern ScmTypeInfo SCM_PARAMETER_TYPE_INFO;

struct ScmParameterRec {
  ScmProcedure proc;
  ScmObj init;
  ScmObj conv;
};

int scm_parameter_initialize(ScmObj prm, ScmObj name, ScmObj conv);
ScmObj scm_parameter_new(SCM_MEM_TYPE_T mtype, ScmObj name, ScmObj conv);

void scm_parameter_gc_initialize(ScmObj obj, ScmObj mem);
int scm_parameter_gc_accept(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler);


static inline ScmObj
scm_parameter_init_val(ScmObj prm)
{
  scm_assert_obj_type(prm, &SCM_PARAMETER_TYPE_INFO);

  return SCM_PARAMETER(prm)->init;
}

static inline ScmObj
scm_parameter_converter(ScmObj prm)
{
  scm_assert_obj_type(prm, &SCM_PARAMETER_TYPE_INFO);

  return SCM_PARAMETER(prm)->conv;
}

static inline void
scm_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  scm_assert_obj_type(prm, &SCM_PARAMETER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  SCM_SLOT_SETQ(ScmParameter, prm, init, val);
}

#endif /* INCLUDE_PROCEDURE_H__ */
