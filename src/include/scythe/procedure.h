#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmProcedureRec ScmProcedure;
typedef struct ScmSubrutineRec ScmSubrutine;
typedef struct ScmClosureRec ScmClosure;

#define SCM_PROCEDURE(obj) ((ScmProcedure *)(obj))
#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))
#define SCM_CLOSURE(obj) ((ScmClosure *)(obj))


#include "scythe/object.h"
#include "scythe/fcd_type.h"


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

struct ScmProcedureRec {
  ScmObjHeader header;
  ScmObj name;
  ScmObj env;
  int arity;
  unsigned int flags;
};

int scm_proc_initialize(ScmObj proc, ScmObj name,
                        int arity, unsigned int flags, ScmObj env);
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
scm_proc_env(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->env;
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
};

int scm_subrutine_initialize(ScmObj subr,  ScmSubrFunc func,
                             ScmObj name, int arity, unsigned int flags,
                             ScmObj env);
int scm_subrutine_obj_print(ScmObj obj, ScmObj port, int kind,
                            ScmObjPrintHandler handler);
void scm_subrutine_gc_initialize(ScmObj obj, ScmObj mem);
int scm_subrutine_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmSubrFunc
scm_subrutine_func(ScmObj subr)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->subr_func;
}

static inline int
scm_subrutine_call(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->subr_func(subr, argc, argv);
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

extern ScmTypeInfo SCM_CLOSURE_TYPE_INFO;

struct ScmClosureRec {
  ScmProcedure proc;
  ScmObj iseq;
};

int scm_closure_initialize(ScmObj clsr,
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

  return scm_proc_env(clsr);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

int scm_subr_func_continuation(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

int scm_subr_func_parameter(ScmObj subr, int argc, const ScmObj *argv);


#endif /* INCLUDE_PROCEDURE_H__ */
