#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmProcedureRec ScmProcedure;

#define SCM_PROCEDURE(obj) ((ScmProcedure *)(obj))

typedef struct ScmSubrutineRec ScmSubrutine;

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

typedef struct ScmClosureRec ScmClosure;

#define SCM_CLOSURE(obj) ((ScmClosure *)(obj))

typedef struct ScmContinuationRec ScmContinuation;

#define SCM_CONT(obj) ((ScmContinuation *)(obj))

#include "object.h"
#include "api_enum.h"
#include "api_type.h"


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

struct ScmProcedureRec {
  ScmObjHeader header;
  ScmObj name;
  int arity;
};

int scm_proc_initialize(ScmObj proc, ScmObj name, int arity);
void scm_proc_gc_initialize(ScmObj obj, ScmObj mem);
int scm_proc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline int
scm_proc_arity(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->arity;
}

inline ScmObj
scm_proc_name(ScmObj proc)
{
  return SCM_PROCEDURE(proc)->name;
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmProcedure proc;
  ScmSubrFunc subr_func;
};

int scm_subrutine_initialize(ScmObj subr,
                             ScmSubrFunc func, ScmObj name, int arity);
ScmObj scm_subrutine_new(SCM_MEM_TYPE_T mtype,
                         ScmSubrFunc func, ScmObj name, int arity);
int scm_subrutine_pretty_print(ScmObj obj, ScmObj port, bool write_p);

inline ScmObj
scm_subrutine_call(ScmObj subr, int argc, ScmObj *argv)
{
  scm_assert_obj_type(subr, &SCM_SUBRUTINE_TYPE_INFO);

  return SCM_SUBRUTINE(subr)->subr_func(argc, argv);
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
int scm_closure_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_closure_gc_initialize(ScmObj obj, ScmObj mem);
int scm_closure_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_closure_body(ScmObj clsr)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);

  return SCM_CLOSURE(clsr)->iseq;
}

inline ScmObj
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
int scm_cont_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_cont_gc_initialize(ScmObj obj, ScmObj mem);
int scm_cont_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_cont_content(ScmObj cont)
{
  scm_assert_obj_type(cont, &SCM_CONTINUATION_TYPE_INFO);

  return SCM_CONT(cont)->contcap;
}


#endif /* INCLUDE_PROCEDURE_H__ */
