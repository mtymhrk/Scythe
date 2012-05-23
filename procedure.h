#ifndef INCLUDE_PROCEDURE_H__
#define INCLUDE_PROCEDURE_H__

typedef struct ScmSubrutineRec ScmSubrutine;

#define SCM_SUBRUTINE(obj) ((ScmSubrutine *)(obj))

typedef struct ScmClosureRec ScmClosure;

#define SCM_CLOSURE(obj) ((ScmClosure *)(obj))

#include "object.h"
#include "api_enum.h"


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

extern ScmTypeInfo SCM_SUBRUTINE_TYPE_INFO;

struct ScmSubrutineRec {
  ScmObjHeader header;
  ScmSubrFunc subr_func;
};

int scm_subrutine_initialize(ScmObj subr, ScmSubrFunc func);
ScmObj scm_subrutine_new(SCM_MEM_TYPE_T mtype, ScmSubrFunc func);
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
  ScmObjHeader header;
  ScmObj iseq;
  ScmObj *free_vars;
  size_t nr_free_vars;
};

int scm_closure_initialize(ScmObj clsr, ScmObj iseq,
                           size_t nr_free_vars, ScmObj *sp);
void scm_closure_finalize(ScmObj clsr);
ScmObj scm_closure_new(SCM_MEM_TYPE_T mtype, ScmObj iseq,
                       size_t nr_free_vars, ScmObj *sp);
int scm_closure_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_closure_gc_initialize(ScmObj obj, ScmObj mem);
void scm_closure_gc_finalize(ScmObj obj);
int scm_closure_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_closure_body(ScmObj clsr)
{
  scm_assert_obj_type(clsr, &SCM_CLOSURE_TYPE_INFO);

  return SCM_CLOSURE(clsr)->iseq;
}

#endif /* INCLUDE_PROCEDURE_H__ */
