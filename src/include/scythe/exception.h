#ifndef INCLUDE_EXCEPTION_H__
#define INCLUDE_EXCEPTION_H__

#include <stddef.h>
#include <limits.h>

typedef struct ScmExceptionRec ScmException;
typedef struct ScmErrorRec ScmError;

#define SCM_EXCEPTION(obj) ((ScmException *)(obj))
#define SCM_ERROR(obj) ((ScmError *)(obj))

#include "scythe/object.h"
#include "scythe/api_type.h"


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

struct ScmExceptionRec {
  ScmObjHeader header;
  ScmObj msg;
};

int scm_exception_initialize(ScmObj exc, ScmObj msg);
void scm_exception_finalize(ScmObj exc);
void scm_exception_gc_initialize(ScmObj obj, ScmObj mem);
int scm_exception_gc_accept(ScmObj obj,
                            ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_exception_msg(ScmObj exc)
{
  scm_assert(scm_obj_type_flag_set_p(exc, SCM_TYPE_FLG_EXC));

  return SCM_EXCEPTION(exc)->msg;
}


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

struct ScmErrorRec {
  ScmException exc;
  ScmObj type;
  ScmObj *irritants;
  size_t nr_irris;
};

extern ScmTypeInfo SCM_ERROR_TYPE_INFO;

#define SCM_ERROR_IRRITANTS_MAX (SIZE_MAX / sizeof(ScmObj))

int scm_error_initialize_cv(ScmObj exc, ScmObj msg,
                            ScmObj type, ScmObj *irris, size_t n);
int scm_error_initialize_lst(ScmObj exc, ScmObj msg, ScmObj type, ScmObj irris);
void scm_error_finalize(ScmObj exc);
ScmObj scm_error_new_cv(SCM_MEM_TYPE_T mtype,
                        ScmObj msg, ScmObj type, ScmObj *irris, size_t n);
ScmObj scm_error_new_lst(SCM_MEM_TYPE_T mtype,
                         ScmObj msg, ScmObj type, ScmObj irris);
ScmObj scm_error_irris_to_list(ScmObj exc);
int scm_error_obj_print(ScmObj obj, ScmObj port, bool ext_rep);
void scm_error_gc_initialize(ScmObj obj, ScmObj mem);
void scm_error_gc_fianlize(ScmObj obj);
int scm_error_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_error_type(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  return SCM_ERROR(exc)->type;
}

inline size_t
scm_error_nr_irritants(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  return SCM_ERROR(exc)->nr_irris;
}

inline ScmObj
scm_error_irritant(ScmObj exc, size_t idx)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);
  scm_assert(idx < SCM_ERROR(exc)->nr_irris);

  return SCM_ERROR(exc)->irritants[idx];
}

#endif /* INCLUDE_EXCEPTION_H__ */
