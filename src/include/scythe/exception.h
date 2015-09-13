#ifndef INCLUDE_EXCEPTION_H__
#define INCLUDE_EXCEPTION_H__

#include <stddef.h>
#include <limits.h>

#include "scythe/object.h"
#include "scythe/memory.h"


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

typedef struct ScmExceptionRec ScmException;

struct ScmExceptionRec {
  ScmObjHeader header;
  ScmObj msg;
};

#define SCM_EXCEPTION(obj) ((ScmException *)(obj))

int scm_exception_initialize(ScmObj exc, ScmObj msg);
void scm_exception_finalize(ScmObj exc);
void scm_exception_gc_initialize(ScmObj obj);
int scm_exception_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline ScmObj
scm_exception_msg(ScmObj exc)
{
  scm_assert(scm_obj_type_flag_set_p(exc, SCM_TYPE_FLG_EXC));

  return SCM_EXCEPTION(exc)->msg;
}


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

#define SCM_ERROR_IRRITANTS_MAX (SIZE_MAX / sizeof(ScmObj))

typedef struct ScmErrorRec ScmError;

struct ScmErrorRec {
  ScmException exc;
  ScmObj type;
  ScmObj *irritants;
  size_t nr_irris;
};

#define SCM_ERROR(obj) ((ScmError *)(obj))

extern ScmTypeInfo SCM_ERROR_TYPE_INFO;

ScmObj scm_error_object_P(ScmObj obj);
int scm_error_obj_initialize_cv(ScmObj exc, ScmObj msg,
                            ScmObj type, ScmObj *irris, size_t n);
int scm_error_obj_initialize_lst(ScmObj exc, ScmObj msg, ScmObj type, ScmObj irris);
void scm_error_obj_finalize(ScmObj exc);
ScmObj scm_error_obj_new_cv(scm_mem_type_t mtype, ScmObj msg,
                        ScmObj type, ScmObj *irris, size_t n);
ScmObj scm_error_obj_new_lst(scm_mem_type_t mtype, ScmObj msg,
                         ScmObj type, ScmObj irris);
ScmObj scm_error_obj_irris_to_list(ScmObj exc);
int scm_error_obj_obj_print(ScmObj obj, ScmObj port, int kind,
                            ScmObjPrintHandler handler);
void scm_error_obj_gc_initialize(ScmObj obj);
void scm_error_obj_gc_fianlize(ScmObj obj);
int scm_error_obj_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_error_object_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_ERROR_TYPE_INFO);
}

static inline ScmObj
scm_error_obj_type(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  return SCM_ERROR(exc)->type;
}

static inline size_t
scm_error_obj_nr_irritants(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  return SCM_ERROR(exc)->nr_irris;
}

static inline ScmObj
scm_error_obj_irritant(ScmObj exc, size_t idx)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);
  scm_assert(idx < SCM_ERROR(exc)->nr_irris);

  return SCM_ERROR(exc)->irritants[idx];
}

static inline ScmObj
scm_error_obj_message(ScmObj obj)
{
  scm_assert(scm_error_object_p(obj));
  return scm_exception_msg(obj);
}

static inline ScmObj
scm_error_obj_irritants(ScmObj obj)
{
  scm_assert(scm_error_object_p(obj));
  return scm_error_obj_irris_to_list(obj);
}


/*******************************************************************/
/*                                                                 */
/*******************************************************************/

int scm_error(const char *msg, size_t n, ...);
int scm_read_error(const char *msg, size_t n, ...);
int scm_file_error(const char *msg, size_t n, ...);
ScmObj scm_error_lst(ScmObj msg, ScmObj irris);
ScmObj scm_read_error_P(ScmObj obj);
ScmObj scm_file_error_P(ScmObj obj);


#endif /* INCLUDE_EXCEPTION_H__ */
