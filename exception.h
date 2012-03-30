#ifndef INCLUDE_EXCEPTION_H__
#define INCLUDE_EXCEPTION_H__

#include <stddef.h>
#include <limits.h>

typedef struct ScmExceptionRec ScmException;

#define SCM_EXCEPTION(obj) ((ScmException *)(obj))

#include "object.h"

struct ScmExceptionRec {
  ScmObjHeader header;
  ScmObj msg;
  ScmObj *irritants;
  size_t nr_irris;
};

extern ScmTypeInfo SCM_EXCEPTION_TYPE_INFO;

#define SCM_EXCEPTION_IRRITANTS_MAX (SIZE_MAX / sizeof(ScmObj))

int scm_exception_initialize_va(ScmObj exc, ScmObj msg,
                                size_t n, va_list irris);
int scm_exception_initialize_ary(ScmObj exc, ScmObj msg,
                                 size_t n, ScmObj *irris);
void scm_exception_finalize(ScmObj exc);
ScmObj scm_exception_new(SCM_MEM_TYPE_T mtype, ScmObj msg, size_t n, ...);
ScmObj scm_exception_new_va(SCM_MEM_TYPE_T mtype, ScmObj msg,
                            size_t n, va_list irris);
ScmObj scm_exception_new_ary(SCM_MEM_TYPE_T mtype, ScmObj msg,
                             size_t n, ScmObj *irris);
int scm_exception_pretty_print(ScmObj obj, ScmObj port, bool write_p);
void scm_exception_gc_initialize(ScmObj obj, ScmObj mem);
void scm_exception_gc_fianlize(ScmObj obj);
int scm_exception_gc_accept(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler);

inline size_t
scm_exception_nr_irritants(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_EXCEPTION_TYPE_INFO);

  return SCM_EXCEPTION(exc)->nr_irris;
}

inline ScmObj
scm_exception_irritant(ScmObj exc, size_t idx)
{
  scm_assert_obj_type(exc, &SCM_EXCEPTION_TYPE_INFO);
  scm_assert(idx < SCM_EXCEPTION(exc)->nr_irris);

  return SCM_EXCEPTION(exc)->irritants[idx];
}

#endif /* INCLUDE_EXCEPTION_H__ */
