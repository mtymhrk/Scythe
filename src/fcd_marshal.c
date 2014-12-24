#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/marshal.h"

extern inline bool
scm_fcd_marshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MARSHAL_TYPE_INFO);
}

ScmObj
scm_fcd_make_marshal(void)
{
  return scm_marshal_new(SCM_MEM_HEAP);
}

bool
scm_fcd_marshal_terminated_p(ScmObj marshal)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  return scm_marshal_terminated_p(marshal);
}

int
scm_fcd_marshal_push(ScmObj marshal, ScmObj obj)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  scm_assert(scm_marshal_terminated_p(marshal));
  scm_assert(scm_obj_not_null_p(obj));
  return scm_marshal_push_obj(marshal, obj);
}

void *
scm_fcd_marshal_terminate(ScmObj marshal, size_t *size)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  scm_assert(scm_marshal_terminated_p(marshal));
  return scm_marshal_terminate(marshal, size);
}

extern inline bool
scm_fcd_unmarshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_UNMARSHAL_TYPE_INFO);
}

ScmObj
scm_fcd_make_unmarshal(const void *data)
{
  scm_assert(data != NULL);
  return scm_unmarshal_new(SCM_MEM_HEAP, data);
}

size_t
scm_fcd_unmarshal_num(ScmObj unmarshal)
{
  scm_assert(scm_fcd_unmarshal_p(unmarshal));
  return scm_unmarshal_num_of_objs(unmarshal);
}

ScmObj
scm_fcd_unmarshal_ref(ScmObj unmarshal, size_t idx)
{
  scm_assert(scm_fcd_unmarshal_p(unmarshal));
  scm_assert(idx < scm_unmarshal_num_of_objs(unmarshal));
  return scm_unmarshal_ref(unmarshal, idx);
}

void *
scm_fcd_marshal_va(size_t *size, va_list arg)
{
  return scm_marshal_va(size, arg);
}

void *
scm_fcd_marshal(size_t *size, ...)
{
  void *data;
  va_list args;

  va_start(args, size);
  data = scm_fcd_marshal_va(size, args);
  va_end(args);

  return data;
}
