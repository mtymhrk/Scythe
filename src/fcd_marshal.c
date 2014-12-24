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
scm_fcd_marshal_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj marshal = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&marshal);

  marshal = scm_fcd_mem_alloc(&SCM_MARSHAL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(marshal)) return SCM_OBJ_NULL;

  r = scm_marshal_initialize(marshal);
  if (r < 0) return SCM_OBJ_NULL;

  return marshal;
}

ScmObj
scm_fcd_make_marshal(void)
{
  return scm_fcd_marshal_new(SCM_MEM_HEAP);
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
scm_fcd_unmarshal_new(SCM_MEM_TYPE_T mtype, const void *data)
{
  ScmObj unmarshal = SCM_OBJ_INIT;
  int r;

  scm_assert(data != NULL);

  unmarshal = scm_fcd_mem_alloc(&SCM_UNMARSHAL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(unmarshal)) return SCM_OBJ_NULL;

  r = scm_unmarshal_initialize(unmarshal, data);
  if (r < 0) return SCM_OBJ_NULL;

  return unmarshal;
}

ScmObj
scm_fcd_make_unmarshal(const void *data)
{
  scm_assert(data != NULL);
  return scm_fcd_unmarshal_new(SCM_MEM_HEAP, data);
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

static void *
marshal_va_internal(size_t *size, size_t nr_obj, va_list args)
{
  ScmObj marshal = SCM_OBJ_INIT, obj[nr_obj];

  for (size_t i = 0; i < nr_obj; i++) obj[i] = va_arg(args, ScmObj);

  SCM_REFSTK_INIT_REG(&marshal);
  SCM_REFSTK_REG_ARY(obj, nr_obj);

  marshal = scm_fcd_marshal_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(marshal)) return NULL;

  for (size_t i = 0; i < nr_obj; i++) {
    int r = scm_marshal_push_obj(marshal, obj[i]);
    if (r < 0) return NULL;
  }

  return scm_marshal_terminate(marshal, size);
}

void *
scm_fcd_marshal_va(size_t *size, va_list args)
{
  ScmObj o = SCM_OBJ_INIT;
  size_t n;
  void *p;
  va_list copy;

  va_copy(copy, args);

  n = 0;
  while (true) {
    o = va_arg(args, ScmObj);
    if (scm_obj_null_p(o)) break;
    n++;
  }

  p = marshal_va_internal(size, n, copy);
  va_end(copy);

  return p;
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
