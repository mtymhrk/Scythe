#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/fcd.h"
#include "scythe/iseq.h"

extern inline bool
scm_fcd_iseq_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_ISEQ_TYPE_INFO) ? true : false);
}

ScmObj
scm_fcd_iseq_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = scm_fcd_mem_alloc(&SCM_ISEQ_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL;

  if (scm_iseq_initialize(iseq) < 0)
    return SCM_OBJ_NULL;

  return iseq;
}

ScmObj
scm_fcd_make_iseq(void)
{
  return scm_fcd_iseq_new(SCM_MEM_HEAP);
}

scm_byte_t *
scm_fcd_iseq_to_ip(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_to_ip(iseq);
}

ssize_t
scm_fcd_iseq_ip_to_offset(ScmObj iseq, scm_byte_t *ip)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_ip_to_offset(iseq, ip);
}

size_t
scm_fcd_iseq_length(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_length(iseq);
}

int
scm_fcd_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt)
{
  bool cmp;
  int r;

  scm_assert(scm_fcd_iseq_p(iseq1));
  scm_assert(scm_fcd_iseq_p(iseq2));

  r = scm_iseq_eq(iseq1, iseq2, &cmp);
  if (r < 0) return 0;

  if (rslt != NULL) *rslt = cmp;
  return 0;
}

ssize_t
scm_fcd_iseq_push_inst(ScmObj iseq, const void *inst, size_t sz,
                       const size_t *objs, size_t n)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(inst != NULL);
  scm_assert(n == 0 || objs != NULL);

  return scm_iseq_push_inst(iseq, inst, sz, objs, n);
}

bool
scm_fcd_iseq_ip_in_range_p(ScmObj iseq, const scm_byte_t *ip)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_ip_in_range_p(iseq, ip);
}
