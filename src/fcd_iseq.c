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

int
scm_fcd_iseq_update_oprand_iof(ScmObj iseq, size_t offset, int iof)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(offset <= SSIZE_MAX);
  scm_assert(scm_iseq_length(iseq) >= SCM_OPFMT_INST_SZ_IOF);
  scm_assert(offset <= scm_iseq_length(iseq) - SCM_OPFMT_INST_SZ_IOF);
  return scm_iseq_update_opd_iof(iseq, offset, iof);
}

int
scm_fcd_inst_update_oprand_obj(scm_byte_t *ip, ScmObj clsr, ScmObj obj)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ssize_t idx;
  int rslt;

  SCM_REFSTK_INIT_REG(&clsr, &obj,
                      &iseq);

  scm_assert(ip != NULL);
  scm_assert(scm_fcd_closure_p(clsr));
  scm_assert(scm_obj_not_null_p(obj));

  iseq = scm_fcd_closure_to_iseq(clsr);
  if (scm_obj_null_p(iseq)) return -1;

  idx = scm_iseq_ip_to_offset(iseq, ip);
  if (idx < 0) return -1;

  rslt = scm_iseq_update_opd_obj(iseq, (size_t)idx, obj);
  if (rslt < 0) return -1;

  return 0;
}
