#ifndef INCLUDE_FCD_ISEQ_H__
#define INCLUDE_FCD_ISEQ_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/fcd_memory.h"

bool scm_fcd_iseq_p(ScmObj obj);
ScmObj scm_fcd_iseq_new(scm_mem_type_t mtype);
ScmObj scm_fcd_make_iseq(void);
scm_byte_t *scm_fcd_iseq_to_ip(ScmObj iseq);
ssize_t scm_fcd_iseq_ip_to_offset(ScmObj iseq, scm_byte_t *ip);
size_t scm_fcd_iseq_length(ScmObj iseq);
int scm_fcd_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt);
ssize_t scm_fcd_iseq_push_inst(ScmObj iseq, const void *inst, size_t sz,
                               const size_t *objs, size_t n);
bool scm_fcd_iseq_ip_in_range_p(ScmObj iseq, const scm_byte_t *ip);

#endif /* INCLUDE_FCD_ISEQ_H__ */
