#ifndef INCLUDE_FCD_PAIR_H__
#define INCLUDE_FCD_PAIR_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"

bool scm_fcd_pair_p(ScmObj pair);
ScmObj scm_fcd_pair_P(ScmObj pair);
ScmObj scm_fcd_pair_new(SCM_MEM_TYPE_T mtype, ScmObj car, ScmObj cdr);
ScmObj scm_fcd_cons(ScmObj car, ScmObj cdr);
ScmObj scm_fcd_car(ScmObj pair);
ScmObj scm_fcd_cdr(ScmObj pair);
void scm_fcd_set_car_i(ScmObj pair, ScmObj elm);
void scm_fcd_set_cdr_i(ScmObj pair, ScmObj elm);
ScmObj scm_fcd_cxr(ScmObj pair, const char *dir);
ScmObj scm_fcd_list_P(ScmObj lst);
ScmObj scm_fcd_make_list(size_t n, ScmObj fill);
ScmObj scm_fcd_list_cv(const ScmObj *elm, size_t n);
ScmObj scm_fcd_list(size_t n, ...);
ssize_t scm_fcd_length(ScmObj lst);
ScmObj scm_fcd_append_lst(ScmObj lst);
ScmObj scm_fcd_append_cv(const ScmObj *lists, size_t n);
ScmObj scm_fcd_append(size_t n, ...);
ScmObj scm_fcd_reverse(ScmObj lst);
ScmObj scm_fcd_list_tail(ScmObj lst, size_t n);
ScmObj scm_fcd_list_ref(ScmObj lst, size_t n);
int scm_fcd_list_set_i(ScmObj lst, size_t n, ScmObj obj);
ScmObj scm_fcd_memq(ScmObj obj, ScmObj lst);
ScmObj scm_fcd_memv(ScmObj obj, ScmObj lst);
ScmObj scm_fcd_member(ScmObj obj, ScmObj lst,
                      ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_fcd_assq(ScmObj obj, ScmObj alist);
ScmObj scm_fcd_assv(ScmObj obj, ScmObj alist);
ScmObj scm_fcd_assoc(ScmObj obj, ScmObj alist,
                     ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_fcd_list_copy(ScmObj lst);

#endif /* INCLUDE_FCD_PAIR_H__ */
