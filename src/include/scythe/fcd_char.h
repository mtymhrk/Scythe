#ifndef INCLUDE_FCD_CHAR_H__
#define INCLUDE_FCD_CHAR_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd_memory.h"

bool scm_fcd_char_p(ScmObj obj);
ScmObj scm_fcd_char_P(ScmObj obj);
ScmObj scm_fcd_char_new(SCM_MEM_TYPE_T mtype,
                        const scm_char_t *value, ScmEncoding *enc);
ScmObj scm_fcd_make_char(const scm_char_t *chr, ScmEncoding *enc);
int scm_fcd_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_fcd_char_eq_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_fcd_char_eq_P_lst(ScmObj lst);
int scm_fcd_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_fcd_char_lt_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_fcd_char_lt_P_lst(ScmObj lst);
int scm_fcd_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_fcd_char_gt_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_fcd_char_gt_P_lst(ScmObj lst);
int scm_fcd_char_le(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_fcd_char_le_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_fcd_char_le_P_lst(ScmObj lst);
int scm_fcd_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_fcd_char_ge_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_fcd_char_ge_P_lst(ScmObj lst);
ScmObj scm_fcd_char_to_integer(ScmObj chr);
ScmObj scm_fcd_integer_to_char(ScmObj num, ScmEncoding *enc);

scm_char_t scm_fcd_char_value(ScmObj chr);
ssize_t scm_fcd_char_to_cchr(ScmObj chr, scm_char_t *cp);
ScmEncoding *scm_fcd_char_encoding(ScmObj chr);
ScmObj scm_fcd_char_encode(ScmObj chr, ScmEncoding *enc);

#endif /* INCLUDE_FCD_CHAR_H__ */
