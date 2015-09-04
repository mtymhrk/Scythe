#ifndef INCLUDE_FCD_STRING_H__
#define INCLUDE_FCD_STRING_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd_type.h"

bool scm_fcd_string_p(ScmObj obj);
ScmObj scm_fcd_string_P(ScmObj obj);
ScmObj scm_fcd_string_new(SCM_MEM_TYPE_T mtype,
                          const void *src, size_t size, ScmEncoding *enc);
ScmObj scm_fcd_make_string_from_cstr(const char *str, ScmEncoding *enc);
ScmObj scm_fcd_make_string_from_bin(const void *data, size_t size,
                                    ScmEncoding *enc);
ScmObj scm_fcd_string_lst(ScmObj lst);
ScmObj scm_fcd_string_cv(const ScmObj *chr, size_t n);
size_t scm_fcd_string(size_t n, ...);
const void *scm_fcd_string_content(ScmObj str);
size_t scm_fcd_string_length(ScmObj str);
size_t scm_fcd_string_bytesize(ScmObj str);
ScmObj scm_fcd_string_ref(ScmObj str, size_t pos);
int scm_fcd_string_set_i(ScmObj str, size_t pos, ScmObj chr);
int scm_fcd_string_eq(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_fcd_string_eq_P(ScmObj s1, ScmObj s2);
ScmObj scm_fcd_string_eq_P_lst(ScmObj lst);
int scm_fcd_string_lt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_fcd_string_lt_P(ScmObj s1, ScmObj s2);
ScmObj scm_fcd_string_lt_P_lst(ScmObj lst);
int scm_fcd_string_gt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_fcd_string_gt_P(ScmObj s1, ScmObj s2);
ScmObj scm_fcd_string_gt_P_lst(ScmObj lst);
int scm_fcd_string_le(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_fcd_string_le_P(ScmObj s1, ScmObj s2);
ScmObj scm_fcd_string_le_P_lst(ScmObj lst);
int scm_fcd_string_ge(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_fcd_string_ge_P(ScmObj s1, ScmObj s2);
ScmObj scm_fcd_string_ge_P_lst(ScmObj lst);
ScmObj scm_fcd_string_upcase(ScmObj str);
ScmObj scm_fcd_string_downcase(ScmObj str);
ScmObj scm_fcd_substring(ScmObj str, size_t start, size_t end);
ScmObj scm_fcd_string_append_lst(ScmObj lst);
ScmObj scm_fcd_string_append_cv(ScmObj *ary, size_t n);
ScmObj scm_fcd_string_append(size_t n, ...);
ScmObj scm_fcd_string_to_list(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_fcd_list_to_string(ScmObj lst);
ScmObj scm_fcd_string_copy(ScmObj str, ssize_t start, ssize_t end);
int scm_fcd_string_copy_i(ScmObj to, size_t at,
                          ScmObj from, ssize_t start, ssize_t end);
int scm_fcd_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end);

ScmEncoding *scm_fcd_string_encoding(ScmObj str);
char *scm_fcd_string_to_cstr(ScmObj str, char *cstr, size_t size);
int scm_fcd_string_push(ScmObj str, scm_char_t chr, ScmEncoding *enc);
ScmObj scm_fcd_string_encode(ScmObj str, ScmEncoding *enc);
ScmObj scm_fcd_string_convert(ScmObj str, const char *enc);
scm_char_t *scm_fcd_string_to_cchr_ary(ScmObj str, size_t pos, ssize_t len,
                                       scm_char_t *ary);
ssize_t scm_fcd_string_to_path_cstr(ScmObj str, char *cstr, size_t sz);

#endif /* INCLUDE_FCD_STRING_H__ */
