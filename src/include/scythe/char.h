#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/memory.h"

typedef struct ScmCharRec ScmChar;

struct ScmCharRec {
  ScmObjHeader header;
  ScmEncoding *enc;
  scm_char_t value;
};

#define SCM_CHAR(obj) ((ScmChar *)(obj))
#define SCM_CHAR_ENC(obj) (SCM_CHAR(obj)->enc)
#define SCM_CHAR_VALUE(obj) (SCM_CHAR(obj)->value)

extern ScmTypeInfo SCM_CHAR_TYPE_INFO;

ScmObj scm_char_P(ScmObj obj);
int scm_char_initialize(ScmObj chr, const scm_char_t *value, ScmEncoding *enc);
void scm_char_finalize(ScmObj chr);
ScmObj scm_char_new(scm_mem_type_t mtype,
                    const scm_char_t *value, ScmEncoding *enc);
ScmObj scm_make_char(const scm_char_t *chr, ScmEncoding *enc);
scm_char_t scm_char_value(ScmObj chr);
long long scm_char_scalar(ScmObj chr);
ScmEncoding *scm_char_encoding(ScmObj chr);
ScmObj scm_char_encode(ScmObj chr, ScmEncoding *enc);
int scm_char_cmp(ScmObj chr1, ScmObj chr2, int *rslt);
int scm_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_char_eq_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_char_eq_P_lst(ScmObj lst);
int scm_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_char_lt_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_char_lt_P_lst(ScmObj lst);
int scm_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_char_gt_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_char_gt_P_lst(ScmObj lst);
int scm_char_le(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_char_le_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_char_le_P_lst(ScmObj lst);
int scm_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_char_ge_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_char_ge_P_lst(ScmObj lst);
ScmObj scm_char_to_integer(ScmObj chr);
ScmObj scm_integer_to_char(ScmObj num, ScmEncoding *enc);
ssize_t scm_char_to_cchr(ScmObj chr, scm_char_t *cp);
int scm_char_obj_print(ScmObj obj, ScmObj port, int kind,
                       ScmObjPrintHandler handler);


static inline bool
scm_char_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_CHAR_TYPE_INFO) ? true : false;
}



#endif /* INCLUDE_CHAR_H__ */
