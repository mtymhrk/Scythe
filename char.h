#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

typedef struct ScmCharRec ScmChar;

#define SCM_CHAR(obj) ((ScmChar *)(obj))

#include "object.h"
#include "encoding.h"
#include "api_enum.h"

extern ScmTypeInfo SCM_CHAR_TYPE_INFO;

struct ScmCharRec {
  ScmObjHeader header;
  SCM_ENC_T enc;
  scm_char_t value;
};

#define SCM_CHAR_ENC(obj) (SCM_CHAR(obj)->enc)
#define SCM_CHAR_VALUE(obj) (SCM_CHAR(obj)->value)
#define SCM_CHAR_IS_LF(c)                                       \
  SCM_CHR_IS_EQUAL(SCM_CHAR_VALUE(c),                           \
                   SCM_ENCODING_CONST_LF_CHAR(SCM_CHAR_ENC(c)))
#define SCM_CHAR_IS_SPACE(c)                                            \
  SCM_CHR_IS_EQUAL(SCM_CHAR_VALUE(c),                                   \
                   SCM_ENCODING_CONST_SPACE_CHAR(SCM_CHAR_ENC(c)))

int scm_char_initialize(ScmObj chr, scm_char_t value, SCM_ENC_T enc);
void scm_char_finalize(ScmObj chr);
ScmObj scm_char_new(SCM_MEM_TYPE_T mtype,
                    scm_char_t value, SCM_ENC_T enc);
ScmObj scm_char_new_newline(SCM_MEM_TYPE_T mtype, SCM_ENC_T enc);
ScmObj scm_char_new_space(SCM_MEM_TYPE_T mtype, SCM_ENC_T enc);
scm_char_t scm_char_value(ScmObj chr);
SCM_ENC_T scm_char_encoding(ScmObj chr);
ScmObj scm_char_encode(ScmObj chr, SCM_ENC_T enc);
int scm_char_cmp(ScmObj chr1, ScmObj chr2, int *rslt);
int scm_char_pretty_print(ScmObj obj, ScmObj port, bool write_p);

#endif /* INCLUDE_CHAR_H__ */
