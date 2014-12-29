#ifndef INCLUDE_CHAR_H__
#define INCLUDE_CHAR_H__

#include <stdbool.h>

typedef struct ScmCharRec ScmChar;

#define SCM_CHAR(obj) ((ScmChar *)(obj))

#include "scythe/object.h"
#include "scythe/encoding.h"

extern ScmTypeInfo SCM_CHAR_TYPE_INFO;

struct ScmCharRec {
  ScmObjHeader header;
  ScmEncoding *enc;
  scm_char_t value;
};

#define SCM_CHAR_ENC(obj) (SCM_CHAR(obj)->enc)
#define SCM_CHAR_VALUE(obj) (SCM_CHAR(obj)->value)

int scm_char_initialize(ScmObj chr, const scm_char_t *value, ScmEncoding *enc);
void scm_char_finalize(ScmObj chr);
scm_char_t scm_char_value(ScmObj chr);
long long scm_char_scalar(ScmObj chr);
ScmEncoding *scm_char_encoding(ScmObj chr);
ScmObj scm_char_encode(ScmObj chr, ScmEncoding *enc);
int scm_char_cmp(ScmObj chr1, ScmObj chr2, int *rslt);
int scm_char_obj_print(ScmObj obj, ScmObj port, int kind,
                       ScmObjPrintHandler handler);

#endif /* INCLUDE_CHAR_H__ */
