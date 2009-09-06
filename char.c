#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "encoding.h"
#include "char.h"

struct ScmCharRec {
  ScmObjHeader header;
  SCM_ENCODING_T enc;
  scm_char_t value;
};

#define SCM_CHAR_IS_LF(c) \
  SCM_CHR_IS_EQUAL((c)->value, SCM_ENCODING_CONST_LF_CHAR((c)->enc))
#define SCM_CHAR_IS_SPACE(c) \
  SCM_CHR_IS_EQUAL((c)->value, SCM_ENCODING_CONST_SPACE_CHAR((c)->enc))

const ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  SCM_OBJ_TYPE_CHAR,          /* type                 */
  scm_char_pretty_print,      /* pp_func              */
  sizeof(ScmChar),            /* obj_size             */
  NULL,                       /* gc_ini_func          */
  NULL,                       /* gc_fin_func          */
  NULL,                       /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};


ScmChar *
scm_char_construct(scm_char_t value, SCM_ENCODING_T enc)
{
  ScmChar *charv;

  assert(/* 0 <= enc && */ enc < SMC_ENCODING_NR_ENC);

  charv = scm_memory_allocate(sizeof(ScmChar));
  scm_obj_init(SCM_OBJ(charv), SCM_OBJ_TYPE_CHAR);
  charv->enc = enc;
  charv->value = value;

  return charv;
}

void
scm_char_destruct(ScmChar *charv)
{
  assert(charv != NULL);
  scm_memory_release(charv);
}

ScmChar *
scm_char_construct_newline(SCM_ENCODING_T enc)
{
  return scm_char_construct(SCM_ENCODING_CONST_LF_CHAR(enc), enc);
}

ScmChar *
scm_char_construct_space(SCM_ENCODING_T enc)
{
  return scm_char_construct(SCM_ENCODING_CONST_SPACE_CHAR(enc), enc);
}

scm_char_t
scm_char_value(ScmChar *charv)
{
  assert(charv != NULL);
  return charv->value;
}

SCM_ENCODING_T
scm_char_encoding(ScmChar *charv)
{
  assert(charv != NULL);
  return charv->enc;
}

bool
scm_char_is_char(ScmObj obj)
{
  assert(obj != NULL);
  return (scm_obj_type(obj) == SCM_OBJ_TYPE_CHAR);
}

void
scm_char_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  char str[32];
  ScmChar *charv;

  assert(obj != NULL); assert(scm_char_is_char(obj));
  assert(obuffer != NULL);

  charv = SCM_CHAR(obj);

  if (SCM_CHAR_IS_LF(charv)) {
    strncpy(str, "#\\newline", sizeof(str));
  }
  else if (SCM_CHAR_IS_SPACE(charv)) {
    strncpy(str, "#\\space", sizeof(str));
  }
  else if (charv->enc == SCM_ENCODING_ASCII
           && SCM_CHR_ASCII(charv->value) < 0x20) {
    snprintf(str, sizeof(str), "#\\0x%02x", SCM_CHR_ASCII(charv->value));
  }
  else {
    int (*char_width)(const void *p, size_t size);
    char tmp[sizeof(charv->value)];
    int w;

    char_width = SCM_ENCODING_VFUNC_CHAR_WIDTH(charv->enc);
    w = char_width(&charv->value, sizeof(charv->value));
    memcpy(tmp, &charv->value, (size_t)w);
    tmp[w] = '\0';

    snprintf(str, sizeof(str), "#\\%s", tmp);
  }

  scm_obuffer_concatenate_string(obuffer, str);
}
