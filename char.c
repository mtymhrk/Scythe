#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "obuffer.h"
#include "encoding.h"
#include "char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  scm_char_pretty_print,      /* pp_func              */
  sizeof(ScmChar),            /* obj_size             */
  NULL,                       /* gc_ini_func          */
  NULL,                       /* gc_fin_func          */
  NULL,                       /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};


void
scm_char_initialize(ScmObj chr, scm_char_t value, SCM_ENCODING_T enc) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(chr, &SCM_CHAR_TYPE_INFO);
  assert(/* 0 <= enc && */ enc < SMC_ENCODING_NR_ENC);

  SCM_CHAR_VALUE(chr) = value;
  SCM_CHAR_ENC(chr) = enc;
}

void
scm_char_finalize(ScmObj chr)   /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_char_construct(scm_char_t value, SCM_ENCODING_T enc) /* GC OK */
{
  ScmObj chr;

  SCM_STACK_FRAME_PUSH(&chr);

  assert(/* 0 <= enc && */ enc < SMC_ENCODING_NR_ENC);


  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_CHAR_TYPE_INFO, SCM_REF_MAKE(chr));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_CHAR_TYPE_INFO, SCM_REF_MAKE(chr)); */
  if (SCM_OBJ_IS_NULL(chr)) return SCM_OBJ_NULL;

  scm_char_initialize(chr, value, enc);

  return chr;
}

ScmObj
scm_char_construct_newline(SCM_ENCODING_T enc) /* GC OK */
{
  return scm_char_construct(SCM_ENCODING_CONST_LF_CHAR(enc), enc);
}

ScmObj
scm_char_construct_space(SCM_ENCODING_T enc) /* GC OK */
{
  return scm_char_construct(SCM_ENCODING_CONST_SPACE_CHAR(enc), enc);
}

scm_char_t
scm_char_value(ScmObj chr)      /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_VALUE(chr);
}

SCM_ENCODING_T
scm_char_encoding(ScmObj chr)   /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_ENC(chr);
}

bool
scm_char_is_char(ScmObj obj)    /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));
  return SCM_OBJ_IS_TYPE(obj, &SCM_CHAR_TYPE_INFO);
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
