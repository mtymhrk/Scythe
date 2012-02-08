#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "encoding.h"
#include "char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  NULL,                       /* pp_func              */
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
scm_char_new(SCM_MEM_ALLOC_TYPE_T mtype,
                   scm_char_t value, SCM_ENCODING_T enc) /* GC OK */
{
  ScmObj chr;

  SCM_STACK_FRAME_PUSH(&chr);

  assert(/* 0 <= enc && */ enc < SMC_ENCODING_NR_ENC);


  /* scm_mem_alloc_root(scm_vm_current_mm(), */
  /*                    &SCM_CHAR_TYPE_INFO, SCM_REF_MAKE(chr)); */
  /* TODO: replace above by below */
  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_CHAR_TYPE_INFO, mtype, SCM_REF_MAKE(chr));
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  scm_char_initialize(chr, value, enc);

  return chr;
}

ScmObj
scm_char_new_newline(SCM_MEM_ALLOC_TYPE_T mtype, SCM_ENCODING_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_LF_CHAR(enc), enc);
}

ScmObj
scm_char_new_space(SCM_MEM_ALLOC_TYPE_T mtype, SCM_ENCODING_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_SPACE_CHAR(enc), enc);
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
  assert(SCM_OBJ_NOT_NULL_P(obj));
  return SCM_OBJ_IS_TYPE(obj, &SCM_CHAR_TYPE_INFO);
}
