#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "encoding.h"
#include "char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmChar),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};


int
scm_char_initialize(ScmObj chr, scm_char_t value, SCM_ENC_T enc) /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(/* 0 <= enc && */ enc < SCM_ENC_NR_ENC);

  if (!SCM_ENCODING_VFUNC_VALID_P(enc)(value))
    return -1;                  /* [ERR] char: invalid byte sequence */

  SCM_CHAR_VALUE(chr) = value;
  SCM_CHAR_ENC(chr) = enc;

  return 0;
}

void
scm_char_finalize(ScmObj chr)   /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_char_new(SCM_MEM_TYPE_T mtype,
             scm_char_t value, SCM_ENC_T enc) /* GC OK */
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&chr);

  scm_assert(/* 0 <= enc && */ enc < SCM_ENC_NR_ENC);

  chr = scm_capi_mem_alloc(&SCM_CHAR_TYPE_INFO, mtype);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  if (scm_char_initialize(chr, value, enc) < 0)
    return SCM_OBJ_NULL;

  return chr;
}

ScmObj
scm_char_new_newline(SCM_MEM_TYPE_T  mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_LF_CHAR(enc), enc);
}

ScmObj
scm_char_new_space(SCM_MEM_TYPE_T mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_SPACE_CHAR(enc), enc);
}

scm_char_t
scm_char_value(ScmObj chr)      /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_VALUE(chr);
}

SCM_ENC_T
scm_char_encoding(ScmObj chr)   /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_ENC(chr);
}

ScmObj
scm_char_encode(ScmObj chr, SCM_ENC_T enc)
{
  const ScmEncVirtualFunc *vf;
  scm_char_t c;
  ssize_t rslt, cw;

  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(/*0 <= enc && */enc < SCM_ENC_NR_ENC && enc != SCM_ENC_SYS);

  /* 今のところ ASCII から他のエンコードへの変換しか対応していない */
  scm_assert(SCM_CHAR(chr)->enc== SCM_ENC_ASCII
             || SCM_CHAR(chr)->enc == SCM_ENC_BIN
             || SCM_CHAR(chr)->enc == enc
             || enc == SCM_ENC_BIN);

  if (SCM_CHAR(chr)->enc == enc)
    return scm_char_new(SCM_MEM_HEAP, SCM_CHAR(chr)->value, enc);

  vf = SCM_ENCODING_VFUNC(SCM_CHAR(chr)->enc);
  cw = vf->char_width(SCM_CHAR(chr)->value.bytes, sizeof(scm_char_t));
  if (cw < 0) return SCM_OBJ_NULL;

  vf = SCM_ENCODING_VFUNC(enc);
  if (SCM_CHAR(chr)->enc == SCM_ENC_BIN) {
    rslt = vf->char_width(SCM_CHAR(chr)->value.bytes, (size_t)cw);
    if (rslt < 0) return SCM_OBJ_NULL;
    scm_char_new(SCM_MEM_HEAP, SCM_CHAR(chr)->value, enc);
  }
  else if (enc == SCM_ENC_BIN) {
    if (cw != 1) return SCM_OBJ_NULL;
    scm_char_new(SCM_MEM_HEAP, SCM_CHAR(chr)->value, enc);
  }

  rslt = vf->ascii_to((char)SCM_CHAR(chr)->value.ascii, &c);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_char_new(SCM_MEM_HEAP, c, enc);
}

/* int */
/* scm_char_pretty_print(ScmObj obj, ScmObj port, bool write_p) */
/* { */
/*   ScmObj str = SCM_OBJ_INIT; */
/*   const ScmEncVirtualFunc *vf; */
/*   char cstr[256]; */
/*   int c; */

/*   SCM_STACK_FRAME_PUSH(&obj, &port, &str); */

/*   scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO); */

/*   vf = SCM_ENCODING_VFUNC(SCM_CHAR(obj)->enc); */

/*   if (write_p) { */
/*     c = vf->to_ascii(SCM_CHAR(obj)->value); */
/*     if (c >= 0) { */
/*       if (c == '\n') */
/*         memcpy(cstr, "#\\newline", sizeof("#\\newline")); */
/*       else if (c == ' ') */
/*         memcpy(cstr, "#\\space", sizeof("#\\space")); */
/*       else if (iscntrl(c)) */
/*         snprintf(cstr, sizeof(cstr), "#\\x%02x", c); */
/*       else */
/*         snprintf(cstr, sizeof(cstr), "#\\%c", c); */

/*       str = scm_capi_make_string_from_cstr(cstr, SCM_ENC_ASCII); */
/*       if (scm_obj_null_p(str)) return -1; */
/*     } */
/*     else { */
/*       str = scm_capi_make_string_from_cstr("#\\", SCM_ENC_ASCII); */
/*       if (scm_obj_null_p(str)) return -1; */

/*       str = scm_capi_string_encode(str, SCM_ENC_SYS); */
/*       if (scm_obj_null_p(str)) return -1; */

/*       str = scm_api_string_push(str, obj); */
/*       if (scm_obj_null_p(str)) return -1; */
/*     } */

/*     port = scm_api_write_string(port, str); */
/*     if (scm_obj_null_p(port)) return -1; */
/*   } */
/*   else { */
/*     port =scm_api_write_char(port, obj); */
/*     if (scm_obj_null_p(port)) return -1; */
/*   } */

/*   return 0; */
/* } */
