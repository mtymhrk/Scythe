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
  .pp_func             = scm_char_pretty_print,
  .obj_size            = sizeof(ScmChar),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};


static int
scm_char_write_ext_rep(ScmObj obj, ScmObj port)
{
  const ScmEncVirtualFunc *vf;
  ScmObj ro = SCM_OBJ_INIT;
  scm_char_t chr;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port, &ro);

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  vf = SCM_ENCODING_VFUNC(SCM_CHAR(obj)->enc);
  chr = SCM_CHAR(obj)->value;

  if (vf->printable_p(chr.bytes, sizeof(chr))) {
    if (vf->space_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\space", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else {
      rslt = scm_capi_write_cstr("#\\", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;

      ro = scm_api_write_char(obj, port);
      if (scm_obj_null_p(ro)) return -1;
    }
  }
  else {
    if (vf->alarm_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\alarm", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->backspace_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\backspaace", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->delete_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\delete", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->escape_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\escape", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->newline_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\newline", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->null_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\null", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->return_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\return", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (vf->tab_p(chr.bytes, sizeof(chr))) {
      rslt = scm_capi_write_cstr("#\\tab", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else {
      char cstr[32];
      long long scalar = vf->to_scalar(chr.bytes, sizeof(chr));
      if (scalar < 0) return -1;
      snprintf(cstr, sizeof(cstr), "#\\x%llx", scalar);
      scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
    }
  }

  return 0;
}

int
scm_char_initialize(ScmObj chr, scm_char_t value, SCM_ENC_T enc) /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(/* 0 <= enc && */ enc < SCM_ENC_NR_ENC);

  if (!SCM_ENCODING_VFUNC_VALID_CHAR_P(enc)(value)) {
    scm_capi_error("can not make character object: invalid byte sequence", 0);
    return -1;                  /* [ERR] char: invalid byte sequence */
  }

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
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return chr;
}

ScmObj
scm_char_new_newline(SCM_MEM_TYPE_T  mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_LF_CHR(enc), enc);
}

ScmObj
scm_char_new_space(SCM_MEM_TYPE_T mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_SP_CHR(enc), enc);
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
  ssize_t rslt;

  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(/*0 <= enc && */enc < SCM_ENC_NR_ENC && enc != SCM_ENC_SYS);

  /* 今のところ ASCII から他のエンコードへの変換しか対応していない */
  scm_assert(SCM_CHAR(chr)->enc== SCM_ENC_ASCII
             || SCM_CHAR(chr)->enc == enc);

  if (SCM_CHAR(chr)->enc == enc)
    return scm_char_new(SCM_MEM_HEAP, SCM_CHAR(chr)->value, enc);

  vf = SCM_ENCODING_VFUNC(enc);
  rslt = vf->ascii_to((char)SCM_CHAR(chr)->value.ascii, &c);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_char_new(SCM_MEM_HEAP, c, enc);
}

int
scm_char_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj ro = SCM_OBJ_INIT;
  int rslt;

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  if (write_p) {
    rslt = scm_char_write_ext_rep(obj, port);
    if (rslt < 0) return -1;    /* [ERR]: [through] */
  }
  else {
    ro = scm_api_write_char(obj, port);
    if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */
  }

  return 0;
}
