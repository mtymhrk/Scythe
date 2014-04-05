#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iconv.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "encoding.h"
#include "char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  .name                = "char",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_char_pretty_print,
  .obj_size            = sizeof(ScmChar),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};


static int
scm_char_write_ext_rep(ScmObj obj, ScmObj port)
{
  ScmObj ro = SCM_OBJ_INIT;
  ScmEncoding *enc;
  scm_char_t chr;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port, &ro);

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  enc = SCM_CHAR_ENC(obj);
  chr = SCM_CHAR_VALUE(obj);

  if (scm_enc_printable_p(enc, chr.bytes, sizeof(chr))) {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), ' ')) {
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
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\a')) {
      rslt = scm_capi_write_cstr("#\\alarm", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\b')) {
      rslt = scm_capi_write_cstr("#\\backspaace", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x7f)) {
      rslt = scm_capi_write_cstr("#\\delete", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x1b)) {
      rslt = scm_capi_write_cstr("#\\escape", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\n')) {
      rslt = scm_capi_write_cstr("#\\newline", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\0')) {
      rslt = scm_capi_write_cstr("#\\null", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\r')) {
      rslt = scm_capi_write_cstr("#\\return", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\t')) {
      rslt = scm_capi_write_cstr("#\\tab", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;
    }
    else {
      char cstr[32];
      long long scalar = scm_enc_cnv_to_scalar(enc, chr.bytes, sizeof(chr));
      if (scalar < 0) return -1;
      snprintf(cstr, sizeof(cstr), "#\\x%llx", scalar);
      scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
    }
  }

  return 0;
}

int
scm_char_initialize(ScmObj chr, const scm_char_t *value, ScmEncoding *enc)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(value != NULL);
  scm_assert(enc != NULL);

  if (!scm_enc_valid_char_p(enc, value)) {
    scm_capi_error("can not make character object: invalid byte sequence", 0);
    return -1;                  /* [ERR] char: invalid byte sequence */
  }

  SCM_CHAR_VALUE(chr) = *value;
  SCM_CHAR_ENC(chr) = enc;

  return 0;
}

void
scm_char_finalize(ScmObj chr)
{
  return;                       /* nothing to do */
}

ScmObj
scm_char_new(SCM_MEM_TYPE_T mtype,
             const scm_char_t *value, ScmEncoding *enc)
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&chr);

  scm_assert(value != NULL);
  scm_assert(enc != NULL);

  chr = scm_capi_mem_alloc(&SCM_CHAR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  if (scm_char_initialize(chr, value, enc) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return chr;
}

ScmObj
scm_char_new_newline(SCM_MEM_TYPE_T  mtype, ScmEncoding *enc)
{
  scm_char_t c;
  scm_enc_cnv_from_ascii(enc, '\n', &c);
  return scm_char_new(mtype, &c, enc);
}

ScmObj
scm_char_new_space(SCM_MEM_TYPE_T mtype, ScmEncoding *enc)
{
  scm_char_t c;
  scm_enc_cnv_from_ascii(enc, ' ', &c);
  return scm_char_new(mtype, &c, enc);
}

scm_char_t
scm_char_value(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_VALUE(chr);
}

long long
scm_char_scalar(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr),
                               SCM_CHAR_VALUE(chr).bytes,
                               sizeof(scm_char_t));
}

ScmEncoding *
scm_char_encoding(ScmObj chr)
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  return SCM_CHAR_ENC(chr);
}

ScmObj
scm_char_encode(ScmObj chr, ScmEncoding *enc)
{
  scm_char_t c;
  iconv_t cd;
  char *in, *out;
  size_t ins, outs, rslt;
  int w;

  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(enc != NULL);

  if (SCM_CHAR(chr)->enc == enc)
    return scm_char_new(SCM_MEM_HEAP, &SCM_CHAR(chr)->value, enc);

  cd = iconv_open(scm_enc_name(enc),
                  scm_enc_name(SCM_CHAR(chr)->enc));
  if (cd == (iconv_t)-1) {
    scm_capi_error("faild to call 'iconv_open'", 0);
    return SCM_OBJ_NULL;
  }

  w = scm_enc_char_width(SCM_CHAR(chr)->enc,
                         SCM_CHAR(chr)->value.bytes, sizeof(scm_char_t));
  scm_assert(w < 0);

  in = (char *)SCM_CHAR(chr)->value.bytes;
  out = (char *)c.bytes;
  ins = (size_t)w;
  outs = sizeof(c);

  rslt = iconv(cd, &in, &ins, &out, &outs);
  if (rslt == (size_t)-1) {
    switch (errno) {
    case EILSEQ:
      scm_capi_error("faild to call 'iconv': illegal multibyte sequence", 0);
      break;
    case EINVAL:
      scm_capi_error("faild to call 'iconv': imcomplete  multibyte sequence", 0);
      break;
    case E2BIG:
      scm_capi_error("faild to call 'iconv': too big multibyte sequence", 0);
      break;
    default:
      scm_capi_error("faild to call 'iconv': unknown error has occurred", 0);
      break;
    }

    goto err;
  }

  iconv_close(cd);
  return scm_char_new(SCM_MEM_HEAP, &c, enc);

 err:
  iconv_close(cd);
  return SCM_OBJ_NULL;
}

int
scm_char_cmp(ScmObj chr1, ScmObj chr2, int *rslt)
{
  long long v1, v2;

  scm_assert_obj_type(chr1, &SCM_CHAR_TYPE_INFO);
  scm_assert_obj_type(chr2, &SCM_CHAR_TYPE_INFO);
  scm_assert(SCM_CHAR_ENC(chr1) == SCM_CHAR_ENC(chr2));

  v1 = scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr1),
                             SCM_CHAR_VALUE(chr1).bytes, sizeof(scm_char_t));
  if (v1 < 0) {
    scm_capi_error("can not get scalar value of character", 0);
    return -1;
  }

  v2 = scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr2),
                             SCM_CHAR_VALUE(chr2).bytes, sizeof(scm_char_t));
  if (v2 < 0) {
    scm_capi_error("can not get scalar value of character", 0);
    return -1;
  }

  if (rslt != NULL) {
    if (v1 < v2)
      *rslt = -1;
    else if (v1 > v2)
      *rslt = 1;
    else
      *rslt = 0;
  }

  return 0;
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
