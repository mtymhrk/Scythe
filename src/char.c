#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iconv.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/encoding.h"
#include "scythe/char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  .name                = "char",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_char_obj_print,
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
  ScmEncoding *enc;
  scm_char_t chr;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  enc = SCM_CHAR_ENC(obj);
  chr = SCM_CHAR_VALUE(obj);

  if (scm_enc_printable_p(enc, chr.bytes, sizeof(chr))) {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), ' ')) {
      rslt = scm_fcd_write_cstr("#\\space", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else {
      rslt = scm_fcd_write_cstr("#\\", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = scm_fcd_write_char(obj, port);
      if (rslt < 0) return -1;
    }
  }
  else {
    if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\a')) {
      rslt = scm_fcd_write_cstr("#\\alarm", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\b')) {
      rslt = scm_fcd_write_cstr("#\\backspaace", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x7f)) {
      rslt = scm_fcd_write_cstr("#\\delete", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), 0x1b)) {
      rslt = scm_fcd_write_cstr("#\\escape", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\n')) {
      rslt = scm_fcd_write_cstr("#\\newline", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\0')) {
      rslt = scm_fcd_write_cstr("#\\null", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\r')) {
      rslt = scm_fcd_write_cstr("#\\return", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\t')) {
      rslt = scm_fcd_write_cstr("#\\tab", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;
    }
    else {
      char cstr[32];
      long long scalar = scm_enc_cnv_to_scalar(enc, chr.bytes, sizeof(chr));
      if (scalar < 0) return -1;
      snprintf(cstr, sizeof(cstr), "#\\x%llx", scalar);
      scm_fcd_write_cstr(cstr, SCM_ENC_SRC, port);
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
    scm_fcd_error("can not make character object: invalid byte sequence", 0);
    return -1;
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
    return scm_fcd_char_new(SCM_MEM_HEAP, &SCM_CHAR(chr)->value, enc);

  cd = iconv_open(scm_enc_name(enc),
                  scm_enc_name(SCM_CHAR(chr)->enc));
  if (cd == (iconv_t)-1) {
    scm_fcd_error("failed to call 'iconv_open'", 0);
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
      scm_fcd_error("failed to call 'iconv': illegal multibyte sequence", 0);
      break;
    case EINVAL:
      scm_fcd_error("failed to call 'iconv': imcomplete  multibyte sequence", 0);
      break;
    case E2BIG:
      scm_fcd_error("failed to call 'iconv': too big multibyte sequence", 0);
      break;
    default:
      scm_fcd_error("failed to call 'iconv': unknown error has occurred", 0);
      break;
    }

    goto err;
  }

  iconv_close(cd);
  return scm_fcd_char_new(SCM_MEM_HEAP, &c, enc);

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
    scm_fcd_error("can not get scalar value of character", 0);
    return -1;
  }

  v2 = scm_enc_cnv_to_scalar(SCM_CHAR_ENC(chr2),
                             SCM_CHAR_VALUE(chr2).bytes, sizeof(scm_char_t));
  if (v2 < 0) {
    scm_fcd_error("can not get scalar value of character", 0);
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
scm_char_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_CHAR_TYPE_INFO);

  if (kind == SCM_OBJ_PRINT_DISPLAY)
    rslt = scm_fcd_write_char(obj, port);
  else
    rslt = scm_char_write_ext_rep(obj, port);

  if (rslt < 0) return -1;

  return 0;
}
