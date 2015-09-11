#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/chashtbl.h"
#include "scythe/string.h"
#include "scythe/symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

ScmTypeInfo SCM_SYMBOL_TYPE_INFO = {
  .name                = "symbol",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_symbol_obj_print,
  .obj_size            = sizeof(ScmSymbol),
  .gc_ini_func         = scm_symbol_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_symbol_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

#define IDENT_SPECIAL_INITIAL_CHARS "!$%&*/:<=>?^_~"

enum { CHK_IDENT_ADAPT = 0,
       CHK_IDENT_PECULIAR,
       CHK_IDENT_PRINT,
       CHK_IDENT_NONPRINT };

static int
check_ident_initial_char(scm_char_t chr, ScmEncoding *enc)
{
  int c;

  if (!scm_enc_ascii_p(enc, chr.bytes, sizeof(chr)))
    return CHK_IDENT_ADAPT;

  c = scm_enc_cnv_to_ascii(enc, &chr);
  scm_assert(c >= 0);

  if ((0x41 <= c && c <= 0x5a) || (0x61 <= c && c <= 0x7a))
    return CHK_IDENT_ADAPT;
  else if (strchr(IDENT_SPECIAL_INITIAL_CHARS, c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (strchr("+-.", c) != NULL)
    return CHK_IDENT_PECULIAR;
  else if (0x20 <= c && c <= 0x7e)
    return CHK_IDENT_PRINT;
  else
    return CHK_IDENT_NONPRINT;
}

static int
check_ident_subsequent_char(scm_char_t chr, ScmEncoding *enc)
{
  int c;

  if (!scm_enc_ascii_p(enc, chr.bytes, sizeof(chr)))
    return CHK_IDENT_ADAPT;

  c = scm_enc_cnv_to_ascii(enc, &chr);
  scm_assert(c >= 0);

  if ((0x41 <= c && c <= 0x5a) || (0x61 <= c && c <= 0x7a))
    return CHK_IDENT_ADAPT;
  else if (0x30 <= c && c <= 0x39)
    return CHK_IDENT_ADAPT;
  else if (strchr(IDENT_SPECIAL_INITIAL_CHARS, c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (strchr("+-.@", c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (0x20 <= c && c <= 0x7e)
    return CHK_IDENT_PRINT;
  else
    return CHK_IDENT_NONPRINT;
}

static int
check_ident_sign_subsequent_char(scm_char_t chr, ScmEncoding *enc)
{
  int c;

  if (!scm_enc_ascii_p(enc, chr.bytes, sizeof(chr)))
    return CHK_IDENT_ADAPT;

  c = scm_enc_cnv_to_ascii(enc, &chr);
  scm_assert(c >= 0);

  if ((0x41 <= c && c <= 0x5a) || (0x61 <= c && c <= 0x7a))
    return CHK_IDENT_ADAPT;
  else if (strchr(IDENT_SPECIAL_INITIAL_CHARS, c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (strchr("+-@", c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (0x20 <= c && c <= 0x7e)
    return CHK_IDENT_PRINT;
  else
    return CHK_IDENT_NONPRINT;
}

static int
check_ident_dot_subsequent_char(scm_char_t chr, ScmEncoding *enc)
{
  int c;

  if (!scm_enc_ascii_p(enc, chr.bytes, sizeof(chr)))
    return CHK_IDENT_ADAPT;

  c = scm_enc_cnv_to_ascii(enc, &chr);
  scm_assert(c >= 0);

  if ((0x41 <= c && c <= 0x5a) || (0x61 <= c && c <= 0x7a))
    return CHK_IDENT_ADAPT;
  else if (strchr(IDENT_SPECIAL_INITIAL_CHARS, c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (strchr("+-@.", c) != NULL)
    return CHK_IDENT_ADAPT;
  else if (0x20 <= c && c <= 0x7e)
    return CHK_IDENT_PRINT;
  else
    return CHK_IDENT_NONPRINT;
}

static int
check_ident_printable_char(scm_char_t chr, ScmEncoding *enc)
{
  int c;

  if (!scm_enc_ascii_p(enc, chr.bytes, sizeof(chr)))
    return CHK_IDENT_ADAPT;

  c = scm_enc_cnv_to_ascii(enc, &chr);
  scm_assert(c >= 0);

  if (0x20 <= c && c <= 0x7e)
    return CHK_IDENT_ADAPT;
  else
    return CHK_IDENT_NONPRINT;
}

static int
escape_print(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\a'))
    return scm_fcd_write_cstr("\\a", SCM_ENC_SRC, port);
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\b'))
    return scm_fcd_write_cstr("\\b", SCM_ENC_SRC, port);
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\t'))
    return scm_fcd_write_cstr("\\t", SCM_ENC_SRC, port);
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\n'))
    return scm_fcd_write_cstr("\\n", SCM_ENC_SRC, port);
  else if (scm_enc_same_char_p(enc, chr.bytes, sizeof(chr), '\r'))
    return scm_fcd_write_cstr("\\c", SCM_ENC_SRC, port);
  else
    return scm_string_inline_hex_escape(chr, enc, port);
}

static int
scm_symbol_write_ext_rep_peculiar(const scm_char_t *ary, ScmObj port,
                                  size_t len, ScmEncoding *enc,
                                  bool *need_vline_p)
{
  size_t idx;
  int type, r;

  SCM_REFSTK_INIT_REG(&port);

  idx = 0;

  if (scm_enc_same_char_p(enc, ary[idx].bytes, sizeof(ary[0]), '+')
      || scm_enc_same_char_p(enc, ary[idx].bytes, sizeof(ary[0]), '-')) {
    r = scm_fcd_write_cchr(ary[idx++], enc, port);
    if (r < 0) return -1;

    if (idx >= len) return 0;

    type = check_ident_sign_subsequent_char(ary[idx], enc);
    if (type != CHK_IDENT_ADAPT) goto dot_start;

    r = scm_fcd_write_cchr(ary[idx++], enc, port);
    if (r < 0) return -1;

    if (idx >= len) return 0;

    while (check_ident_subsequent_char(ary[idx], enc) == CHK_IDENT_ADAPT) {
      r = scm_fcd_write_cchr(ary[idx++], enc, port);
      if (r < 0) return -1;

      if (idx >= len) return 0;
    }
  }

 dot_start:

  if (scm_enc_same_char_p(enc, ary[idx].bytes, sizeof(ary[0]), '.')) {
    r = scm_fcd_write_cchr(ary[idx++], enc, port);
    if (r < 0) return -1;

    if (idx >= len) return 0;

    type = check_ident_dot_subsequent_char(ary[idx], enc);
    if (type != CHK_IDENT_ADAPT) goto non_peculiar;

    r = scm_fcd_write_cchr(ary[idx++], enc, port);
    if (r < 0) return -1;

    if (idx >= len) return 0;

    while (check_ident_subsequent_char(ary[idx], enc) == CHK_IDENT_ADAPT) {
      r = scm_fcd_write_cchr(ary[idx++], enc, port);
      if (r < 0) return -1;

      if (idx >= len) return 0;
    }
  }

 non_peculiar:

  *need_vline_p = true;
  while (idx < len) {
    type = check_ident_printable_char(ary[idx], enc);
    if (type == CHK_IDENT_ADAPT)
      r = scm_fcd_write_cchr(ary[idx++], enc, port);
    else
      r = escape_print(ary[idx++], enc, port);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_symbol_write_ext_rep_inner(ScmObj str, ScmObj port, size_t len,
                               bool *need_vline_p)
{
  scm_char_t ary[len], *p;
  ScmEncoding *enc;
  int type, r;

  SCM_REFSTK_INIT_REG(&str, &port);

  p = scm_string_to_char_ary(str, 0, (ssize_t)len, ary);
  if (p == NULL) return -1;

  enc = scm_string_encoding(str);

  *need_vline_p = false;
  type = check_ident_initial_char(ary[0], enc);
  switch (type) {
  case CHK_IDENT_ADAPT:
    r = scm_fcd_write_cchr(ary[0], enc, port);
    if (r < 0) return -1;
    break;
  case CHK_IDENT_PECULIAR:
    return scm_symbol_write_ext_rep_peculiar(ary, port, len, enc, need_vline_p);
    break;
  case CHK_IDENT_PRINT:
    *need_vline_p = true;
    r = scm_fcd_write_cchr(ary[0], enc, port);
    if (r < 0) return -1;
    break;
  case CHK_IDENT_NONPRINT:
    *need_vline_p = true;
    r = escape_print(ary[0], enc, port);
    if (r < 0) return -1;
    break;
  default:
    scm_assert(false);           /* must not happend */
    break;
  }

  for (size_t i = 1; i < len; i++) {
    type = check_ident_subsequent_char(ary[i], enc);
    switch (type) {
    case CHK_IDENT_ADAPT:
      r = scm_fcd_write_cchr(ary[i], enc, port);
      if (r < 0) return -1;
      break;
    case CHK_IDENT_PRINT:
      *need_vline_p = true;
      r = scm_fcd_write_cchr(ary[i], enc, port);
      if (r < 0) return -1;
      break;
    case CHK_IDENT_NONPRINT:
      *need_vline_p = true;
      r = escape_print(ary[i], enc, port);
      if (r < 0) return -1;
      break;
    default:
      scm_assert(false);           /* must not happend */
      break;
    }
  }

  return 0;
}

int
scm_symbol_write_ext_rep(ScmObj sym, ScmObj port)
{
  ScmObj strport = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  int r;
  bool need_vline_p;

  SCM_REFSTK_INIT_REG(&sym, &port,
                      &strport, &str);

  strport = scm_fcd_open_output_string();
  if (scm_obj_null_p(strport)) return -1;

  r = scm_symbol_write_ext_rep_inner(SCM_SYMBOL(sym)->str,
                                     strport,
                                     scm_string_length(SCM_SYMBOL(sym)->str),
                                     &need_vline_p);
  if (r < 0) return -1;

  if (need_vline_p) {
    r = scm_fcd_write_cstr("|", SCM_ENC_SRC, port);
    if (r < 0) return -1;
  }

  str = scm_fcd_get_output_string(strport);
  if (scm_obj_null_p(str)) return -1;

  r = scm_fcd_write_string(str, port, -1, -1);
  if (r < 0) return -1;

  if (need_vline_p) {
    r = scm_fcd_write_cstr("|", SCM_ENC_SRC, port);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_symbol_initialize(ScmObj sym, ScmObj str)
{
  SCM_REFSTK_INIT_REG(&sym, &str);

  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);
  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  SCM_SLOT_SETQ(ScmSymbol, sym, str, scm_string_dup(str));

  return 0;
}

size_t
scm_symbol_length(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_length(SCM_SYMBOL_STR(sym));
}

ScmObj
scm_symbol_string(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  /* TODO: string を複製するのではなく、シンボルが immutable な文字列を保持す
   *       るに変更し、それを直接返すようにする
   */
  return scm_string_dup(SCM_SYMBOL_STR(sym));
}

size_t
scm_symbol_hash_value(ScmObj sym)
{
  scm_assert_obj_type(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_hash_value(SCM_SYMBOL_STR(sym));
}

int
scm_symbol_cmp(ScmObj s1, ScmObj s2, int *rslt)
{
  scm_assert_obj_type(s1, &SCM_SYMBOL_TYPE_INFO);
  scm_assert_obj_type(s1, &SCM_SYMBOL_TYPE_INFO);

  if (rslt == NULL) return 0;

  if (scm_obj_same_instance_p(s1, s2)) {
    *rslt = 0;
    return 0;
  }
  else {
    return scm_string_cmp(SCM_SYMBOL_STR(s1), SCM_SYMBOL_STR(s2), rslt);
  }
}

int
scm_symbol_obj_print(ScmObj obj, ScmObj port, int kind,
                     ScmObjPrintHandler handler)
{
  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  if (kind == SCM_OBJ_PRINT_DISPLAY) {
    int r = scm_fcd_write_string(SCM_SYMBOL_STR(obj), port, -1, -1);
    if (r < 0) return -1;
  }
  else {
    int r = scm_symbol_write_ext_rep(obj, port);
    if (r < 0) return -1;
  }

  return 0;
}

void
scm_symbol_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  SCM_SYMBOL_STR(obj) = SCM_OBJ_NULL;
}

int
scm_symbol_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_SYMBOL_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_STR(obj));

  return rslt;
}


/*******************************************************************/
/*  Symbol Table                                                   */
/*******************************************************************/

ScmTypeInfo SCM_SYMTBL_TYPE_INFO = {
  .name                = "symtbl",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmSymTbl),
  .gc_ini_func         = scm_symtbl_gc_initialize,
  .gc_fin_func         = scm_symtbl_gc_finalize,
  .gc_accept_func      = scm_symtbl_gc_accept,
  .gc_accept_func_weak = scm_symtbl_gc_accept_weak,
  .extra               = NULL,
};

#define SCM_SYMTBL_SIZE 256

static size_t
scm_symtbl_hash_func(ScmCHashTblKey key)
{
  return scm_string_hash_value(SCM_OBJ(key));
}

static bool
scm_symtbl_cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_string_is_equal(key1, key2);
}

int
scm_symtbl_initialize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  SCM_SYMTBL(tbl)->tbl =
    scm_chash_tbl_new(tbl, SCM_SYMTBL_SIZE,
                      SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ_W,
                      scm_symtbl_hash_func, scm_symtbl_cmp_func);
  if (scm_obj_null_p(tbl)) return -1;

  return 0;
}

void
scm_symtbl_finalize(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  if (SCM_SYMTBL(tbl)->tbl != NULL) {
    scm_chash_tbl_end(SCM_SYMTBL(tbl)->tbl);
    SCM_SYMTBL(tbl)->tbl = NULL;
  }
}

ScmObj
scm_symtbl_symbol(ScmObj tbl, ScmObj str)
{
  ScmObj sym = SCM_OBJ_INIT;
  bool found;
  int rslt;

  SCM_REFSTK_INIT_REG(&sym, &tbl, &str);

  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  str = scm_string_dup(str);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_get(SCM_SYMTBL(tbl)->tbl,
                           str,
                           (ScmCHashTblVal *)SCM_CSETTER_L(sym),
                           &found);
  if (rslt != 0) return SCM_OBJ_NULL;

  if (found) return sym;

  sym = scm_fcd_symbol_new(SCM_MEM_HEAP, str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  rslt = scm_chash_tbl_insert(SCM_SYMTBL(tbl)->tbl, str, sym);
  if (rslt != 0) return SCM_OBJ_NULL;

  return sym;
}

void
scm_symtbl_clean(ScmObj tbl)
{
  scm_assert_obj_type(tbl, &SCM_SYMTBL_TYPE_INFO);

  scm_chash_tbl_clean(SCM_SYMTBL(tbl)->tbl);
}

void
scm_symtbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  SCM_SYMTBL(obj)->tbl = NULL;
}

void
scm_symtbl_gc_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  scm_symtbl_finalize(obj);
}

int
scm_symtbl_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  return scm_chash_tbl_gc_accept(SCM_SYMTBL(obj)->tbl,
                                 obj, handler, false);
}

int
scm_symtbl_gc_accept_weak(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_SYMTBL_TYPE_INFO);

  return scm_chash_tbl_gc_accept_weak(SCM_SYMTBL(obj)->tbl, obj, handler);
}


/*******************************************************************/
/*  Symbol (interface)                                             */
/*******************************************************************/

bool
scm_fcd_symbol_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

 ScmObj
scm_fcd_symbol_P(ScmObj obj)
{
  return scm_fcd_symbol_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_symbol_new(scm_mem_type_t mtype, ScmObj str)
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str, &sym);

  scm_assert_obj_type(str, &SCM_STRING_TYPE_INFO);

  sym = scm_fcd_mem_alloc(&SCM_SYMBOL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  if (scm_symbol_initialize(sym, str) < 0)
    return SCM_OBJ_NULL;

  return sym;
}

bool
scm_fcd_symbol_eq_p(ScmObj sym1, ScmObj sym2)
{
  scm_assert(scm_fcd_symbol_p(sym1));
  scm_assert(scm_fcd_symbol_p(sym2));
  return scm_obj_same_instance_p(sym1, sym2);
}

static int
symbol_cmp_fold(ScmObj lst, bool (*cmp)(ScmObj s1, ScmObj s2),
                bool *rslt)

{
  ScmObj sym = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &sym, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    sym = scm_fcd_car(l);
    if (!scm_fcd_symbol_p(sym)) {
      scm_fcd_error("failed to compare symbols: symbol required, but got",
                    1, sym);
      return -1;
    }

    if (scm_obj_not_null_p(prv)) {
      if (!cmp(prv, sym)) {
        *rslt = false;
        return 0;
      }
    }

    prv = sym;
  }

  *rslt = true;

  return 0;
}

ScmObj
scm_fcd_symbol_eq_P(ScmObj sym1, ScmObj sym2)
{
  return scm_fcd_symbol_eq_p(sym1, sym2) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_symbol_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  scm_assert(scm_obj_not_null_p(lst));

  r = symbol_cmp_fold(lst, scm_fcd_symbol_eq_p, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_symbol_to_string(ScmObj sym)
{
  scm_assert(scm_fcd_symbol_p(sym));
  return scm_symbol_string(sym);
}

ScmObj
scm_fcd_string_to_symbol(ScmObj str)
{
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&str);

  scm_assert(scm_fcd_string_p(str));

  enc = scm_string_encoding(str);
  if (enc != scm_fcd_system_encoding())
    str = scm_string_encode(str, scm_fcd_system_encoding());

  return scm_symtbl_symbol(scm_fcd_current_symbol_table(), str);
}

ScmObj
scm_fcd_make_symbol_from_cstr(const char *str, ScmEncoding *enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s);

  s = scm_fcd_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

  if (enc != NULL && enc != scm_fcd_system_encoding()) {
    s = scm_string_encode(s, scm_fcd_system_encoding());
    if (scm_obj_null_p(s)) return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_symbol(s);
}

ScmObj
scm_fcd_make_symbol_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&s);

  s = scm_fcd_make_string_from_bin(data, size, enc);
  if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

  if (enc != NULL && enc != scm_fcd_system_encoding()) {
    s = scm_string_encode(s, scm_fcd_system_encoding());
    if (scm_obj_null_p(s)) return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_symbol(s);
}

/* TODO: symbol_bytesize, symbol_to_cstr, symbol_hash_value についてはインタ
 * フェースの見直しが必要
 */

size_t
scm_fcd_symbol_bytesize(ScmObj sym)
{
  scm_assert(scm_fcd_symbol_p(sym));
  return scm_fcd_string_bytesize(scm_fcd_symbol_to_string(sym));
}

char *
scm_fcd_symbol_to_cstr(ScmObj sym, char *cstr, size_t size)
{
  return scm_fcd_string_to_cstr(scm_fcd_symbol_to_string(sym),
                                cstr, size);
}

size_t
scm_fcd_symbol_hash_value(ScmObj sym)
{
  if (!scm_fcd_symbol_p(sym))
    return SIZE_MAX;                  /* provisional implementation */

  return scm_symbol_hash_value(sym);
}

int
scm_fcd_symbol_eq_cstr(ScmObj sym, const char *str, ScmEncoding *enc, bool *cmp)
{
  ScmObj x = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym,
                      &x);

  x = scm_fcd_make_symbol_from_cstr(str, enc);
  if (scm_obj_null_p(x)) return -1;

  if (cmp != NULL)
    *cmp = scm_fcd_symbol_eq_p(sym, x);

  return 0;
}


/*******************************************************************/
/*  Symbol Table (interface)                                       */
/*******************************************************************/

ScmObj
scm_fcd_symtbl_new(scm_mem_type_t mtype)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&tbl);

  tbl = scm_fcd_mem_alloc(&SCM_SYMTBL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL;

  if (scm_symtbl_initialize(tbl) < 0)
    return SCM_OBJ_NULL;

  return tbl;
}
