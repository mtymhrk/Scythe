#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/symbol.h"
#include "scythe/string.h"
#include "scythe/vm.h"

extern inline bool
scm_fcd_symbol_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_fcd_symbol_P(ScmObj obj)
{
  return scm_fcd_symbol_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_symbol_new(SCM_MEM_TYPE_T mtype, ScmObj str)
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

ScmObj
scm_fcd_symtbl_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&tbl);

  tbl = scm_fcd_mem_alloc(&SCM_SYMTBL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(tbl)) return SCM_OBJ_NULL;

  if (scm_symtbl_initialize(tbl) < 0)
    return SCM_OBJ_NULL;

  return tbl;
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

  return scm_symtbl_symbol(scm_bedrock_symtbl(scm_fcd_current_br()), str);
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

extern inline char *
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
