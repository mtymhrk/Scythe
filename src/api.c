#include <sys/types.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/bedrock.h"
#include "scythe/vm.h"
#include "scythe/refstk.h"
#include "scythe/assembler.h"
#include "scythe/char.h"
#include "scythe/file.h"
#include "scythe/format.h"
#include "scythe/number.h"
#include "scythe/compiler.h"
#include "scythe/exception.h"
#include "scythe/iseq.h"
#include "scythe/miscobjects.h"
#include "scythe/module.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/procedure.h"
#include "scythe/record.h"
#include "scythe/string.h"
#include "scythe/scythe.h"
#include "scythe/symbol.h"
#include "scythe/syntax.h"
#include "scythe/vector.h"
#include "scythe/api.h"

#include "scythe/core_modules.h"


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

ScmObj
scm_api_cons(ScmObj car, ScmObj cdr)
{
  if (scm_obj_null_p(car)) {
    scm_capi_error("cons: invalid argument", 1, car);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(cdr)) {
    scm_capi_error("cons: invalid argument", 1, cdr);
    return SCM_OBJ_NULL;
  }

  return scm_cons(car, cdr);
}

ScmObj
scm_api_car(ScmObj pair)
{
  if (!scm_pair_p(pair)) {
    scm_capi_error("car: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_car(pair);
}

ScmObj
scm_api_cdr(ScmObj pair)
{
  if (!scm_pair_p(pair)) {
    scm_capi_error("cdr: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_cdr(pair);
}

ScmObj
scm_api_set_car_i(ScmObj pair, ScmObj elm)
{
  if (!scm_pair_p(pair)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 1, elm);
    return SCM_OBJ_NULL;
  }

  scm_set_car(pair, elm);

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_set_cdr_i(ScmObj pair, ScmObj elm)
{
  if (!scm_pair_p(pair)) {
    scm_capi_error("set-cdr!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-cdr!: invalid argument", 1, elm);
    return SCM_OBJ_NULL;
  }

  scm_set_cdr(pair, elm);

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_make_list(ScmObj n, ScmObj fill)
{
  size_t s;
  int r;

  if (!scm_num_integer_p(n)) {
    scm_capi_error("make-list: invalid argument", 1, n);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_make_list(s, fill);
}

ssize_t
scm_capi_length(ScmObj lst)
{
  if (!scm_nil_p(lst) && !scm_pair_p(lst)) {
    scm_capi_error("length: list required, but got", 1, lst);
    return -1;
  }

  return (ssize_t)scm_length(lst);
}

ScmObj
scm_api_length(ScmObj lst)
{
  ssize_t len;

  len = scm_capi_length(lst);
  if (len < 0) return SCM_OBJ_NULL;

  return scm_make_number_from_sword(len);
}

ScmObj
scm_api_append_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("append: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_append_lst(lst);
}

ScmObj
scm_api_reverse(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("reverse: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_reverse(lst);
}

ScmObj
scm_api_list_tail(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_num_integer_p(n)) {
    scm_capi_error("list-tail: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_list_tail(lst, s);
}

ScmObj
scm_api_list_ref(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_num_integer_p(n)) {
    scm_capi_error("list-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_list_ref(lst, s);
}

ScmObj
scm_api_list_set_i(ScmObj lst, ScmObj n, ScmObj obj)
{
  size_t s;
  int r;

  if (!scm_num_integer_p(n)) {
    scm_capi_error("list-set!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_list_set(lst, s, obj);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

ScmObj
scm_api_num_eq_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("=: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_eq_P_lst(lst);
}

ScmObj
scm_api_num_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("<: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_lt_P_lst(lst);
}

ScmObj
scm_api_num_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error(">: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_gt_P_lst(lst);
}

ScmObj
scm_api_num_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("<=: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_le_P_lst(lst);
}

ScmObj
scm_api_num_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error(">=: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_ge_P_lst(lst);
}

ScmObj
scm_api_zero_P(ScmObj num)
{
  if (!scm_number_p(num)) {
    scm_capi_error("zero?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_num_zero_P(num);
}

ScmObj
scm_api_positive_P(ScmObj num)
{
  if (!scm_number_p(num)) {
    scm_capi_error("positive?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_num_positive_P(num);
}

ScmObj
scm_api_negative_P(ScmObj num)
{
  if (!scm_number_p(num)) {
    scm_capi_error("negative?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_num_negative_P(num);
}

ScmObj
scm_api_odd_P(ScmObj num)
{
  if (!scm_number_p(num)) {
    scm_capi_error("odd?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_num_odd_P(num);
}

ScmObj
scm_api_even_P(ScmObj num)
{
  if (!scm_number_p(num)) {
    scm_capi_error("even?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_num_even_P(num);
}

ScmObj
scm_api_max_lst(ScmObj lst)
{
  if (!scm_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_max_lst(lst);
}

ScmObj
scm_api_min_lst(ScmObj lst)
{
  if (!scm_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_min_lst(lst);
}

ScmObj
scm_api_plus_lst(ScmObj lst)
{
  if (!scm_nil_p(lst) && !scm_pair_p(lst)) {
    scm_capi_error("+: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_plus_lst(lst);
}

ScmObj
scm_api_mul_lst(ScmObj lst)
{
  if (!scm_nil_p(lst) && !scm_pair_p(lst)) {
    scm_capi_error("*: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_mul_lst(lst);
}

ScmObj
scm_api_minus_lst(ScmObj lst)
{
  if (!scm_pair_p(lst)) {
    scm_capi_error("-: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_num_minus_lst(lst);
}

int
scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_num_integer_p(x)) {
    scm_capi_error("floor/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_num_integer_p(y)) {
    scm_capi_error("floor/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_floor_div(x, y, q, r);
}

ScmObj
scm_api_floor_quo(ScmObj x, ScmObj y)
{
  if (!scm_num_integer_p(x)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_num_integer_p(y)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_floor_quo(x, y);
}

ScmObj
scm_api_floor_rem(ScmObj x, ScmObj y)
{
  if (!scm_num_integer_p(x)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_num_integer_p(y)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_floor_rem(x, y);
}

int
scm_capi_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_num_integer_p(x)) {
    scm_capi_error("truncate/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_num_integer_p(y)) {
    scm_capi_error("truncate/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_truncate_div(x, y, q, r);
}

ScmObj
scm_api_truncate_quo(ScmObj x, ScmObj y)
{
  if (!scm_number_p(x)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_number_p(y)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_truncate_quo(x, y);
}

ScmObj
scm_api_truncate_rem(ScmObj x, ScmObj y)
{
  if (!scm_num_integer_p(x)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_num_integer_p(y)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_num_truncate_rem(x, y);
}


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

ScmObj
scm_api_symbol_eq_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("symbol=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_symbol_eq_P_lst(lst);
}

ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if  (!scm_symbol_p(sym)) {
    scm_capi_error("symbol->string: symbol required, but got", 1, sym);
    return SCM_OBJ_NULL;
  }

  return scm_symbol_to_string(sym);
}

ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if  (!scm_string_p(str)) {
    scm_capi_error("string->symbol: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_string_to_symbol(str);
}


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

ScmObj
scm_api_char_eq_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_char_eq_P_lst(lst);
}

ScmObj
scm_api_char_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_char_lt_P_lst(lst);
}

ScmObj
scm_api_char_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char>?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_char_gt_P_lst(lst);
}

ScmObj
scm_api_char_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_char_le_P_lst(lst);
}

ScmObj
scm_api_char_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_char_ge_P_lst(lst);
}

ScmObj
scm_api_char_to_integer(ScmObj chr)
{
  if (!scm_char_p(chr)) {
    scm_capi_error("char->integer: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  return scm_char_to_integer(chr);
}

ScmObj
scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  if (!scm_num_integer_p(num)) {
    scm_capi_error("integer->char: integer required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_integer_to_char(num, enc);
}


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

ScmObj
scm_api_string_lst(ScmObj lst)
{
  return scm_api_list_to_string(lst);
}

ScmObj
scm_api_string_length(ScmObj str)
{
  size_t len;

  if (!scm_string_p(str)) {
    scm_capi_error("string-length: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  len = scm_string_length(str);
  return scm_make_number_from_size_t(len);
}

ScmObj
scm_api_string_bytesize(ScmObj str)
{
  size_t len;

  if (!scm_string_p(str)) {
    scm_capi_error("string-bytesize: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  len = scm_string_bytesize(str);
  return scm_make_number_from_size_t(len);
}

ScmObj
scm_api_string_ref(ScmObj str, ScmObj pos)
{
  size_t s;
  int r;

  if (!scm_string_p(str)) {
    scm_capi_error("string-ref: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  if (s >= scm_string_length(str)) {
    scm_capi_error("string-ref: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_ref_char(str, s);
}

ScmObj
scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr)
{
  size_t s;
  int r;

  SCM_REFSTK_INIT_REG(&str, &pos, &chr);

  if (!scm_string_p(str)) {
    scm_capi_error("string-set!: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }
  else if (!scm_char_p(chr)) {
    scm_capi_error("string-set!: character require, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  if (s >= scm_string_length(str)) {
    scm_capi_error("string-set!: out of range", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_string_set_char(str, s, chr);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_string_eq_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_string_eq_P_lst(lst);
}

ScmObj
scm_api_string_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_string_lt_P_lst(lst);
}

ScmObj
scm_api_string_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_string_gt_P_lst(lst);
}

ScmObj
scm_api_string_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_string_le_P_lst(lst);
}

ScmObj
scm_api_string_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_string_ge_P_lst(lst);
}

ScmObj
scm_api_substring(ScmObj str, ScmObj start, ScmObj end)
{
  size_t ss, se;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (!scm_string_p(str)) {
    scm_capi_error("substring: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(start)) {
    scm_capi_error("substring: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(end)) {
    scm_capi_error("substring: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(start, &ss);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_integer_to_size_t(end, &se);
  if (r < 0) return SCM_OBJ_NULL;

  if (ss > SSIZE_MAX || se > SSIZE_MAX || ss > se) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (se > scm_string_length(str)) {
    scm_capi_error("substring: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_copy(str, (ssize_t)ss, (ssize_t)se);
}

static ScmObj
string_to_list_aux(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_string_p(str)) {
    scm_capi_error("string->list: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_string_length(str)) {
    scm_capi_error("string->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(str)) {
    scm_capi_error("string->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_to_list(str, start, end);
}

ScmObj
scm_api_string_to_list(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_num_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_num_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_integer_to_size_t(start, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, start);
      return SCM_OBJ_NULL;
    }

    sss = (ssize_t)s;
  }

  sse = -1;
  if (scm_obj_not_null_p(end)) {
    size_t s;
    int r = scm_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return string_to_list_aux(str, sss, sse);
}

static ScmObj
string_copy_aux(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_string_p(str)) {
    scm_capi_error("string-copy: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_string_length(str)) {
    scm_capi_error("string-copy: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(str)) {
    scm_capi_error("string-copy: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_copy(str, start, end);
}

ScmObj
scm_api_string_copy(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_num_integer_p(start)) {
    scm_capi_error("string-copy: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_num_integer_p(end)) {
    scm_capi_error("string-copy: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_integer_to_size_t(start, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-copy: too big", 1, start);
      return SCM_OBJ_NULL;
    }

    sss = (ssize_t)s;
  }

  sse = -1;
  if (scm_obj_not_null_p(end)) {
    size_t s;
    int r = scm_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-copy: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return string_copy_aux(str, sss, sse);
}

static ScmObj
string_copy_i_aux(ScmObj to, size_t at, ScmObj from, ssize_t start, ssize_t end)
{
  int r;

  if (!scm_string_p(to)) {
    scm_capi_error("string-copy!: string require, but got", 1, to);
    return SCM_OBJ_NULL;
  }
  else if (at >= scm_string_length(to)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_string_p(from)) {
    scm_capi_error("string-copy!: string require, but got", 1, from);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-copy!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_string_copy_i(to, at, from, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_string_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t sa;
  ssize_t sss, sse;
  int r;

  SCM_REFSTK_INIT_REG(&to, &at, &from, &start, &end);

  if (!scm_num_integer_p(at)) {
    scm_capi_error("string->list: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(start) && !scm_num_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_num_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(at, &sa);
  if (r < 0) return SCM_OBJ_NULL;

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_integer_to_size_t(start, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, start);
      return SCM_OBJ_NULL;
    }

    sss = (ssize_t)s;
  }

  sse = -1;
  if (scm_obj_not_null_p(end)) {
    size_t s;
    r = scm_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return string_copy_i_aux(to, sa, from, sss, sse);
}

static ScmObj
string_fill_i_aux(ScmObj str, ScmObj fill, ssize_t start, ssize_t end)
{
  int r;

  if (!scm_string_p(str)) {
    scm_capi_error("string-fill!: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_char_p(fill)) {
    scm_capi_error("string-fill!: character required, but got", 1, fill);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-fill!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_string_fill_i(str, fill, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_num_integer_p(start)) {
    scm_capi_error("string-fill!: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_num_integer_p(end)) {
    scm_capi_error("string-fill!: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_integer_to_size_t(start, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-fill!: too big", 1, start);
      return SCM_OBJ_NULL;
    }

    sss = (ssize_t)s;
  }

  sse = -1;
  if (scm_obj_not_null_p(end)) {
    size_t s;
    r = scm_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-fill!: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return  string_fill_i_aux(str, fill, sss, sse);
}


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

ScmObj
scm_api_make_vector(ScmObj len, ScmObj fill)
{
  size_t sz;
  int r;

  if (!scm_num_integer_p(len)) {
    scm_capi_error("make-vector: integer required, but got", 1, len);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(len, &sz);
  if (r < 0) return SCM_OBJ_NULL;

  if (sz > SSIZE_MAX) {
    scm_capi_error("make-vector: too long", 0);
    return SCM_OBJ_NULL;
  }

  return scm_make_vector(sz, fill);
}

ScmObj
scm_api_vector_lst(ScmObj lst)
{
  return scm_api_list_to_vector(lst);
}

ScmObj
scm_api_vector_length(ScmObj vec)
{
  size_t len;

  if (!scm_vector_p(vec)) {
    scm_capi_error("vector-length: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  len = scm_vector_length(vec);
  return scm_make_number_from_size_t(len);
}

ScmObj
scm_api_vector_ref(ScmObj vec, ScmObj idx)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx);

  if (!scm_vector_p(vec)) {
    scm_capi_error("vector-ref: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(idx)) {
    scm_capi_error("vector-ref: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  if (i >= scm_vector_length(vec)) {
    scm_capi_error("vector-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_ref(vec, i);
}

ScmObj
scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx, &obj);

  if (!scm_vector_p(vec)) {
    scm_capi_error("vector-set!: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_integer_p(idx)) {
    scm_capi_error("vector-set!: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("vector-set!: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  if (i >= scm_vector_length(vec)) {
    scm_capi_error("vector-set!: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  scm_vector_set(vec, i, obj);
  return SCM_UNDEF_OBJ;
}

static ScmObj
vector_to_list_aux(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_vector_p(vec)) {
    scm_capi_error("vector->list: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_to_list(vec, start, end);
}

static int
vector_cnv_start_end(const char *op, ScmObj i, ssize_t *o)
{
  char msg[256];
  size_t n;

  if (scm_obj_null_p(i)) {
    *o = -1;
    return 0;
  }
  else if (!scm_num_integer_p(i)) {
    snprintf(msg, sizeof(msg), "%s: integer required, but got", op);
    scm_capi_error(msg, 1, i);
    return -1;
  }
  else {
    int r = scm_integer_to_size_t(i, &n);
    if (r < 0) return -1;

    if (n > SSIZE_MAX) {
      snprintf(msg, sizeof(msg), "%s: out of range", op);
      scm_capi_error(msg, 1, i);
      return -1;
    }

    *o = (ssize_t)n;
    return 0;
  }
}

ScmObj
scm_api_vector_to_list(ScmObj vec, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  r = vector_cnv_start_end("vector->list", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector->list", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return vector_to_list_aux(vec, s, e);
}

static ScmObj
vector_to_string_aux(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_vector_p(vec)) {
    scm_capi_error("vector->string: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_vector_length(vec)) {
    scm_capi_error("vector->string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_vector_length(vec)) {
    scm_capi_error("vector->string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_to_string(vec, start, end);
}

ScmObj
scm_api_vector_to_string(ScmObj vec, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  r = vector_cnv_start_end("vector->string", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector->string", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return vector_to_string_aux(vec, s, e);
}

static ScmObj
string_to_vector_aux(ScmObj str, ssize_t start, ssize_t end)
{
  if (!scm_string_p(str)) {
    scm_capi_error("string->vector: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
    else if (start >= 0 && (size_t)start >= scm_string_length(str)) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(str)) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_to_vector(str, start, end);
}

ScmObj
scm_api_string_to_vector(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  r = vector_cnv_start_end("string->vector", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("string->vector", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return string_to_vector_aux(str, s, e);
}

static ScmObj
vector_copy_aux(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_vector_p(vec)) {
    scm_capi_error("vector-copy: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_copy(vec, start, end);
}

ScmObj
scm_api_vector_copy(ScmObj vec, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  r = vector_cnv_start_end("vector-copy", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector-copy", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return vector_copy_aux(vec, s, e);
}

static ScmObj
vector_copy_i_aux(ScmObj to, size_t at, ScmObj from, ssize_t start, ssize_t end)
{
  int r;

  if (!scm_vector_p(to)) {
    scm_capi_error("vector-copy!: vector required, but got", 1, to);
    return SCM_OBJ_NULL;
  }
  else if (at >= scm_vector_length(to)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_vector_p(from)) {
    scm_capi_error("vector-copy!: vectore required, but got", 1, from);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_vector_copy_i(to, at, from, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_vector_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t a;
  ssize_t s, e;
  int r;

  SCM_REFSTK_INIT_REG(&to, &at, &from, &start, &end);

  if (!scm_num_integer_p(at)) {
    scm_capi_error("vector-copy!: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(at, &a);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector-copy!", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector-copy!", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return vector_copy_i_aux(to, a, from, s, e);
}

static ScmObj
vector_fill_i_aux(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end)
{
  if (!scm_vector_p(vec)) {
    scm_capi_error("vector-fill!: vectore required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
    else if (start >= 0 && (size_t)start >= scm_vector_length(vec)) {
    scm_capi_error("vector-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_vector_length(vec)) {
    scm_capi_error("vector-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-fill!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(fill)) {
    scm_capi_error("vector-fill!: invalid argument", 1, fill);
    return SCM_OBJ_NULL;
  }

  scm_vector_fill_i(vec, fill, start, end);
  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_vector_fill_i(ScmObj vec, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &fill);

  r = vector_cnv_start_end("vector-fill!", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = vector_cnv_start_end("vector-fill!", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return vector_fill_i_aux(vec, fill, s, e);
}


/*******************************************************************/
/*  Exceptions                                                     */
/*******************************************************************/

int
scm_capi_raise(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("raise: invalid argument", 1, obj);
    return -1;
  }

  return scm_raise(obj);
}

int
scm_capi_raise_continuable(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("raise-continuable: invalid argument", 1, obj);
    return -1;
  }

  return scm_raise_continuable(obj);
}

ScmObj
scm_api_push_exception_handler(ScmObj handler)
{
  int r;

  if (!scm_procedure_p(handler)) {
    scm_capi_error("failed to install exception handler: "
                   "invalid argument", 1, handler);
    return SCM_OBJ_NULL;
  }

  r = scm_push_exception_handler(handler);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_pop_exception_handler(void)
{
  int r;

  r = scm_pop_exception_handler();
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_error_lst(ScmObj msg, ScmObj irris)
{
  if (!scm_string_p(msg)) {
    scm_capi_error("error: string required, but got", 1, msg);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(irris)) {
    scm_capi_error("error: invalid argument", 1, irris);
    return SCM_OBJ_NULL;
  }

  return scm_error_lst(msg, irris);
}

ScmObj
scm_api_error_object_message(ScmObj obj)
{
  if (!scm_error_object_p(obj)) {
    scm_capi_error("error-object-message: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_error_obj_message(obj);
}

ScmObj
scm_api_error_object_irritants(ScmObj obj)
{
  if (!scm_error_object_p(obj)) {
    scm_capi_error("error-object-irritants: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_error_obj_irritants(obj);
}


/*******************************************************************/
/*  Ports                                                          */
/*******************************************************************/

ScmObj
scm_api_open_input_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_string_p(path)) {
    scm_capi_error("open-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_string_to_path_cstr(path, path_str, sizeof(path_str));
  if (r < 0) return SCM_OBJ_NULL;

  return scm_open_input_file(path_str, NULL);
}

ScmObj
scm_api_open_binary_input_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_string_p(path)) {
    scm_capi_error("open-binary-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_string_to_path_cstr(path, path_str, sizeof(path_str));
  if (r < 0) return SCM_OBJ_NULL;

  return scm_open_binary_input_file(path_str);
}

ScmObj
scm_api_open_output_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_string_p(path)) {
    scm_capi_error("open-output-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_string_to_path_cstr(path, path_str, sizeof(path_str));
  if (r < 0) return SCM_OBJ_NULL;

  return scm_open_output_file(path_str, NULL);
}

ScmObj
scm_api_open_binary_output_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_string_p(path)) {
    scm_capi_error("open-binary-output-file: string required, but got",
                   1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_string_to_path_cstr(path, path_str, sizeof(path_str));
  if (r < 0) return SCM_OBJ_NULL;

  return scm_open_binary_output_file(path_str);
}

ScmObj
scm_api_close_port(ScmObj port)
{
  int r;

  if (!scm_port_p(port)) {
    scm_capi_error("close-port: port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_close_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_input_port(ScmObj port)
{
  int r;

  if (!scm_input_port_p(port)) {
    scm_capi_error("close-input-port: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_close_input_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_output_port(ScmObj port)
{
  int r;

  if (!scm_output_port_p(port)) {
    scm_capi_error("close-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  r = scm_close_output_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_open_input_string(ScmObj str)
{
  if (!scm_string_p(str)) {
    scm_capi_error("open-input-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_open_input_string(str);
}

ScmObj
scm_api_get_output_string(ScmObj port)
{
  if (!scm_output_port_p(port)) {
    scm_capi_error("get-output-string: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_string_port_p(port)) {
    scm_capi_error("get-output-string: string-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_textual_port_p(port)) {
    scm_capi_error("get-output-string: "
                   "textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  return scm_get_output_string(port);
}

ScmObj
scm_api_open_input_bytevector(ScmObj vec)
{
  if (!scm_bytevector_p(vec)) {
    scm_capi_error("open-input-bytevector: bytevector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  return scm_open_input_bytevector(vec);
}

ScmObj
scm_api_get_output_bytevector(ScmObj port)
{
  if (!scm_output_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_string_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "bytevector-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_binary_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "binary-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  return scm_get_output_bytevector(port);
}


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj
scm_api_read(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("read: input-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("read: textual-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_read(port);
}

ScmObj
scm_api_read_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("read-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("read-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_read_char(port);
}

ScmObj
scm_api_peek_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("peek-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("peek-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_peek_char(port);
}

ScmObj
scm_api_read_line(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("read-line: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("read-line: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_read_line(port);
}

ScmObj
scm_api_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("char-ready?: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("char-ready?: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  ret = scm_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_read_string(ScmObj n, ScmObj port)
{
  size_t nc;
  int r;

  SCM_REFSTK_INIT_REG(&n, &port);

  if (!scm_num_integer_p(n)) {
    scm_capi_error("read-string: integer required, but got", 1, n);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_input_port_p(port)) {
      scm_capi_error("read-string: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("read-string: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  if (scm_num_negative_p(n))
    return scm_make_string_from_bin(NULL, 0, scm_port_internal_enc(port));

  r = scm_integer_to_size_t(n, &nc);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_read_string(nc, port);
}


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

ScmObj
scm_api_write(ScmObj obj, ScmObj port)
{
  return scm_api_write_shared(obj, port);
}

ScmObj
scm_api_write_shared(ScmObj obj, ScmObj port)
{
  int rslt;

  if (scm_obj_null_p(obj)) {
    scm_capi_error("write-shared: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("write-shared: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("write-shared: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_write_shared(obj, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write_simple(ScmObj obj, ScmObj port)
{
  int rslt;

  if (scm_obj_null_p(obj)) {
    scm_capi_error("write-simple: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("write-simple: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("write-simple: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_write_simple(obj, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_display(ScmObj obj, ScmObj port)
{
  int rslt;

  if (scm_obj_null_p(obj)) {
    scm_capi_error("display: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("display: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("display: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_display(obj, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_newline(ScmObj port)
{
  int rslt;

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("newline: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("newline: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_newline(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write_char(ScmObj chr, ScmObj port)
{
  int rslt;

  if (!scm_char_p(chr)) {
    scm_capi_error("write-char: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("write-char: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("write-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_write_char(chr, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
write_string_aux(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  int r;

  if (!scm_string_p(str)) {
    scm_capi_error("write-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_output_port_p(port)) {
      scm_capi_error("write-string: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_textual_port_p(port)) {
      scm_capi_error("write-string: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  if (start >= 0 && (size_t)start >= scm_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("write-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_write_string(str, port, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &port, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_num_integer_p(start)) {
    scm_capi_error("write-string: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_num_integer_p(end)) {
    scm_capi_error("write-string: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_integer_to_size_t(start, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("write-string: too big", 1, start);
      return SCM_OBJ_NULL;
    }

    sss = (ssize_t)s;
  }

  sse = -1;
  if (scm_obj_not_null_p(end)) {
    size_t s;
    int r = scm_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("write-string: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return write_string_aux(str, port, sss, sse);
}

ScmObj
scm_api_flush_output_port(ScmObj port)
{
  int rslt;

  if (scm_obj_not_null_p(port) && !scm_output_port_p(port)) {
    scm_capi_error("flush-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_flush_output_port(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

ScmObj
scm_api_make_syntax(ScmObj keyword, ScmObj handler)
{
  if (!scm_symbol_p(keyword)) {
    scm_capi_error("failed to make syntax object: "
                   "invalid argument", 1, keyword);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(handler)) {
    scm_capi_error("failed to make syntax object: "
                   "invalid argument", 1, handler);
    return SCM_OBJ_NULL;
  }

  return scm_make_syntax(keyword, handler);
}

ScmObj
scm_api_syntax_keyword(ScmObj syx)
{
  if (!scm_syntax_p(syx)) {
    scm_capi_error("failed to get syntax keyword: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_syntax_keyword(syx);
}

ScmObj
scm_api_syntax_handler(ScmObj syx)
{
  if (!scm_syntax_p(syx)) {
    scm_capi_error("failed to get syntax handler: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_syntax_handler(syx);
}


/*******************************************************************/
/*  Macro                                                          */
/*******************************************************************/

ScmObj
scm_api_make_macro(ScmObj transformer, ScmObj env)
{
  if (!scm_procedure_p(transformer)) {
    scm_capi_error("failed to make a macro object: procedure required, but got",
                   1, transformer);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(env)) {
    scm_capi_error("failed to make a macro object: invalid argument", 1, env);
    return SCM_OBJ_NULL;
  }

  return scm_make_macro(transformer, env);
}

ScmObj
scm_api_macro_env(ScmObj macro)
{
  if (!scm_macro_p(macro)) {
    scm_capi_error("failed to get syntactic environment of the macro: "
                   "macro required, but got", 1, macro);
    return SCM_OBJ_NULL;
  }

  return scm_macro_env(macro);
}

int
scm_api_trmp_macro_transformer(ScmObj macro, ScmObj form, ScmObj use_env)
{
  if (!scm_macro_p(macro)) {
    scm_capi_error("failed to call transformer: macro requird, but got", 1, macro);
    return -1;
  }
  else if (scm_obj_null_p(form)) {
    scm_capi_error("failed to call transformer: invalid argument", 1, form);
    return -1;
  }
  else if (scm_obj_null_p(use_env)) {
    scm_capi_error("failed to call transformer: invalid argument", 1, use_env);
  }

  return scm_macro_trmp_transformer(macro, form, use_env);
}


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj
scm_api_assemble(ScmObj lst, ScmObj acc)
{
  if (!(scm_pair_p(lst) || scm_nil_p(lst))) {
    scm_capi_error("asm: pair required, but got", 1, lst);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(acc)
      && !scm_iseq_p(acc) && !scm_assembler_p(acc)) {
    scm_capi_error("asm: iseq or assembler required, but got", 1, acc);
    return SCM_OBJ_NULL;
  }

  return scm_assemble(lst, acc);
}

ScmObj
scm_api_make_assebmler(ScmObj iseq)
{
  if (scm_obj_not_null_p(iseq) && !scm_iseq_p(iseq)) {
    scm_capi_error("make-assembler: <iseq> required, but got", 1, iseq);
    return SCM_OBJ_NULL;
  }

  return scm_make_assembler(iseq);
}

ScmObj
scm_api_assembler_assgin_label_id_i(ScmObj asmb)
{
  ssize_t i;

  if (!scm_assembler_p(asmb)) {
    scm_capi_error("assembler-assgin-label-id!: <assmbler> required, but got",
                   1, asmb);
    return SCM_OBJ_NULL;
  }

  i = scm_asm_assign_label_id(asmb);
  if (i < 0) return SCM_OBJ_NULL;

  return scm_make_number_from_size_t((size_t)i);
}

ScmObj
scm_api_assembler_push_i_cv(ScmObj asmb, const ScmObj *cv, size_t n)
{
  ScmObj r = SCM_OBJ_INIT;

  if (!scm_assembler_p(asmb)) {
    scm_capi_error("assembler-push!: <assmbler> required, but got", 1, asmb);
    return SCM_OBJ_NULL;
  }
  else if (n > 0 && cv == NULL) {
    scm_capi_error("assembler-push!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  for (size_t i = 0; i < n; i++) {
    if (scm_obj_null_p(cv[i])) {
      scm_capi_error("assembler-push!: invalid-argument", 0);
      return SCM_OBJ_NULL;
    }
  }

  r = scm_assemble_1inst_cv(cv, n, asmb);
  if (scm_obj_null_p(r)) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_assembler_commit_i(ScmObj asmb)
{
  int r;

  if (!scm_assembler_p(asmb)) {
    scm_capi_error("assembler-assgin-label-id!: <assmbler> required, but got",
                   asmb);
    return SCM_OBJ_NULL;
  }

  r = scm_asm_commit(asmb);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

ScmObj
scm_api_compiler_base_env(ScmObj cmpl)
{
  if (!scm_compiler_p(cmpl)) {
    scm_capi_error("failed to get current module: invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_cmpl_base_env(cmpl);
}

ScmObj
scm_api_compiler_current_expr(ScmObj cmpl)
{
  if (!scm_compiler_p(cmpl)) {
    scm_capi_error("failed to get current expression: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_cmpl_expr(cmpl);
}

ScmObj
scm_api_compiler_select_base_env_i(ScmObj cmpl, ScmObj env)
{
  SCM_REFSTK_INIT_REG(&cmpl, &env);

  if (!scm_compiler_p(cmpl)) {
    scm_capi_error("failed to change current module: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(env)) {
    scm_capi_error("failed to change current module: "
                   "invalid argument", 1, env);
    return SCM_OBJ_NULL;
  }

  scm_cmpl_select_base_env(cmpl, env);
  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_compiler_select_module_i(ScmObj cmpl, ScmObj mod)
{
  int r;

  SCM_REFSTK_INIT_REG(&cmpl, &mod);

  if (!scm_compiler_p(cmpl)) {
    scm_capi_error("failed to change current module: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  r = scm_cmpl_select_module(cmpl, mod);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_compiler_select_expr_i(ScmObj cmpl, ScmObj expr)
{
  SCM_REFSTK_INIT_REG(&cmpl, &expr);

  if (!scm_compiler_p(cmpl)) {
    scm_capi_error("failed to change current expression: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(expr)) {
    scm_capi_error("failed to change current expression: "
                   "invalid argument", 1, expr);
    return SCM_OBJ_NULL;
  }

  scm_cmpl_select_expr(cmpl, expr);
  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/* Quasiquotation                                                  */
/*******************************************************************/

ScmObj
scm_api_compile_qq_template(ScmObj tmpl)
{
  if (scm_obj_null_p(tmpl)) {
    scm_capi_error("failed to compile quasiquotation: invalid argument",
                   1, tmpl);
    return SCM_OBJ_NULL;
  }

  return scm_compile_qq_template(tmpl);
}

ScmObj
scm_api_substitute_qq_template_lst(ScmObj tmpl, ScmObj values)
{
  if (!scm_qqtmpl_p(tmpl)) {
    scm_capi_error("failed to substitute quasiquotation: "
                   "compiled <qq template> required, but got", 1, tmpl);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(values)) {
    scm_capi_error("failed to substitute quasiquotation: invalid argument",
                   1, values);
  }

  return scm_substitute_qq_template(tmpl, values);
}

ScmObj
scm_api_qq_template_num_of_unquoted(ScmObj tmpl)
{
  if (!scm_qqtmpl_p(tmpl)) {
    scm_capi_error("failed to access number of unquoted expressions: "
                   "compiled <qq template> required, but got", 1, tmpl);
    return SCM_OBJ_NULL;
  }

  return scm_make_number_from_size_t(scm_qqtmpl_nr_unquoted_expr(tmpl));
}

ScmObj
scm_api_qq_template_unquoted(ScmObj tmpl, ScmObj idx)
{
  size_t i;
  int r;

  if (!scm_qqtmpl_p(tmpl)) {
    scm_capi_error("failed to access unquoted expressions: "
                   "compiled <qq template> required, but got", 1, tmpl);
    return SCM_OBJ_NULL;
  }
  else if (!scm_num_exact_integer_p(idx)) {
    scm_capi_error("failed to access unquoted expressions: invalid argument",
                   1, tmpl);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  if (i >= scm_qqtmpl_nr_unquoted_expr(tmpl)) {
    scm_capi_error("failed to access unquoted expressions: "
                   "argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_qqtmpl_unquoted_expr(tmpl, i);
}


/*******************************************************************/
/* Identifier                                                      */
/*******************************************************************/

ScmObj
scm_api_make_identifier(ScmObj name, ScmObj env)
{
  if (!scm_symbol_p(name)) {
    scm_capi_error("failed to make a identifier object: symbol required, but got",
                   1, name);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(env)) {
    scm_capi_error("failed to make a identifier object: invalid argument",
                   1, env);
    return SCM_OBJ_NULL;
  }

  return scm_make_identifier(name, env);
}

ScmObj
scm_api_identifier_name(ScmObj ident)
{
  if (!scm_identifier_p(ident)) {
    scm_capi_error("failed to get the identifier name: "
                   "identifier required, but got", 1, ident);
    return SCM_OBJ_NULL;
  }

  return scm_ident_name(ident);
}

ScmObj
scm_api_identifier_env(ScmObj ident)
{
  if (!scm_identifier_p(ident)) {
    scm_capi_error("failed to get the syntactic environmanet "
                   "associated with the identifier: "
                   "identifier required, but got", 1, ident);
    return SCM_OBJ_NULL;
  }

  return scm_ident_env(ident);
}


/*******************************************************************/
/* Dynamic Bindings                                                */
/*******************************************************************/

ScmObj
scm_api_push_dynamic_bindings(ScmObj alist)
{
  ScmObj x = SCM_OBJ_INIT;
  int r;

  if (!scm_nil_p(alist) && !scm_pair_p(alist)) {
    scm_capi_error("failed to contsruct dynamic bindins: "
                   "alist required, but got", 1, alist);
    return SCM_OBJ_NULL;
  }

  for (x = alist; scm_pair_p(x); x = scm_cdr(x)) {
    if (!scm_parameter_p(scm_cxr(x, "aa"))) {
      scm_capi_error("failed to contsruct dynamic bindins: "
                     "parameter object required, but got",
                     1, scm_cxr(x, "aa"));
      return SCM_OBJ_NULL;
    }
  }

  r = scm_push_dynamic_bindings(alist);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_pop_dynamic_bindings(void)
{
  scm_pop_dynamic_bindings();
  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/* Dynamic Wind                                                    */
/*******************************************************************/

ScmObj
scm_api_push_dynamic_wind_handler(ScmObj before, ScmObj after)
{
  int r;

  if (!scm_procedure_p(before)) {
    scm_capi_error("failed to push the dynaimc-wind handler: "
                   "precedure required, but got", 1, before);
    return SCM_OBJ_NULL;
  }
  else if (!scm_procedure_p(after)) {
    scm_capi_error("failed to push the dynaimc-wind handler: "
                   "precedure required, but got", 1, after);
    return SCM_OBJ_NULL;
  }

  r = scm_push_dynamic_wind_handler(before, after);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_pop_dynamic_wind_handler(void)
{
  scm_pop_dynamic_wind_handler();
  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/* Record                                                          */
/*******************************************************************/

ScmObj
scm_api_make_record_type(ScmObj name)
{
  if (!scm_symbol_p(name)) {
    scm_capi_error("failed to make a record type: symbol required, but got",
                   1, name);
    return SCM_OBJ_NULL;
  }

  return scm_make_recordtype(name);
}

ScmObj
scm_capi_make_record(ScmObj type, size_t n, ScmObj slots)
{
  if (!scm_recordtype_p(type)) {
    scm_capi_error("failed to make a record: "
                   "record-type required, but got", 1, type);
    return SCM_OBJ_NULL;
  }
  else if (n > 0 && !scm_pair_p(slots)) {
    scm_capi_error("failed to make a record: "
                   "pair required, but got", 1, slots);
    return SCM_OBJ_NULL;
  }

  return scm_make_record(type, n, slots);
}

ScmObj
scm_api_make_record(ScmObj type, ScmObj n, ScmObj slots)
{
  size_t nr;
  int r;

  if (!scm_num_integer_p(n)) {
    scm_capi_error("failed to make a record: integer required, but got", 1, n);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(n, &nr);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_record(type, nr, slots);
}

ScmObj
scm_api_record_type(ScmObj rec)
{
  if (!scm_record_p(rec)) {
    scm_capi_error("failed to refer record-type of a record: "
                   "record required, but got", 1, rec);
    return SCM_OBJ_NULL;
  }

  return scm_record_type(rec);
}

ScmObj
scm_capi_record_ref(ScmObj rec, size_t i)
{
  if (!scm_record_p(rec)) {
    scm_capi_error("failed to refer field value of a record: "
                   "record required, but got", 1, rec);
    return SCM_OBJ_NULL;
  }
  else if (i >= scm_record_nr_slots(rec)) {
    scm_capi_error("failed to refer field value of a record: "
                   "out of range", 0);
    return SCM_OBJ_NULL;

  }

  return scm_record_slot_ref(rec, i);
}

ScmObj
scm_api_record_ref(ScmObj rec, ScmObj i)
{
  size_t idx;
  int r;

  if (!scm_num_integer_p(i)) {
    scm_capi_error("failed to refer field value of a record: "
                   "integer required, but got", 1, i);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(i, &idx);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_record_ref(rec, idx);
}

int
scm_capi_record_set_i(ScmObj rec, size_t i, ScmObj val)
{
  if (!scm_record_p(rec)) {
    scm_capi_error("failed to modify field value of a record: "
                   "record required, but got", 1, rec);
    return -1;
  }
  else if (i >= scm_record_nr_slots(rec)) {
    scm_capi_error("failed to modify field value of a record: "
                   "out of range", 0);
    return -1;

  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to modify field value of a record: "
                   "invalid argument", 1, val);
    return -1;
  }

  scm_record_slot_set(rec, i, val);
  return 0;
}

ScmObj
scm_api_record_set_i(ScmObj rec, ScmObj i, ScmObj val)
{
  size_t idx;
  int r;

  if (!scm_num_integer_p(i)) {
    scm_capi_error("failed to modify field value of a record: "
                   "integer required, but got", 1, i);
    return SCM_OBJ_NULL;
  }

  r = scm_integer_to_size_t(i, &idx);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_record_set_i(rec, idx, val);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/* System interface                                                */
/*******************************************************************/

ScmObj
scm_api_file_exists_P(ScmObj path)
{
  bool exists;
  int r;

  if (!scm_string_p(path)) {
    scm_capi_error("file-exists?: string required but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_file_exists(path, &exists);
  if (r < 0) return SCM_OBJ_NULL;

  return (exists ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_api_delete_file(ScmObj path)
{
  int r;

  if (!scm_string_p(path)) {
    scm_capi_error("delete-file: string required but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_delete_file(path);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

ScmObj
scm_api_module_P(ScmObj module)
{
  return scm_module_P(module);
}

ScmObj
scm_api_module_name(ScmObj module)
{
  if (!scm_module_p(module)) {
    scm_capi_error("failed to get a name from module: "
                   "invalid argument", 1, module);
    return SCM_OBJ_NULL;
  }

  return scm_module_name(module);
}

ScmObj
scm_api_module_export(ScmObj module, ScmObj sym)
{
  int r;

  if (!scm_module_p(module)) {
    scm_capi_error("failed to export identifier: invalid argument", module);
    return SCM_OBJ_NULL;
  }
  else if (!scm_symbol_p(sym)) {
    scm_capi_error("failed to export identifier: invalid argument", sym);
    return SCM_OBJ_NULL;
  }

  r = scm_module_export(module, sym);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export)
{
  if (!scm_module_specifier_p(module)) {
    scm_capi_error("failed to define variable: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_symbol_p(sym)) {
    scm_capi_error("failed to define variable: invalid argument", 1, sym);
    return -1;
  }
  else if (scm_obj_null_p(val) || scm_landmine_object_p(val)) {
    scm_capi_error("failed to define variable: invalid argument", 1, val);
    return -1;
  }

  return scm_define_global_var(module, sym, val, export);
}

int
scm_capi_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  if (!scm_module_specifier_p(module)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_symbol_p(sym)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, sym);
    return -1;
  }
  else if (scm_obj_null_p(syx) || scm_landmine_object_p(syx)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, syx);
    return -1;
  }

  return scm_define_global_syx(module, sym, syx, export);
}


int
scm_capi_refer_global_syx(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  if (!scm_module_specifier_p(module)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_symbol_p(sym)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, sym);
    return -1;
  }

  return scm_refer_global_syx(module, sym, syx);
}


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int
scm_capi_trampolining(ScmObj proc, ScmObj args,
                      ScmObj postproc, ScmObj handover)
{
  if (!scm_procedure_p(proc)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (!scm_pair_p(args) && !scm_nil_p(args)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (scm_obj_not_null_p(postproc) && !scm_procedure_p(postproc)) {
    scm_capi_error("", 0);
    return -1;
  }

  return scm_trampolining(proc, args, postproc, handover);
}


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

ScmObj
scm_api_format_lst(ScmObj fmt, ScmObj lst)
{
  if (!scm_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }

  return scm_format_lst(fmt, lst);
}


/*******************************************************************/
/*  Exit                                                           */
/*******************************************************************/

ScmObj
scm_api_exit(ScmObj obj)
{
  scm_exit(obj);
  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Scythe                                                         */
/*******************************************************************/

int
scm_capi_scythe_init(void)
{
  return scm_prepare_scythe();
}

ScmScythe *
scm_capi_scythe_new(void)
{
  return scm_scythe_new();
}

void
scm_capi_scythe_end(ScmScythe *scy)
{
  if (scy == NULL)
    return;

  return scm_scythe_end(scy);
}

int
scm_capi_scythe_bootup(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  return scm_scythe_bootup(scy);
}

void
scm_capi_scythe_shutdown(ScmScythe *scy)
{
  if (scy == NULL)
    return;

  scm_scythe_shutdown(scy);
}

int
scm_capi_scythe_load_core(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  return scm_scythe_load_core(scy);
}

int
scm_capi_scythe_add_load_path(ScmScythe *scy, const char *path)
{
  if (scy == NULL || path == NULL)
    return -1;

  return scm_scythe_add_load_path(scy, path);
}

int
scm_capi_scythe_clear_load_path(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  scm_scythe_clear_load_path(scy);
  return 0;
}

int
scm_capi_scythe_add_load_suffix(ScmScythe *scy, const char *suffix)
{
  if (scy == NULL || suffix == NULL)
    return -1;

  return scm_scythe_add_load_suffix(scy, suffix);
}

int
scm_capi_scythe_clear_load_suffix(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  scm_scythe_clear_load_suffix(scy);
  return 0;
}

int
scm_capi_scythe_default_setup(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  return scm_scythe_default_setup(scy);
}

int
scm_capi_scythe_apply(ScmScythe *scy, const char *cmd,
                      const char * const *args, size_t n)
{
  if (scy == NULL)
    return -1;
  else if (cmd == NULL)
    return -1;
  else if (n > 0 && args == NULL)
    return -1;

  return scm_scythe_apply(scy, cmd, args, n);
}

int
scm_capi_scythe_run_repl(ScmScythe *scy)
{
  if (scy == NULL)
    return -1;

  return scm_scythe_run_repl(scy);
}

int
scm_capi_scythe_exec_file(ScmScythe *scy, const char * const *argv, size_t n)
{
  if (scy == NULL || argv == NULL)
    return -1;

  return scm_scythe_exec_file(scy, argv, n);
}

int
scm_capi_scythe_eval_str(ScmScythe *scy, const char *expr)
{
  if (scy == NULL || expr == NULL)
    return -1;

  return scm_scythe_eval_str(scy, expr);
}

int
scm_capi_scythe_compile_file(ScmScythe *scy,
                             const char *path, const char *output)
{
  if (scy == NULL || path == NULL)
    return -1;

  return scm_scythe_compile_file(scy, path, output);
}
