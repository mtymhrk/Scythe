#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
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

  return scm_fcd_cons(car, cdr);
}

ScmObj
scm_api_car(ScmObj pair)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("car: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_car(pair);
}

ScmObj
scm_api_cdr(ScmObj pair)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("cdr: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_cdr(pair);
}

ScmObj
scm_api_set_car_i(ScmObj pair, ScmObj elm)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 1, elm);
    return SCM_OBJ_NULL;
  }

  scm_fcd_set_car_i(pair, elm);

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_set_cdr_i(ScmObj pair, ScmObj elm)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("set-cdr!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-cdr!: invalid argument", 1, elm);
    return SCM_OBJ_NULL;
  }

  scm_fcd_set_cdr_i(pair, elm);

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_make_list(ScmObj n, ScmObj fill)
{
  size_t s;
  int r;

  if (!scm_fcd_integer_p(n)) {
    scm_capi_error("make-list: invalid argument", 1, n);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_make_list(s, fill);
}

ssize_t
scm_capi_length(ScmObj lst)
{
  if (!scm_fcd_nil_p(lst) && !scm_fcd_pair_p(lst)) {
    scm_capi_error("length: list required, but got", 1, lst);
    return -1;
  }

  return (ssize_t)scm_fcd_length(lst);
}

ScmObj
scm_api_length(ScmObj lst)
{
  ssize_t len;

  len = scm_capi_length(lst);
  if (len < 0) return SCM_OBJ_NULL;

  return scm_fcd_make_number_from_sword(len);
}

ScmObj
scm_api_append_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("append: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_append_lst(lst);
}

ScmObj
scm_api_reverse(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("reverse: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_reverse(lst);
}

ScmObj
scm_api_list_tail(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_fcd_integer_p(n)) {
    scm_capi_error("list-tail: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_list_tail(lst, s);
}

ScmObj
scm_api_list_ref(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_fcd_integer_p(n)) {
    scm_capi_error("list-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_list_ref(lst, s);
}

ScmObj
scm_api_list_set_i(ScmObj lst, ScmObj n, ScmObj obj)
{
  size_t s;
  int r;

  if (!scm_fcd_integer_p(n)) {
    scm_capi_error("list-set!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_list_set_i(lst, s, obj);
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

  return scm_fcd_num_eq_P_lst(lst);
}

ScmObj
scm_api_num_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("<: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_num_lt_P_lst(lst);
}

ScmObj
scm_api_num_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error(">: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_num_gt_P_lst(lst);
}

ScmObj
scm_api_num_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("<=: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_num_le_P_lst(lst);
}

ScmObj
scm_api_num_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error(">=: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_num_ge_P_lst(lst);
}

ScmObj
scm_api_zero_P(ScmObj num)
{
  if (!scm_fcd_number_p(num)) {
    scm_capi_error("zero?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_zero_P(num);
}

ScmObj
scm_api_positive_P(ScmObj num)
{
  if (!scm_fcd_number_p(num)) {
    scm_capi_error("positive?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_positive_P(num);
}

ScmObj
scm_api_negative_P(ScmObj num)
{
  if (!scm_fcd_number_p(num)) {
    scm_capi_error("negative?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_negative_P(num);
}

ScmObj
scm_api_odd_P(ScmObj num)
{
  if (!scm_fcd_number_p(num)) {
    scm_capi_error("odd?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_odd_P(num);
}

ScmObj
scm_api_even_P(ScmObj num)
{
  if (!scm_fcd_number_p(num)) {
    scm_capi_error("even?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_even_P(num);
}

ScmObj
scm_api_max_lst(ScmObj lst)
{
  if (!scm_fcd_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_max_lst(lst);
}

ScmObj
scm_api_min_lst(ScmObj lst)
{
  if (!scm_fcd_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_min_lst(lst);
}

ScmObj
scm_api_plus_lst(ScmObj lst)
{
  if (!scm_fcd_nil_p(lst) && !scm_fcd_pair_p(lst)) {
    scm_capi_error("+: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_plus_lst(lst);
}

ScmObj
scm_api_mul_lst(ScmObj lst)
{
  if (!scm_fcd_nil_p(lst) && !scm_fcd_pair_p(lst)) {
    scm_capi_error("*: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_mul_lst(lst);
}

ScmObj
scm_api_minus_lst(ScmObj lst)
{
  if (!scm_fcd_pair_p(lst)) {
    scm_capi_error("-: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_minus_lst(lst);
}

int
scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_fcd_integer_p(x)) {
    scm_capi_error("floor/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_integer_p(y)) {
    scm_capi_error("floor/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_div(x, y, q, r);
}

ScmObj
scm_api_floor_quo(ScmObj x, ScmObj y)
{
  if (!scm_fcd_integer_p(x)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_integer_p(y)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_quo(x, y);
}

ScmObj
scm_api_floor_rem(ScmObj x, ScmObj y)
{
  if (!scm_fcd_integer_p(x)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_integer_p(y)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_rem(x, y);
}

int
scm_capi_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_fcd_integer_p(x)) {
    scm_capi_error("truncate/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_integer_p(y)) {
    scm_capi_error("truncate/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_div(x, y, q, r);
}

ScmObj
scm_api_truncate_quo(ScmObj x, ScmObj y)
{
  if (!scm_fcd_number_p(x)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_number_p(y)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_quo(x, y);
}

ScmObj
scm_api_truncate_rem(ScmObj x, ScmObj y)
{
  if (!scm_fcd_integer_p(x)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_fcd_integer_p(y)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_rem(x, y);
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

  return scm_fcd_symbol_eq_P_lst(lst);
}

ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if  (!scm_fcd_symbol_p(sym)) {
    scm_capi_error("symbol->string: symbol required, but got", 1, sym);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_symbol_to_string(sym);
}

ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if  (!scm_fcd_string_p(str)) {
    scm_capi_error("string->symbol: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_symbol(str);
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

  return scm_fcd_char_eq_P_lst(lst);
}

ScmObj
scm_api_char_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_lt_P_lst(lst);
}

ScmObj
scm_api_char_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char>?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_gt_P_lst(lst);
}

ScmObj
scm_api_char_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_le_P_lst(lst);
}

ScmObj
scm_api_char_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("char=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_ge_P_lst(lst);
}

ScmObj
scm_api_char_to_integer(ScmObj chr)
{
  if (!scm_fcd_char_p(chr)) {
    scm_capi_error("char->integer: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_to_integer(chr);
}

ScmObj
scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  if (!scm_fcd_integer_p(num)) {
    scm_capi_error("integer->char: integer required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_integer_to_char(num, enc);
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

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-length: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  len = scm_fcd_string_length(str);
  return scm_fcd_make_number_from_size_t(len);
}

ScmObj
scm_api_string_bytesize(ScmObj str)
{
  size_t len;

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-bytesize: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  len = scm_fcd_string_bytesize(str);
  return scm_fcd_make_number_from_size_t(len);
}

ScmObj
scm_api_string_ref(ScmObj str, ScmObj pos)
{
  size_t s;
  int r;

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-ref: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  if (s >= scm_fcd_string_length(str)) {
    scm_capi_error("string-ref: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_ref(str, s);
}

ScmObj
scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr)
{
  size_t s;
  int r;

  SCM_REFSTK_INIT_REG(&str, &pos, &chr);

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-set!: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_char_p(chr)) {
    scm_capi_error("string-set!: character require, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  if (s >= scm_fcd_string_length(str)) {
    scm_capi_error("string-set!: out of range", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_string_set_i(str, s, chr);
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

  return scm_fcd_string_eq_P_lst(lst);
}

ScmObj
scm_api_string_lt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_lt_P_lst(lst);
}

ScmObj
scm_api_string_gt_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_gt_P_lst(lst);
}

ScmObj
scm_api_string_le_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_le_P_lst(lst);
}

ScmObj
scm_api_string_ge_P_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>=?: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_ge_P_lst(lst);
}

ScmObj
scm_api_substring(ScmObj str, ScmObj start, ScmObj end)
{
  size_t ss, se;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("substring: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(start)) {
    scm_capi_error("substring: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(end)) {
    scm_capi_error("substring: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(start, &ss);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_integer_to_size_t(end, &se);
  if (r < 0) return SCM_OBJ_NULL;

  if (ss > SSIZE_MAX || se > SSIZE_MAX || ss > se) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (se > scm_fcd_string_length(str)) {
    scm_capi_error("substring: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_substring(str, ss, se);
}

static ScmObj
string_to_list_aux(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string->list: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("string->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("string->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_list(str, start, end);
}

ScmObj
scm_api_string_to_list(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_fcd_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_fcd_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_fcd_integer_to_size_t(start, &s);
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
    int r = scm_fcd_integer_to_size_t(end, &s);
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

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-copy: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("string-copy: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("string-copy: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_copy(str, start, end);
}

ScmObj
scm_api_string_copy(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_fcd_integer_p(start)) {
    scm_capi_error("string-copy: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_fcd_integer_p(end)) {
    scm_capi_error("string-copy: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_fcd_integer_to_size_t(start, &s);
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
    int r = scm_fcd_integer_to_size_t(end, &s);
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

  if (!scm_fcd_string_p(to)) {
    scm_capi_error("string-copy!: string require, but got", 1, to);
    return SCM_OBJ_NULL;
  }
  else if (at >= scm_fcd_string_length(to)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_string_p(from)) {
    scm_capi_error("string-copy!: string require, but got", 1, from);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-copy!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_string_copy_i(to, at, from, start, end);
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

  if (!scm_fcd_integer_p(at)) {
    scm_capi_error("string->list: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(start) && !scm_fcd_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_fcd_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(at, &sa);
  if (r < 0) return SCM_OBJ_NULL;

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_fcd_integer_to_size_t(start, &s);
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
    r = scm_fcd_integer_to_size_t(end, &s);
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

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string-fill!: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_char_p(fill)) {
    scm_capi_error("string-fill!: character required, but got", 1, fill);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-fill!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_string_fill_i(str, fill, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_fcd_integer_p(start)) {
    scm_capi_error("string-fill!: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_fcd_integer_p(end)) {
    scm_capi_error("string-fill!: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_fcd_integer_to_size_t(start, &s);
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
    r = scm_fcd_integer_to_size_t(end, &s);
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

  if (!scm_fcd_integer_p(len)) {
    scm_capi_error("make-vector: integer required, but got", 1, len);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(len, &sz);
  if (r < 0) return SCM_OBJ_NULL;

  if (sz > SSIZE_MAX) {
    scm_capi_error("make-vector: too long", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_vector(sz, fill);
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

  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector-length: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  len = scm_fcd_vector_length(vec);
  return scm_fcd_make_number_from_size_t(len);
}

ScmObj
scm_api_vector_ref(ScmObj vec, ScmObj idx)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx);

  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector-ref: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(idx)) {
    scm_capi_error("vector-ref: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  if (i >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_ref(vec, i);
}

ScmObj
scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx, &obj);

  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector-set!: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_integer_p(idx)) {
    scm_capi_error("vector-set!: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("vector-set!: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  if (i >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-set!: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  scm_fcd_vector_set_i(vec, i, obj);
  return SCM_UNDEF_OBJ;
}

static ScmObj
vector_to_list_aux(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector->list: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_to_list(vec, start, end);
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
  else if (!scm_fcd_integer_p(i)) {
    snprintf(msg, sizeof(msg), "%s: integer required, but got", op);
    scm_capi_error(msg, 1, i);
    return -1;
  }
  else {
    int r = scm_fcd_integer_to_size_t(i, &n);
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
  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector->string: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_to_string(vec, start, end);
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
  if (!scm_fcd_string_p(str)) {
    scm_capi_error("string->vector: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
    else if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_vector(str, start, end);
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
  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector-copy: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(vec)) {
    scm_capi_error("vector->list: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_copy(vec, start, end);
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

  if (!scm_fcd_vector_p(to)) {
    scm_capi_error("vector-copy!: vector required, but got", 1, to);
    return SCM_OBJ_NULL;
  }
  else if (at >= scm_fcd_vector_length(to)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_vector_p(from)) {
    scm_capi_error("vector-copy!: vectore required, but got", 1, from);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_vector_copy_i(to, at, from, start, end);
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

  if (!scm_fcd_integer_p(at)) {
    scm_capi_error("vector-copy!: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_integer_to_size_t(at, &a);
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
  if (!scm_fcd_vector_p(vec)) {
    scm_capi_error("vector-fill!: vectore required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
    else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-fill!: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(vec)) {
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

  scm_fcd_vector_fill_i(vec, fill, start, end);
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

  return scm_fcd_raise(obj);
}

int
scm_capi_raise_continuable(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("raise-continuable: invalid argument", 1, obj);
    return -1;
  }

  return scm_fcd_raise_continuable(obj);
}

int
scm_capi_push_exception_handler(ScmObj handler)
{
  if (!scm_fcd_procedure_p(handler)) {
    scm_capi_error("failed to install exception handler: "
                   "invalid argument", 1, handler);
    return -1;
  }

  return scm_fcd_push_exception_handler(handler);
}

ScmObj
scm_api_error_lst(ScmObj msg, ScmObj irris)
{
  if (!scm_fcd_string_p(msg)) {
    scm_capi_error("error: string required, but got", 1, msg);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(irris)) {
    scm_capi_error("error: invalid argument", 1, irris);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_lst(msg, irris);
}

ScmObj
scm_api_error_object_message(ScmObj obj)
{
  if (!scm_fcd_error_object_p(obj)) {
    scm_capi_error("error-object-message: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_object_message(obj);
}

ScmObj
scm_api_error_object_irritants(ScmObj obj)
{
  if (!scm_fcd_error_object_p(obj)) {
    scm_capi_error("error-object-irritants: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_object_irritants(obj);
}


/*******************************************************************/
/*  Ports                                                          */
/*******************************************************************/

static ssize_t
scm_port_path_str_to_cstr(ScmObj path, char *cstr)
{
  size_t s;
  char *p;

  /* TODO: `path' を外部エンーディングへ変換する */

  s = scm_fcd_string_bytesize(path);
  if (s >= PATH_MAX) {
    scm_capi_error("too long pathname", 1, path);
    return -1;
  }

  p = scm_fcd_string_to_cstr(path, cstr, PATH_MAX);
  if (p == NULL) return -1;

  return (ssize_t)s;
}

ScmObj
scm_api_open_input_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_fcd_string_p(path)) {
    scm_capi_error("open-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_port_path_str_to_cstr(path, path_str);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_open_input_file(path_str, NULL);
}

ScmObj
scm_api_open_binary_input_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_fcd_string_p(path)) {
    scm_capi_error("open-binary-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_port_path_str_to_cstr(path, path_str);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_open_binary_input_file(path_str);
}

ScmObj
scm_api_open_output_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_fcd_string_p(path)) {
    scm_capi_error("open-output-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_port_path_str_to_cstr(path, path_str);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_open_output_file(path_str, NULL);
}

ScmObj
scm_api_open_binary_output_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_fcd_string_p(path)) {
    scm_capi_error("open-binary-output-file: string required, but got",
                   1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_port_path_str_to_cstr(path, path_str);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_open_binary_output_file(path_str);
}

ScmObj
scm_api_close_port(ScmObj port)
{
  int r;

  if (!scm_fcd_port_p(port)) {
    scm_capi_error("close-port: port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_fcd_close_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_input_port(ScmObj port)
{
  int r;

  if (!scm_fcd_input_port_p(port)) {
    scm_capi_error("close-input-port: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_fcd_close_input_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_output_port(ScmObj port)
{
  int r;

  if (!scm_fcd_output_port_p(port)) {
    scm_capi_error("close-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  r = scm_fcd_close_output_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_open_input_string(ScmObj str)
{
  if (!scm_fcd_string_p(str)) {
    scm_capi_error("open-input-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_string(str);
}

ScmObj
scm_api_get_output_string(ScmObj port)
{
  if (!scm_fcd_output_port_p(port)) {
    scm_capi_error("get-output-string: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_string_port_p(port)) {
    scm_capi_error("get-output-string: string-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_textual_port_p(port)) {
    scm_capi_error("get-output-string: "
                   "textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_get_output_string(port);
}

ScmObj
scm_api_open_input_bytevector(ScmObj vec)
{
  if (!scm_fcd_bytevector_p(vec)) {
    scm_capi_error("open-input-bytevector: bytevector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_bytevector(vec);
}

ScmObj
scm_api_get_output_bytevector(ScmObj port)
{
  if (!scm_fcd_output_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_string_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "bytevector-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_binary_port_p(port)) {
    scm_capi_error("get-output-bytevector: "
                   "binary-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_get_output_bytevector(port);
}


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj
scm_api_read(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("read: input-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("read: textual-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read(port);
}

ScmObj
scm_api_read_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("read-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("read-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read_char(port);
}

ScmObj
scm_api_peek_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("peek-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("peek-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_peek_char(port);
}

ScmObj
scm_api_read_line(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("read-line: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("read-line: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read_line(port);
}

ScmObj
scm_api_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("char-ready?: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("char-ready?: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  ret = scm_fcd_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_read_string(ScmObj n, ScmObj port)
{
  size_t nc;
  int r;

  SCM_REFSTK_INIT_REG(&n, &port);

  if (!scm_fcd_integer_p(n)) {
    scm_capi_error("read-string: integer required, but got", 1, n);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_input_port_p(port)) {
      scm_capi_error("read-string: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("read-string: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  if (scm_fcd_negative_p(n))
    return scm_fcd_make_string_from_bin(NULL, 0,
                                        scm_fcd_port_internal_encoding(port));

  r = scm_fcd_integer_to_size_t(n, &nc);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_read_string(nc, port);
}


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

ScmObj
scm_api_write_shared(ScmObj obj, ScmObj port)
{
  int rslt;

  if (scm_obj_null_p(obj)) {
    scm_capi_error("write-shared: invalid argument", 1, obj);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("write-shared: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("write-shared: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_write_shared(obj, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write(ScmObj obj, ScmObj port)
{
  /* TODO: write me */
  return scm_api_write_simple(obj, port);
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
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("write-simple: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("write-simple: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_write_simple(obj, port);
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
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("display: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("display: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_display(obj, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_newline(ScmObj port)
{
  int rslt;

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("newline: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("newline: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_newline(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write_char(ScmObj chr, ScmObj port)
{
  int rslt;

  if (!scm_fcd_char_p(chr)) {
    scm_capi_error("write-char: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("write-char: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("write-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_write_char(chr, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
write_string_aux(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  int r;

  if (!scm_fcd_string_p(str)) {
    scm_capi_error("write-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_fcd_output_port_p(port)) {
      scm_capi_error("write-string: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_fcd_textual_port_p(port)) {
      scm_capi_error("write-string: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("write-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_write_string(str, port, start, end);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_REFSTK_INIT_REG(&str, &port, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_fcd_integer_p(start)) {
    scm_capi_error("write-string: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_fcd_integer_p(end)) {
    scm_capi_error("write-string: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_fcd_integer_to_size_t(start, &s);
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
    int r = scm_fcd_integer_to_size_t(end, &s);
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

  if (scm_obj_not_null_p(port) && !scm_fcd_output_port_p(port)) {
    scm_capi_error("flush-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_fcd_flush_output_port(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmObj
scm_api_make_parameter(ScmObj conv)
{
  if (scm_obj_not_null_p(conv) && !scm_fcd_procedure_p(conv)) {
    scm_capi_error("failed to make parameter object: "
                   "invalid argument", 1, conv);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_parameter(conv);
}


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

ScmObj
scm_api_make_syntax(ScmObj keyword, ScmObj handler)
{
  if (!scm_fcd_symbol_p(keyword)) {
    scm_capi_error("failed to make syntax object: "
                   "invalid argument", 1, keyword);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(handler)) {
    scm_capi_error("failed to make syntax object: "
                   "invalid argument", 1, handler);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_syntax(keyword, handler);
}

ScmObj
scm_api_syntax_keyword(ScmObj syx)
{
  if (!scm_fcd_syntax_p(syx)) {
    scm_capi_error("failed to get syntax keyword: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_syntax_keyword(syx);
}

ScmObj
scm_api_syntax_handler(ScmObj syx)
{
  if (!scm_fcd_syntax_p(syx)) {
    scm_capi_error("failed to get syntax handler: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_syntax_handler(syx);
}


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj
scm_api_assemble(ScmObj lst, ScmObj iseq)
{
  if (!(scm_fcd_pair_p(lst) || scm_fcd_nil_p(lst))) {
    scm_capi_error("asm: pair required, but got", 1, lst);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(iseq) && !scm_fcd_iseq_p(iseq)) {
    scm_capi_error("asm: iseq required, but got", 1, iseq);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_assemble(lst, iseq);
}


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

ScmObj
scm_api_compiler_current_module(ScmObj cmpl)
{
  if (!scm_fcd_compiler_p(cmpl)) {
    scm_capi_error("failed to get current module: invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_compiler_current_module(cmpl);
}

ScmObj
scm_api_compiler_current_expr(ScmObj cmpl)
{
  if (!scm_fcd_compiler_p(cmpl)) {
    scm_capi_error("failed to get current expression: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_compiler_current_expr(cmpl);
}

ScmObj
scm_api_compiler_select_module_i(ScmObj cmpl, ScmObj mod)
{
  SCM_REFSTK_INIT_REG(&cmpl, &mod);

  if (!scm_fcd_compiler_p(cmpl)) {
    scm_capi_error("failed to change current module: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_compiler_select_module_i(cmpl, mod);
}

ScmObj
scm_api_compiler_select_expr_i(ScmObj cmpl, ScmObj expr)
{
  SCM_REFSTK_INIT_REG(&cmpl, &expr);

  if (!scm_fcd_compiler_p(cmpl)) {
    scm_capi_error("failed to change current expression: "
                   "invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(expr)) {
    scm_capi_error("failed to change current expression: "
                   "invalid argument", 1, expr);
    return SCM_OBJ_NULL;
  }

  scm_fcd_compiler_select_expr_i(cmpl, expr);

  return SCM_UNDEF_OBJ;
}

int
scm_capi_compiler_assign_label_id_i(ScmObj cmpl)
{
  if (!scm_fcd_compiler_p(cmpl)) {
    scm_capi_error("failed to assign label id: invalid argument", 1, cmpl);
    return -1;
  }

  return scm_fcd_compiler_assign_label_id_i(cmpl);
}


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

ScmObj
scm_api_module_name(ScmObj module)
{
  if (!scm_fcd_module_p(module)) {
    scm_capi_error("failed to get a name from module: "
                   "invalid argument", 1, module);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_module_name(module);
}

int
scm_capi_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_fcd_symbol_p(sym)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, sym);
    return -1;
  }
  else if (!scm_fcd_syntax_p(syx)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, syx);
    return -1;
  }

  return scm_fcd_define_global_syx(module, sym, syx, export);
}


int
scm_capi_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_fcd_symbol_p(sym)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, sym);
    return -1;
  }

  return scm_fcd_global_syx_ref(module, sym, syx);
}


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int
scm_capi_trampolining(ScmObj proc, ScmObj args,
                      ScmObj postproc, ScmObj handover)
{
  if (!scm_fcd_procedure_p(proc)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (!scm_fcd_pair_p(args) && !scm_fcd_nil_p(args)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (scm_obj_not_null_p(postproc) && !scm_fcd_procedure_p(postproc)) {
    scm_capi_error("", 0);
    return -1;
  }

  return scm_fcd_trampolining(proc, args, postproc, handover);
}


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

ScmObj
scm_api_format_lst(ScmObj fmt, ScmObj lst)
{
  if (!scm_fcd_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_format_lst(fmt, lst);
}


/*******************************************************************/
/*  Exit                                                           */
/*******************************************************************/

ScmObj
scm_api_exit(ScmObj obj)
{
  scm_fcd_exit(obj);
  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

ScmEvaluator *
scm_capi_evaluator(void)
{
  ScmEvaluator *ev;

  ev = malloc(sizeof(*ev));
  if (ev == NULL) return NULL;

  ev->vm = SCM_OBJ_NULL;

  return ev;
}

void
scm_capi_evaluator_end(ScmEvaluator *ev)
{
  if (ev == NULL) return;

  if (scm_obj_not_null_p(ev->vm))
    scm_capi_evaluator_delete_vm(ev);

  free(ev);
}

int
scm_capi_evaluator_make_vm(ScmEvaluator *ev)
{
  if (ev == NULL) return -1;

  scm_fcd_chg_current_br(NULL);
  scm_fcd_chg_current_vm(SCM_OBJ_NULL);
  scm_fcd_chg_current_ref_stack(SCM_OBJ_NULL);

  ev->vm = scm_fcd_vm_new();
  if (scm_obj_null_p(ev->vm)) return -1;

  ev->bedrock = scm_fcd_current_br();
  ev->stack = scm_fcd_current_ref_stack();

  return 0;
}

int
scm_capi_evaluator_load_core(ScmEvaluator *ev)
{
  if (ev == NULL) return -1;
  scm_fcd_chg_current_br(ev->bedrock);
  scm_fcd_chg_current_vm(ev->vm);
  scm_fcd_chg_current_ref_stack(ev->stack);
  return scm_load_core_modules();
}

int
scm_capi_evaluator_delete_vm(ScmEvaluator *ev)
{
  if (ev == NULL) return -1;

  if (scm_obj_null_p(ev->vm)) return 0;

  scm_fcd_chg_current_br(ev->bedrock);
  scm_fcd_chg_current_vm(ev->vm);
  scm_fcd_chg_current_ref_stack(ev->stack);

  ev->bedrock = scm_fcd_current_br();
  ev->stack = scm_fcd_current_ref_stack();

  scm_fcd_vm_end(ev->vm);

  ev->vm = SCM_OBJ_NULL;
  ev->bedrock = NULL;
  ev->stack = SCM_OBJ_NULL;

  return 0;
}

static ScmObj
scm_get_proc(const char *name, const char * const *module, size_t n)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, mod_name = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&sym, &mod, &mod_name,
                      &proc, &o);

  mod_name = SCM_NIL_OBJ;
  for (size_t i = n; i > 0; i--) {
    o = scm_fcd_make_symbol_from_cstr(module[i - 1], SCM_ENC_SRC);
    if (scm_obj_null_p(o)) return SCM_OBJ_NULL;

    mod_name = scm_api_cons(o, mod_name);
    if (scm_obj_null_p(mod_name)) return SCM_OBJ_NULL;
  }

  sym = scm_fcd_make_symbol_from_cstr(name, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  r = scm_fcd_find_module(mod_name, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_capi_error("failed to find module", 1, mod_name);
    return SCM_OBJ_NULL;
  }

  r = scm_fcd_global_var_ref(mod, sym, SCM_CSETTER_L(proc));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(proc)) {
    scm_capi_error("unbund variable", 1, sym);
    return SCM_OBJ_NULL;
  }

  return proc;
}

int
scm_capi_run_repl(ScmEvaluator *ev)
{
  ScmObj proc = SCM_OBJ_INIT;
  int rslt;

  if (ev == NULL) return -1;

  rslt = scm_capi_evaluator_make_vm(ev);
  if (rslt < 0) return -1;

  rslt = scm_capi_evaluator_load_core(ev);
  if (rslt < 0) goto end;

  {
    SCM_REFSTK_INIT_REG(&proc);

    proc = scm_get_proc("read-eval-print-loop",
                        (const char *[]){"scythe", "internal", "repl"}, 3);
    if(scm_obj_null_p(proc)) goto end;

    scm_fcd_vm_apply(scm_fcd_current_vm(), proc, SCM_NIL_OBJ);
  }

 end:
  scm_fcd_vm_disposal_unhandled_exc(ev->vm);

  scm_capi_evaluator_delete_vm(ev);

  return 0;
}

int
scm_capi_exec_file(const char *path, ScmEvaluator *ev)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int rslt;

  if (ev == NULL) return -1;

  rslt = scm_capi_evaluator_make_vm(ev);
  if (rslt < 0) return -1;

  rslt = scm_capi_evaluator_load_core(ev);
  if (rslt < 0) goto end;

  {
    SCM_REFSTK_INIT_REG(&port, &str,
                        &proc, &args);

    port = scm_fcd_open_input_string_cstr(path, NULL);
    if (scm_obj_null_p(port)) goto end;

    /* TODO: read_line ではなく port から全て読みよっとものを 1 つの文字列に
     *       する */
    str = scm_api_read_line(port);
    if (scm_obj_null_p(str)) goto end;

    proc = scm_get_proc("eval-file",
                        (const char *[]){"scythe", "internal", "command"}, 3);
    if(scm_obj_null_p(proc)) goto end;

    args = scm_api_cons(str, SCM_NIL_OBJ);
    if (scm_obj_null_p(args)) goto end;

    scm_fcd_vm_apply(scm_fcd_current_vm(), proc, args);
  }

 end:
  scm_fcd_vm_disposal_unhandled_exc(ev->vm);

  scm_capi_evaluator_delete_vm(ev);

  return 0;
}

int
scm_capi_exec_cstr(const char *expr, ScmEvaluator *ev)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int rslt;

  if (ev == NULL) return -1;

  rslt = scm_capi_evaluator_make_vm(ev);
  if (rslt < 0) return -1;

  rslt = scm_capi_evaluator_load_core(ev);
  if (rslt < 0) goto end;

  {
    SCM_REFSTK_INIT_REG(&port, &str,
                        &proc, &args);

    port = scm_fcd_open_input_string_cstr(expr, NULL);
    if (scm_obj_null_p(port)) goto end;

    /* TODO: read_line ではなく port から全て読みよっとものを 1 つの文字列に
     *       する */
    str = scm_api_read_line(port);
    if (scm_obj_null_p(str)) goto end;

    proc = scm_get_proc("eval-string",
                        (const char *[]){"scythe", "internal", "command"}, 3);
    if(scm_obj_null_p(proc)) goto end;

    args = scm_api_cons(str, SCM_NIL_OBJ);
    if (scm_obj_null_p(args)) goto end;

    scm_fcd_vm_apply(scm_fcd_current_vm(), proc, args);
  }

 end:
  scm_fcd_vm_disposal_unhandled_exc(ev->vm);

  scm_capi_evaluator_delete_vm(ev);

  return 0;
}

static int
dump_marshal(const char *path, const char *ext,
             const void *marshal, size_t size)
{
  FILE *fp = NULL;
  char *str = NULL;
  size_t path_len, ext_len, n;

  path_len = strlen(path);
  ext_len = (ext != NULL) ? strlen(ext) : 0;
  str = scm_fcd_malloc(path_len + ext_len + 1);
  if (str == NULL) goto err;

  memcpy(str, path, path_len);
  memcpy(str + path_len, ext, ext_len);
  str[path_len + ext_len] = '\0';

  fp = fopen(str, "wb");
  if (fp == NULL) goto err;

  n = fwrite(marshal, 1, size, fp);
  if (n < size) goto err;

  fclose(fp);
  scm_fcd_free(str);
  return 0;

 err:
  scm_capi_error("failed to dump marshale data", 0);
  if (fp != NULL) fclose(fp);
  if (str != NULL) scm_fcd_free(str);
  return -1;
}

int
scm_capi_compile_file(const char *path, ScmEvaluator *ev)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  void *marshal;
  size_t size;
  int rslt;

  if (ev == NULL) return -1;

  rslt = scm_capi_evaluator_make_vm(ev);
  if (rslt < 0) return -1;

  rslt = scm_capi_evaluator_load_core(ev);
  if (rslt < 0) goto end;

  {
    SCM_REFSTK_INIT_REG(&port, &str, &mod,
                        &proc, &args, &val);

    port = scm_fcd_open_input_string_cstr(path, NULL);
    if (scm_obj_null_p(port)) goto end;

    /* TODO: read_line ではなく port から全て読みよっとものを 1 つの文字列に
     *       する */
    str = scm_api_read_line(port);
    if (scm_obj_null_p(str)) goto end;

    port = scm_fcd_open_input_string_cstr("(main)", SCM_ENC_NAME_SRC);
    if (scm_obj_null_p(port)) goto end;

    mod = scm_api_read(port);
    if (scm_obj_null_p(str)) goto end;

    proc = scm_get_proc("compile-file",
                        (const char *[]){"scythe", "internal", "compile"}, 3);
    if(scm_obj_null_p(proc)) goto end;

    args = scm_fcd_list(2, str, mod);
    if (scm_obj_null_p(args)) goto end;

    val = scm_fcd_vm_apply(scm_fcd_current_vm(), proc, args);
    if (scm_obj_null_p(val)) goto end;

    val = scm_fcd_vector_ref(val, 0);
    if (scm_obj_null_p(val)) goto end;

    val = scm_api_assemble(val, SCM_OBJ_NULL);
    if (scm_obj_null_p(val)) goto end;

    marshal = scm_fcd_marshal(&size, val, SCM_OBJ_NULL);
    if (marshal == NULL) goto end;

    dump_marshal("marshal.out", NULL, marshal, size);
    scm_fcd_free(marshal);
  }

 end:
  scm_fcd_vm_disposal_unhandled_exc(ev->vm);

  scm_capi_evaluator_delete_vm(ev);

  return 0;
}


/*******************************************************************/
/*  XXX                                                            */
/*******************************************************************/

int
scm_capi_load_iseq(ScmObj iseq)
{
  if (!scm_fcd_iseq_p(iseq)) {
    scm_capi_error("load: invalid argument", 1, iseq);
    return -1;
  }

  return scm_fcd_load_iseq(iseq);
}
