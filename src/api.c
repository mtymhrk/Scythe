#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/api.h"

#include "scythe/core_modules.h"

/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

void
scm_capi_fatal(const char *msg)
{
  scm_fcd_fatal(msg);
}

extern inline void
scm_capi_fatalf(const char *fmt, ...)
{
}

extern inline bool
scm_capi_fatal_p(void)
{
  return scm_fcd_fatal_p();
}


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

ScmObj
scm_capi_mem_alloc_heap(ScmTypeInfo *type, size_t add_size)
{
  if (type == NULL) {
    scm_capi_fatal("memory allocation error: invalid object type");
    return SCM_OBJ_NULL;
  }

  return scm_fcd_mem_alloc_heap(type, add_size);
}

ScmObj
scm_capi_mem_alloc_root(ScmTypeInfo *type, size_t add_size)
{
  if (type == NULL) {
    scm_capi_fatal("memory allocation error: invalid object type");
    return SCM_OBJ_NULL;
  }

  return scm_fcd_mem_alloc_root(type, add_size);
}

ScmObj
scm_capi_mem_alloc(ScmTypeInfo *otype, size_t add_size, SCM_MEM_TYPE_T mtype)
{
  return scm_fcd_mem_alloc(otype, add_size, mtype);
}

ScmObj
scm_capi_mem_free_root(ScmObj obj)
{
  return scm_fcd_mem_free_root(obj);
}

ScmRef
scm_capi_mem_register_extra_rfrn(ScmRef ref)
{
  return scm_fcd_mem_register_extra_rfrn(ref);
}

void
scm_capi_gc_start(void)
{
  return scm_fcd_gc_start();
}

void
scm_capi_gc_enable(void)
{
  return scm_fcd_gc_enable();
}

void
scm_capi_gc_disable(void)
{
  return scm_fcd_gc_disable();
}


/*******************************************************************/
/*  Equivalence predicates                                         */
/*******************************************************************/

/* 述語関数について、C の bool 方を返すものは _p を関数名の後ろに付与する。
 * Scheme の #t/#f を返すものは _P を関数名の後ろに付与する。
 */

extern inline bool
scm_capi_eq_p(ScmObj obj1, ScmObj obj2)
{
  return scm_fcd_eq_p(obj1, obj2);
}

ScmObj
scm_api_eq_P(ScmObj obj1, ScmObj obj2)
{
  return scm_fcd_eq_P(obj1, obj2);
}

int
scm_capi_eqv(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  return scm_fcd_eqv(obj1, obj2, rslt);
}

ScmObj
scm_api_eqv_P(ScmObj obj1, ScmObj obj2)
{
  return scm_fcd_eqv_P(obj1, obj2);
}

int
scm_capi_equal(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  return scm_fcd_equal(obj1, obj2, rslt);
}

ScmObj
scm_api_equal_P(ScmObj obj1, ScmObj obj2)
{
  return scm_fcd_equal_P(obj1, obj2);
}


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

/* Memo:
 *  scm_api_nil() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_NIL_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_nil(void)
{
  return scm_fcd_nil();
}

extern inline bool
scm_capi_nil_p(ScmObj obj)
{
  return scm_fcd_nil_p(obj);
}

ScmObj
scm_api_nil_P(ScmObj obj)
{
  return scm_fcd_nil_P(obj);
}


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/


bool
scm_capi_boolean_p(ScmObj obj)
{
  return scm_fcd_boolean_p(obj);
}

ScmObj
scm_api_boolean_P(ScmObj obj)
{
  return scm_fcd_boolean_P(obj);
}

/* Memo:
 *  scm_api_true() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_TRUE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_true(void)
{
  return scm_fcd_true();
}

/* Memo:
 *  scm_api_false() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_FALSE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_false(void)
{
  return scm_fcd_false();
}

extern inline bool
scm_capi_true_object_p(ScmObj obj)
{
  return scm_fcd_true_object_p(obj);
}

extern inline bool
scm_capi_false_object_p(ScmObj obj)
{
  return scm_fcd_false_object_p(obj);
}

extern inline bool
scm_capi_true_p(ScmObj obj)
{
  return scm_fcd_true_p(obj);
}

extern inline bool
scm_capi_false_p(ScmObj obj)
{
  return scm_fcd_false_p(obj);
}

ScmObj
scm_api_not(ScmObj obj)
{
  return scm_fcd_not(obj);
}


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

/* Memo:
 *  scm_api_eof() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_EOF_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_eof(void)
{
  return scm_fcd_eof();
}

extern inline bool
scm_capi_eof_object_p(ScmObj obj)
{
  return scm_fcd_eof_object_p(obj);
}


/*******************************************************************/
/*  undef                                                          */
/*******************************************************************/

/* Memo:
 *  scm_api_undef() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_UNDEF_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_undef(void)
{
  return scm_fcd_undef();
}

extern inline bool
scm_capi_undef_object_p(ScmObj obj)
{
  return scm_fcd_undef_object_p(obj);
}


/*******************************************************************/
/*  Landmine                                                       */
/*******************************************************************/


extern inline bool
scm_capi_landmine_object_p(ScmObj obj)
{
  return scm_fcd_landmine_object_p(obj);
}


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

extern inline bool
scm_capi_pair_p(ScmObj pair)
{
  return scm_fcd_pair_p(pair);
}

ScmObj
scm_api_pair_P(ScmObj pair)
{
  return scm_fcd_pair_P(pair);
}

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

int
scm_capi_set_car_i(ScmObj pair, ScmObj elm)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return -1;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 1, elm);
    return -1;
  }

  scm_fcd_set_car_i(pair, elm);

  return 0;
}

ScmObj
scm_api_set_car_i(ScmObj pair, ScmObj elm)
{
  if (scm_capi_set_car_i(pair, elm) < 0)
    return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_set_cdr_i(ScmObj pair, ScmObj elm)
{
  if (!scm_fcd_pair_p(pair)) {
    scm_capi_error("set-cdr!: pair required, but got", 1, pair);
    return -1;
  }
  else if (scm_obj_null_p(elm)) {
    scm_capi_error("set-cdr!: invalid argument", 1, elm);
    return -1;
  }

  scm_fcd_set_cdr_i(pair, elm);

  return 0;
}

ScmObj
scm_api_set_cdr_i(ScmObj pair, ScmObj elm)
{
  if (scm_capi_set_cdr_i(pair, elm) < 0)
    return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_capi_cxr(ScmObj pair, const char *dir)
{
  if (dir == NULL) {
    scm_capi_error("failed to execute cxr: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_cxr(pair, dir);
}

ScmObj
scm_api_list_P(ScmObj lst)
{
  return scm_fcd_list_P(lst);
}

ScmObj
scm_capi_make_list(size_t n, ScmObj fill)
{
  return scm_fcd_make_list(n, fill);
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

  return scm_capi_make_list(s, fill);
}

ScmObj
scm_capi_list_cv(const ScmObj *elm, size_t n)
{
  return scm_fcd_list_cv(elm, n);
}

ScmObj
scm_capi_list(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_fcd_list_cv(args, n);
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

  return scm_capi_make_number_from_sword(len);
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
scm_capi_append_cv(const ScmObj *lists, size_t n)
{
  return scm_fcd_append_cv(lists, n);
}

ScmObj
scm_capi_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_capi_append_cv(args, n);
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
scm_capi_list_tail(ScmObj lst, size_t n)
{
  return scm_fcd_list_tail(lst, n);
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

  r = scm_capi_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_list_tail(lst, s);
}

ScmObj
scm_capi_list_ref(ScmObj lst, size_t n)
{
  return scm_fcd_list_ref(lst, n);
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

  r = scm_capi_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_list_ref(lst, s);
}

int
scm_capi_list_set_i(ScmObj lst, size_t n, ScmObj obj)
{
  return scm_fcd_list_set_i(lst, n, obj);
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

  r = scm_capi_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_list_set_i(lst, s, obj);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_memq(ScmObj obj, ScmObj lst)
{
  return scm_fcd_memq(obj, lst);
}

ScmObj
scm_api_memv(ScmObj obj, ScmObj lst)
{
  return scm_fcd_memv(obj, lst);
}

ScmObj
scm_capi_member(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return scm_fcd_member(obj, lst, cmp);
}

ScmObj
scm_api_assq(ScmObj obj, ScmObj alist)
{
  return scm_fcd_assq(obj, alist);
}

ScmObj
scm_api_assv(ScmObj obj, ScmObj alist)
{
  return scm_fcd_assv(obj, alist);
}

ScmObj
scm_capi_assoc(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return scm_fcd_assoc(obj, alist, cmp);
}

ScmObj
scm_api_list_copy(ScmObj lst)
{
  return scm_fcd_list_copy(lst);
}


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

extern inline bool
scm_capi_fixnum_p(ScmObj obj)
{
  return scm_fcd_fixnum_p(obj);
}

ScmObj
scm_api_fixnum_P(ScmObj obj)
{
  return scm_fcd_fixnum_P(obj);
}

extern inline bool
scm_capi_bignum_p(ScmObj obj)
{
  return scm_fcd_bignum_p(obj);
}

ScmObj
scm_api_bignum_P(ScmObj obj)
{
  return scm_fcd_bignum_P(obj);
}

extern inline bool
scm_capi_number_p(ScmObj obj)
{
  return scm_fcd_number_p(obj);
}

ScmObj
scm_api_number_P(ScmObj obj)
{
  return scm_fcd_number_P(obj);
}

extern inline bool
scm_capi_complex_p(ScmObj obj)
{
  return scm_fcd_complex_p(obj);
}

ScmObj
scm_api_complex_P(ScmObj obj)
{
  return scm_fcd_complex_P(obj);
}

extern inline bool
scm_capi_real_p(ScmObj obj)
{
  return scm_fcd_real_p(obj);
}

ScmObj
scm_api_real_P(ScmObj obj)
{
  return scm_fcd_real_P(obj);
}

extern inline bool
scm_capi_rational_p(ScmObj obj)
{
  return scm_fcd_rational_p(obj);
}

ScmObj
scm_api_rational_P(ScmObj obj)
{
  return scm_fcd_rational_P(obj);
}

extern inline bool
scm_capi_integer_p(ScmObj obj)
{
  return scm_fcd_integer_p(obj);
}

ScmObj
scm_api_integer_P(ScmObj obj)
{
  return scm_fcd_integer_P(obj);
}

extern inline bool
scm_capi_exact_p(ScmObj obj)
{
  return scm_fcd_exact_p(obj);
}

ScmObj
scm_api_exact_P(ScmObj obj)
{
  return scm_fcd_exact_P(obj);
}

extern inline bool
scm_capi_inexact_p(ScmObj obj)
{
  return scm_fcd_inexact_p(obj);
}

ScmObj
scm_api_inexact_P(ScmObj obj)
{
  return scm_fcd_inexact_P(obj);
}

extern inline bool
scm_capi_exact_integer_p(ScmObj obj)
{
  return scm_fcd_exact_integer_p(obj);
}

ScmObj
scm_api_exact_integer_P(ScmObj obj)
{
  return scm_fcd_exact_integer_P(obj);
}

extern inline bool
scm_capi_finite_p(ScmObj obj)
{
  return scm_fcd_finite_p(obj);
}

ScmObj
scm_api_finite_P(ScmObj obj)
{
  return scm_fcd_finite_P(obj);
}

extern inline bool
scm_capi_infinite_p(ScmObj obj)
{
  return scm_fcd_infinite_p(obj);
}

ScmObj
scm_api_infinite_P(ScmObj obj)
{
  return scm_fcd_infinite_P(obj);
}

extern inline bool
scm_capi_nan_p(ScmObj obj)
{
  return scm_fcd_nan_p(obj);
}

ScmObj
scm_api_nan_P(ScmObj obj)
{
  return scm_fcd_nan_P(obj);
}

ScmObj
scm_capi_make_number_from_literal(const void *literal, ScmEncoding *enc)
{
  if (literal == NULL) {
    scm_capi_error("failed to make number: invalid literal", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_number_from_literal(literal, enc);
}

ScmObj
scm_capi_make_number_from_sword(scm_sword_t num)
{
  return scm_fcd_make_number_from_sword(num);
}

ScmObj
scm_capi_make_number_from_size_t(size_t num)
{
  return scm_fcd_make_number_from_size_t(num);
}

int
scm_capi_num_eq(ScmObj n1, ScmObj n2, bool *rslt)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error("=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("=: number required, but got", 1, n2);
    return -1;
  }

  return scm_fcd_num_eq(n1, n2, rslt);
}

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
scm_api_num_eq_P(ScmObj n1, ScmObj n2)
{
  return scm_fcd_num_eq_P(n1, n2);
}

int
scm_capi_num_lt(ScmObj n1, ScmObj n2, bool *rslt)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error("<: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("<: number required, but got", 1, n2);
    return -1;
  }

  return scm_fcd_num_lt(n1, n2, rslt);
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
scm_api_num_lt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_lt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_num_gt(ScmObj n1, ScmObj n2, bool *rslt)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error(">: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error(">: number required, but got", 1, n2);
    return -1;
  }

  return scm_fcd_num_gt(n1, n2, rslt);
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
scm_api_num_gt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  SCM_REFSTK_INIT_REG(&n1, &n2);

  rslt = scm_capi_num_gt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_num_le(ScmObj n1, ScmObj n2, bool *rslt)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error("<=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("<=: number required, but got", 1, n2);
    return -1;
  }

  return scm_fcd_num_le(n1, n2, rslt);
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
scm_api_num_le_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_le(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_num_ge(ScmObj n1, ScmObj n2, bool *rslt)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error(">=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error(">=: number required, but got", 1, n2);
    return -1;
  }

  return scm_fcd_num_ge(n1, n2, rslt);
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
scm_api_num_ge_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_ge(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_capi_zero_p(ScmObj num)
{
  return scm_fcd_zero_p(num);
}

ScmObj
scm_api_zero_P(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("zero?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_zero_P(num);
}

bool
scm_capi_positive_p(ScmObj num)
{
  return scm_fcd_positive_p(num);
}

ScmObj
scm_api_positive_P(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("positive?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_positive_P(num);
}

bool
scm_capi_negative_p(ScmObj num)
{
  return scm_fcd_negative_p(num);
}

ScmObj
scm_api_negative_P(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("negative?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_negative_P(num);
}

bool
scm_capi_odd_p(ScmObj num)
{
  return scm_fcd_odd_p(num);
}

ScmObj
scm_api_odd_P(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("odd?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_odd_P(num);
}

bool
scm_capi_even_p(ScmObj num)
{
  return scm_fcd_even_p(num);
}

ScmObj
scm_api_even_P(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("even?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_even_P(num);
}

ScmObj
scm_api_max(ScmObj n1, ScmObj n2)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error("max: number required, but got", 1, n1);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n2)) {
    scm_capi_error("max: number required, but got", 1, n2);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_max(n1, n2);
}

ScmObj
scm_api_max_lst(ScmObj lst)
{
  if (!scm_capi_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_max_lst(lst);
}

ScmObj
scm_api_min(ScmObj n1, ScmObj n2)
{
  if (!scm_capi_number_p(n1)) {
    scm_capi_error("min: number required, but got", 1, n1);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n2)) {
    scm_capi_error("min: number required, but got", 1, n2);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_min(n1, n2);
}

ScmObj
scm_api_min_lst(ScmObj lst)
{
  if (!scm_capi_pair_p(lst)) {
    scm_capi_error("max: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_min_lst(lst);
}

ScmObj
scm_api_plus(ScmObj x, ScmObj y)
{
  if (!scm_capi_number_p(x)) {
    scm_capi_error("+: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("+: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_plus(x, y);
}

ScmObj
scm_api_plus_lst(ScmObj lst)
{
  if (!scm_capi_nil_p(lst) && !scm_capi_pair_p(lst)) {
    scm_capi_error("+: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_plus_lst(lst);
}

ScmObj
scm_api_mul(ScmObj x, ScmObj y)
{
  if (!scm_capi_number_p(x)) {
    scm_capi_error("*: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("*: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_mul(x, y);
}

ScmObj
scm_api_mul_lst(ScmObj lst)
{
  if (!scm_capi_nil_p(lst) && !scm_capi_pair_p(lst)) {
    scm_capi_error("*: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_mul_lst(lst);
}

ScmObj
scm_api_minus(ScmObj x, ScmObj y)
{
  if (!scm_capi_number_p(x)) {
    scm_capi_error("-: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("-: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_minus(x, y);
}

ScmObj
scm_api_minus_lst(ScmObj lst)
{
  if (!scm_capi_pair_p(lst)) {
    scm_capi_error("-: invalid argument", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_minus_lst(lst);
}

ScmObj
scm_api_abs(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("abs: number required, bug got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_abs(num);
}

int
scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_div(x, y, q, r);
}

ScmObj
scm_api_floor_quo(ScmObj x, ScmObj y)
{
  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_quo(x, y);
}

ScmObj
scm_api_floor_rem(ScmObj x, ScmObj y)
{
  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_floor_rem(x, y);
}

int
scm_capi_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  if (!scm_capi_integer_p(x)) {
    scm_capi_error("truncate/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("truncate/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_div(x, y, q, r);
}

ScmObj
scm_api_truncate_quo(ScmObj x, ScmObj y)
{
  if (!scm_capi_number_p(x)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_quo(x, y);
}

ScmObj
scm_api_truncate_rem(ScmObj x, ScmObj y)
{
  if (!scm_capi_integer_p(x)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_truncate_rem(x, y);
}

ScmObj
scm_api_exact(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("exact: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_exact(num);
}

ScmObj
scm_api_inexact(ScmObj num)
{
  if (!scm_capi_number_p(num)) {
    scm_capi_error("inexact: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_exact(num);
}

int
scm_capi_integer_to_sword(ScmObj num, scm_sword_t *w)
{
  if (!scm_capi_integer_p(num)) {
    scm_capi_error("failed to convert number to scm_sword_t: "
                   "integer required, but got", 1, num);
    return -1;
  }

  return scm_fcd_integer_to_sword(num, w);
}

int
scm_capi_integer_to_size_t(ScmObj num, size_t *s)
{
  if (!scm_capi_integer_p(num)) {
    scm_capi_error("failed to convert number to size_t: "
                   "integer required, but got", 1, num);
    return -1;
  }

  return scm_fcd_integer_to_size_t(num, s);
}


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

extern inline bool
scm_capi_symbol_p(ScmObj obj)
{
  return scm_fcd_symbol_p(obj);
}

ScmObj
scm_api_symbol_P(ScmObj obj)
{
  return scm_fcd_symbol_P(obj);
}

int
scm_capi_symbol_eq(ScmObj sym1, ScmObj sym2, bool *rslt)
{
  if (!scm_capi_symbol_p(sym1)) {
    scm_capi_error("symbol=?: symbol required, but got", 1, sym1);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym2)) {
    scm_capi_error("symbol=?: symbol required, but got", 1, sym2);
    return -1;
  }

  if (rslt != NULL)
    *rslt = scm_fcd_symbol_eq_p(sym1, sym2);

  return 0;
}

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
scm_api_symbol_eq_P(ScmObj sym1, ScmObj sym2)
{
  bool cmp;
  int r;

  r = scm_capi_symbol_eq(sym1, sym2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if  (!scm_capi_symbol_p(sym)) {
    scm_capi_error("symbol->string: symbol required, but got", 1, sym);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_symbol_to_string(sym);
}

ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if  (!scm_capi_string_p(str)) {
    scm_capi_error("string->symbol: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_to_symbol(str);
}

ScmObj
scm_capi_make_symbol_from_cstr(const char *str, ScmEncoding *enc)
{
  return scm_fcd_make_symbol_from_cstr(str, enc);
}

ScmObj
scm_capi_make_symbol_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  return scm_fcd_make_symbol_from_bin(data, size, enc);
}

/* TODO: symbol_bytesize, symbol_to_cstr, symbol_hash_value についてはインタ
 * フェースの見直しが必要
 */

ssize_t
scm_capi_symbol_bytesize(ScmObj sym)
{
  if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("symbol-bytesize: symbol required, but got", 1, sym);
    return -1;
  }

  return (ssize_t)scm_fcd_symbol_bytesize(sym);
}

extern inline char *
scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size)
{
  if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to convert symbol object into C string", 1, sym);
    return NULL;
  }

  return scm_fcd_symbol_to_cstr(sym, cstr, size);
}

size_t
scm_capi_symbol_hash_value(ScmObj sym)
{
  return scm_fcd_symbol_hash_value(sym);
}


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

extern inline bool
scm_capi_char_p(ScmObj obj)
{
  return scm_fcd_char_p(obj);
}

ScmObj
scm_api_char_P(ScmObj obj)
{
  return scm_fcd_char_P(obj);
}

ScmObj
scm_capi_make_char(const scm_char_t *chr, ScmEncoding *enc)
{
  return scm_fcd_make_char(chr, enc);
}

int
scm_capi_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char=?: character required, but got", 1, chr2);
    return -1;
  }

  return scm_fcd_char_eq(chr1, chr2, rslt);
}

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
scm_api_char_eq_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_eq(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char<?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char<?: character required, but got", 1, chr2);
    return -1;
  }

  return scm_fcd_char_lt(chr1, chr2, rslt);
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
scm_api_char_lt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_lt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char>?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char>?: character required, but got", 1, chr2);
    return -1;
  }

  return scm_fcd_char_gt(chr1, chr2, rslt);
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
scm_api_char_gt_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_gt(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_char_le(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char<=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char<=?: character required, but got", 1, chr2);
    return -1;
  }

  return scm_fcd_char_le(chr1, chr2, rslt);
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
scm_api_char_le_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_le(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char>=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char>=?: character required, but got", 1, chr2);
    return -1;
  }

  return scm_fcd_char_ge(chr1, chr2, rslt);
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
scm_api_char_ge_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_ge(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_char_to_integer(ScmObj chr)
{
  if (!scm_capi_char_p(chr)) {
    scm_capi_error("char->integer: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_char_to_integer(chr);
}

ScmObj
scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  if (!scm_capi_integer_p(num)) {
    scm_capi_error("integer->char: integer required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_integer_to_char(num, enc);
}


/* TODO: char_to_cchr、char_encoding はインタフェースの見直しが必要
 */


ssize_t
scm_capi_char_to_cchr(ScmObj chr, scm_char_t *cp)
{
  if (!scm_capi_char_p(chr)) {
    scm_capi_error("can not get byte sequence from character object: "
                   "invalid argument", 0);
    return -1;
  }

  return scm_fcd_char_to_cchr(chr, cp);
}

ScmEncoding *
scm_capi_char_encoding(ScmObj chr)
{
  if (!scm_capi_char_p(chr)) {
    scm_capi_error("char-encoding: character required, but got", 1, chr);
    return NULL;
  }

  return scm_fcd_char_encoding(chr);
}


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

extern inline bool
scm_capi_string_p(ScmObj obj)
{
  return scm_fcd_string_p(obj);
}

ScmObj
scm_api_string_P(ScmObj obj)
{
  return scm_fcd_string_P(obj);
}

ScmObj
scm_capi_make_string(size_t n, ScmObj chr)
{
  /* TODO: write me */
  return SCM_OBJ_NULL;
}

ScmObj
scm_api_make_string(ScmObj n, ScmObj chr)
{
  /* TODO: write me */
  return SCM_OBJ_NULL;
}

ScmObj
scm_capi_make_string_from_cstr(const char *str, ScmEncoding *enc)
{
  return scm_fcd_make_string_from_cstr(str, enc);
}

ScmObj
scm_capi_make_string_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  return scm_fcd_make_string_from_bin(data, size, enc);
}

ScmObj
scm_api_string_lst(ScmObj lst)
{
  return scm_api_list_to_string(lst);
}

ScmObj
scm_capi_string_cv(const ScmObj *chr, size_t n)
{
  return scm_fcd_string_cv(chr, n);
}

size_t
scm_capi_string(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_capi_string_cv(args, n);
}

ssize_t
scm_capi_string_length(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-length: string required, but got", 1, str);
    return -1;
  }

  return (ssize_t)scm_fcd_string_length(str);
}

ScmObj
scm_api_string_length(ScmObj str)
{
  ssize_t len;

  len = scm_capi_string_length(str);
  if (len < 0) return SCM_OBJ_NULL;

  return scm_capi_make_number_from_sword(len);
}

ssize_t
scm_capi_string_bytesize(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-bytesize: string required, but got", 1, str);
    return -1;
  }

  return (ssize_t)scm_fcd_string_bytesize(str);
}

ScmObj
scm_api_string_bytesize(ScmObj str)
{
  ssize_t len;

  len = scm_capi_string_bytesize(str);
  if (len < 0) return SCM_OBJ_NULL;

  return scm_capi_make_number_from_sword(len);
}

ScmObj
scm_capi_string_ref(ScmObj str, size_t pos)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-ref: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (pos >= scm_fcd_string_length(str)) {
    scm_capi_error("string-ref: out of range", 0);
    return SCM_OBJ_NULL;
  }


  return scm_fcd_string_ref(str, pos);
}

ScmObj
scm_api_string_ref(ScmObj str, ScmObj pos)
{
  size_t s;
  int r;

  if (!scm_capi_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_string_ref(str, s);
}

int
scm_capi_string_set_i(ScmObj str, size_t pos, ScmObj chr)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-set!: string required, but got", 1, str);
    return -1;
  }
  else if (pos >= scm_fcd_string_length(str)) {
    scm_capi_error("string-set!: out of range", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("string-set!: character require, but got", 1, chr);
    return -1;
  }

  return scm_fcd_string_set_i(str, pos, chr);
}

ScmObj
scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr)
{
  size_t s;
  int r;

  SCM_REFSTK_INIT_REG(&str, &pos, &chr);

  if (!scm_capi_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_string_set_i(str, s, chr);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_string_eq(ScmObj s1, ScmObj s2, bool *rslt)
{
  if (!scm_capi_string_p(s1)) {
    scm_capi_error("string=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string=?: string required, but got", 1, s2);
    return -1;
  }

  return scm_fcd_string_eq(s1, s2, rslt);
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
scm_api_string_eq_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_eq(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_string_lt(ScmObj s1, ScmObj s2, bool *rslt)
{
  if (!scm_capi_string_p(s1)) {
    scm_capi_error("string<?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string<?: string required, but got", 1, s2);
    return -1;
  }

  return scm_fcd_string_lt(s1, s2, rslt);
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
scm_api_string_lt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_lt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_string_gt(ScmObj s1, ScmObj s2, bool *rslt)
{
  if (!scm_capi_string_p(s1)) {
    scm_capi_error("string>?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string>?: string required, but got", 1, s2);
    return -1;
  }

  return scm_fcd_string_gt(s1, s2, rslt);
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
scm_api_string_gt_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_gt(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_string_le(ScmObj s1, ScmObj s2, bool *rslt)
{
  if (!scm_capi_string_p(s1)) {
    scm_capi_error("string<=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string<=?: string required, but got", 1, s2);
    return -1;
  }

  return scm_fcd_string_le(s1, s2, rslt);
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
scm_api_string_le_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_le(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_string_ge(ScmObj s1, ScmObj s2, bool *rslt)
{
  if (!scm_capi_string_p(s1)) {
    scm_capi_error("string>=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string>=?: string required, but got", 1, s2);
    return -1;
  }

  return scm_fcd_string_ge(s1, s2, rslt);
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
scm_api_string_ge_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_ge(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_string_upcase(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-upcase: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_upcase(str);
}

ScmObj
scm_api_string_downcase(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-downcase: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_string_downcase(str);
}

ScmObj
scm_capi_substring(ScmObj str, size_t start, size_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_capi_string_p(str)) {
    scm_capi_error("substring: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start > SSIZE_MAX || end > SSIZE_MAX || start > end) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (end > scm_fcd_string_length(str)) {
    scm_capi_error("substring: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_substring(str, start, end);
}

ScmObj
scm_api_substring(ScmObj str, ScmObj start, ScmObj end)
{
  size_t ss, se;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (!scm_capi_integer_p(start)) {
    scm_capi_error("substring: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(end)) {
    scm_capi_error("substring: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(start, &ss);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_integer_to_size_t(end, &se);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_substring(str, ss, se);
}

ScmObj
scm_api_string_append_lst(ScmObj lst)
{
  return scm_fcd_string_append_lst(lst);
}

ScmObj
scm_capi_string_append_cv(ScmObj *ary, size_t n)
{
  return scm_fcd_string_append_cv(ary, n);
}

ScmObj
scm_capi_string_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_capi_string_append_cv(args, n);
}

ScmObj
scm_capi_string_to_list(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_capi_string_p(str)) {
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

  if (scm_obj_not_null_p(start) && !scm_capi_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_capi_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_capi_integer_to_size_t(start, &s);
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
    int r = scm_capi_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return scm_capi_string_to_list(str, sss, sse);
}

ScmObj
scm_api_list_to_string(ScmObj lst)
{
  return scm_fcd_list_to_string(lst);
}

ScmObj
scm_capi_string_copy(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_REFSTK_INIT_REG(&str);

  if (!scm_capi_string_p(str)) {
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

  if (scm_obj_not_null_p(start) && !scm_capi_integer_p(start)) {
    scm_capi_error("string-copy: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_capi_integer_p(end)) {
    scm_capi_error("string-copy: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_capi_integer_to_size_t(start, &s);
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
    int r = scm_capi_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-copy: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  return scm_capi_string_copy(str, sss, sse);
}

int
scm_capi_string_copy_i(ScmObj to, size_t at,
                       ScmObj from, ssize_t start, ssize_t end)
{
  if (!scm_capi_string_p(to)) {
    scm_capi_error("string-copy!: string require, but got", 1, to);
    return -1;
  }
  else if (at >= scm_fcd_string_length(to)) {
    scm_capi_error("string-copy!: out of range", 0);
    return -1;
  }
  else if (!scm_capi_string_p(from)) {
    scm_capi_error("string-copy!: string require, but got", 1, from);
    return -1;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return -1;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(from)) {
    scm_capi_error("string-copy!: out of range", 0);
    return -1;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-copy!: invalid argument", 0);
    return -1;
  }

  return scm_fcd_string_copy_i(to, at, from, start, end);
}

ScmObj
scm_api_string_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t sa;
  ssize_t sss, sse;
  int r;

  SCM_REFSTK_INIT_REG(&to, &at, &from, &start, &end);

  if (!scm_capi_integer_p(at)) {
    scm_capi_error("string->list: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(start) && !scm_capi_integer_p(start)) {
    scm_capi_error("string->list: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_capi_integer_p(end)) {
    scm_capi_error("string->list: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(at, &sa);
  if (r < 0) return SCM_OBJ_NULL;

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_capi_integer_to_size_t(start, &s);
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
    r = scm_capi_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string->list: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  r = scm_capi_string_copy_i(to, sa, from, sss, sse);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-fill!: string required, but got", 1, str);
    return -1;
  }
  else if (!scm_capi_char_p(fill)) {
    scm_capi_error("string-fill!: character required, but got", 1, fill);
    return -1;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return -1;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("string-fill!: out of range", 0);
    return -1;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-fill!: invalid argument", 0);
    return -1;
  }

  return scm_fcd_string_fill_i(str, fill, start, end);
}

ScmObj
scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;
  int r;

  SCM_REFSTK_INIT_REG(&str, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_capi_integer_p(start)) {
    scm_capi_error("string-fill!: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_capi_integer_p(end)) {
    scm_capi_error("string-fill!: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    r = scm_capi_integer_to_size_t(start, &s);
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
    r = scm_capi_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("string-fill!: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  r = scm_capi_string_fill_i(str, fill, sss, sse);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/* TODO: string_encoding, string_to_cstr, string_push はインタフェースの見直し
 *      が必要
 */

ScmEncoding *
scm_capi_string_encoding(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-encoding: string required, but got", 1, str);
    return NULL;
  }

  return scm_fcd_string_encoding(str);
}

char *
scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("failed to get byte sequence from string: "
                   "invalid argument", 0);
    return NULL;
  }

  return scm_fcd_string_to_cstr(str, cstr, size);
}

int
scm_capi_string_push(ScmObj str, scm_char_t chr, ScmEncoding *enc)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("can not push character into string: invalid argument", 0);
    return -1;
  }

  return scm_fcd_string_push(str, chr, enc);
}

ScmObj
scm_api_string_push(ScmObj str, ScmObj c)
{
  ScmEncoding *s_enc, *c_enc;
  scm_char_t cv;
  int rslt;

  if (!scm_capi_string_p(str)) {
    scm_capi_error("string-push: string required, but got", 1, str);
    return SCM_OBJ_NULL;                  /* provisional implementation */
  }
  else if (!scm_capi_char_p(c)) {
    scm_capi_error("string-push: character required, but got", 1, c);
    return SCM_OBJ_NULL;                  /* provisional implementation */
  }

  s_enc = scm_fcd_string_encoding(str);
  c_enc = scm_fcd_char_encoding(c);

  if (s_enc != c_enc) {
    scm_capi_error("string-push: encoding mismatch", 0);
    return SCM_OBJ_NULL;
  }

  scm_fcd_char_to_cchr(c, &cv);
  rslt = scm_fcd_string_push(str, cv, c_enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

extern inline bool
scm_capi_vector_p(ScmObj obj)
{
  return scm_fcd_vector_p(obj);
}

ScmObj
scm_api_vector_P(ScmObj obj)
{
  return scm_fcd_vector_P(obj);
}

ScmObj
scm_capi_make_vector(size_t len, ScmObj fill)
{
  if (len > SSIZE_MAX) {
    scm_capi_error("make-vector: too long", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_vector(len , fill);
}

ScmObj
scm_api_make_vector(ScmObj len, ScmObj fill)
{
  size_t sz;
  int r;

  if (!scm_capi_integer_p(len)) {
    scm_capi_error("make-vector: integer required, but got", 1, len);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(len, &sz);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_vector(sz, fill);
}

ScmObj
scm_api_vector_lst(ScmObj lst)
{
  return scm_api_list_to_vector(lst);
}

ScmObj
scm_capi_vector_cv(const ScmObj *elm, size_t n)
{
  if (n > SSIZE_MAX) {
    scm_capi_error("vector: too long", 0);
    return SCM_OBJ_NULL;
  }
  else if (n > 0 && elm == NULL) {
    scm_capi_error("vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_cv(elm, n);
}

ScmObj
scm_capi_vector(size_t n, ...)
{
  ScmObj vec = SCM_OBJ_INIT, args[n];
  va_list ap;

  SCM_REFSTK_INIT_REG(&vec);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_capi_vector_cv(args, n);
}

ssize_t
scm_capi_vector_length(ScmObj vec)
{
  if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-length: vector required, but got", 1, vec);
    return -1;
  }

  return (ssize_t)scm_fcd_vector_length(vec);
}

ScmObj
scm_api_vector_length(ScmObj vec)
{
  ssize_t len = scm_capi_vector_length(vec);
  if (len < 0) return SCM_OBJ_NULL;
  return scm_capi_make_number_from_size_t((size_t)len);
}

ScmObj
scm_capi_vector_ref(ScmObj vec, size_t idx)
{
  if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-ref: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (idx >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_vector_ref(vec, idx);
}

ScmObj
scm_api_vector_ref(ScmObj vec, ScmObj idx)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx);

  if (!scm_capi_integer_p(idx)) {
    scm_capi_error("vector-ref: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_ref(vec, i);
}

int
scm_capi_vector_set_i(ScmObj vec, size_t idx, ScmObj obj)
{
  SCM_REFSTK_INIT_REG(&vec, &obj);

  if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-set!: vector required, but got", 1, vec);
    return -1;
  }
  else if (idx >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-set!: argument out of range", 0);
    return -1;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("vector-set!: invalid argument", 1, obj);
    return -1;
  }

  scm_fcd_vector_set_i(vec, idx, obj);
  return 0;
}

ScmObj
scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj)
{
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &idx, &obj);

  if (!scm_capi_integer_p(idx)) {
    scm_capi_error("vector-set!: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_set_i(vec, i, obj);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_capi_vector_to_list(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_capi_vector_p(vec)) {
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
scm_capi_vector_cnv_start_end(const char *op, ScmObj i, ssize_t *o)
{
  char msg[256];
  size_t n;

  if (scm_obj_null_p(i)) {
    *o = -1;
    return 0;
  }
  else if (!scm_capi_integer_p(i)) {
    snprintf(msg, sizeof(msg), "%s: integer required, but got", op);
    scm_capi_error(msg, 1, i);
    return -1;
  }
  else {
    int r = scm_capi_integer_to_size_t(i, &n);
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

  r = scm_capi_vector_cnv_start_end("vector->list", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector->list", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_to_list(vec, s, e);
}

ScmObj
scm_api_list_to_vector(ScmObj lst)
{
  return scm_fcd_list_to_vector(lst);
}

ScmObj
scm_capi_vector_to_string(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_capi_vector_p(vec)) {
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

  r = scm_capi_vector_cnv_start_end("vector->string", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector->string", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_to_string(vec, s, e);
}

ScmObj
scm_capi_string_to_vector(ScmObj str, ssize_t start, ssize_t end)
{
  if (!scm_capi_string_p(str)) {
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

  r = scm_capi_vector_cnv_start_end("string->vector", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("string->vector", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_string_to_vector(str, s, e);
}

ScmObj
scm_capi_vector_copy(ScmObj vec, ssize_t start, ssize_t end)
{
  if (!scm_capi_vector_p(vec)) {
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

  r = scm_capi_vector_cnv_start_end("vector-copy", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector-copy", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_copy(vec, s, e);
}

int
scm_capi_vector_copy_i(ScmObj to, size_t at,
                       ScmObj from, ssize_t start, ssize_t end)
{
  if (!scm_capi_vector_p(to)) {
    scm_capi_error("vector-copy!: vector required, but got", 1, to);
    return -1;
  }
  else if (at >= scm_fcd_vector_length(to)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return -1;
  }
  else if (!scm_capi_vector_p(from)) {
    scm_capi_error("vector-copy!: vectore required, but got", 1, from);
    return -1;
  }
  else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return -1;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(from)) {
    scm_capi_error("vector-copy!: out of range", 0);
    return -1;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return -1;
  }

  return scm_fcd_vector_copy_i(to, at, from, start, end);
}

ScmObj
scm_api_vector_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t a;
  ssize_t s, e;
  int r;

  SCM_REFSTK_INIT_REG(&to, &at, &from, &start, &end);

  if (!scm_capi_integer_p(at)) {
    scm_capi_error("vector-copy!: integer required, but got", 1, at);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(at, &a);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector-copy!", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector-copy!", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_copy_i(to, a, from, s, e);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_vector_append_lst(ScmObj lst)
{
  return scm_fcd_vector_append_lst(lst);
}

ScmObj
scm_capi_vector_append_cv(ScmObj *ary, size_t n)
{
  return scm_fcd_vector_append_cv(ary, n);
}

ScmObj
scm_capi_vector_append(size_t n, ...)
{
  ScmObj ary[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    ary[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(ary, n);

  return scm_capi_vector_append_cv(ary, n);
}

int
scm_capi_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end)
{
  if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-fill!: vectore required, but got", 1, vec);
    return -1;
  }
    else if (start >= 0 && (size_t)start >= scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-fill!: out of range", 0);
    return -1;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_vector_length(vec)) {
    scm_capi_error("vector-fill!: out of range", 0);
    return -1;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-fill!: invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(fill)) {
    scm_capi_error("vector-fill!: invalid argument", 1, fill);
    return -1;
  }

  scm_fcd_vector_fill_i(vec, fill, start, end);
  return 0;
}

ScmObj
scm_api_vector_fill_i(ScmObj vec, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  SCM_REFSTK_INIT_REG(&vec, &fill);

  r = scm_capi_vector_cnv_start_end("vector-fill!", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector-fill!", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_fill_i(vec, fill, s, e);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_vector_push(ScmObj vec, ScmObj obj)
{
  if (!scm_capi_vector_p(vec)) {
    scm_capi_error("failed to add object into vector: "
                   "vectore required, but got", 1, vec);
    return -1;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("failed to add object into vector: "
                   "invalid argument", 1, obj);
    return -1;
  }

  return scm_fcd_vector_push(vec, obj);
}


/*******************************************************************/
/*  Bytevectors                                                    */
/*******************************************************************/

bool
scm_capi_bytevector_p(ScmObj obj)
{
  return scm_fcd_bytevector_p(obj);
}

ScmObj
scm_api_bytevector_P(ScmObj obj)
{
  return scm_fcd_bytevector_P(obj);
}

ScmObj
scm_capi_make_bytevector(size_t len, int fill)
{
  if (len > SSIZE_MAX) {
    scm_capi_error("failed to make bytevector: too large", 0);
    return SCM_OBJ_NULL;
  }
  else if (fill > 255) {
    scm_capi_error("failed to make bytevector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_bytevector(len, fill);
}

ScmObj
scm_capi_make_bytevector_from_cv(const void *bytes, size_t length)
{
  if (bytes == NULL && length > 0) {
    scm_capi_error("failed to make bytevector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (length > SSIZE_MAX) {
    scm_capi_error("failed to make bytevector: too large", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_bytevector_from_cv(bytes, length);
}

ssize_t
scm_capi_bytevector_length(ScmObj vec)
{
  if (!scm_capi_bytevector_p(vec)) {
    scm_capi_error("bytevector-length: bytevector required, but got", vec);
    return -1;
  }

  return (ssize_t)scm_fcd_bytevector_length(vec);
}

int
scm_capi_bytevector_u8_set_i(ScmObj vec, size_t idx, int val)
{
  if (!scm_capi_bytevector_p(vec)) {
    scm_capi_error("bytevector-u8-set!: bytevector required, but got", vec);
    return -1;
  }
  else if (idx >= scm_fcd_bytevector_length(vec)) {
    scm_capi_error("bytevector-u8-set!: argument out of range", 0);
    return -1;
  }
  else if (val < 0 || 255 < val) {
    scm_capi_error("bytevector-u8-set!: invalid argument", 0);
    return -1;
  }

  scm_fcd_bytevector_u8_set_i(vec, idx, val);
  return 0;
}

void *
scm_capi_bytevector_to_cv(ScmObj vec, void *buf, size_t size)
{
  if (!scm_capi_bytevector_p(vec)) {
    scm_capi_error("failed to get byte sequence from bytevector: "
                   "invalid argument", 0);
    return NULL;
  }

  return scm_fcd_bytevector_to_cv(vec, buf, size);
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

bool
scm_capi_raised_p(void)
{
  return scm_fcd_raised_p();
}

ScmObj
scm_api_raised_obj(void)
{
  return scm_fcd_raised_obj();
}

void
scm_api_discard_raised_obj(void)
{
  scm_fcd_discard_raised_obj();
}

int
scm_capi_push_exception_handler(ScmObj handler)
{
  if (!scm_capi_procedure_p(handler)) {
    scm_capi_error("failed to install exception handler: "
                   "invalid argument", 1, handler);
    return -1;
  }

  return scm_fcd_push_exception_handler(handler);
}

int
scm_capi_pop_exception_handler(void)
{
  return scm_fcd_pop_exception_handler();
}

ScmObj
scm_api_error_lst(ScmObj msg, ScmObj irris)
{
  if (!scm_capi_string_p(msg)) {
    scm_capi_error("error: string required, but got", 1, msg);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(irris)) {
    scm_capi_error("error: invalid argument", 1, irris);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_lst(msg, irris);
}

extern inline bool
scm_capi_error_object_p(ScmObj obj)
{
  return scm_fcd_error_object_p(obj);
}

ScmObj
scm_api_error_object_P(ScmObj obj)
{
  return scm_fcd_error_object_P(obj);
}

ScmObj
scm_api_error_object_message(ScmObj obj)
{
  if (!scm_capi_error_object_p(obj)) {
    scm_capi_error("error-object-message: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_object_message(obj);
}

ScmObj
scm_api_error_object_irritants(ScmObj obj)
{
  if (!scm_capi_error_object_p(obj)) {
    scm_capi_error("error-object-irritants: "
                   "error-object required, but got", 1, obj);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_error_object_irritants(obj);
}

ScmObj
scm_api_read_error_P(ScmObj obj)
{
  return scm_fcd_read_error_P(obj);
}

ScmObj
scm_api_file_error_P(ScmObj obj)
{
  return scm_fcd_file_error_P(obj);
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

  p = scm_capi_string_to_cstr(path, cstr, PATH_MAX);
  if (p == NULL) return -1;

  return (ssize_t)s;
}

extern inline bool
scm_capi_port_p(ScmObj obj)
{
  return scm_fcd_port_p(obj);
}

ScmObj
scm_api_port_P(ScmObj obj)
{
  return scm_fcd_port_P(obj);
}

extern inline bool
scm_capi_input_port_p(ScmObj obj)
{
  return scm_fcd_input_port_p(obj);
}

ScmObj
scm_api_input_port_P(ScmObj obj)
{
  return scm_fcd_input_port_P(obj);
}

extern inline bool
scm_capi_output_port_p(ScmObj obj)
{
  return scm_fcd_output_port_p(obj);
}

ScmObj
scm_api_output_port_P(ScmObj obj)
{
  return scm_fcd_output_port_P(obj);
}

extern inline bool
scm_capi_textual_port_p(ScmObj obj)
{
  return scm_fcd_textual_port_p(obj);
}

ScmObj
scm_api_textual_port_P(ScmObj obj)
{
  return scm_fcd_textual_port_P(obj);
}

extern inline bool
scm_capi_binary_port_p(ScmObj obj)
{
  return scm_fcd_binary_port_p(obj);
}

ScmObj
scm_api_binary_port_P(ScmObj obj)
{
  return scm_fcd_binary_port_P(obj);
}

extern inline bool
scm_capi_input_port_open_p(ScmObj port)
{
  return scm_fcd_input_port_open_p(port);
}

ScmObj
scm_api_input_port_open_P(ScmObj port)
{
  return scm_fcd_input_port_open_P(port);
}

extern inline bool
scm_capi_output_port_open_p(ScmObj port)
{
  return scm_fcd_output_port_open_p(port);
}

ScmObj
scm_api_output_port_open_P(ScmObj port)
{
  return scm_fcd_output_port_open_P(port);
}

ScmObj
scm_capi_open_input_fd(int fd, const char *enc)
{
  if (fd < 0) {
    scm_capi_error("open-input-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_fd(fd, enc);
}

ScmObj
scm_capi_open_binary_input_fd(int fd)
{
  if (fd < 0) {
    scm_capi_error("open-binary-input-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_binary_input_fd(fd);
}

ScmObj
scm_capi_open_output_fd(int fd, const char *enc)
{
  if (fd < 0) {
    scm_capi_error("open-output-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_output_fd(fd, enc);
}

ScmObj
scm_capi_open_binary_output_fd(int fd)
{
  if (fd < 0) {
    scm_capi_error("open-binary-output-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_binary_output_fd(fd);
}

ScmObj
scm_capi_open_input_file(const char *path, const char *enc)
{
  if (path == NULL) {
    scm_capi_error("open-input-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_file(path, enc);
}

ScmObj
scm_capi_open_binary_input_file(const char *path)
{
  if (path == NULL) {
    scm_capi_error("open-binary-input-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_binary_input_file(path);
}

ScmObj
scm_api_open_input_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_capi_string_p(path)) {
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

  if (!scm_capi_string_p(path)) {
    scm_capi_error("open-binary-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_port_path_str_to_cstr(path, path_str);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_open_binary_input_file(path_str);
}

ScmObj
scm_capi_open_output_file(const char *path, const char *enc)
{
  if (path == NULL) {
    scm_capi_error("open-output-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_output_file(path, enc);
}

ScmObj
scm_capi_open_binary_output_file(const char *path)
{
  if (path == NULL) {
    scm_capi_error("open-bianry-output-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_binary_output_file(path);
}

ScmObj
scm_api_open_output_file(ScmObj path)
{
  char path_str[PATH_MAX];
  ssize_t r;

  if (!scm_capi_string_p(path)) {
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

  if (!scm_capi_string_p(path)) {
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

  if (!scm_capi_port_p(port)) {
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

  if (!scm_capi_input_port_p(port)) {
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

  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("close-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  r = scm_fcd_close_output_port(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_capi_open_input_string_cstr(const char *str, const char *enc)
{
  return scm_fcd_open_input_string_cstr(str, enc);
}

ScmObj
scm_api_open_input_string(ScmObj str)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("open-input-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_string(str);
}

ScmObj
scm_api_open_output_string(void)
{
  return scm_fcd_open_output_string();
}

ScmObj
scm_api_get_output_string(ScmObj port)
{
  if (!scm_capi_output_port_p(port)) {
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
scm_capi_open_input_bytevector_cbytes(const void *bytes, size_t size)
{
  return scm_fcd_open_input_bytevector_cbytes(bytes, size);
}

ScmObj
scm_api_open_input_bytevector(ScmObj vec)
{
  if (!scm_capi_bytevector_p(vec)) {
    scm_capi_error("open-input-bytevector: bytevector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_open_input_bytevector(vec);
}

ScmObj
scm_api_open_output_bytevector(void)
{
  return scm_fcd_open_output_bytevector();
}

ScmObj
scm_api_get_output_bytevector(ScmObj port)
{
  if (!scm_capi_output_port_p(port)) {
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

off_t
scm_capi_port_seek(ScmObj port, off_t offset, int whence)
{
  if (!scm_capi_port_p(port)) {
    scm_capi_error("port required, but got", 1, port);
    return -1;
  }

  return scm_fcd_port_seek(port, offset, whence);
}

off_t
scm_capi_port_pos(ScmObj port)
{
  if (!scm_capi_port_p(port)) {
    scm_capi_error("port required, but got", 1, port);
    return -1;
  }

  return scm_fcd_port_pos(port);
}

const char *
scm_capi_port_encoding(ScmObj port)
{
  if (!scm_capi_port_p(port)) {
    scm_capi_error("port-encoding: port required, but got", 1, port);
    return NULL;                  /* provisional implemntation */
  }

  return scm_fcd_port_encoding(port);
}

ScmEncoding *
scm_capi_port_internal_encoding(ScmObj port)
{
  if (!scm_capi_port_p(port)) {
    scm_capi_error("port-internal-encoding: port required, but got", 1, port);
    return NULL;
  }

  return scm_fcd_port_internal_encoding(port);
}


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj
scm_api_read(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read: input-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("read: textual-port requried, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read(port);
}

ssize_t
scm_capi_read_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read-char: input-port required, but got", 1, port);
      return -1;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("read-char: textual-port required, but got", 1, port);
      return -1;
    }
  }

  if (chr == NULL) {
    scm_capi_error("read-char: invalid argument", 0);
    return -1;
  }

  return scm_fcd_read_cchr(chr, port);
}

ScmObj
scm_api_read_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("read-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read_char(port);
}

ssize_t
scm_capi_peek_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("peek-char: input-port required, but got", 1, port);
      return -1;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("peek-char: textual-port required, but got", 1, port);
      return -1;
    }
  }

  if (chr == NULL) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }

  return scm_fcd_peek_cchr(chr, port);
}

ScmObj
scm_api_peek_char(ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("peek-char: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
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
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read-line: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("read-line: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_read_line(port);
}

int
scm_capi_char_ready(ScmObj port, bool *rslt)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("char-ready?: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("char-ready?: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_char_ready(port, rslt);
}

ScmObj
scm_api_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  ret = scm_capi_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_read_string(ScmObj n, ScmObj port)
{
  size_t nc;
  int r;

  SCM_REFSTK_INIT_REG(&n, &port);

  if (!scm_capi_integer_p(n)) {
    scm_capi_error("read-string: integer required, but got", 1, n);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read-string: input-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("read-string: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  if (scm_capi_negative_p(n))
    return scm_capi_make_string_from_bin(NULL, 0,
                                         scm_fcd_port_internal_encoding(port));

  r = scm_capi_integer_to_size_t(n, &nc);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_read_string(nc, port);
}

ssize_t
scm_capi_read_cbytes(void *buf, size_t size, ScmObj port)
{
  if (buf == NULL) {
    scm_capi_error("read-bytevector: invalid argument", 0);
    return -1;
  }
  else if (size > SSIZE_MAX) {
    scm_capi_error("read-bytevector: invalid argument", 0);
    return -1;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_input_port_p(port)) {
      scm_capi_error("read-bytevector: input-port required, but got", 1, port);
      return -1;
    }
    else if (!scm_capi_binary_port_p(port)) {
      scm_capi_error("read-bytevector: binary-port required, but got", 1, port);
      return -1;
    }
  }

  return scm_fcd_read_cbytes(buf, size, port);
}


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

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
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("write-simple: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
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
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("display: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
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
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("newline: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("newline: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_newline(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_write_cchr(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  ScmObj c = SCM_OBJ_INIT, r = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &c, &r);

  if (enc == NULL) {
    scm_capi_error("write-char: invalid argument", 0);
    return -1;
  }

  c = scm_capi_make_char(&chr, enc);
  if (scm_obj_null_p(c)) return -1;

  r = scm_api_write_char(c, port);
  if (scm_obj_null_p(r)) return -1;

  return 0;
}

ScmObj
scm_api_write_char(ScmObj chr, ScmObj port)
{
  int rslt;

  if (!scm_capi_char_p(chr)) {
    scm_capi_error("write-char: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("write-char: output-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("write-char: textual-port required, but got", 1, port);
      return SCM_OBJ_NULL;
    }
  }

  rslt = scm_fcd_write_char(chr, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_write_cstr(const char *str, ScmEncoding *enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &s);

  if (enc == NULL) {
    scm_capi_error("write-string: invalid argument", 0);
    return -1;
  }

  s = scm_capi_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return -1;

  return scm_capi_write_string(s, port, -1, -1);
}

int
scm_capi_write_string(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  if (!scm_capi_string_p(str)) {
    scm_capi_error("write-string: string required, but got", 1, str);
    return -1;
  }

  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("write-string: output-port required, but got", 1, port);
      return -1;
    }
    else if (!scm_capi_textual_port_p(port)) {
      scm_capi_error("write-string: textual-port required, but got", 1, port);
      return -1;
    }
  }

  if (start >= 0 && (size_t)start >= scm_fcd_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return -1;
  }
  else if (end >= 0 && (size_t)end > scm_fcd_string_length(str)) {
    scm_capi_error("write-string: out of range", 0);
    return -1;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("write-string: invalid argument", 0);
    return -1;
  }

  return scm_fcd_write_string(str, port, start, end);
}

ScmObj
scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;
  int rslt;

  SCM_REFSTK_INIT_REG(&str, &port, &start, &end);

  if (scm_obj_not_null_p(start) && !scm_capi_integer_p(start)) {
    scm_capi_error("write-string: integer required, but got", 1, start);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(end) && !scm_capi_integer_p(end)) {
    scm_capi_error("write-string: integer required, but got", 1, end);
    return SCM_OBJ_NULL;
  }

  sss = -1;
  if (scm_obj_not_null_p(start)) {
    size_t s;
    int r = scm_capi_integer_to_size_t(start, &s);
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
    int r = scm_capi_integer_to_size_t(end, &s);
    if (r < 0) return SCM_OBJ_NULL;

    if (s > SSIZE_MAX) {
      scm_capi_error("write-string: too big", 1, end);
      return SCM_OBJ_NULL;
    }

    sse = (ssize_t)s;
  }

  rslt = scm_capi_write_string(str, port, sss, sse);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_write_cbytes(const void *bytes, size_t size, ScmObj port)
{
  if (scm_obj_not_null_p(port)) {
    if (!scm_capi_output_port_p(port)) {
      scm_capi_error("write-cbytes: output-port required, but got", 1, port);
      return -1;
    }
    else if (!scm_capi_binary_port_p(port)) {
      scm_capi_error("write-cbytes: binary-port required, but got", 1, port);
      return -1;
    }
  }

  return scm_fcd_write_cbytes(bytes, size, port);
}

ScmObj
scm_api_flush_output_port(ScmObj port)
{
  int rslt;

  if (scm_obj_not_null_p(port) && !scm_capi_output_port_p(port)) {
    scm_capi_error("flush-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_fcd_flush_output_port(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

extern inline bool
scm_capi_procedure_p(ScmObj proc)
{
  return scm_fcd_procedure_p(proc);
}

ScmObj
scm_api_procedure_P(ScmObj proc)
{
  return scm_fcd_procedure_P(proc);
}

int
scm_capi_arity(ScmObj proc, int *arity)
{
  if (!scm_capi_procedure_p(proc)) {
    scm_capi_error("arity: procedure required, but got", 1, proc);
    return -1;
  }

  if (arity != NULL)
    *arity = scm_fcd_arity(proc);

  return 0;
}

int
scm_capi_procedure_flg_set_p(ScmObj proc, SCM_PROC_FLG_T flg, bool *rslt)
{
  if (!scm_capi_procedure_p(proc)) {
    scm_capi_error("failed to get procedure information: "
                   "invalid argument", 1, proc);
    return -1;
  }

  if (rslt != NULL)
    *rslt = scm_fcd_procedure_flg_set_p(proc, flg);

  return 0;
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj
scm_capi_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags,
                        ScmObj module)
{
  if (func == NULL) {
    scm_capi_error("failed to make subrutine: invaild argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(module) && !scm_capi_module_p(module)) {
    scm_capi_error("failed to make subrutine: invalid argument", 1, module);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_subrutine(func, arity, flags, module);
}

int
scm_api_call_subrutine(ScmObj subr, int argc, const ScmObj *argv)
{
  if (!scm_capi_subrutine_p(subr)) {
    scm_capi_error("failed to call subrutine: invalid argument", 1, subr);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_call_subrutine(subr, argc, argv);
}

extern inline bool
scm_capi_subrutine_p(ScmObj obj)
{
  return scm_fcd_subrutine_p(obj);
}

int
scm_capi_subrutine_module(ScmObj subr, scm_csetter_t *mod)
{
  if (!scm_capi_subrutine_p(subr)) {
    scm_capi_error("failed to get a module defines the subrutine: "
                   "invalid argument", 1, subr);
    return -1;
  }

  if (mod != NULL)
    scm_csetter_setq(mod, scm_fcd_subrutine_module(subr));

  return 0;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmObj
scm_capi_make_closure(ScmObj iseq, ScmObj env, int arity)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to make closure: invalid argument", 1, iseq);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_closure(iseq, env, arity);
}

extern inline bool
scm_capi_closure_p(ScmObj obj)
{
  return scm_fcd_closure_p(obj);
}

ScmObj
scm_api_closure_to_iseq(ScmObj clsr)
{
  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("failed to get iseq object from closure: "
                   "invalid argument", 1, clsr);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_closure_to_iseq(clsr);
}

scm_byte_t *
scm_capi_closure_to_ip(ScmObj clsr)
{
  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("failed to get iseq object from closure: "
                   "invalid argument", 1, clsr);
    return NULL;
  }

  return scm_fcd_closure_to_ip(clsr);
}

int
scm_capi_closure_env(ScmObj clsr, scm_csetter_t *env)
{
  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("failed to get closed environment object from closure: "
                   "invalid argument", 1, clsr);
    return -1;
  }

  if (env != NULL)
    scm_csetter_setq(env, scm_fcd_closure_env(clsr));

  return 0;
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmObj
scm_api_make_parameter(ScmObj conv)
{
  if (scm_obj_not_null_p(conv) && !scm_capi_procedure_p(conv)) {
    scm_capi_error("failed to make parameter object: "
                   "invalid argument", 1, conv);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_parameter(conv);
}

extern inline bool
scm_capi_parameter_p(ScmObj obj)
{
  return scm_fcd_parameter_p(obj);
}

int
scm_capi_parameter_init_val(ScmObj prm, scm_csetter_t *val)
{
  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to get a initial value of a parameter object: "
                   "invalid argument", 1, prm);
    return -1;
  }

  if (val != NULL)
    scm_csetter_setq(val, scm_fcd_parameter_init_val(prm));

  return 0;
}

int
scm_capi_parameter_converter(ScmObj prm, scm_csetter_t *conv)
{
  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to get a converter from a parameter object: "
                   "invalid argument", 1, prm);
    return -1;
  }

  if (conv != NULL)
    scm_csetter_setq(conv, scm_fcd_parameter_converter(prm));

  return 0;
}

int
scm_capi_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  SCM_REFSTK_INIT_REG(&prm, &val);

  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to set a initial value of a parameter object: "
                   "invalid argument", 1, prm);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to set a initial value of a parameter object: "
                   "invalid argument", 1, val);
    return -1;
  }

  scm_fcd_parameter_set_init_val(prm, val);

  return 0;
}

ScmObj
scm_api_parameter_value(ScmObj prm)
{
  if (scm_obj_null_p(prm)) {
    scm_capi_error("failed to get bound value: invalid argument", 1, prm);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_parameter_value(prm);
}


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

extern inline bool
scm_capi_syntax_p(ScmObj obj)
{
  return scm_fcd_syntax_p(obj);
}

ScmObj
scm_api_syntax_P(ScmObj obj)
{
  return scm_fcd_syntax_P(obj);
}

ScmObj
scm_api_make_syntax(ScmObj keyword, ScmObj handler)
{
  if (!scm_capi_symbol_p(keyword)) {
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

extern inline ScmObj
scm_api_syntax_keyword(ScmObj syx)
{
  if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("failed to get syntax keyword: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_syntax_keyword(syx);
}

extern inline ScmObj
scm_api_syntax_handler(ScmObj syx)
{
  if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("failed to get syntax handler: invalid argument", 1, syx);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_syntax_handler(syx);
}


/*******************************************************************/
/*  ISeq                                                           */
/*******************************************************************/

ScmObj
scm_api_make_iseq(void)
{
  return scm_fcd_make_iseq();
}

extern inline bool
scm_capi_iseq_p(ScmObj obj)
{
  return scm_fcd_iseq_p(obj);
}

scm_byte_t *
scm_capi_iseq_to_ip(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to get instruction pointer from iseq: "
                   "invalid argument", 1, iseq);
    return NULL;
  }

  return scm_fcd_iseq_to_ip(iseq);
}

ssize_t
scm_capi_iseq_length(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to get length of instruction seqeunce: "
                   "invalid argument", 1, iseq);
    return -1;
  }

  return (ssize_t)scm_fcd_iseq_length(iseq);
}

int
scm_capi_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt)
{
  if (!scm_capi_iseq_p(iseq1)) {
    scm_capi_error("iseq=?: iseq required, but got", 1, iseq1);
    return -1;
  }
  else if (!scm_capi_iseq_p(iseq2)) {
    scm_capi_error("iseq=?: iseq required, but got", 1, iseq2);
    return -1;
  }

  return scm_fcd_iseq_eq(iseq1, iseq2, rslt);
}

ssize_t
scm_capi_iseq_push_inst(ScmObj iseq, scm_opcode_t op, ...)
{
  ssize_t ret;
  va_list ap;

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to push a instruction to ISeq: Invalid argument",
                   1, iseq);
    return -1;
  }
  else if (op < 0 || SCM_VMINST_NR_OP <= op) {
    scm_capi_error("failed to push a instruction to ISeq: unknown opcode", 0);
    return -1;
  }

  va_start(ap, op);
  ret = scm_fcd_iseq_push_inst_va(iseq, op, ap);
  va_end(ap);

  return ret;
}

int
scm_capi_iseq_push_br_dst(ScmObj iseq, size_t offset)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to push branch destination to ISeq: "
                   "invalid argument", 1, iseq);
    return -1;
  }

  return scm_fcd_iseq_push_br_dst(iseq, offset);
}

ssize_t
scm_capi_iseq_nr_br_dst(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to get number of branch destinations from ISeq: "
                   "invalid argument", 1, iseq);
    return -1;
  }

  return (ssize_t)scm_fcd_iseq_nr_br_dst(iseq);
}

const size_t *
scm_capi_iseq_br_dsts(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to get branch destination from ISeq: "
                   "invalid argument", 1, iseq);
    return NULL;
  }

  return scm_fcd_iseq_br_dsts(iseq);
}

int
scm_capi_iseq_update_oprand_iof(ScmObj iseq, size_t offset, int iof)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("failed to update instruction operand in iseq: "
                   "invalid argument", 1, iseq);
    return -1;
  }
  else if (offset > SSIZE_MAX
           || scm_fcd_iseq_length(iseq) < SCM_OPFMT_INST_SZ_IOF
           || offset > scm_fcd_iseq_length(iseq) - SCM_OPFMT_INST_SZ_IOF) {
    scm_capi_error("failed to update instruction operand in iseq: "
                   "out of range", 0);
    return -1;
  }

  return scm_fcd_iseq_update_oprand_iof(iseq, offset, iof);
}

int
scm_capi_inst_update_oprand_obj(scm_byte_t *ip, ScmObj clsr, ScmObj obj)
{
  if (ip == NULL) {
    scm_capi_error("failed to updated operands: invalid ip", 0);
    return -1;
  }
  else if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("failed to updated operands: invalid argument", 1, clsr);
    return -1;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("failed to updated operands: invalid argument", 1, obj);
    return -1;
  }

  return scm_fcd_inst_update_oprand_obj(ip, clsr, obj);
}


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj
scm_api_assemble(ScmObj lst, ScmObj iseq)
{
  if (!(scm_capi_pair_p(lst) || scm_capi_nil_p(lst))) {
    scm_capi_error("asm: pair required, but got", 1, lst);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(iseq) && !scm_capi_iseq_p(iseq)) {
    scm_capi_error("asm: iseq required, but got", 1, iseq);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_assemble(lst, iseq);
}


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

bool
scm_capi_compiler_p(ScmObj obj)
{
  return scm_fcd_compiler_p(obj);
}

ScmObj
scm_api_compiler_P(ScmObj obj)
{
  return scm_fcd_compiler_P(obj);
}

ScmObj
scm_api_make_compiler(ScmObj mod)
{
  return scm_fcd_make_compiler(mod);
}

ScmObj
scm_api_compiler_current_module(ScmObj cmpl)
{
  if (!scm_capi_compiler_p(cmpl)) {
    scm_capi_error("failed to get current module: invalid argument", 1, cmpl);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_compiler_current_module(cmpl);
}

ScmObj
scm_api_compiler_current_expr(ScmObj cmpl)
{
  if (!scm_capi_compiler_p(cmpl)) {
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

  if (!scm_capi_compiler_p(cmpl)) {
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

  if (!scm_capi_compiler_p(cmpl)) {
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
  if (!scm_capi_compiler_p(cmpl)) {
    scm_capi_error("failed to assign label id: invalid argument", 1, cmpl);
    return -1;
  }

  return scm_fcd_compiler_assign_label_id_i(cmpl);
}


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

bool
scm_capi_gloc_p(ScmObj obj)
{
  return scm_fcd_gloc_p(obj);
}

int
scm_capi_gloc_value(ScmObj gloc, scm_csetter_t *val)
{
  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("failed to get a value of gloc: invalid argument", 1, gloc);
    return -1;
  }

  if (val != NULL)
    scm_csetter_setq(val, scm_fcd_gloc_value(gloc));

  return 0;
}

int
scm_capi_gloc_symbol(ScmObj gloc, scm_csetter_t *sym)
{
  SCM_REFSTK_INIT_REG(&gloc);

  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("failed to get a symbol of gloc: invalid argument", 1, gloc);
    return -1;
  }

  if (sym != NULL)
    scm_csetter_setq(sym, scm_fcd_gloc_symbol(gloc));

  return 0;
}

int
scm_capi_gloc_bind(ScmObj gloc, ScmObj val)
{
  SCM_REFSTK_INIT_REG(&gloc, &val);

  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("failed to update value of gloc: invalid argument", 1, gloc);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to update value of gloc: invalid argument", 1, val);
    return -1;
  }

  scm_fcd_gloc_bind(gloc, val);

  return 0;
}

ScmObj
scm_api_make_module(ScmObj name)
{
  if (!scm_fcd_module_name_p(name)) {
    scm_capi_error("failed to make module: invalid argument", 1, name);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_module(name);
}

extern inline bool
scm_capi_module_p(ScmObj obj)
{
  return scm_fcd_module_p(obj);
}

int
scm_capi_find_module(ScmObj name, scm_csetter_t *mod)
{
  SCM_REFSTK_INIT_REG(&name);

  if (!scm_fcd_module_name_p(name)) {
    scm_capi_error("failed to find module: invalid argument", 1, name);
    return -1;
  }

  return scm_fcd_find_module(name, mod);
}

ScmObj
scm_api_module_name(ScmObj module)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to get a name from module: "
                   "invalid argument", 1, module);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_module_name(module);
}

int
scm_capi_import(ScmObj module, ScmObj imported, bool restrictive)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to import a module: invalid argument", 1, module);
    return -1;
  }

  if (!scm_fcd_module_specifier_p(imported)) {
    scm_capi_error("failed to import a module: no such a module", 1, imported);
    return -1;
  }

  return scm_fcd_import(module, imported, restrictive);
}

ScmObj
scm_api_get_gloc(ScmObj module, ScmObj sym)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to obtain a GLoc object: "
                   "invalid argument", 1, module);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to obtain a GLoc object: "
                   "invalid argument", 1, sym);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_get_gloc(module, sym);
}

int
scm_capi_find_gloc(ScmObj module, ScmObj sym, scm_csetter_t *gloc)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to find a GLoc object: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to find a GLoc object: invalid argument", 1, sym);
    return -1;
  }

  return scm_fcd_find_gloc(module, sym, gloc);
}

int
scm_capi_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to define global variable: "
                   "invalid argument", 1, module);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to define global variable: "
                   "invalid argument", 1, sym);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to define global variable: "
                   "invalid argument", 1, val);
    return -1;
  }

  return scm_fcd_define_global_var(module, sym, val, export);
}

int
scm_capi_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, sym);
    return -1;
  }
  else if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("failed to define syntax: invalid argument", 1, syx);
    return -1;
  }

  return scm_fcd_define_global_syx(module, sym, syx, export);
}

int
scm_capi_global_var_ref(ScmObj module, ScmObj sym, scm_csetter_t *val)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to get a value of global variable:"
                   " invalid argument", 1, module);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to get a value of global variable:"
                   " invalid argument", 1, sym);
    return -1;
  }

  return scm_fcd_global_var_ref(module, sym, val);
}

int
scm_capi_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  if (!scm_fcd_module_specifier_p(module)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, module);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to get a syntax: invalid argument", 1, sym);
    return -1;
  }

  return scm_fcd_global_syx_ref(module, sym, syx);
}

int
scm_capi_cached_global_var_ref(int kind, scm_csetter_t *val)
{
  return scm_fcd_cached_global_var_ref(kind, val);
}


/*******************************************************************/
/*  Return Value                                                   */
/*******************************************************************/

int
scm_capi_return_val(const ScmObj *val, int vc)
{
  if (vc < 0) {
    scm_capi_error("failed to setup return value: invalid VC value", 0);
    return -1;
  }
  else if (vc > 0 && val == NULL) {
    scm_capi_error("failed to setup return value: invalid VAL", 0);
    return -1;
  }

  return scm_fcd_return_val(val, vc);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

ScmObj
scm_api_capture_cont(void)
{
  return scm_fcd_capture_cont();
}

bool
scm_capi_continuation_p(ScmObj obj)
{
  return scm_fcd_continuation_p(obj);
}

ScmObj
scm_api_cont_capture_obj(ScmObj cont)
{
  if (!scm_capi_continuation_p(cont)) {
    scm_capi_error("failed to get capture object from continuation: "
                   "invalid argument", 1, cont);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_cont_capture_obj(cont);
}


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int
scm_capi_trampolining(ScmObj proc, ScmObj args,
                      ScmObj postproc, ScmObj handover)
{
  if (!scm_capi_procedure_p(proc)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (!scm_capi_pair_p(args) && !scm_capi_nil_p(args)) {
    scm_capi_error("", 0);
    return -1;
  }
  else if (scm_obj_not_null_p(postproc) && !scm_capi_procedure_p(postproc)) {
    scm_capi_error("", 0);
    return -1;
  }

  return scm_fcd_trampolining(proc, args, postproc, handover);
}


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

int
scm_capi_pformat_lst(ScmObj port, ScmObj fmt, ScmObj lst)
{
  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("format: output-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_fcd_textual_port_p(port)) {
    scm_capi_error("format: textual-port required, but got", 1, port);
    return -1;
  }
  if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return -1;
  }

  return scm_fcd_pformat_lst(port, fmt, lst);
}

ScmObj
scm_api_format_lst(ScmObj fmt, ScmObj lst)
{
  if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_format_lst(fmt, lst);
}

int
scm_capi_pformat_cv(ScmObj port, ScmObj fmt, ScmObj *obj, size_t n)
{
  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("format: output-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_fcd_textual_port_p(port)) {
    scm_capi_error("format: textual-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return -1;
  }
  else if (n > 0 && obj == NULL) {
    scm_capi_error("format: invalid arugment", 0);
    return -1;
  }

  return scm_fcd_pformat_cv(port, fmt, obj, n);
}

ScmObj
scm_capi_format_cv(ScmObj fmt, ScmObj *obj, size_t n)
{
  if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }
  else if (n > 0 && obj == NULL) {
    scm_capi_error("format: invalid arugment", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_format_cv(fmt, obj, n);
}

ScmObj
scm_api_pformat(ScmObj port, ScmObj fmt, ...)
{
  va_list arg;
  int r;

  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("format: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_fcd_textual_port_p(port)) {
    scm_capi_error("format: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }

  va_start(arg, fmt);
  r = scm_fcd_pformat_va(port, fmt, arg);
  va_end(arg);

  return ((r < 0) ? SCM_OBJ_NULL : SCM_UNDEF_OBJ);
}


ScmObj
scm_api_format(ScmObj fmt, ...)
{
  ScmObj ret = SCM_OBJ_INIT;
  va_list arg;

  if (!scm_capi_string_p(fmt)) {
    scm_capi_error("format: string required, but got", 1, fmt);
    return SCM_OBJ_NULL;
  }

  va_start(arg, fmt);
  ret = scm_fcd_format_va(fmt, arg);
  va_end(arg);

  return ret;
}

int
scm_capi_pformat_cstr(ScmObj port, const char *fmt, ...)
{
  va_list arg;
  int r;

  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("format: output-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_fcd_textual_port_p(port)) {
    scm_capi_error("format: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  va_start(arg, fmt);
  r = scm_fcd_pformat_cstr_va(port, fmt, arg);
  va_end(arg);

  return r;
}

ScmObj
scm_capi_format_cstr(const char *fmt, ...)
{
  ScmObj ret = SCM_OBJ_INIT;
  va_list arg;

  va_start(arg, fmt);
  ret = scm_fcd_format_cstr_va(fmt, arg);
  va_end(arg);

  return ret;
}


/*******************************************************************/
/*  Marshal/Unmarshal                                              */
/*******************************************************************/

extern inline bool
scm_capi_marshal_p(ScmObj obj)
{
  return scm_fcd_marshal_p(obj);
}

ScmObj
scm_api_make_marshal(void)
{
  return scm_fcd_make_marshal();
}

int
scm_capi_marshal_push(ScmObj marshal, ScmObj obj)
{
  if (!scm_capi_marshal_p(marshal)) {
    scm_capi_error("failed to marshal object: "
                   "marshaling object required, but got", 1, marshal);
    return -1;
  }
  else if (scm_fcd_marshal_terminated_p(marshal)) {
    scm_capi_error("failed to marshal object: "
                   "marshaling object has already terminated", 1, marshal);
    return -1;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("failed to marshal object: invalid argument", 1, obj);
    return -1;
  }

  return scm_fcd_marshal_push(marshal, obj);
}

void *
scm_capi_marshal_terminate(ScmObj marshal, size_t *size)
{
  if (!scm_capi_marshal_p(marshal)) {
    scm_capi_error("failed to marshal object: "
                   "marshaling object required, but got", 1, marshal);
    return NULL;
  }
  else if (scm_fcd_marshal_terminated_p(marshal)) {
    scm_capi_error("failed to marshal object: "
                   "marshaling object has already terminated", 1, marshal);
    return NULL;
  }

  return scm_fcd_marshal_terminate(marshal, size);
}

extern inline bool
scm_capi_unmarshal_p(ScmObj obj)
{
  return scm_fcd_unmarshal_p(obj);
}

ScmObj
scm_capi_make_unmarshal(const void *data)
{
  if (data == NULL) {
    scm_capi_error("failed to make unmarshal: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_unmarshal(data);
}

ScmObj
scm_capi_unmarshal_ref(ScmObj unmarshal, size_t idx)
{
  if (!scm_capi_unmarshal_p(unmarshal)) {
    scm_capi_error("failed to unmarshal object: "
                   "unmarshaling object required, but got", 1, unmarshal);
    return SCM_OBJ_NULL;
  }
  else if (idx >= scm_fcd_unmarshal_num(unmarshal)) {
    scm_capi_error("failed to unmarshal object: out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_unmarshal_ref(unmarshal, idx);
}

void *
scm_capi_marshal(size_t *size, ...)
{
  void *data;
  va_list args;

  va_start(args, size);
  data = scm_fcd_marshal_va(size, args);
  va_end(args);

  return data;
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
/*  System Environment                                             */
/*******************************************************************/

ScmEncoding *
scm_capi_system_encoding(void)
{
  return scm_fcd_system_encoding();
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
    o = scm_capi_make_symbol_from_cstr(module[i - 1], SCM_ENC_SRC);
    if (scm_obj_null_p(o)) return SCM_OBJ_NULL;

    mod_name = scm_api_cons(o, mod_name);
    if (scm_obj_null_p(mod_name)) return SCM_OBJ_NULL;
  }

  sym = scm_capi_make_symbol_from_cstr(name, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  r = scm_capi_find_module(mod_name, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_capi_error("failed to find module", 1, mod_name);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_global_var_ref(mod, sym, SCM_CSETTER_L(proc));
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

    port = scm_capi_open_input_string_cstr(path, NULL);
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

    port = scm_capi_open_input_string_cstr(expr, NULL);
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
  str = scm_capi_malloc(path_len + ext_len + 1);
  if (str == NULL) goto err;

  memcpy(str, path, path_len);
  memcpy(str + path_len, ext, ext_len);
  str[path_len + ext_len] = '\0';

  fp = fopen(str, "wb");
  if (fp == NULL) goto err;

  n = fwrite(marshal, 1, size, fp);
  if (n < size) goto err;

  fclose(fp);
  scm_capi_free(str);
  return 0;

 err:
  scm_capi_error("failed to dump marshale data", 0);
  if (fp != NULL) fclose(fp);
  if (str != NULL) scm_capi_free(str);
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

    port = scm_capi_open_input_string_cstr(path, NULL);
    if (scm_obj_null_p(port)) goto end;

    /* TODO: read_line ではなく port から全て読みよっとものを 1 つの文字列に
     *       する */
    str = scm_api_read_line(port);
    if (scm_obj_null_p(str)) goto end;

    port = scm_capi_open_input_string_cstr("(main)", SCM_ENC_NAME_SRC);
    if (scm_obj_null_p(port)) goto end;

    mod = scm_api_read(port);
    if (scm_obj_null_p(str)) goto end;

    proc = scm_get_proc("compile-file",
                        (const char *[]){"scythe", "internal", "compile"}, 3);
    if(scm_obj_null_p(proc)) goto end;

    args = scm_capi_list(2, str, mod);
    if (scm_obj_null_p(args)) goto end;

    val = scm_fcd_vm_apply(scm_fcd_current_vm(), proc, args);
    if (scm_obj_null_p(val)) goto end;

    val = scm_capi_vector_ref(val, 0);
    if (scm_obj_null_p(val)) goto end;

    val = scm_api_assemble(val, SCM_OBJ_NULL);
    if (scm_obj_null_p(val)) goto end;

    marshal = scm_capi_marshal(&size, val, SCM_OBJ_NULL);
    if (marshal == NULL) goto end;

    dump_marshal("marshal.out", NULL, marshal, size);
    scm_capi_free(marshal);
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
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("load: invalid argument", 1, iseq);
    return -1;
  }

  return scm_fcd_load_iseq(iseq);
}
