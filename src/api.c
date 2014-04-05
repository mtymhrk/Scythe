#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "reference.h"
#include "miscobjects.h"
#include "char.h"
#include "string.h"
#include "symbol.h"
#include "procedure.h"
#include "number.h"
#include "pair.h"
#include "vector.h"
#include "port.h"
#include "parser.h"
#include "syntax.h"
#include "iseq.h"
#include "module.h"
#include "assembler.h"
#include "compiler.h"
#include "exception.h"

#include "encoding.h"
#include "impl_utils.h"

#include "api_enum.h"
#include "api.h"


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

void
scm_capi_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_bedrock_current_br(), msg);
}

extern inline void
scm_capi_fatalf(const char *fmt, ...)
{
}

extern inline bool
scm_capi_fatal_p(void)
{
  return scm_bedrock_fatal_p(scm_bedrock_current_br());
}


/*******************************************************************/
/*  C Stack                                                        */
/*******************************************************************/

void
scm_capi_ref_stack_push_ary(ScmObj *ary, size_t n)
{
  if (ary == NULL) return;

  scm_ref_stack_push_ary(scm_bedrock_current_br()->ref_stack, ary, n);
}

void
scm_capi_ref_stack_push(int dummy, ...)
{
  va_list ap;

  va_start(ap, dummy);
  scm_ref_stack_push_va(scm_bedrock_current_br()->ref_stack, ap);
  va_end(ap);
}

void
scm_capi_ref_stack_save(ScmRefStackInfo *info)
{
  scm_ref_stack_save(scm_bedrock_current_br()->ref_stack, info);
}

void
scm_capi_ref_stack_restore(ScmRefStackInfo *info)
{
  scm_ref_stack_restore(scm_bedrock_current_br()->ref_stack, info);
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

  return scm_mem_alloc_heap(scm_vm_current_mm(), type, add_size);
}

ScmObj
scm_capi_mem_alloc_root(ScmTypeInfo *type, size_t add_size)
{
  if (type == NULL) {
    scm_capi_fatal("memory allocation error: invalid object type");
    return SCM_OBJ_NULL;
  }

  return scm_mem_alloc_root(scm_vm_current_mm(), type, add_size);
}

ScmObj
scm_capi_mem_alloc(ScmTypeInfo *otype, size_t add_size, SCM_MEM_TYPE_T mtype)
{
  switch(mtype) {
  case SCM_MEM_HEAP:
    return scm_capi_mem_alloc_heap(otype, add_size);
    break;
  case SCM_MEM_ROOT:
    return scm_capi_mem_alloc_root(otype, add_size);
    break;
  default:
    scm_capi_fatal("memory allocation error: invalid memory type");
    return SCM_OBJ_NULL;          /* provisional implemntation */
    break;
  };
}

ScmObj
scm_capi_mem_free_root(ScmObj obj)
{
  if (obj == SCM_OBJ_NULL) return SCM_OBJ_NULL;
  return scm_mem_free_root(scm_vm_current_mm(), obj);
}

ScmRef
scm_capi_mem_register_extra_rfrn(ScmRef ref)
{
  if (ref == SCM_REF_NULL) return ref;
  return scm_mem_register_extra_rfrn(scm_vm_current_mm(), ref);
}

void
scm_capi_gc_start(void)
{
  scm_mem_gc_start(scm_vm_current_mm());
}

void
scm_capi_gc_enable(void)
{
  scm_mem_enable_gc(scm_vm_current_mm());
}

void
scm_capi_gc_disable(void)
{
  scm_mem_disable_gc(scm_vm_current_mm());
}


/*******************************************************************/
/*  NULL Value                                                     */
/*******************************************************************/

/* TODO: この API は削除する */
extern inline bool
scm_capi_null_value_p(ScmObj obj)
{
  return scm_obj_null_p(obj);
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
  return scm_obj_same_instance_p(obj1, obj2);
}

int
scm_capi_eq(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("eq?: invalid argument", 0);
    return -1;
  }

  if (rslt != NULL)
    *rslt = scm_capi_eq_p(obj1, obj2);

  return 0;
}

ScmObj
scm_api_eq_P(ScmObj obj1, ScmObj obj2)
{
  bool cmp;
  int r;

  r = scm_capi_eq(obj1, obj2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_eqv(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  bool cmp;

  SCM_STACK_FRAME_PUSH(&obj1, &obj2);

  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("eqv?: invalid argument", 0);
    return -1;
  }

  if (scm_capi_eq_p(obj1, obj2)) {
    cmp = true;
  }
  else if (scm_capi_number_p(obj1)) {
    if (scm_capi_number_p(obj2)) {
      int r = scm_capi_num_eq(obj1, obj2, &cmp);
      if (r < 0) return -1;
    }
    else {
      cmp = false;
    }
  }
  else if (!scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2))) {
    cmp = false;
  }
  else if (scm_capi_symbol_p(obj1)) {
    int r = scm_capi_symbol_eq(obj1, obj2, &cmp);
    if (r < 0) return -1;
  }
  else if (scm_capi_char_p(obj1)) {
    int r = scm_capi_char_eq(obj1, obj2, &cmp);
    if (r < 0) return -1;
  }
  else {
    cmp = false;
  }

  if (rslt != NULL) *rslt = cmp;
  return 0;
}

ScmObj
scm_api_eqv_P(ScmObj obj1, ScmObj obj2)
{
  bool cmp;
  int r;

  r = scm_capi_eqv(obj1, obj2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

enum { NON_CIRCULATIVE,
       SAME_CIRCULATION,
       DIFFERENT_CIRCULATION };

static int
scm_api_equal_check_circular(ScmObj obj1, ScmObj obj2,
                             ScmObj stack1, ScmObj stack2, int *rslt)
{
  ScmObj elm1 = SCM_OBJ_INIT, elm2 = SCM_OBJ_INIT;
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  size_t cnt1 = 0, cnt2 = 0;

  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));
  scm_assert(scm_capi_nil_p(stack1) || scm_capi_pair_p(stack1));
  scm_assert(scm_capi_nil_p(stack2) || scm_capi_pair_p(stack2));
  scm_assert(rslt != NULL);

  SCM_STACK_FRAME_PUSH(&obj1, &obj2, &stack1, &stack2,
                       &elm1, &elm2,
                       &lst1, &lst2);

  for (lst1 = stack1, cnt1 = 0;
       scm_capi_pair_p(lst1);
       lst1 = scm_api_cdr(lst1), cnt1++) {
    elm1 = scm_api_car(lst1);
    if (scm_obj_null_p(elm1)) return -1;

    if (scm_capi_eq_p(obj1, elm1)) break;
  }
  if (scm_obj_null_p(lst1)) return -1;

  if (!scm_capi_nil_p(lst1)) {     /* 循環構造がある */
    for (lst2 = stack2, cnt2 = 0;
         cnt2 <= cnt1 && scm_capi_pair_p(lst2);
         lst2 = scm_api_cdr(lst2), cnt2++) {
      elm2 = scm_api_car(lst2);
      if (scm_obj_null_p(elm2)) return -1;

      if (scm_capi_eq_p(obj2, elm2)) break;
    }
    if (scm_obj_null_p(lst2)) return -1;

    if (cnt1 == cnt2)         /* 循環構造が一致 */
      *rslt = SAME_CIRCULATION;
    else                      /* 循環構造が不一致 */
      *rslt = DIFFERENT_CIRCULATION;
  }
  else {     /* 循環構造がない */
    *rslt = NON_CIRCULATIVE;
  }

  return 0;
}

/*
 * Memo: equal? procedure の動作について
 * 以下の 2 つの循環リストはいずれも、シンボル a、b が交互に表われるリストであ
 * るが、これらの equal? は実装上、偽となる。R7RS では真を返さなければならない。
 *
 *   1. #0=(a b a b . #0#)
 *   2. (a b . #0=(a b . #0#))
 *
 *  (equal? '#0=(a b a b . #0#) '(a b . #0=(a b . #0#)))  ; => #f
 *
 */

static int
scm_capi_equal_aux(ScmObj obj1, ScmObj obj2,
                   ScmObj stack1, ScmObj stack2, bool *rslt)
{
  ScmObj elm1 = SCM_OBJ_INIT, elm2 = SCM_OBJ_INIT;
  bool cmp;
  int r, cir;

  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));
  scm_assert(scm_capi_nil_p(stack1) || scm_capi_pair_p(stack1));
  scm_assert(scm_capi_nil_p(stack2) || scm_capi_pair_p(stack2));

  SCM_STACK_FRAME_PUSH(&obj1, &obj2, &stack1, &stack2,
                       &elm1, &elm2);

  r = scm_capi_eqv(obj1, obj2, &cmp);
  if (r < 0) return -1;

  if (!cmp  && scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2))) {
    if (scm_capi_pair_p(obj1) || scm_capi_vector_p(obj1)) {
      r = scm_api_equal_check_circular(obj1, obj2, stack1, stack2, &cir);
      if (r < 0) return -1;

      if (cir == SAME_CIRCULATION) {
        cmp = true;
        goto success;
      }
      else if (cir == DIFFERENT_CIRCULATION) {
        cmp = false;
        goto success;
      }
      else {
        stack1 = scm_api_cons(obj1, stack1);
        if (scm_obj_null_p(stack1)) return -1;

        stack2 = scm_api_cons(obj2, stack2);
        if (scm_obj_null_p(stack2)) return -1;
      }
    }

    if (scm_capi_string_p(obj1)) {
      r = scm_capi_string_eq(obj1, obj2, &cmp);
      if (r < 0) return -1;
    }
    else if (scm_capi_pair_p(obj1)) {
      elm1 = scm_api_car(obj1);
      if (scm_obj_null_p(elm1)) return -1;

      elm2 = scm_api_car(obj2);
      if (scm_obj_null_p(elm2)) return -1;

      r = scm_capi_equal_aux(elm1, elm2, stack1, stack2, &cmp);
      if (r < 0) return -1;

      if (cmp) {
        elm1 = scm_api_cdr(obj1);
        if (scm_obj_null_p(elm1)) return -1;

        elm2 = scm_api_cdr(obj2);
        if (scm_obj_null_p(elm2)) return -1;

        r = scm_capi_equal_aux(elm1, elm2, stack1, stack2, &cmp);
        if (r < 0) return -1;
      }
    }
    else if (scm_capi_vector_p(obj1)) {
      ssize_t len1 = scm_capi_vector_length(obj1);
      ssize_t len2 = scm_capi_vector_length(obj2);

      if (len1 < 0 || len2 < 0) return -1;

      if (len1 != len2) {
        cmp = false;
      }
      else {
        cmp = true;
        for (ssize_t i = 0; i < len1; i++) {
          elm1 = scm_capi_vector_ref(obj1, (size_t)i);
          if (scm_obj_null_p(elm1)) return SCM_OBJ_NULL;

          elm2 = scm_capi_vector_ref(obj2, (size_t)i);
          if (scm_obj_null_p(elm2)) return SCM_OBJ_NULL;

          r = scm_capi_equal_aux(elm1, elm2, stack1, stack2, &cmp);
          if (r < 0) return -1;

          if (!cmp) break;
        }
      }
    }
    else {
      cmp = false;
    }
  }

 success:

  if (rslt != NULL) *rslt = cmp;
  return 0;
}

int
scm_capi_equal(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  ScmObj stack1 = SCM_OBJ_INIT, stack2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj1, &obj2,
                       &stack1, &stack2);

  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("equal?: invalid argument", 0);
    return -1;         /* provisional implemntation */
  }

  stack1 = stack2 = SCM_NIL_OBJ;

  return scm_capi_equal_aux(obj1, obj2, stack1, stack2, rslt);
}

ScmObj
scm_api_equal_P(ScmObj obj1, ScmObj obj2)
{
  bool cmp;
  int r;

  r = scm_capi_equal(obj1, obj2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  return scm_vm_nil(scm_vm_current_vm());
}

extern inline bool
scm_capi_nil_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, SCM_NIL_OBJ);
}

ScmObj
scm_api_nil_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("null?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_nil_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/


bool
scm_capi_boolean_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return scm_obj_type_p(obj, &SCM_BOOL_TYPE_INFO);
}

ScmObj
scm_api_boolean_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("boolean?: invaid argument", 0);
    return SCM_OBJ_NULL;
  }
  return scm_capi_boolean_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

/* Memo:
 *  scm_api_true() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_TRUE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_true(void)
{
  return scm_vm_true(scm_vm_current_vm());
}

/* Memo:
 *  scm_api_false() の関数の実行では GC が発生してはダメ。
 *  (マクロ SCM_FALSE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_api_false(void)
{
  return scm_vm_false(scm_vm_current_vm());
}

extern inline bool
scm_capi_true_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, SCM_TRUE_OBJ);
}

extern inline bool
scm_capi_false_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, SCM_FALSE_OBJ);
}

extern inline bool
scm_capi_true_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return !scm_capi_false_object_p(obj);
}

extern inline bool
scm_capi_false_p(ScmObj obj)
{
  return scm_capi_false_object_p(obj);
}

ScmObj
scm_api_not(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("not: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_false_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  return scm_vm_eof(scm_vm_current_vm());
}

extern inline bool
scm_capi_eof_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, SCM_EOF_OBJ);
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
  return scm_vm_undef(scm_vm_current_vm());
}

extern inline bool
scm_capi_undef_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, SCM_UNDEF_OBJ);
}


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

int
scm_capi_raise(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("raise: invalid argument", 0);
    return -1;
  }

  return scm_vm_setup_stat_raised(scm_vm_current_vm(), obj);
}

extern inline ScmObj
scm_api_raise(ScmObj obj)
{
  return (scm_capi_raise(obj) < 0) ? SCM_OBJ_NULL : SCM_UNDEF_OBJ;
}

extern inline bool
scm_capi_raised_p(void)
{
  return scm_vm_raised_p(scm_vm_current_vm());
}

extern inline int
scm_capi_unraise(void)
{
  return scm_vm_clear_stat_raised(scm_vm_current_vm());
}

int
scm_capi_error(const char *msg, size_t n, ...)
{
  ScmObj str = SCM_OBJ_INIT, exc = SCM_OBJ_INIT;
  va_list irris;
  int rslt;

  SCM_STACK_FRAME_PUSH(&str, &exc);

  /* BUG: 可変引数 の PUSH の前に GC が走る可能性がある */
  if (msg == NULL)
    str = scm_capi_make_string_from_cstr("", SCM_ENC_ASCII);
  else
    str = scm_capi_make_string_from_cstr(msg, SCM_ENC_ASCII);

  if (scm_obj_null_p(str)) return -1;

  va_start(irris, n);
  exc = scm_exception_new_va(SCM_MEM_HEAP, str, n, irris);
  va_end(irris);

  if (scm_obj_null_p(exc)) return -1;

  rslt = scm_capi_raise(exc);
  if (rslt < 0) return -1;

  return 0;
}

ScmObj
scm_api_error_ary(ScmObj msg, size_t n, ScmObj *irris)
{
  ScmObj exc = SCM_OBJ_INIT;
  int rslt;

  if (!scm_capi_string_p(msg)) return SCM_OBJ_NULL;

  exc = scm_exception_new_ary(SCM_MEM_HEAP, msg, n, irris);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL;

  rslt = scm_capi_raise(exc);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

extern inline bool
scm_capi_error_object_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return scm_obj_type_p(obj, &SCM_EXCEPTION_TYPE_INFO);
}

/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

extern inline bool
scm_capi_pair_p(ScmObj pair)
{
  if (scm_obj_null_p(pair)) return false;
  return scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO);
}

ScmObj
scm_api_pair_P(ScmObj pair)
{
  if (scm_obj_null_p(pair)) {
    scm_capi_error("pair?: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  return scm_capi_pair_p(pair) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_cons(ScmObj car, ScmObj cdr)
{
  if (scm_obj_null_p(car) || scm_obj_null_p(cdr)) {
    scm_capi_error("cons: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr);
}

ScmObj
scm_api_car(ScmObj pair)
{
  if (scm_obj_null_p(pair)) {
    scm_capi_error("car: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("car: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_pair_car(pair);
}

ScmObj
scm_api_cdr(ScmObj pair)
{
  if (scm_obj_null_p(pair)) {
    scm_capi_error("cdr: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("cdr: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  return scm_pair_cdr(pair);
}

int
scm_capi_set_car_i(ScmObj pair, ScmObj elm)
{
  if (scm_obj_null_p(pair) || scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 0);
    return -1;
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return -1;
  }

  if (scm_pair_set_car(pair, elm) < 0)
    return -1;

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
  if (scm_obj_null_p(pair) || scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 0);
    return -1;
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return -1;
  }

  if (scm_pair_set_cdr(pair, elm) < 0)
    return -1;

  return 0;
}

ScmObj
scm_api_set_cdr_i(ScmObj pair, ScmObj elm)
{
  if (scm_capi_set_cdr_i(pair, elm) < 0)
    return SCM_OBJ_NULL;

  return scm_api_undef();
}

ScmObj
scm_capi_cxr(ScmObj pair, const char *dir)
{
  ScmObj x = SCM_OBJ_INIT;
  size_t i;

  SCM_STACK_FRAME_PUSH(&pair,
                       &x);

  if (dir == NULL) {
    scm_capi_error("faild to execute car or cdr: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  x = pair;
  for (i = strlen(dir); i > 0; i--) {
    switch (dir[i - 1]) {
    case 'a':                   /* fall through */
    case 'A':
      x = scm_api_car(x);
      break;
    case 'd':                   /* fall through */
    case 'D':
      x = scm_api_cdr(x);
      break;
    default:
      scm_capi_error("faild to execute car or cdr: invalid argument", 0);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(x)) return SCM_OBJ_NULL;
  }

  return x;
}

ScmObj
scm_api_list_P(ScmObj lst)
{
  ScmObj rabbit = SCM_OBJ_INIT, tortoise = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &rabbit, &tortoise);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("list?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_nil_p(lst))
    return SCM_TRUE_OBJ;
  else if (!scm_capi_pair_p(lst))
    return SCM_FALSE_OBJ;

  rabbit = tortoise = lst;

  do {
    tortoise = scm_api_cdr(tortoise);
    if (scm_obj_null_p(tortoise))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(lst))
      return SCM_TRUE_OBJ;
    else if (!scm_capi_pair_p(lst))
      return SCM_FALSE_OBJ;

    rabbit = scm_api_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_capi_pair_p(rabbit))
      return SCM_FALSE_OBJ;

    rabbit = scm_api_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_capi_pair_p(rabbit))
      return SCM_FALSE_OBJ;
  } while (!scm_capi_eq_p(tortoise, rabbit));

  return SCM_FALSE_OBJ;
}

ScmObj
scm_capi_make_list(size_t n, ScmObj fill)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&fill,
                       &lst);

  if (scm_obj_null_p(fill))
    fill = scm_api_undef();

  lst = scm_api_nil();
  for (size_t i = 0; i < n; i++) {
    lst = scm_api_cons(fill, lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_api_make_list(ScmObj n, ScmObj fill)
{
  size_t s;
  int r;

  if (!scm_capi_integer_p(n)) {
    scm_capi_error("make-list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_list(s, fill);
}

ScmObj
scm_capi_list_cv(const ScmObj *elm, size_t n)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);

  if (elm == NULL)
    return scm_api_nil();

  lst = scm_api_nil();
  for (size_t i = n; i > 0; i--) {
    if (scm_obj_null_p(elm[i - 1])) {
      scm_capi_error("list: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
    lst = scm_api_cons(elm[i - 1], lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_capi_list(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_STACK_FRAME;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  for (unsigned int i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  return scm_capi_list_cv(args, n);
}

ssize_t
scm_capi_length(ScmObj lst)
{
  ScmObj node = SCM_OBJ_INIT;
  size_t n;

  SCM_STACK_FRAME_PUSH(&node);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("length: invalid argument", 0);
    return -1;
  }
  else if (scm_capi_nil_p(lst)) {
    return 0;
  }
  else if (!scm_capi_pair_p(lst)) {
    scm_capi_error("length: list required, but got", 1, lst);
    return -1;
  }

  for (node = lst, n = 0;
       scm_capi_pair_p(node);
       node = scm_api_cdr(node), n++) {
    if (n > SSIZE_MAX) {
      scm_capi_error("length: too long list", 0);
      return -1;
    }
  }

  if (scm_obj_null_p(node)) {
    return -1;
  }
  else if (scm_capi_nil_p(node)) {
    return (ssize_t)n;
  }
  else {
    scm_capi_error("lenght: improper list is passed", 0);
    return -1;
  }
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
scm_capi_append_lst(ScmObj lst)
{
  ScmObj arg[] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst);
  SCM_STACK_PUSH_ARY(arg, sizeof(arg)/sizeof(arg[0]));

  if (scm_obj_null_p(lst)) {
    scm_capi_error("append: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  lst = scm_api_reverse(lst);
  if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;

  if (!scm_capi_pair_p(lst))
    return SCM_NIL_OBJ;

  arg[1] = scm_api_car(lst);
  if (scm_obj_null_p(arg[1])) return SCM_OBJ_NULL;

  for (l = scm_api_cdr(lst); scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    arg[0] = scm_api_car(l);
    if (scm_obj_null_p(arg[0])) return SCM_OBJ_NULL;

    arg[1] = scm_capi_append_cv(arg, 2);
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return arg[1];
}

ScmObj
scm_capi_append_cv(const ScmObj *lists, size_t n)
{
  ScmObj tail = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj new_lst = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, cur = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&tail, &lst, &elm,
                       &new_lst, &prv, &cur);

  if (lists == NULL || n == 0)
    return SCM_NIL_OBJ;

  if (scm_obj_null_p(lists[0])) {
    scm_capi_error("append: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (n == 1)
    return lists[0];

  tail = scm_capi_append_cv(lists + 1, n - 1);
  if (scm_obj_null_p(tail)) return SCM_OBJ_NULL;

  if (scm_capi_nil_p(lists[0]))
    return tail;

  if (!scm_capi_pair_p(lists[0])) {
    scm_capi_error("append: list required, but got", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  new_lst = prv = SCM_OBJ_NULL;
  for (lst = lists[0]; scm_capi_pair_p(lst); lst = scm_api_cdr(lst)) {
    elm = scm_api_car(lst);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    cur = scm_api_cons(elm, SCM_NIL_OBJ);
    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prv)) {
      rslt  = scm_capi_set_cdr_i(prv, cur);
      if (rslt < 0) return SCM_OBJ_NULL;
    }
    else {
      new_lst = cur;
    }

    prv = cur;
  }

  if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;

  if (!scm_capi_nil_p(lst)) {
    scm_capi_error("failed to append lists: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_set_cdr_i(prv, tail);
  if (rslt < 0) return SCM_OBJ_NULL;

  return new_lst;
}

ScmObj
scm_capi_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_STACK_FRAME;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  for (size_t i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  return scm_capi_append_cv(args, n);
}

ScmObj
scm_api_reverse(ScmObj lst)
{
  ScmObj new_lst = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &new_lst, &cur, &elm);

  new_lst = scm_api_nil();
  for (cur = lst; scm_capi_pair_p(cur); cur = scm_api_cdr(cur)) {
    elm = scm_api_car(cur);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    new_lst = scm_api_cons(elm, new_lst);
    if (scm_obj_null_p(new_lst)) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

  return new_lst;
}

ScmObj
scm_capi_list_tail(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(l)) {
      l = scm_api_cdr(l);
      if (scm_obj_null_p(l)) return SCM_OBJ_NULL;
    }
    else {
      scm_capi_error("list-tail: argument out of range", 0);
      return SCM_OBJ_NULL;
    }
  }

  return l;
}

ScmObj
scm_api_list_tail(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_capi_integer_p(n)) {
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
  ScmObj l = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(l)) {
      l = scm_api_cdr(l);
      if (scm_obj_null_p(l)) return SCM_OBJ_NULL;
    }
    else {
      scm_capi_error("list-ref: argument out of range", 0);
      return SCM_OBJ_NULL;
    }
  }

  if (!scm_capi_pair_p(l)) {
    scm_capi_error("list-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_car(l);
}

ScmObj
scm_api_list_ref(ScmObj lst, ScmObj n)
{
  size_t s;
  int r;

  if (!scm_capi_integer_p(n)) {
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
  ScmObj l = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(l))
      l = scm_api_cdr(l);
    else {
      scm_capi_error("list-set!: argument out of range", 0);
      return -1;
    }
  }

  if (!scm_capi_pair_p(l)) {
    scm_capi_error("list-set!: argument out of range", 0);
    return -1;
  }

  return scm_capi_set_car_i(l, obj);
}

ScmObj
scm_api_list_set_i(ScmObj lst, ScmObj n, ScmObj obj)
{
  size_t s;
  int r;

  if (!scm_capi_integer_p(n)) {
    scm_capi_error("list-set!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(n, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_list_set_i(lst, s, obj);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

static ScmObj
scm_capi_member_aux(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT, c = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj, &lst,
                       &l, &e, &c);

  scm_assert(cmp != NULL);

  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    e = scm_api_car(l);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    c = cmp(e, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_capi_true_p(c))
      return l;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return SCM_FALSE_OBJ;
}

ScmObj
scm_capi_memq(ScmObj obj, ScmObj lst)
{
  return scm_capi_member_aux(obj, lst, scm_api_eq_P);
}

ScmObj
scm_capi_memv(ScmObj obj, ScmObj lst)
{
  return scm_capi_member_aux(obj, lst, scm_api_eqv_P);
}

ScmObj
scm_capi_member(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  if (cmp == NULL)
    cmp = scm_api_equal_P;

  return scm_capi_member_aux(obj, lst, cmp);
}

static ScmObj
scm_capi_assoc_aux(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT, k = SCM_OBJ_INIT, c = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj, &alist,
                       &l, &e, &k, &c);

  scm_assert(cmp != NULL);

  for (l = alist; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    e = scm_api_car(l);
    if (scm_obj_null_p(e)) return SCM_OBJ_NULL;

    k = scm_api_car(e);
    if (scm_obj_null_p(k)) return SCM_OBJ_NULL;

    c = cmp(k, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_capi_true_p(c))
      return e;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return SCM_FALSE_OBJ;
}

ScmObj
scm_capi_assq(ScmObj obj, ScmObj alist)
{
  return scm_capi_assoc_aux(obj, alist, scm_api_eq_P);
}

ScmObj
scm_capi_assv(ScmObj obj, ScmObj alist)
{
  return scm_capi_assoc_aux(obj, alist, scm_api_eqv_P);
}

ScmObj
scm_capi_assoc(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  if (cmp == NULL)
    cmp = scm_api_equal_P;

  return scm_capi_assoc_aux(obj, alist, cmp);
}

ScmObj
scm_api_list_copy(ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, pair = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &cur, &elm, &nil,
                       &head, &pair, &prev,
                       &rslt);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("list-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_capi_nil_p(lst)) {
    return lst;
  }
  else if (!scm_capi_pair_p(lst)) {
    scm_capi_error("list-copy: list required, but got", 1, lst);
    return SCM_OBJ_NULL;
  }

  nil = scm_api_nil();

  prev = SCM_OBJ_NULL;
  head = SCM_OBJ_NULL;
  for (cur = lst; scm_capi_pair_p(cur); cur = scm_api_cdr(cur)) {
    elm = scm_api_car(cur);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    pair = scm_api_cons(elm, nil);
    if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prev)) {
      rslt = scm_api_set_cdr_i(prev, pair);
      if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
    }
    else {
      head = pair;
    }
    prev = pair;
  }

  if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

  rslt = scm_api_set_cdr_i(prev, cur);
  if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;

  return scm_obj_null_p(head) ? nil : head;
}


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

extern inline bool
scm_capi_fixnum_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_FIXNUM_TYPE_INFO);
}

ScmObj
scm_api_fixnum_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("fixnum?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_fixnum_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_bignum_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_BIGNUM_TYPE_INFO);
}

ScmObj
scm_api_bignum_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("bignum?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_bignum_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_number_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_NUM);
}

ScmObj
scm_api_number_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("number?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_number_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_complex_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, complex_p);
}

ScmObj
scm_api_complex_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("complex?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_complex_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_real_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, real_p);
}

ScmObj
scm_api_real_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("real?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_real_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_rational_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, rational_p);
}

ScmObj
scm_api_rational_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("rational?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_rational_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_integer_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, integer_p);
}

ScmObj
scm_api_integer_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("integer?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_integer_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_exact_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, exact_p);
}

ScmObj
scm_api_exact_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("exact?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_exact_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_inexact_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, inexact_p);
}

ScmObj
scm_api_inexact_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("inexact?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_inexact_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_exact_integer_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (scm_capi_integer_p(obj) && scm_capi_exact_p(obj))
    return true;
  else
    return false;
}

ScmObj
scm_api_exact_integer_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("exact-integer?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_exact_integer_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_finite_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, finite_p);
}

ScmObj
scm_api_finite_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("finite?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_finite_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_infinite_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, infinite_p);
}

ScmObj
scm_api_infinite_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("infinite?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_infinite_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_nan_p(ScmObj obj)
{
  SCM_STACK_FRAME_PUSH(&obj);

  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_FUNC(obj, nan_p);
}

ScmObj
scm_api_nan_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("nan?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_nan_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_capi_make_number_from_literal(const char *literal, size_t size)
{
  if (literal == NULL) {
    scm_capi_error("can not make number: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (size > SSIZE_MAX) {
    scm_capi_error("can not make number: too long literal", 0);
    return SCM_OBJ_NULL;
  }

  return scm_num_make_from_literal(literal, size);
}

ScmObj
scm_capi_make_number_from_sword(scm_sword_t num)
{
  if (num < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < num)
    return scm_bignum_new_from_sword(SCM_MEM_HEAP, num);
  else
    return scm_fixnum_new(num);
}

ScmObj
scm_capi_make_number_from_size_t(size_t num)
{
  if (num > SCM_FIXNUM_MAX)
    return scm_bignum_new_from_uword(SCM_MEM_HEAP, num);
  else
    return scm_fixnum_new((scm_sword_t)num);
}

static int
scm_capi_num_cmp_fold(ScmObj lst,
                      int (*cmp)(ScmObj n1, ScmObj n2, bool *rslt),
                      bool *rslt)

{
  ScmObj num = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &num, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    num = scm_api_car(l);

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, num, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = num;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

int
scm_capi_num_eq(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error("=: invalid argument", 0);
    return -1;
  }

  if (!scm_capi_number_p(n1)) {
    scm_capi_error("=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("=: number required, but got", 1, n2);
    return -1;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_capi_num_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("=: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_num_cmp_fold(lst, scm_capi_num_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_num_eq_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_eq(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_num_lt(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error("<: invalid argument", 0);
    return -1;
  }

  if (!scm_capi_number_p(n1)) {
    scm_capi_error("<: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("<: number required, but got", 1, n2);
    return -1;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1) ? true : false;

  return 0;
}

ScmObj
scm_capi_num_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("<: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_num_cmp_fold(lst, scm_capi_num_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error(">: invalid argument", 0);
    return -1;
  }

  if (!scm_capi_number_p(n1)) {
    scm_capi_error(">: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error(">: number required, but got", 1, n2);
    return -1;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_num_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error(">: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_num_cmp_fold(lst, scm_capi_num_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_api_num_gt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  rslt = scm_capi_num_gt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_num_le(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error("<=: invalid argument", 0);
    return -1;
  }

  if (!scm_capi_number_p(n1)) {
    scm_capi_error("<=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error("<=: number required, but got", 1, n2);
    return -1;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1 || cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_capi_num_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("<=: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_num_cmp_fold(lst, scm_capi_num_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error(">=: invalid argument", 0);
    return -1;
  }

  if (!scm_capi_number_p(n1)) {
    scm_capi_error(">=: number required, but got", 1, n1);
    return -1;
  }

  if (!scm_capi_number_p(n2)) {
    scm_capi_error(">=: number required, but got", 1, n2);
    return -1;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0 || cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_num_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error(">=: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_num_cmp_fold(lst, scm_capi_num_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  SCM_STACK_FRAME_PUSH(&num);

  if (!scm_capi_number_p(num)) return false;

  return SCM_NUM_CALL_FUNC(num, zero_p);
}

ScmObj
scm_api_zero_P(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (scm_obj_null_p(num)) {
    scm_capi_error("zero?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("zero?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_capi_zero_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_capi_positive_p(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (!scm_capi_number_p(num)) return false;

  return SCM_NUM_CALL_FUNC(num, positive_p);
}

ScmObj
scm_api_positive_P(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (scm_obj_null_p(num)) {
    scm_capi_error("positive?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("positive?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_capi_positive_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_capi_negative_p(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (!scm_capi_number_p(num)) return false;

  return SCM_NUM_CALL_FUNC(num, negative_p);
}

ScmObj
scm_api_negative_P(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (scm_obj_null_p(num)) {
    scm_capi_error("negative?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("negative?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_capi_negative_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_capi_odd_p(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (!scm_capi_number_p(num)) return false;

  return SCM_NUM_CALL_FUNC(num, odd_p);
}

ScmObj
scm_api_odd_P(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (scm_obj_null_p(num)) {
    scm_capi_error("odd?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("odd?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_capi_odd_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_capi_even_p(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (!scm_capi_number_p(num)) return false;

  return SCM_NUM_CALL_FUNC(num, even_p);
}

ScmObj
scm_api_even_P(ScmObj num)
{
  SCM_STACK_FRAME_PUSH(&num);

  if (scm_obj_null_p(num)) {
    scm_capi_error("even?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("even?: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return scm_capi_even_p(num) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

static ScmObj
scm_capi_num_bop_fold(ScmObj lst,
                      ScmObj (*func)(ScmObj n1, ScmObj n2))
{
  ScmObj rslt = SCM_OBJ_INIT, num = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &rslt, &num, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(func != NULL);

  rslt = scm_api_car(lst);
  if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;

  for (l = scm_api_cdr(lst); scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    num = scm_api_car(l);
    if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

    rslt = func(rslt, num);
    if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return rslt;
}

ScmObj
scm_api_max(ScmObj n1, ScmObj n2)
{
  bool n1_ie, n2_ie;
  int cmp, err;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error("max: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n1)) {
    scm_capi_error("max: number required, but got", 1, n1);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n2)) {
    scm_capi_error("max: number required, but got", 1, n2);
    return SCM_OBJ_NULL;
  }

  n1_ie = SCM_NUM_CALL_FUNC(n1, inexact_p);
  n2_ie = SCM_NUM_CALL_FUNC(n2, inexact_p);

  if (n1_ie && !n2_ie) {
    n2 = scm_api_inexact(n2);
    if (scm_obj_null_p(n2)) return SCM_OBJ_NULL;
  }
  else if (!n1_ie && n2_ie) {
    n1 = scm_api_inexact(n1);
    if (scm_obj_null_p(n1)) return SCM_OBJ_NULL;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return SCM_OBJ_NULL;

  return (cmp == 0 || cmp == 1) ? n1 : n2;
}

ScmObj
scm_capi_max_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("max: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_num_bop_fold(lst, scm_api_max);
}

ScmObj
scm_api_min(ScmObj n1, ScmObj n2)
{
  bool n1_ie, n2_ie;
  int cmp, err;

  SCM_STACK_FRAME_PUSH(&n1, &n2);

  if (scm_obj_null_p(n1) || scm_obj_null_p(n2)) {
    scm_capi_error("min: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n1)) {
    scm_capi_error("min: number required, but got", 1, n1);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(n2)) {
    scm_capi_error("min: number required, but got", 1, n2);
    return SCM_OBJ_NULL;
  }

  n1_ie = SCM_NUM_CALL_FUNC(n1, inexact_p);
  n2_ie = SCM_NUM_CALL_FUNC(n2, inexact_p);

  if (n1_ie && !n2_ie) {
    n2 = scm_api_inexact(n2);
    if (scm_obj_null_p(n2)) return SCM_OBJ_NULL;
  }
  else if (!n1_ie && n2_ie) {
    n1 = scm_api_inexact(n1);
    if (scm_obj_null_p(n1)) return SCM_OBJ_NULL;
  }

  err = SCM_NUM_CALL_FUNC(n1, cmp, n2, &cmp);
  if (err < 0) return SCM_OBJ_NULL;

  return (cmp == 0 || cmp == -1) ? n1 : n2;
}

ScmObj
scm_capi_min_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("max: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_num_bop_fold(lst, scm_api_min);
}

ScmObj
scm_api_plus(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("+: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(x)) {
    scm_capi_error("+: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("+: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_FUNC(x, plus, y);
}

ScmObj
scm_capi_plus_lst(ScmObj lst)
{
  ScmObj a, d;

  SCM_STACK_FRAME_PUSH(&lst,
                       &a, &d);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("+: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_nil_p(lst))
    return SCM_FIXNUM_ZERO;

  a = scm_api_car(lst);
  if (scm_obj_null_p(a)) return SCM_OBJ_NULL;

  d = scm_api_cdr(lst);
  if (scm_obj_null_p(d)) return SCM_OBJ_NULL;

  if (scm_capi_nil_p(d))
    return a;

  return scm_capi_num_bop_fold(lst, scm_api_plus);
}

ScmObj
scm_api_mul(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("*: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(x)) {
    scm_capi_error("*: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("*: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_FUNC(x, mul, y);
}

ScmObj
scm_capi_mul_lst(ScmObj lst)
{
  ScmObj a, d;

  SCM_STACK_FRAME_PUSH(&lst,
                       &a, &d);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("*: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_nil_p(lst))
    return SCM_FIXNUM_ONE;

  a = scm_api_car(lst);
  if (scm_obj_null_p(a)) return SCM_OBJ_NULL;

  d = scm_api_cdr(lst);
  if (scm_obj_null_p(d)) return SCM_OBJ_NULL;

  if (scm_capi_nil_p(d))
    return a;

  return scm_capi_num_bop_fold(lst, scm_api_mul);
}

ScmObj
scm_api_minus(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("-: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(x)) {
    scm_capi_error("-: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(y)) {
    scm_capi_error("-: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_FUNC(x, minus, y);
}

ScmObj
scm_capi_minus_lst(ScmObj lst)
{
  ScmObj a, d;

  SCM_STACK_FRAME_PUSH(&lst,
                       &a, &d);

  if (scm_obj_null_p(lst) || scm_capi_nil_p(lst)) {
    scm_capi_error("-: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  a = scm_api_car(lst);
  if (scm_obj_null_p(a)) return SCM_OBJ_NULL;

  d = scm_api_cdr(lst);
  if (scm_obj_null_p(d)) return SCM_OBJ_NULL;

  if (scm_capi_nil_p(d))
    return SCM_NUM_CALL_FUNC(a, invert_sign);

  return scm_capi_num_bop_fold(lst, scm_api_minus);
}

ScmObj
scm_api_abs(ScmObj num)
{
  if (scm_obj_null_p(num)) {
    scm_capi_error("abs: invalid argumnet", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("abs: number required, bug got", 1, num);
    return SCM_OBJ_NULL;
  }

  if (SCM_NUM_CALL_FUNC(num, positive_p))
    return SCM_NUM_CALL_FUNC(num, copy);
  else
    return SCM_NUM_CALL_FUNC(num, invert_sign);
}

int
scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("floor/: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_FUNC(x, floor_div, y, q, r);
}

ScmObj
scm_api_floor_quo(ScmObj x, ScmObj y)
{
  ScmObj q = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &q);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("floor-quotient: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor-quotient: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_FUNC(x, floor_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_api_floor_rem(ScmObj x, ScmObj y)
{
  ScmObj r = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &r);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("floor-remainder: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("floor-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_FUNC(x, floor_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}

int
scm_capi_truncate_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("truncate/: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("truncate/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("truncate/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_FUNC(x, truncate_div, y, q, r);
}

ScmObj
scm_api_truncate_quo(ScmObj x, ScmObj y)
{
  ScmObj q = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &q);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("truncate-quotient: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(x)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("truncate-quotient: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_FUNC(x, truncate_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_api_truncate_rem(ScmObj x, ScmObj y)
{
  ScmObj r = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &r);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("truncate-remainder: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("truncate-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_FUNC(x, truncate_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}

ScmObj
scm_api_exact(ScmObj num)
{
  /* 仮実装 */

  if (scm_obj_null_p(num)) {
    scm_capi_error("exact: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("exact: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return num;
}

ScmObj
scm_api_inexact(ScmObj num)
{
  /* 仮実装 */

  if (scm_obj_null_p(num)) {
    scm_capi_error("inexact: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_number_p(num)) {
    scm_capi_error("inexact: number required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  return num;
}

int
scm_capi_integer_to_sword(ScmObj num, scm_sword_t *w)
{
  if (scm_obj_null_p(num) || w == NULL) {
    scm_capi_error("can not convert number to scm_sword_t: "
                   "invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_integer_p(num)) {
    scm_capi_error("can not convert number to scm_sword_t: "
                   "integer required, but got", 1, num);
    return -1;
  }

  if (scm_capi_fixnum_p(num)) {
    *w = scm_fixnum_value(num);
  }
  else if (scm_capi_bignum_p(num)) {
    int r = scm_bignum_to_sword(num, w);
    if (r < 0) {
      scm_capi_error("can not convert number to scm_sword_t: overflow", 1, num);
      return -1;
    }
  }

  return 0;
}

int
scm_capi_integer_to_size_t(ScmObj num, size_t *s)
{
  if (scm_obj_null_p(num) || s == NULL) {
    scm_capi_error("can not convert number to size_t: "
                   "invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_integer_p(num)) {
    scm_capi_error("can not convert number to size_t: "
                   "integer required, but got", 1, num);
    return -1;
  }

  if (scm_capi_fixnum_p(num)) {
    scm_sword_t w = scm_fixnum_value(num);
    if (w < 0) {
      scm_capi_error("can not convert number to size_t: overflow", 1, num);
      return -1;
    }
    *s = (size_t)w;
  }
  else if (scm_capi_bignum_p(num)) {
    int r = scm_bignum_to_size_t(num, s);
    if (r < 0) {
      scm_capi_error("can not convert number to size_t: overflow", 1, num);
      return -1;
    }
  }

  return 0;
}


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

extern inline bool
scm_capi_symbol_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

ScmObj
scm_api_symbol_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  return scm_capi_symbol_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_symbol_eq(ScmObj sym1, ScmObj sym2, bool *rslt)
{
  /* int r, cmp; */

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  if (scm_obj_null_p(sym1) || scm_obj_null_p(sym2)) {
    scm_capi_error("symbol=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym1)) {
    scm_capi_error("symbol=?: symbol required, but got", 1, sym1);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym2)) {
    scm_capi_error("symbol=?: symbol required, but got", 1, sym2);
    return -1;
  }

  /* r = scm_symbol_cmp(sym1, sym2, &cmp); */
  /* if (r < 0) return -1; */

  /* if (rslt != NULL) */
  /*   *rslt = (cmp == 0) ? true : false; */

  if (rslt != NULL)
    *rslt = scm_obj_same_instance_p(sym1, sym2);

  return 0;
}

static int
scm_capi_symbol_cmp_fold(ScmObj lst,
                         int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt),
                         bool *rslt)

{
  ScmObj str = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &str, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    str = scm_api_car(l);

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, str, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = str;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

ScmObj
scm_capi_symbol_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("symbol=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_symbol_cmp_fold(lst, scm_capi_symbol_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  if (scm_obj_null_p(sym)) {
    scm_capi_error("symbol->string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if  (!scm_capi_symbol_p(sym)) {
    scm_capi_error("symbol->string: symbol required, but got", 1, sym);
    return SCM_OBJ_NULL;
  }

  return scm_symbol_string(sym);
}

ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string->symbol: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if  (!scm_capi_string_p(str)) {
    scm_capi_error("string->symbol: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  enc = scm_string_encoding(str);
  if (enc != scm_capi_system_encoding())
    str = scm_string_encode(str, scm_capi_system_encoding());

  return scm_symtbl_symbol(scm_vm_symtbl(scm_vm_current_vm()), str);
}

ScmObj
scm_capi_make_symbol_from_cstr(const char *str, ScmEncoding *enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&s);

  s = scm_capi_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s))
    return SCM_OBJ_NULL;        /* provisional implemntation */

  if (enc != NULL && enc != scm_capi_system_encoding()) {
    s = scm_string_encode(s, scm_capi_system_encoding());
    if (scm_obj_null_p(s))
      return SCM_OBJ_NULL;        /* provisional implemntation */
  }

  return scm_api_string_to_symbol(s);
}

ScmObj
scm_capi_make_symbol_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&s);

  s = scm_capi_make_string_from_bin(data, size, enc);
  if (scm_obj_null_p(s))
    return SCM_OBJ_NULL;        /* provisional implemntation */

  if (enc != NULL && enc != scm_capi_system_encoding()) {
    s = scm_string_encode(s, scm_capi_system_encoding());
    if (scm_obj_null_p(s))
      return SCM_OBJ_NULL;        /* provisional implemntation */
  }

  return scm_api_string_to_symbol(s);
}

/* TODO: symbol_bytesize, symbol_to_cstr, symbol_hash_value についてはインタ
 * フェースの見直しが必要
 */

ssize_t
scm_capi_symbol_bytesize(ScmObj sym)
{
  if (scm_obj_null_p(sym)) {
    scm_capi_error("symbol-bytesize: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("symbol-bytesize: symbol required, but got", 1, sym);
    return -1;
  }

  return scm_capi_string_bytesize(scm_api_symbol_to_string(sym));
}

extern inline char *
scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size)
{
  return scm_capi_string_to_cstr(scm_api_symbol_to_string(sym),
                                 cstr, size);
}

size_t
scm_capi_symbol_hash_value(ScmObj sym)
{
  if (!scm_capi_symbol_p(sym))
    return SIZE_MAX;                  /* provisional implementation */

  return scm_symbol_hash_value(sym);
}


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

extern inline bool
scm_capi_char_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_CHAR_TYPE_INFO) ? true : false;
}

ScmObj
scm_api_char_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  return scm_capi_char_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_capi_make_char(const scm_char_t *chr, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  if (!scm_enc_valid_char_p(enc, chr)) {
    scm_capi_error("can not make character object: invalid sequence", 0);
    return SCM_OBJ_NULL;          /* provisional implemntation */
  }

  return scm_char_new(SCM_MEM_ALLOC_HEAP, chr, enc);
}

ScmObj
scm_api_make_char_newline(ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  return scm_char_new_newline(SCM_MEM_ALLOC_HEAP, enc);
}

ScmObj
scm_api_make_char_space(ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  return scm_char_new_space(SCM_MEM_ALLOC_HEAP, enc);
}

int
scm_capi_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt)
{
  int err, cmp;

  if (scm_obj_null_p(chr1) || scm_obj_null_p(chr2)) {
    scm_capi_error("char=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char=?: character required, but got", 1, chr2);
    return -1;
  }
  else if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_capi_error("char=?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

static int
scm_capi_char_cmp_fold(ScmObj lst,
                       int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt),
                       bool *rslt)

{
  ScmObj str = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &str, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    str = scm_api_car(l);

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, str, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = str;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

ScmObj
scm_capi_char_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("char=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_char_cmp_fold(lst, scm_capi_char_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  if (scm_obj_null_p(chr1) || scm_obj_null_p(chr2)) {
    scm_capi_error("char<?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char<?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char<?: character required, but got", 1, chr2);
    return -1;
  }
  else if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_capi_error("char<?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1) ? true : false;

  return 0;
}

ScmObj
scm_capi_char_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_char_cmp_fold(lst, scm_capi_char_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  if (scm_obj_null_p(chr1) || scm_obj_null_p(chr2)) {
    scm_capi_error("char>?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char>?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char>?: character required, but got", 1, chr2);
    return -1;
  }
  else if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_capi_error("char>?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_char_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("char>?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_char_cmp_fold(lst, scm_capi_char_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  if (scm_obj_null_p(chr1) || scm_obj_null_p(chr2)) {
    scm_capi_error("char<=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char<=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char<=?: character required, but got", 1, chr2);
    return -1;
  }
  else if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_capi_error("char<=?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1 || cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_capi_char_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("char<=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_char_cmp_fold(lst, scm_capi_char_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  if (scm_obj_null_p(chr1) || scm_obj_null_p(chr2)) {
    scm_capi_error("char>=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr1)) {
    scm_capi_error("char>=?: character required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char>=?: character required, but got", 1, chr2);
    return -1;
  }
  else if (scm_char_encoding(chr1) != scm_char_encoding(chr2)) {
    scm_capi_error("char>=?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_char_cmp(chr1, chr2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0 || cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_char_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("char=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_char_cmp_fold(lst, scm_capi_char_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  long long scalar;

  if (scm_obj_null_p(chr)) {
    scm_capi_error("char->integer: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("char->integer: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  scalar = scm_char_scalar(chr);

  /* TODO: エンコーディングが Unicode のものではない場合、scalar に 0x110000
   * を足す
   */

  if (scalar > SCM_SWORD_MAX) {
    scm_capi_error("char->integer: internal error: overflow", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_make_number_from_sword((scm_sword_t)scalar);
}

ScmObj
scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc)
{
  scm_sword_t scalar;
  scm_char_t c;
  ssize_t s;
  int r;

  if (scm_obj_null_p(num)) {
    scm_capi_error("integer->char: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(num)) {
    scm_capi_error("integer->char: integer required, but got", 1, num);
    return SCM_OBJ_NULL;
  }

  if (enc == NULL)
    enc = scm_capi_system_encoding();

  r = scm_capi_integer_to_sword(num, &scalar);
  if (r < 0) return SCM_OBJ_NULL;

  /* TODO: エンコーディングが Unicode のものではない場合、scalar から 0x110000
   * を引く
   */

  s = scm_enc_cnv_from_scalar(enc, scalar, &c);
  /* scalar 値に相当する文字が無い場合、#f を返す。r7rs-draft-9 未定義 */
  if (s < 0) return SCM_FALSE_OBJ;

  return scm_char_new(SCM_MEM_HEAP, &c, enc);
}


/* TODO: char_to_cchar、char_encoding はインタフェースの見直しが必要
 */


ssize_t
scm_capi_char_to_cchar(ScmObj chr, scm_char_t *cp)
{
  scm_char_t c;

  if (!scm_capi_char_p(chr)) {
    scm_capi_error("can not get byte sequence from character object: "
                   "invalid argument", 0);
    return -1;
  }

  c = scm_char_value(chr);

  if (cp != NULL) *cp = c;

  return scm_enc_char_width(scm_char_encoding(chr), c.bytes, sizeof(c));
}

ScmEncoding *
scm_capi_char_encoding(ScmObj chr)
{
  if (scm_obj_null_p(chr)) {
    scm_capi_error("char-encoding: invalid argument", 0);
    return NULL;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("char-encoding: character required, but got", 1, chr);
    return NULL;
  }

  return scm_char_encoding(chr);
}


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

extern inline bool
scm_capi_string_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_STRING_TYPE_INFO) ? true : false;
}

ScmObj
scm_api_string_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  return scm_capi_string_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  if (str == NULL) {
    return scm_string_new(SCM_MEM_ALLOC_HEAP, "", 0, enc);
  }
  else {
    size_t sz = strlen(str);
    if (sz > SSIZE_MAX) {
      scm_capi_error("can not make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_ALLOC_HEAP, str, sz, enc);
  }
}

ScmObj
scm_capi_make_string_from_bin(const void *data, size_t size, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  if (data == NULL) {
    return scm_string_new(SCM_MEM_ALLOC_HEAP, "", 0, enc);
  }
  else {
    if (size > SSIZE_MAX) {
      scm_capi_error("can not make string object: too long", 0);
      return SCM_OBJ_NULL;
    }
    return scm_string_new(SCM_MEM_ALLOC_HEAP, data, size, enc);
  }
}

ScmObj
scm_api_string_lst(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, l = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&str, &chr, &l);

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    chr = scm_api_car(l);
    if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

    if (!scm_capi_char_p(chr)) {
      scm_capi_error("string: required character, but got", 1, chr);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      enc = scm_char_encoding(chr);
      str = scm_string_new(SCM_MEM_HEAP, NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr);

    if (e != enc) {
      scm_capi_error("string: encoding mismatch", 0);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr);

    r = scm_string_push(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  if (scm_obj_null_p(str))
    return scm_string_new(SCM_MEM_HEAP, NULL, 0, scm_capi_system_encoding());
  else
    return str;
}

ScmObj
scm_capi_string_cv(const ScmObj *chr, size_t n)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&str);

  if (chr == NULL || n == 0)
    return scm_string_new(SCM_MEM_HEAP, NULL, 0, scm_capi_system_encoding());

  enc = 0;
  for (size_t i = 0; i < n; i++) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    if (scm_obj_null_p(chr[i])) {
      scm_capi_error("string: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_char_p(chr[i])) {
      scm_capi_error("string: required character, but got", 1, chr[i]);
      return SCM_OBJ_NULL;
    }

    if (i == 0) {
      enc = scm_char_encoding(chr[i]);
      str = scm_string_new(SCM_MEM_HEAP, NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr[i]);

    if (e != enc) {
      scm_capi_error("string: encoding mismatch", 0);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr[i]);

    r = scm_string_push(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

size_t
scm_capi_string(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_STACK_FRAME;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  for (size_t i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  return scm_capi_string_cv(args, n);
}

ssize_t
scm_capi_string_length(ScmObj str)
{
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-length: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-length: string required, but got", 1, str);
    return -1;
  }

  return (ssize_t)scm_string_length(str);
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
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-bytesize: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-bytesize: string required, but got", 1, str);
    return -1;
  }

  return (ssize_t)scm_string_bytesize(str);
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
  scm_char_t c;
  ScmEncoding *enc;
  int r;

  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-ref: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (pos >= scm_string_length(str)) {
    scm_capi_error("string-ref: out of range", 0);
    return SCM_OBJ_NULL;
  }

  enc = scm_string_encoding(str);
  r = scm_string_ref(str, pos, &c);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_char(&c, enc);
}

ScmObj
scm_api_string_ref(ScmObj str, ScmObj pos)
{
  size_t s;
  int r;

  SCM_STACK_FRAME_PUSH(&str, &pos);

  if (scm_obj_null_p(pos)) {
    scm_capi_error("string-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
  }

  r = scm_capi_integer_to_size_t(pos, &s);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_string_ref(str, s);
}

int
scm_capi_string_set_i(ScmObj str, size_t pos, ScmObj chr)
{
  scm_char_t c;

  SCM_STACK_FRAME_PUSH(&str, &chr);

  if (scm_obj_null_p(str) || scm_obj_null_p(chr)) {
    scm_capi_error("string-set!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-set!: string required, but got", 1, str);
    return -1;
  }
  else if (pos >= scm_string_length(str)) {
    scm_capi_error("string-set!: out of range", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("string-set!: character require, but got", 1, chr);
    return -1;
  }
  else if (scm_string_encoding(str) != scm_char_encoding(chr)) {
    scm_capi_error("string-set!: encoding mismatch", 0);
    return -1;
  }

  c = scm_char_value(chr);
  return scm_string_set(str, pos, &c);
}

ScmObj
scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr)
{
  size_t s;
  int r;

  SCM_STACK_FRAME_PUSH(&str, &pos, &chr);

  if (scm_obj_null_p(pos)) {
    scm_capi_error("string-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(pos)) {
    scm_capi_error("string-ref: integer required, but got", 1, pos);
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&s1, &s2);

  if (scm_obj_null_p(s1) || scm_obj_null_p(s2)) {
    scm_capi_error("string=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(s1)) {
    scm_capi_error("string=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string=?: string required, but got", 1, s2);
    return -1;
  }
  else if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_capi_error("string=?: invalid argument: encoding mismatch", 0);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

static int
scm_capi_string_cmp_fold(ScmObj lst,
                         int (*cmp)(ScmObj s1, ScmObj s2, bool *rslt),
                         bool *rslt)

{
  ScmObj str = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, l = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &str, &prv, &l);

  scm_assert(scm_obj_not_null_p(lst));
  scm_assert(rslt != NULL);

  prv = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    str = scm_api_car(l);

    if (scm_obj_not_null_p(prv)) {
      bool cr;
      int r;

      r = cmp(prv, str, &cr);
      if (r < 0) return -1;

      if (!cr) {
        *rslt = false;
        return 0;
      }
    }

    prv = str;
  }

  if (scm_obj_null_p(l)) return -1;

  *rslt = true;

  return 0;
}

ScmObj
scm_capi_string_eq_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_string_cmp_fold(lst, scm_capi_string_eq, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&s1, &s2);

  if (scm_obj_null_p(s1) || scm_obj_null_p(s2)) {
    scm_capi_error("string<?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(s1)) {
    scm_capi_error("string<?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string<?: string required, but got", 1, s2);
    return -1;
  }
  else if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_capi_error("string<?: encoding mismatch", 0);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1) ? true : false;

  return 0;
}

ScmObj
scm_capi_string_lt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_string_cmp_fold(lst, scm_capi_string_lt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&s1, &s2);

  if (scm_obj_null_p(s1) || scm_obj_null_p(s2)) {
    scm_capi_error("string>?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(s1)) {
    scm_capi_error("string>?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string>?: string required, but got", 1, s2);
    return -1;
  }
  else if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_capi_error("string>?: encoding mismatch", 0);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_string_gt_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_string_cmp_fold(lst, scm_capi_string_gt, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&s1, &s2);

  if (scm_obj_null_p(s1) || scm_obj_null_p(s2)) {
    scm_capi_error("string<=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(s1)) {
    scm_capi_error("string<=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string<=?: string required, but got", 1, s2);
    return -1;
  }
  else if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_capi_error("string<=?: encoding mismatch", 0);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == -1 || cmp == 0) ? true : false;

  return 0;
}

ScmObj
scm_capi_string_le_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string<=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_string_cmp_fold(lst, scm_capi_string_le, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  int err, cmp;

  SCM_STACK_FRAME_PUSH(&s1, &s2);

  if (scm_obj_null_p(s1) || scm_obj_null_p(s2)) {
    scm_capi_error("string>=?: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(s1)) {
    scm_capi_error("string>=?: string required, but got", 1, s1);
    return -1;
  }
  else if (!scm_capi_string_p(s2)) {
    scm_capi_error("string>=?: string required, but got", 1, s2);
    return -1;
  }
  else if (scm_string_encoding(s1) != scm_string_encoding(s2)) {
    scm_capi_error("string>=?: encoding mismatch", 0);
    return -1;
  }

  err = scm_string_cmp(s1, s2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0 || cmp == 1) ? true : false;

  return 0;
}

ScmObj
scm_capi_string_ge_P_lst(ScmObj lst)
{
  bool cmp;
  int r;

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string>=?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_string_cmp_fold(lst, scm_capi_string_ge, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-upcase: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-upcase: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_string_upcase(str);
}

ScmObj
scm_api_string_downcase(ScmObj str)
{
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-downcase: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-downcase: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_string_downcase(str);
}

ScmObj
scm_capi_substring(ScmObj str, size_t start, size_t end)
{
  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("substring: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start > SSIZE_MAX || end > SSIZE_MAX || start > end) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_string_substr(str, start, end - start);
}

ScmObj
scm_api_substring(ScmObj str, ScmObj start, ScmObj end)
{
  size_t ss, se;
  int r;

  SCM_STACK_FRAME_PUSH(&str, &start, &end);

  if (scm_obj_null_p(start) || scm_obj_null_p(end)) {
    scm_capi_error("substring: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(start)) {
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
scm_capi_string_append_lst(ScmObj lst)
{
  ScmObj str = SCM_OBJ_INIT, l = SCM_OBJ_INIT, s = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&lst,
                       &str, &l ,s);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("string-append: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    ScmEncoding *e;
    int r;

    s = scm_api_car(l);
    if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

    if (!scm_capi_string_p(s)) {
      scm_capi_error("string_append: string required, but got", 1, s);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      str = scm_string_dup(s);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

      enc = scm_string_encoding(s);
    }
    else {
      e = scm_string_encoding(s);
      if (enc != e) {
        scm_capi_error("string-append: encoding mismatch", 0);
        return SCM_OBJ_NULL;
      }

      r = scm_string_append(str, s);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(str))
    return scm_capi_make_string_from_bin(NULL, 0, scm_capi_system_encoding());
  else
    return str;
}

ScmObj
scm_capi_string_append_cv(ScmObj *ary, size_t n)
{
  ScmObj str = SCM_OBJ_NULL;
  ScmEncoding *enc, *e;

  SCM_STACK_FRAME_PUSH(&str);

  if (ary == NULL || n == 0)
    return scm_capi_make_string_from_bin(NULL, 0, scm_capi_system_encoding());

  str = scm_capi_string_copy(ary[0], -1, -1);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  enc = scm_string_encoding(str);

  for (size_t i = 1; i < n; i++) {
    int r;

    if (scm_obj_null_p(ary[i])) {
      scm_capi_error("string-append: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_string_p(ary[i])) {
      scm_capi_error("string-append: string required, but got", 1, ary[i]);
      return SCM_OBJ_NULL;
    }

    e = scm_string_encoding(ary[i]);
    if (enc != e) {
      scm_capi_error("string-append: encoding mismatch", 0);
      return SCM_OBJ_NULL;
    }

    r = scm_string_append(str, ary[i]);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return str;
}

ScmObj
scm_capi_string_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_STACK_FRAME;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  for (unsigned int i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  return scm_capi_string_append_cv(args, n);
}

static ScmObj
scm_capi_string_to_list_aux(ScmObj str, size_t pos, size_t len)
{
  ScmObj ary[len];

  SCM_STACK_FRAME_PUSH(&str);

  for (size_t i = 0; i < len; i++) {
    ary[i] = scm_capi_string_ref(str, pos + i);
    SCM_STACK_PUSH(&ary[i]);

    if (scm_obj_null_p(ary[i])) return SCM_OBJ_NULL;
  }

  return scm_capi_list_cv(ary, len);
}

ScmObj
scm_capi_string_to_list(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string->list: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  return scm_capi_string_to_list_aux(str, (size_t)start, (size_t)(end - start));
}

ScmObj
scm_api_string_to_list(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_STACK_FRAME_PUSH(&str, &start, &end);

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
  ScmObj str = SCM_OBJ_INIT, chr = SCM_OBJ_INIT, l = SCM_OBJ_INIT;
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&lst,
                       &str, &chr, &l);

  enc = 0;
  str = SCM_OBJ_NULL;
  for (l = lst; scm_capi_pair_p(l); l = scm_api_cdr(l)) {
    scm_char_t c;
    ScmEncoding *e;
    int r;

    chr = scm_api_car(l);
    if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

    if (!scm_capi_char_p(chr)) {
      scm_capi_error("list->string: character require, but got", 1, chr);
      return SCM_OBJ_NULL;
    }

    if (scm_obj_null_p(str)) {
      enc = scm_char_encoding(chr);
      str = scm_capi_make_string_from_bin(NULL, 0, enc);
      if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
    }

    e = scm_char_encoding(chr);

    if (e != enc) {
      scm_capi_error("list->string: encoding mismatch", 0);
      return SCM_OBJ_NULL;
    }

    c = scm_char_value(chr);

    r = scm_string_push(str, &c);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  if (scm_obj_null_p(str))
    return scm_capi_make_string_from_bin(NULL, 0, scm_capi_system_encoding());
  else
    return str;
}

ScmObj
scm_capi_string_copy(ScmObj str, ssize_t start, ssize_t end)
{
  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-copy: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  return scm_string_substr(str, (size_t)start, (size_t)(end - start));
}

ScmObj
scm_api_string_copy(ScmObj str, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;

  SCM_STACK_FRAME_PUSH(&str, &start, &end);

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

static int
scm_capi_string_copy_i_use_tmp_strage(ScmObj to, size_t at,
                                      size_t pos, size_t len)
{
  scm_char_t chr[len];

  SCM_STACK_FRAME_PUSH(&to);

  for (size_t i = 0; i < len; i++) {
    int r = scm_string_ref(to, pos + i, chr + i);
    if (r < 0) return -1;
  }

  for (size_t i = 0; i < len; i++) {
    int r = scm_string_set(to, at + i, &chr[i]);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_capi_string_copy_i_aux(ScmObj to, size_t at,
                           ScmObj from, size_t pos, size_t len)
{
  scm_char_t chr;

  SCM_STACK_FRAME_PUSH(&to, &from);

  for (size_t i = 0; i < len; i++) {
    int r = scm_string_ref(from, pos + i, &chr);
    if (r < 0) return -1;

    r = scm_string_set(to, at + i, &chr);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_capi_string_copy_i(ScmObj to, size_t at,
                       ScmObj from, ssize_t start, ssize_t end)
{
  size_t to_len, from_len, sub_len, len;

  SCM_STACK_FRAME_PUSH(&to, &from);

  if (scm_obj_null_p(to) || scm_obj_null_p(from)) {
    scm_capi_error("string-copy!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(to)) {
    scm_capi_error("string-copy!: string require, but got", 1, to);
    return -1;
  }
  else if (!scm_capi_string_p(from)) {
    scm_capi_error("string-copy!: string require, but got", 1, from);
    return -1;
  }
  else if (scm_string_encoding(to) != scm_string_encoding(from)) {
    scm_capi_error("string-copy!: encoding mismatch", 0);
    return -1;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-copy!: invalid argument", 0);
    return -1;
  }

  to_len = scm_string_length(to);
  from_len = scm_string_length(from);

  if (at >= to_len) {
    scm_capi_error("string-copy!: out of range", 0);
    return -1;
  }

  sub_len = to_len - at;

  if (start < 0)
    start = 0;
  else if ((size_t)start >= from_len) {
    scm_capi_error("string-copy!: out of range", 0);
    return -1;
  }

  if (end > 0) {
    if ((size_t)end > from_len) {
      scm_capi_error("string-copy!: out of range", 0);
      return -1;
    }
    len = (size_t)(end - start);
    if (len > sub_len) {
      scm_capi_error("string-copy!: invalid argument", 0);
      return -1;
    }
  }
  else {
    if (sub_len < from_len - (size_t)start)
      len = sub_len;
    else
      len = from_len - (size_t)start;
  }

  if (scm_capi_eq_p(to, from) && (size_t)start < at)
    return scm_capi_string_copy_i_use_tmp_strage(to, at, (size_t)start, len);
  else
    return scm_capi_string_copy_i_aux(to, at, from, (size_t)start, len);
}

ScmObj
scm_api_string_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t sa;
  ssize_t sss, sse;
  int r;

  SCM_STACK_FRAME_PUSH(&to, &at, &from, &start, &end);

  if (scm_obj_null_p(at)) {
    scm_capi_error("string->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(at)) {
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
  scm_char_t c;
  size_t len;

  if (scm_obj_null_p(str) || scm_obj_null_p(fill)) {
    scm_capi_error("string-fill!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-fill!: string required, but got", 1, str);
    return -1;
  }
  else if (!scm_capi_char_p(fill)) {
    scm_capi_error("string-fill!: character required, but got", 1, fill);
    return -1;
  }
  else if (scm_string_encoding(str) != scm_char_encoding(fill)) {
    scm_capi_error("string-fill!: encoding mismatch", 0);
    return -1;
  }
  else if (start > 0 && end > 0 && start > end) {
    scm_capi_error("string-fill!: invalid argument", 0);
    return -1;
  }

  len = scm_string_length(str);

  if (start < 0)
    start = 0;
  else if ((size_t)start >= len) {
    scm_capi_error("string-fill!: out of range", 0);
    return -1;
  }

  if (end < 0)
    end = (ssize_t)len;
  else if ((size_t)end > len) {
    scm_capi_error("string-fill!: out of range", 0);
    return -1;
  }

  c = scm_char_value(fill);

  for (ssize_t i = start; i < end; i++) {
    int r = scm_string_set(str, (size_t)i, &c);
    if (r < 0) return -1;
  }

  return 0;
}

ScmObj
scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t sss, sse;
  int r;

  SCM_STACK_FRAME_PUSH(&str, &start, &end);

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
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-encoding: invalid argument", 0);
    return NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-encoding: string required, but got", 1, str);
    return NULL;
  }

  return scm_string_encoding(str);
}

char *
scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  size_t n;

  if (!scm_capi_string_p(str)) {
    scm_capi_error("faild to get byte sequence from string: "
                   "invalid argument", 0);
    return NULL;
  }

  n = scm_string_bytesize(str);
  if (n >= SIZE_MAX) {
    scm_capi_error("faild to get byte sequence from string: "
                   "too long", 0);
    return NULL;
  }

  if (cstr == NULL) {
    size = n + 1;
    cstr = scm_capi_malloc(size);
    if (cstr == NULL) return SCM_OBJ_NULL;
  }
  else if (size == 0) {
    return cstr;
  }
  else if (size - 1 < n) {
    n = size - 1;
  }

  memcpy(cstr, scm_string_content(str), n);
  cstr[n] = '\0';

  return cstr;
}

int
scm_capi_string_push(ScmObj str, scm_char_t chr, ScmEncoding *enc)
{
  ScmEncoding *s_enc;

  if (!scm_capi_string_p(str)) {
    scm_capi_error("can not push character into string: invalid argument", 0);
    return -1;
  }

  s_enc = scm_string_encoding(str);

  if (s_enc != enc) {
    scm_capi_error("can not push character into string: encoding mismatch", 0);
    return -1;
  }

  return scm_string_push(str, &chr);
}

ScmObj
scm_api_string_push(ScmObj str, ScmObj c)
{
  ScmEncoding *s_enc, *c_enc;
  scm_char_t cv;
  int rslt;

  if (scm_obj_null_p(str) || scm_obj_null_p(c)) {
    scm_capi_error("string-push: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-push: string required, but got", 1, str);
    return SCM_OBJ_NULL;                  /* provisional implementation */
  }
  else if (!scm_capi_char_p(c)) {
    scm_capi_error("string-push: character required, but got", 1, c);
    return SCM_OBJ_NULL;                  /* provisional implementation */
  }

  s_enc = scm_string_encoding(str);
  c_enc = scm_char_encoding(c);

  if (s_enc != c_enc) {
    scm_capi_error("string-push: encoding mismatch", 0);
    return SCM_OBJ_NULL;
  }

  cv = scm_char_value(c);

  rslt = scm_string_push(str, &cv);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

extern inline bool
scm_capi_vector_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_VECTOR_TYPE_INFO) ? true : false;
}

ScmObj
scm_api_vector_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  return scm_capi_vector_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_capi_make_vector(size_t len, ScmObj fill)
{
  if (len > SSIZE_MAX) {
    scm_capi_error("make-vector: too long", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(fill))
    return scm_vector_new(SCM_MEM_ALLOC_HEAP, len, SCM_UNDEF_OBJ);
  else
    return scm_vector_new(SCM_MEM_ALLOC_HEAP, len, fill);
}

ScmObj
scm_api_make_vector(ScmObj len, ScmObj fill)
{
  size_t sz;
  int r;

  if (scm_obj_null_p(len)) {
    scm_capi_error("make-vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_capi_integer_p(len)) {
    scm_capi_error("make-vector: integer required, but got", 1, len);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(len, &sz);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_vector(sz, fill);
}

ScmObj
scm_capi_vector_lst(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("vector: invalid argumnet", 0);
    return SCM_OBJ_NULL;
  }

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

  return scm_vector_new_from_ary(SCM_MEM_HEAP, elm, n);
}

ScmObj
scm_capi_vector(size_t n, ...)
{
  ScmObj vec, args[n];
  va_list ap;

  SCM_STACK_FRAME_PUSH(&vec);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    args[i] = va_arg(ap, ScmObj);
    SCM_STACK_PUSH(args + i);
  }
  va_end(ap);

  return scm_capi_vector_cv(args, n);
}

ssize_t
scm_capi_vector_length(ScmObj vec)
{
  SCM_STACK_FRAME_PUSH(&vec);

  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector-length: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-length: vector required, but got", 1, vec);
    return -1;
  }

  return (ssize_t)scm_vector_length(vec);
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
  SCM_STACK_FRAME_PUSH(&vec);

  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-ref: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (idx >= scm_vector_length(vec)) {
    scm_capi_error("vector-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_ref(vec, idx);
}

ScmObj
scm_api_vector_ref(ScmObj vec, ScmObj idx)
{
  size_t i;
  int r;

  SCM_STACK_FRAME_PUSH(&vec, &idx);

  if (scm_obj_null_p(idx)) {
    scm_capi_error("vector-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(idx)) {
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
  SCM_STACK_FRAME_PUSH(&vec, &obj);

  if (scm_obj_null_p(vec) || scm_obj_null_p(obj)) {
    scm_capi_error("vector-set!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-set!: vector required, but got", 1, vec);
    return -1;
  }
  else if (idx >= scm_vector_length(vec)) {
    scm_capi_error("vector-set!: argument out of range", 0);
    return -1;
  }

  return scm_vector_set(vec, idx, obj);
}

ScmObj
scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj)
{
  size_t i;
  int r;

  SCM_STACK_FRAME_PUSH(&vec, &idx, &obj);

  if (scm_obj_null_p(idx)) {
    scm_capi_error("vector-set!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(idx)) {
    scm_capi_error("vector-set!: integer require, but got", 1, idx);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_integer_to_size_t(idx, &i);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_set_i(vec, i, obj);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

static int
scm_capi_vector_norm_star_end(const char *op, ScmObj vec,
                              ssize_t *start, ssize_t *end)
{
  char msg[256];
  size_t len;

  SCM_STACK_FRAME_PUSH(&vec);

  if (*start >= 0 && *end >= 0 && *start > *end) {
    snprintf(msg, sizeof(msg), "%s: invalid argument", op);
    scm_capi_error(msg, 0);
    return -1;
  }

  len = scm_vector_length(vec);
  scm_assert(len <= SSIZE_MAX);

  if (*start >= 0 && (size_t)*start >= len) {
    snprintf(msg, sizeof(msg), "%s: out of range", op);
    scm_capi_error(msg, 0);
    return -1;
  }

  if (*end >= 0 && (size_t)*end > len) {
    snprintf(msg, sizeof(msg), "%s: out of range", op);
    scm_capi_error(msg, 0);
    return -1;
  }

  if (*start < 0) *start = 0;
  if (*end < 0) *end = (ssize_t)len;

  return 0;
}


static ScmObj
scm_capi_vector_to_list_aux(ScmObj vec, size_t start, size_t n)
{
  ScmObj elm[n];

  SCM_STACK_FRAME_PUSH(&vec);

  for (size_t i = 0; i < n; i++) {
    elm[i] = SCM_OBJ_NULL;
    SCM_STACK_PUSH(&elm[i]);

    elm[i] = scm_vector_ref(vec, start + i);
    if (scm_obj_null_p(elm[i])) return SCM_OBJ_NULL;
  }

  return scm_capi_list_cv(elm, n);
}

ScmObj
scm_capi_vector_to_list(ScmObj vec, ssize_t start, ssize_t end)
{
  int r;

  SCM_STACK_FRAME_PUSH(&vec);

  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector->list: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector->list: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_vector_norm_star_end("vector->list", vec, &start, &end);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_to_list_aux(vec, (size_t)start, (size_t)(end - start));
}

int
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
  ssize_t n;

  SCM_STACK_FRAME_PUSH(&lst);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("list->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_capi_nil_p(lst) || !scm_capi_pair_p(lst)) {
    return scm_vector_new(SCM_MEM_HEAP, 0, SCM_OBJ_NULL);
  }

  n = scm_capi_length(lst);
  if (n < 0) return SCM_OBJ_NULL;;

  return scm_vector_new_from_list(SCM_MEM_HEAP, (size_t)n, lst);
}

static ScmObj
scm_capi_vector_to_string_aux(ScmObj vec, size_t start, size_t n)
{
  ScmObj elm[n];

  SCM_STACK_FRAME_PUSH(&vec);

  for (size_t i = 0; i < n; i++) {
    elm[i] = SCM_OBJ_NULL;
    SCM_STACK_PUSH(&elm[i]);

    elm[i] = scm_vector_ref(vec, start + i);
    if (scm_obj_null_p(elm[i])) return SCM_OBJ_NULL;
  }

  return scm_capi_string_cv(elm, n);
}

ScmObj
scm_capi_vector_to_string(ScmObj vec, ssize_t start, ssize_t end)
{
  int r;

  SCM_STACK_FRAME_PUSH(&vec);

  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector->string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector->string: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_vector_norm_star_end("vector->string", vec, &start, &end);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_vector_to_string_aux(vec,
                                       (size_t)start, (size_t)(end - start));
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
scm_capi_string_to_vector_aux(ScmObj str, size_t start, size_t n)
{
  ScmObj elm[n];

  SCM_STACK_FRAME_PUSH(&str);

  for (size_t i = 0; i < n; i++) {
    elm[i] = SCM_OBJ_NULL;
    SCM_STACK_PUSH(&elm[i]);

    elm[i] = scm_capi_string_ref(str, start + i);
    if (scm_obj_null_p(elm[i])) return SCM_OBJ_NULL;
  }

  return scm_capi_vector_cv(elm, n);
}

ScmObj
scm_capi_string_to_vector(ScmObj str, ssize_t start, ssize_t end)
{
  size_t len;

  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string->vector: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("string->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  len = scm_string_length(str);
  scm_assert(len <= SSIZE_MAX);

  if (start >= 0 && (size_t)start >= len) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (end >= 0 && (size_t)end > len) {
    scm_capi_error("string->vector: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (start < 0) start = 0;
  if (end < 0) end = (ssize_t)len;

  return scm_capi_string_to_vector_aux(str,
                                       (size_t)start, (size_t)(end - start));
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
  ScmObj copy = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t n;
  int r;

  SCM_STACK_FRAME_PUSH(&vec,
                       &copy);

  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector-copy: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-copy: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  r = scm_capi_vector_norm_star_end("vector-copy", vec, &start, &end);
  if (r < 0) return SCM_OBJ_NULL;

  n = (size_t)(end - start);

  copy = scm_vector_new(SCM_MEM_HEAP, n, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(copy)) return SCM_OBJ_NULL;

  for (size_t i = 0; i < n; i++) {
    elm = scm_vector_ref(vec, (size_t)start + i);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    r = scm_vector_set(copy, i, elm);
    if (r < 0) return SCM_OBJ_NULL;
  }

  return copy;
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

static int
scm_capi_vector_copy_i_aux(ScmObj to, size_t at,
                           ScmObj from, size_t pos, size_t len)
{
  ScmObj elm;
  int r;

  SCM_STACK_FRAME_PUSH(&to, &from,
                       &elm);

  for (size_t i = 0; i < len; i++) {
    elm = scm_vector_ref(from, pos + i);
    r = scm_vector_set(to, at + i, elm);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_capi_vector_copy_i_aux_in_reverse(ScmObj to, size_t at,
                                      ScmObj from, size_t pos, size_t len)
{
  ScmObj elm;
  int r;

  SCM_STACK_FRAME_PUSH(&to, &from,
                       &elm);

  for (size_t i = len; i > 0; i--) {
    elm = scm_vector_ref(from, pos + i - 1);
    r = scm_vector_set(to, at + i - 1, elm);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_capi_vector_copy_i(ScmObj to, size_t at,
                       ScmObj from, ssize_t start, ssize_t end)
{
  size_t len, to_len, from_len;

  if (scm_obj_null_p(to) || scm_obj_null_p(from)) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_vector_p(to)) {
    scm_capi_error("vector-copy!: vectore required, but got", 1, to);
    return -1;
  }
  else if (!scm_capi_vector_p(from)) {
    scm_capi_error("vector-copy!: vectore required, but got", 1, from);
    return -1;
  }
  else if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return -1;
  }

  to_len = scm_vector_length(to);
  from_len = scm_vector_length(from);

  if (at >= to_len) {
    scm_capi_error("vector-copy!: out of range", 0);
    return -1;
  }

  if (start < 0) {
    start = 0;
  }
  else if ((size_t)start >= from_len) {
    scm_capi_error("vector-copy!: out of range", 0);
    return -1;
  }

  if (end > 0) {
    if ((size_t)end > from_len) {
      scm_capi_error("vector-copy!: out of range", 0);
      return -1;
    }
    len = (size_t)(end - start);
    if (len > to_len - at) {
      scm_capi_error("vector-copy!: out of range", 0);
      return -1;
    }
  }
  else {
    if (to_len - at < from_len - (size_t)start)
      len = to_len - at;
    else
      len = from_len - (size_t)start;
  }

  if (scm_capi_eq_p(to, from) && (size_t)start < at)
    return scm_capi_vector_copy_i_aux_in_reverse(to, at,
                                                 from, (size_t)start, len);
  else
    return scm_capi_vector_copy_i_aux(to, at, from, (size_t)start, len);
}

ScmObj
scm_api_vector_copy_i(ScmObj to, ScmObj at,
                      ScmObj from, ScmObj start, ScmObj end)
{
  size_t a;
  ssize_t s, e;
  int r;

  SCM_STACK_FRAME_PUSH(&to, &at, &from, &start, &end);

  if (scm_obj_null_p(at)) {
    scm_capi_error("vector-copy!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(at)) {
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
scm_capi_vector_append_lst(ScmObj lst)
{
  ScmObj acc = SCM_OBJ_INIT, vec = SCM_OBJ_INIT;
  ScmObj elm = SCM_OBJ_INIT, ls = SCM_OBJ_INIT;
  size_t len, sum, idx;
  int r;

  SCM_STACK_FRAME_PUSH(&lst,
                       &acc, &vec,
                       &elm, &ls);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("vector-append: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  sum = 0;
  for (ls = lst; scm_capi_pair_p(ls); ls = scm_api_cdr(ls)) {
    vec = scm_api_car(ls);
    if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

    if (!scm_capi_vector_p(vec)) {
      scm_capi_error("vector-append: vector required, but got", 1, vec);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(vec);
    if (SSIZE_MAX - sum < len) {
      scm_capi_error("vector-append: too long", 0);
      return SCM_OBJ_NULL;
    }

    sum += len;
  }

  if (scm_obj_null_p(ls)) return SCM_OBJ_NULL;

  acc = scm_vector_new(SCM_MEM_HEAP, sum, SCM_OBJ_NULL);
  if (scm_obj_null_p(acc)) return SCM_OBJ_NULL;

  idx = 0;
  for (ls = lst; scm_capi_pair_p(ls); ls = scm_api_cdr(ls)) {
    vec = scm_api_car(ls);
    if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

    len = scm_vector_length(vec);
    for (size_t i = 0; i < len; i++) {
      elm = scm_vector_ref(vec, i);
      if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

      r = scm_vector_set(acc, idx++, elm);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(ls)) return SCM_OBJ_NULL;

  return acc;
}

ScmObj
scm_capi_vector_append_cv(ScmObj *ary, size_t n)
{
  ScmObj vec = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t len, sum, idx;
  int r;

  SCM_STACK_FRAME_PUSH(&vec, &elm);

  if (ary == NULL || n == 0)
    return scm_vector_new(SCM_MEM_HEAP, 0, SCM_OBJ_NULL);

  sum = 0;
  for (size_t i = 0; i < n; i++) {
    if (scm_obj_null_p(ary[i])) {
      scm_capi_error("vector-append: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
    else if (!scm_capi_vector_p(ary[i])) {
      scm_capi_error("vector-append: vector required, but got", 1, ary[i]);
      return SCM_OBJ_NULL;
    }

    len = scm_vector_length(ary[i]);
    if (SSIZE_MAX - sum < len) {
      scm_capi_error("vector-append: too long", 0);
      return SCM_OBJ_NULL;
    }

    sum += len;
  }

  vec = scm_vector_new(SCM_MEM_HEAP, sum, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  idx = 0;
  for (size_t i = 0; i < n; i++) {
    len = scm_vector_length(ary[i]);
    for (size_t j = 0; j < len; j++) {
      elm = scm_vector_ref(ary[i], j);
      if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

      r = scm_vector_set(vec, idx++, elm);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  return vec;
}

ScmObj
scm_capi_vector_append(size_t n, ...)
{
  ScmObj ary[n];
  va_list ap;

  SCM_STACK_FRAME;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    ary[i] = va_arg(ap, ScmObj);
    SCM_STACK_PUSH(&ary[i]);
  }
  va_end(ap);

  return scm_capi_vector_append_cv(ary, n);
}

int
scm_capi_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end)
{
  int r;

  SCM_STACK_FRAME_PUSH(&vec, &fill);

  if (scm_obj_null_p(vec) || scm_obj_null_p(fill)) {
    scm_capi_error("vector-fill!: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_vector_p(vec)) {
    scm_capi_error("vector-fill!: vectore required, but got", 1, vec);
    return -1;
  }

  r = scm_capi_vector_norm_star_end("vector-fill!", vec, &start, &end);
  if (r < 0) return -1;

  for (ssize_t i = start; i < end; i++) {
    r = scm_vector_set(vec, (size_t)i, fill);
    if (r < 0) return -1;
  }

  return 0;
}

ScmObj
scm_api_vector_fill_i(ScmObj vec, ScmObj fill, ScmObj start, ScmObj end)
{
  ssize_t s, e;
  int r;

  SCM_STACK_FRAME_PUSH(&vec, &fill);

  r = scm_capi_vector_cnv_start_end("vector-fill!", start, &s);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_cnv_start_end("vector-fill!", end, &e);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_capi_vector_fill_i(vec, fill, s, e);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Ports                                                          */
/*******************************************************************/

extern inline bool
scm_capi_port_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_PORT_TYPE_INFO);
}

ScmObj
scm_api_port_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("port?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_input_port_p(ScmObj obj)
{
  return (scm_capi_port_p(obj) && scm_port_input_port_p(obj));
}

ScmObj
scm_api_input_port_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("input-port?: invaid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_input_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_output_port_p(ScmObj obj)
{
  return (scm_capi_port_p(obj) && scm_port_output_port_p(obj));
}

ScmObj
scm_api_output_port_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("output-port?: invaid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_output_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_textual_port_p(ScmObj obj)
{
  return (scm_capi_port_p(obj) && scm_port_textual_port_p(obj));
}

ScmObj
scm_api_textual_port_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("textual-port?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_textual_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_capi_binary_port_p(ScmObj obj)
{
  return (scm_capi_port_p(obj) && scm_port_binary_port_p(obj));
}

ScmObj
scm_api_binary_port_P(ScmObj obj)
{
  if (scm_obj_null_p(obj)) {
    scm_capi_error("binary-port?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_binary_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_input_port_open_p(ScmObj port, bool *rslt)
{
  SCM_STACK_FRAME_PUSH(&port);

  if (scm_obj_null_p(port)) {
    scm_capi_error("input-port-open?: invalid argument", 0);
    return -1;
  }
  else if (scm_capi_input_port_p(port)) {
    scm_capi_error("input-port-open?: input-port required, but got", 1, port);
    return -1;
  }

  if (rslt != NULL)
    *rslt = scm_port_closed_p(port);

  return 0;
}

ScmObj
scm_api_input_port_open_P(ScmObj port)
{
  bool o;
  int r;

  r = scm_capi_input_port_open_p(port, &o);
  if (r < 0) return SCM_OBJ_NULL;

  return o ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_capi_output_port_open_p(ScmObj port, bool *rslt)
{
  SCM_STACK_FRAME_PUSH(&port);

  if (scm_obj_null_p(port)) {
    scm_capi_error("output-port-open?: invalid argument", 0);
    return -1;
  }
  else if (scm_capi_output_port_p(port)) {
    scm_capi_error("output-port-open?: output-port required, but got", 1, port);
    return -1;
  }

  if (rslt != NULL)
    *rslt = scm_port_closed_p(port);

  return 0;
}

ScmObj
scm_api_output_port_open_P(ScmObj port)
{
  bool o;
  int r;

  r = scm_capi_output_port_open_p(port, &o);
  if (r < 0) return SCM_OBJ_NULL;

  return o ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_capi_open_input_fd(int fd, const char *enc)
{
  char ext_enc_name[64];

  if (fd < 0) {
    scm_capi_error("open-input-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_capi_error("open-input-fd: faild to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_fd(fd, "r", SCM_PORT_BUF_DEFAULT,
                          scm_capi_system_encoding(), enc);
}

ScmObj
scm_capi_open_output_fd(int fd, const char *enc)
{
  char ext_enc_name[64];

  if (fd < 0) {
    scm_capi_error("open-output-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_capi_error("open-output-fd: faild to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_fd(fd, "w", SCM_PORT_BUF_DEFAULT,
                          scm_capi_system_encoding(), enc);
}

ScmObj
scm_capi_open_input_file(const char *path, const char *enc)
{
  char ext_enc_name[64];

  if (path == NULL) {
    scm_capi_error("open-input-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_capi_error("open-input-file: faild to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_file(path, "r", SCM_PORT_BUF_DEFAULT,
                            0, scm_capi_system_encoding(), enc);
}

ScmObj
scm_api_open_input_file(ScmObj path)
{
  char ext_enc_name[64];
  char path_str[PATH_MAX], *p;
  size_t s;
  ssize_t r;

  if (scm_obj_null_p(path)) {
    scm_capi_error("open-input-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(path)) {
    scm_capi_error("open-input-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
  if (r < 0) {
    scm_capi_error("open-input-file: faild to get external encoding name", 0);
    return SCM_OBJ_NULL;
  }

  /* TODO: `path' を外部エンーディングへ変換する */

  s = scm_string_bytesize(path);
  if (s >= PATH_MAX) {
    scm_capi_error("open-input-file: too long pathname", 0);
    return SCM_OBJ_NULL;
  }

  p = scm_capi_string_to_cstr(path, path_str, sizeof(path_str));
  if (p == NULL) return SCM_OBJ_NULL;

  return scm_capi_open_input_file(path_str, ext_enc_name);
}

ScmObj
scm_capi_open_output_file(const char *path, const char *enc)
{
  char ext_enc_name[64];

  if (path == NULL) {
    scm_capi_error("open-output-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_capi_error("open-output-file: "
                     "faild to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_file(path, "w", SCM_PORT_BUF_DEFAULT,
                            0644, scm_capi_system_encoding(), enc);
}

ScmObj
scm_api_open_output_file(ScmObj path)
{
  char ext_enc_name[64];
  char path_str[PATH_MAX], *p;
  size_t s;
  ssize_t r;

  if (scm_obj_null_p(path)) {
    scm_capi_error("open-output-file: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(path)) {
    scm_capi_error("open-output-file: string required, but got", 1, path);
    return SCM_OBJ_NULL;
  }

  r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
  if (r < 0) {
    scm_capi_error("open-output-file: faild to get external encoding name", 0);
    return SCM_OBJ_NULL;
  }

  /* TODO: `path' を外部エンーディングへ変換する */

  s = scm_string_bytesize(path);
  if (s >= PATH_MAX) {
    scm_capi_error("open-output-file: too long pathname", 0);
    return SCM_OBJ_NULL;
  }

  p = scm_capi_string_to_cstr(path, path_str, sizeof(path_str));
  if (p == NULL) return SCM_OBJ_NULL;

  return scm_capi_open_output_file(path_str, ext_enc_name);
}

ScmObj
scm_api_close_port(ScmObj port)
{
  int r;

  if (scm_obj_null_p(port)) {
    scm_capi_error("close-port: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_port_p(port)) {
    scm_capi_error("close-port: port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_port_close(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_input_port(ScmObj port)
{
  int r;

  if (scm_obj_null_p(port)) {
    scm_capi_error("close-input-port: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("close-input-port: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;;
  }

  r = scm_port_close(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_close_output_port(ScmObj port)
{
  int r;

  if (scm_obj_null_p(port)) {
    scm_capi_error("close-output-port: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("close-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  r = scm_port_close(port);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_capi_open_input_string_cstr(const char *str, ScmEncoding *enc)
{
  if (enc == NULL)
    enc = scm_capi_system_encoding();

  return scm_port_open_string(str, (str == NULL)? 0 : strlen(str),
                              "r",
                              scm_capi_system_encoding(), scm_enc_name(enc));
}

ScmObj
scm_api_open_input_string(ScmObj str)
{
  if (scm_obj_null_p(str)) {
    scm_capi_error("open-input-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("open-input-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  return scm_port_open_string(scm_string_content(str),
                              scm_string_bytesize(str),
                              "r",
                              scm_capi_system_encoding(),
                              scm_enc_name(scm_string_encoding(str)));
}

ScmObj
scm_api_open_output_string(void)
{
  return scm_port_open_string(NULL, 0, "w", scm_capi_system_encoding(), NULL);
}

ScmObj
scm_api_get_output_string(ScmObj port)
{
  const void *p;
  ssize_t s;
  const char *enc_name;
  ScmEncoding *e;

  if (scm_obj_null_p(port)) {
    scm_capi_error("get-output-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("get-output-string: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_port_string_port_p(port)) {
    scm_capi_error("get-output-string: string-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_port_textual_port_p(port)) {
    scm_capi_error("get-output-string: "
                   "textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  p = scm_port_string_buffer(port);
  if (p == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  s = scm_port_string_buffer_length(port);
  if (s < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  enc_name = scm_port_external_enc(port);
  e = scm_enc_find_enc(enc_name);
  if (e == NULL) {
    scm_capi_error("get-output-string: unsupported encoding", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_make_string_from_bin(p, (size_t)s, e);
}

const char *
scm_capi_port_encoding(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("port-encoding: invalid argument", 0);
    return NULL;
  }
  else if (!scm_obj_type_p(port, &SCM_PORT_TYPE_INFO)) {
    scm_capi_error("port-encoding: port required, but got", 1, port);
    return NULL;                  /* provisional implemntation */
  }

  return scm_port_external_enc(port);
}

ScmEncoding *
scm_capi_port_internal_encoding(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("port-internal-encoding: invalid argument", 0);
    return NULL;
  }
  else if (!scm_capi_port_p(port)) {
    scm_capi_error("port-internal-encoding: port required, but got", 1, port);
    return NULL;
  }

  return scm_port_internal_enc(port);
}

extern inline ScmObj
scm_api_standard_input_port(void)
{
  return scm_vm_standard_input_port(scm_vm_current_vm());
}

extern inline ScmObj
scm_api_standard_output_port(void)
{
  return scm_vm_standard_output_port(scm_vm_current_vm());
}

extern inline ScmObj
scm_api_standard_error_port(void)
{
  return scm_vm_standard_error_port(scm_vm_current_vm());
}


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj
scm_api_read(ScmObj port)
{
  ScmLexer *lexer;
  ScmParser *parser;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする)
   * TODO: パーサーエラーの場合 read-error? が #t を返すオブジェクトを raise
   *       する
   */

  if (scm_obj_null_p(port)) {
    scm_capi_error("read: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read: input-port requried, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("read: textual-port requried, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("read: input-port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  lexer = scm_lexer_new();
  if (lexer == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  parser = scm_parser_new(lexer);
  if (parser == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_parser_parse_expression(parser, port);
}

ssize_t
scm_capi_read_cchar(scm_char_t *chr, ScmObj port)
{
  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  if (scm_obj_null_p(port)) {
    scm_capi_error("read-char: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read-char: input-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("read-char: textual-port required, but got", 1, port);
    return -1;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("read-char: input-port is closed", 1, port);
    return -1;
  }
  else if (chr == NULL) {
    scm_capi_error("read-char: invalid argument", 0);
    return -1;
  }

  return scm_port_read_char(port, chr);
}

ScmObj
scm_api_read_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  SCM_STACK_FRAME_PUSH(&port);

  s = scm_capi_read_cchar(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return scm_api_eof();
  else
    return scm_capi_make_char(&chr, scm_port_internal_enc(port));
}

ssize_t
scm_capi_peek_cchar(scm_char_t *chr, ScmObj port)
{
  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  if (scm_obj_null_p(port)) {
    scm_capi_error("peek-char: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("peek-char: input-port required, but got", 1, port);
    return -1;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("peek-char: textual-port required, but got", 1, port);
    return -1;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("peek-char: input-port is closed", 1, port);
    return -1;
  }
  else if (chr == NULL) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }

  return scm_port_peek_char(port, chr);
}

ScmObj
scm_api_peek_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  s = scm_capi_peek_cchar(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return scm_api_eof();
  else
    return scm_capi_make_char(&chr, scm_port_internal_enc(port));
}

ScmObj
scm_api_read_line(ScmObj port)
{
  ScmObj line = SCM_OBJ_INIT;
  ScmStringIO *sio;
  ssize_t ret;

  SCM_STACK_FRAME_PUSH(&port, &line);

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  if (scm_obj_null_p(port)) {
    scm_capi_error("read-line: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read-line: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("read-line: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("read-line: input-port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  ret = scm_port_read_line(port, (ScmIO *)sio);

  if (ret < 0)
    line = SCM_OBJ_NULL;
  else if (ret == 0)
    line = scm_api_eof();
  else
    line = scm_capi_make_string_from_bin(scm_stringio_buffer(sio),
                                         scm_stringio_length(sio),
                                         scm_port_internal_enc(port));

  scm_stringio_end(sio);

  return line;
}

int
scm_capi_char_ready(ScmObj port, bool *rslt)
{
  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  if (scm_obj_null_p(port)) {
    scm_capi_error("char-ready?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("char-ready?: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("char-ready?: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("char-ready?: input-port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  return scm_port_char_ready(port, rslt);
}

ScmObj
scm_api_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  ret = scm_capi_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? scm_api_true() : scm_api_false();
}

ScmObj
scm_api_read_string(ScmObj n, ScmObj port)
{
  ScmObj fn = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmStringIO *sio;
  size_t nc;
  ssize_t nr;
  int ret;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&n, &port,
                       &fn, &str);

  if (scm_obj_null_p(n)) {
    scm_capi_error("read-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_integer_p(n)) {
    scm_capi_error("read-string: integer required, but got", 1, n);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(port)) {
    scm_capi_error("read-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read-string: input-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("read-string: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("read-string: input-port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_zero_p(n) || scm_capi_negative_p(n))
    return scm_capi_make_string_from_bin(NULL, 0, scm_port_internal_enc(port));

  nc = SCM_FIXNUM_MAX;
  fn = scm_capi_make_number_from_size_t(nc);
  if (scm_obj_null_p(fn)) return SCM_OBJ_NULL;

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  str = SCM_OBJ_NULL;
  while (scm_capi_bignum_p(n)) {
    nr = scm_port_read_string(nc, port, (ScmIO *)sio);
    if (nr < 0) goto end;
    else if (nr == 0) goto mkstr;

    n = scm_api_minus(n, fn);
    if (scm_obj_null_p(n)) goto end;
  }

  ret = scm_capi_integer_to_size_t(n, &nc);
  if (ret < 0) goto end;

  nr = scm_port_read_string(nc, port, (ScmIO *)sio);
  if (nr < 0) goto end;

 mkstr:
  if (scm_stringio_length(sio) == 0)
    str = scm_api_eof();
  else
    str = scm_capi_make_string_from_bin(scm_stringio_buffer(sio),
                                        scm_stringio_length(sio),
                                        scm_port_internal_enc(port));

 end:
  scm_stringio_end(sio);
  return str;
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

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&obj, &port);

  if (scm_obj_null_p(obj)) {
    scm_capi_error("write-simple: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-simple: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("write-simple: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("write-simple: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_obj_call_pp_func(obj, port, true);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_display(ScmObj obj, ScmObj port)
{
  int rslt;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */
  /* TODO: obj の構造が循環していても無限ループにならないようにする */

  SCM_STACK_FRAME_PUSH(&obj, &port);

  if (scm_obj_null_p(obj)) {
    scm_capi_error("display: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_output_port_p(port)) {
    scm_capi_error("display: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("display: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("display: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_obj_call_pp_func(obj, port, false);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

ScmObj
scm_api_newline(ScmObj port)
{
  ScmEncoding *enc;
  scm_char_t nl;
  ssize_t rslt;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  if (scm_obj_null_p(port)) {
    scm_capi_error("newline: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("newline: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("newline: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("newline: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  enc = scm_port_internal_enc(port);
  scm_enc_cnv_from_ascii(enc, '\n', &nl);
  rslt = scm_port_write_char(port, nl);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

int
scm_capi_write_cchar(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  ScmObj c = SCM_OBJ_INIT, r = SCM_OBJ_INIT;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&port,
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
  ScmEncoding *p_enc, *c_enc;
  ssize_t rslt;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&chr, &port);

  if (scm_obj_null_p(chr)) {
    scm_capi_error("write-char: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("write-char: character required, but got", 1, chr);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(port)) {
    scm_capi_error("write-char: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-char: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("write-char: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("write-char: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  p_enc = scm_port_internal_enc(port);
  c_enc = scm_char_encoding(chr);

  if (p_enc != c_enc) {
    chr = scm_char_encode(chr, p_enc);
    if (scm_obj_null_p(chr)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  rslt = scm_port_write_char(port, scm_char_value(chr));
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  return SCM_UNDEF_OBJ;
}

int
scm_capi_write_cstr(const char *str, ScmEncoding *enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&port, &s);

  if (enc == NULL) {
    scm_capi_error("write-string: invalid argument", 0);
    return -1;
  }

  s = scm_capi_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return -1;

  s = scm_api_write_string(s, port, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (scm_obj_null_p(s)) return -1;

  return 0;
}

ssize_t
scm_capi_write_string(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  ScmEncoding *p_enc;
  ssize_t rslt, size;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&str, &port);

  if (scm_obj_null_p(str)) {
    scm_capi_error("write-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("write-string: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(port)) {
    scm_capi_error("write-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-string: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_textual_port_p(port)) {
    scm_capi_error("write-string: textual-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("write-string: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  if (start >= 0 && end >= 0 && start > end) {
    scm_capi_error("write-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  if (start != 0 || end != (ssize_t)scm_string_length(str)) {
    str = scm_string_substr(str, (size_t)start, (size_t)(end - start));
    if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
  }

  p_enc = scm_port_internal_enc(port);
  if (p_enc != scm_string_encoding(str)) {
    str = scm_string_encode(str, p_enc);
    if (scm_obj_null_p(str)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  size = scm_capi_string_bytesize(str);
  if (size < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  rslt = scm_port_write_bytes(port, scm_string_content(str), (size_t)size);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return end - start;
}

ScmObj
scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end)
{
  ssize_t sss, sse, rslt;

  SCM_STACK_FRAME_PUSH(&str, &port, &start, &end);

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

ScmObj
scm_api_flush_output_port(ScmObj port)
{
  int rslt;

  /* TODO: 引数の port のは指定省略可能にする (SCM_OBJ_NULL を指定可能にする) */

  SCM_STACK_FRAME_PUSH(&port);

  if (scm_obj_null_p(port)) {
    scm_capi_error("flush-output-port: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("flush-output-port: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }
  else if (scm_port_closed_p(port)) {
    scm_capi_error("flush-output-port: port is closed", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_port_flush(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

bool
scm_capi_procedure_p(ScmObj proc)
{
  if (scm_obj_null_p(proc)) return false;

  return scm_obj_type_flag_set_p(proc, SCM_TYPE_FLG_PROC);
}

int
scm_capi_arity(ScmObj proc, int *arity)
{
  if (!scm_capi_procedure_p(proc)) {
    scm_capi_error("arity: invalid argument", 0);
    return -1;
  }
  else if (arity == NULL) {
    scm_capi_error("arity: invalid argument", 0);
    return -1;
  }

  *arity = scm_proc_arity(proc);

  return 0;
}

int
scm_capi_procedure_flg_set_p(ScmObj proc, SCM_PROC_FLG_T flg, bool *rslt)
{
  if (!scm_capi_procedure_p(proc) || rslt == NULL) {
    scm_capi_error("failed to get procedure information: invalid argument", 0);
    return -1;
  }

  *rslt = scm_proc_flg_set_p(proc, flg);

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
    scm_capi_error("can not make subrutine: invaild argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(module) && !scm_capi_module_p(module)) {
    scm_capi_error("failed to make subrutine: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_subrutine_new(SCM_MEM_ALLOC_HEAP, func,
                           SCM_OBJ_NULL, arity, flags, module);
}

int
scm_api_call_subrutine(ScmObj subr, int argc, const ScmObj *argv)
{
  if (!scm_capi_subrutine_p(subr)) {
    scm_capi_error("can not call subrutine: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_subrutine_call(subr, argc, argv);
}

extern inline bool
scm_capi_subrutine_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_SUBRUTINE_TYPE_INFO) ? true : false);
}

int
scm_capi_subrutine_module(ScmObj subr, scm_csetter_t *mod)
{
  if (!scm_capi_subrutine_p(subr)) {
    scm_capi_error("faild to get a module defines the subrutine: "
                   "invalid argument", 0);
    return -1;
  }
  else if (mod == NULL) {
    scm_capi_error("faild to get a module defines the subrutine: "
                   "invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(mod, scm_subrutine_module(subr));

  return 0;
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmObj
scm_capi_make_closure(ScmObj iseq, ScmObj env, int arity)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not make closure: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_closure_new(SCM_MEM_ALLOC_HEAP, iseq, env, SCM_OBJ_NULL, arity);
}

extern inline bool
scm_capi_closure_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_CLOSURE_TYPE_INFO) ? true : false);
}

ScmObj
scm_capi_closure_to_iseq(ScmObj clsr)
{
  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("can not get iseq object from closure: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_closure_body(clsr);
}

scm_byte_t *
scm_capi_closure_to_ip(ScmObj clsr)
{
  ScmObj iseq = SCM_OBJ_INIT;

  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("can not get iseq object from closure: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  iseq = scm_closure_body(clsr);
  if (scm_obj_null_p(iseq)) return NULL;

  return scm_iseq_to_ip(iseq);
}

int
scm_capi_closure_env(ScmObj clsr, scm_csetter_t *env)
{
  if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("can not get closed environment object from closure: "
                   "invalid argument", 0);
    return -1;
  }
  else if (env == NULL) {
    scm_capi_error("can not get closed environment object from closure: "
                   "invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(env, scm_closure_env(clsr));

  return 0;
}


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmObj
scm_capi_make_parameter(ScmObj conv)
{
  SCM_STACK_FRAME_PUSH(&conv);

  if (scm_obj_not_null_p(conv) && !scm_capi_procedure_p(conv)) {
    scm_capi_error("faild to make parameter object: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_parameter_new(SCM_MEM_HEAP, SCM_OBJ_NULL, conv);
}

extern inline bool
scm_capi_parameter_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_PARAMETER_TYPE_INFO);
}

int
scm_capi_parameter_init_val(ScmObj prm, scm_csetter_t *val)
{
  SCM_STACK_FRAME_PUSH(&prm);

  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to get a initial value of a parameter object: "
                   "invalid argument", 0);
    return -1;
  }
  else if (val == NULL) {
    scm_capi_error("failed to get a initial value of a parameter object: "
                   "invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(val, scm_parameter_init_val(prm));

  return 0;
}

int
scm_capi_parameter_converter(ScmObj prm, scm_csetter_t *conv)
{
  SCM_STACK_FRAME_PUSH(&prm);

  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to get a converter from a parameter object: "
                   "invalid argument", 0);
    return -1;
  }
  else if (conv != NULL) {
    scm_capi_error("failed to get a converter from a parameter object: "
                   "invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(conv, scm_parameter_converter(prm));

  return 0;
}

int
scm_capi_parameter_set_init_val(ScmObj prm, ScmObj val)
{
  SCM_STACK_FRAME_PUSH(&prm, &val);

  if (!scm_capi_parameter_p(prm)) {
    scm_capi_error("failed to set a initial value of a parameter object: "
                   "invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to set a initial value of a parameter object: "
                   "invalid argument", 0);
    return -1;
  }

  scm_parameter_set_init_val(prm, val);

  return 0;
}

ScmObj
scm_capi_parameter_value(ScmObj prm)
{
  SCM_STACK_FRAME_PUSH(&prm);

  if (scm_obj_null_p(prm)) {
    scm_capi_error("failed to get bound value: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vm_parameter_value(scm_vm_current_vm(), prm);
}


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

ScmObj
scm_capi_make_syntax(ScmObj keyword, ScmSyntaxHandlerFunc handler)
{
  if (!scm_capi_symbol_p(keyword)) {
    scm_capi_error("failed to make syntax object: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (handler == NULL) {
    scm_capi_error("failed to make syntax object: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_syntax_new(SCM_MEM_HEAP, keyword, handler);
}

extern inline bool
scm_capi_syntax_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ? true : false);
}

extern inline ScmObj
scm_api_syntax_keyword(ScmObj syx)
{
  if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("faild to get syntax keyword: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_syntax_keyword(syx);
}

extern inline int
scm_capi_syntax_handler(ScmObj syx, ScmSyntaxHandlerFunc *handler)
{
  if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("faild to get syntax handler: invalid argument", 0);
    return -1;
  }
  else if (handler == NULL) {
    scm_capi_error("faild to get syntax handler: invalid argument", 0);
    return -1;
  }

  *handler = scm_syntax_handler(syx);
  return 0;
}


/*******************************************************************/
/*  ISeq                                                           */
/*******************************************************************/

ScmObj
scm_api_make_iseq(void)
{
  return scm_iseq_new(SCM_MEM_HEAP);
}

extern inline bool
scm_capi_iseq_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_ISEQ_TYPE_INFO) ? true : false);
}

scm_byte_t *
scm_capi_iseq_to_ip(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not get instruction pointer from iseq: "
                   "invalid argument", 0);
    return NULL;
  }

  return scm_iseq_to_ip(iseq);
}

ssize_t
scm_capi_iseq_length(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not get length of instruction seqeunce: "
                   "invalid argument", 0);
    return -1;
  }

  return scm_iseq_length(iseq);
}

ssize_t
scm_capi_iseq_push_opfmt_noarg(ScmObj iseq, SCM_OPCODE_T op)
{
  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  return scm_iseq_push_ushort(iseq, op);
}

ssize_t
scm_capi_iseq_push_opfmt_obj(ScmObj iseq, SCM_OPCODE_T op, ScmObj val)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &val);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  if (scm_obj_null_p(val)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_ushort(iseq, op);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return scm_iseq_push_obj(iseq, val);
}

ssize_t
scm_capi_iseq_push_opfmt_obj_obj(ScmObj iseq,
                                 SCM_OPCODE_T op, ScmObj val1, ScmObj val2)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &val1, &val2);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  if (scm_obj_null_p(val1)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  if (scm_obj_null_p(val2)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_ushort(iseq, op);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  rslt = scm_iseq_push_obj(iseq, val1);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return scm_iseq_push_obj(iseq, val2);
}

ssize_t
scm_capi_iseq_push_opfmt_si(ScmObj iseq, SCM_OPCODE_T op, int val)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_ushort(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_uint(iseq, (unsigned int)val);
}

ssize_t
scm_capi_iseq_push_opfmt_si_si(ScmObj iseq, SCM_OPCODE_T op, int val1, int val2)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_ushort(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint(iseq, (unsigned int)val1);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_uint(iseq, (unsigned int)val2);
}

ssize_t
scm_capi_iseq_push_opfmt_si_si_obj(ScmObj iseq, SCM_OPCODE_T op,
                                   int val1, int val2, ScmObj obj)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq,
                       &obj);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  if (scm_obj_null_p(obj)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_ushort(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint(iseq, (unsigned int)val1);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint(iseq, (unsigned int)val2);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_obj(iseq, obj);
}

ssize_t
scm_capi_iseq_push_opfmt_iof(ScmObj iseq, SCM_OPCODE_T op, int offset)
{
  return scm_capi_iseq_push_opfmt_si(iseq, op, offset);
}

ssize_t
scm_capi_iseq_set_si(ScmObj iseq, size_t idx, int val)
{
  ssize_t rslt;

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not update instruction operand in iseq: "
                   "invalid argument", 0);
    return -1;
  }
  else if (idx > SSIZE_MAX || (ssize_t)idx > scm_iseq_length(iseq) - 4) {
    scm_capi_error("can not update instruction operand in iseq: "
                   "invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_set_uint(iseq, idx, (unsigned int)val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return (ssize_t)idx;
}

int
scm_capi_opcode_to_opfmt(int opcode)
{
  static const int tbl[] = {
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_NOP */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_HALT */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_UNDEF */
    SCM_OPFMT_SI,               /* SCM_OPCODE_CALL */
    SCM_OPFMT_SI,               /* SCM_OPCODE_TAIL_CALL */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_APPLY */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_TAIL_APPLY */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_RETURN */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_FRAME */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_CFRAME */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_EFRAME */
    SCM_OPFMT_SI,               /* SCM_OPCODE_ECOMMIT */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_EPOP */
    SCM_OPFMT_SI,               /* SCM_OPCODE_EREBIND */
    SCM_OPFMT_OBJ,              /* SCM_OPCODE_IMMVAL */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_PUSH */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_MVPUSH */
    SCM_OPFMT_OBJ_OBJ,          /* SCM_OPCODE_GREF */
    SCM_OPFMT_OBJ_OBJ,          /* SCM_OPCODE_GDEF */
    SCM_OPFMT_OBJ_OBJ,          /* SCM_OPCODE_GSET */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_SREF */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_SSET */
    SCM_OPFMT_IOF,              /* SCM_OPCODE_JMP */
    SCM_OPFMT_IOF,              /* SCM_OPCODE_JMPT */
    SCM_OPFMT_IOF,              /* SCM_OPCODE_JMPF */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_RAISE */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_BOX */
    SCM_OPFMT_SI_SI_OBJ,        /* SCM_OPCODE_CLOSE */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_DEMINE */
    SCM_OPFMT_SI,               /* SCM_OPCODE_EMINE */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_EDEMINE */
    SCM_OPFMT_SI,               /* SCM_OPCODE_ARITY */
  };

  if (opcode < 0 || sizeof(tbl)/sizeof(tbl[0]) <= (size_t)opcode) {
    scm_capi_error("can not get opcode format id: invalid opcode", 0);
    return -1;
  }

  return tbl[opcode];
}

scm_byte_t *
scm_capi_inst_fetch_oprand_obj(scm_byte_t *ip, scm_csetter_t *obj)
{
  ScmObj opr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&opr);

  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (obj == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  opr = scm_iseq_fetch_obj(&ip);

  scm_csetter_setq(obj, opr);

  return ip;
}

scm_byte_t *
scm_capi_inst_fetch_oprand_obj_obj(scm_byte_t *ip,
                                   scm_csetter_t *obj1, scm_csetter_t *obj2)
{
  ScmObj opr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&opr);

  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (obj1 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (obj2 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  opr = scm_iseq_fetch_obj(&ip);
  scm_csetter_setq(obj1, opr);

  opr = scm_iseq_fetch_obj(&ip);
  scm_csetter_setq(obj2, opr);

  return ip;
}

scm_byte_t *
scm_capi_inst_fetch_oprand_si(scm_byte_t *ip, int *si)
{
  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (si == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  *si = scm_iseq_fetch_int(&ip);

  return ip;
}

scm_byte_t *
scm_capi_inst_fetch_oprand_si_si(scm_byte_t *ip, int *si1, int *si2)
{
  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (si1 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (si2 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  *si1 = scm_iseq_fetch_int(&ip);
  *si2 = scm_iseq_fetch_int(&ip);

  return ip;
}

scm_byte_t *
scm_capi_inst_fetch_oprand_si_si_obj(scm_byte_t *ip,
                                     int *si1, int *si2, scm_csetter_t *obj)
{
  ScmObj opr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&opr);

  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (si1 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (si2 == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (obj == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  *si1 = scm_iseq_fetch_int(&ip);
  *si2 = scm_iseq_fetch_int(&ip);
  opr = scm_iseq_fetch_obj(&ip);

  scm_csetter_setq(obj, opr);

  return ip;
}

scm_byte_t *
scm_capi_inst_fetch_oprand_iof(scm_byte_t *ip, int *offset)
{
  return scm_capi_inst_fetch_oprand_si(ip, offset);
}

int
scm_capi_inst_update_oprand_obj(scm_byte_t *ip, ScmObj clsr, ScmObj obj)
{
  ScmObj iseq;
  ssize_t idx;

  SCM_STACK_FRAME_PUSH(&clsr, &obj,
                       &iseq);

  if (ip == NULL) {
    scm_capi_error("can not updated operands: invalid ip", 0);
    return -1;
  }
  else if (!scm_capi_closure_p(clsr)) {
    scm_capi_error("can not updated operands: invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(obj)) {
    scm_capi_error("can not updated operands: invalid argument", 0);
    return -1;
  }

  iseq = scm_capi_closure_to_iseq(clsr);
  if (scm_obj_null_p(iseq)) return -1;

  idx = scm_iseq_ip_to_idx(iseq, ip);
  if (idx < 0) {
    scm_capi_error("can not updated operands: invalid ip", 0);
    return -1;
  }

  idx = scm_iseq_set_obj(iseq, (size_t)idx, obj);
  if (idx < 0) return -1;

  return 0;
}


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj
scm_api_assemble(ScmObj lst)
{
  if (scm_obj_null_p(lst)) {
    scm_capi_error("asm: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_pair_p(lst)) {
    scm_capi_error("asm: pair required, but got", 1, lst);
    return SCM_OBJ_NULL;
  }

  return scm_asm_assemble(lst);
}


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

bool
scm_capi_compiler_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_COMPILER_TYPE_INFO);
}

ScmObj
scm_api_current_module(ScmObj cmpl)
{
  if (!scm_capi_compiler_p(cmpl)) {
    scm_capi_error("failed to get current module: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_cmpl_current_module(cmpl);
}

ScmObj
scm_api_compile(ScmObj exp, ScmObj arg)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, cmpl = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &arg,
                       &name, &mod, &cmpl);

  if (scm_obj_null_p(exp)) {
    scm_capi_error("compile: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  name = mod = cmpl = SCM_OBJ_NULL;
  if (scm_obj_not_null_p(arg)) {
    if (scm_capi_symbol_p(arg))
      name = arg;
    else if (scm_capi_module_p(arg))
      mod = arg;
    else if (scm_capi_compiler_p(arg))
      cmpl = arg;
    else {
      scm_capi_error("compile: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
  }

  if (scm_obj_null_p(cmpl)) {
    if (scm_obj_null_p(mod)) {
      if (scm_obj_null_p(name)) {
        name = scm_capi_make_symbol_from_cstr("main", SCM_ENC_ASCII);
        if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
      }

      rslt = scm_capi_find_module(name, SCM_CSETTER_L(mod));
      if (rslt < 0) return SCM_OBJ_NULL;

      if (scm_obj_null_p(mod)) {
        scm_capi_error("compile: specified module is not exist", 0);
        return SCM_OBJ_NULL;
      }
    }

    cmpl = scm_cmpl_new(SCM_MEM_HEAP);
    if (scm_obj_null_p(cmpl)) return SCM_OBJ_NULL;

    scm_cmpl_select_module(cmpl, mod);
  }

  return scm_cmpl_compile(cmpl, exp);
}


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

bool
scm_capi_gloc_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_GLOC_TYPE_INFO);
}

int
scm_capi_gloc_value(ScmObj gloc, scm_csetter_t *val)
{
  SCM_STACK_FRAME_PUSH(&gloc);

  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("faild to get a value of gloc: invalid argument", 0);
    return -1;
  }
  else if (val == NULL) {
    scm_capi_error("faild to get a value of gloc: invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(val, scm_gloc_value(gloc));

  return 0;
}

int
scm_capi_gloc_symbol(ScmObj gloc, scm_csetter_t *sym)
{
  SCM_STACK_FRAME_PUSH(&gloc);

  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("faild to get a symbol of gloc: invalid argument", 0);
    return -1;
  }
  else if (sym == NULL) {
    scm_capi_error("faild to get a symbol of gloc: invalid argument", 0);
    return -1;
  }

  scm_csetter_setq(sym, scm_gloc_symbol(gloc));

  return 0;
}

int
scm_capi_gloc_bind(ScmObj gloc, ScmObj val)
{
  SCM_STACK_FRAME_PUSH(&gloc, &val);

  if (!scm_capi_gloc_p(gloc)) {
    scm_capi_error("faild to update value of gloc: invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("faild to update value of gloc: invalid argument", 0);
    return -1;
  }

  scm_gloc_bind(gloc, val);

  return 0;
}

ScmObj
scm_api_make_module(ScmObj name)
{
  ScmObj mod = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name,
                       &mod);

  if (!scm_capi_symbol_p(name) && !scm_capi_pair_p(name)) {
    scm_capi_error("failed to make module: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  rslt = scm_moduletree_find(scm_vm_moduletree(scm_vm_current_vm()),
                             name, SCM_CSETTER_L(mod));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(mod)) {
    scm_capi_error("failed to make a module: already exist", 0);
    return SCM_OBJ_NULL;
  }

  return scm_moduletree_module(scm_vm_moduletree(scm_vm_current_vm()), name);
}

extern inline bool
scm_capi_module_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_MODULE_TYPE_INFO);
}

int
scm_capi_find_module(ScmObj name, scm_csetter_t *mod)
{
  SCM_STACK_FRAME_PUSH(&name);

  if (!scm_capi_symbol_p(name) && !scm_capi_pair_p(name)) {
    scm_capi_error("failed to find module: invalid argument", 0);
    return -1;
  }

  return scm_moduletree_find(scm_vm_moduletree(scm_vm_current_vm()), name, mod);
}

ScmObj
scm_api_module_name(ScmObj module)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to get a name from module: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_module_name(module);
}

int
scm_capi_import(ScmObj module, ScmObj imported)
{
  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to import a module: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_module_p(imported)) {
    scm_capi_error("failed to import a module: invalid argument", 0);
    return -1;
  }

  return scm_module_import(module, imported);
}

ScmObj
scm_capi_make_gloc(ScmObj module, ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module, &sym,
                       &gloc);

  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to make a GLoc object: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to make a GLoc object: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  rslt = scm_module_find_sym_eval(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_not_null_p(gloc)) {
    scm_capi_error("failed to make a GLoc object: already exist", 0);
    return SCM_OBJ_NULL;
  }

  return scm_module_gloc_eval(module, sym);
}

int
scm_capi_find_gloc(ScmObj module, ScmObj sym, scm_csetter_t *gloc)
{
  SCM_STACK_FRAME_PUSH(&module, &sym);

  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to find a GLoc object: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to find a GLoc object: invalid argument", 0);
    return -1;
  }
  else if (gloc == NULL) {
    scm_capi_error("failed to find a GLoc object: invalid argument", 0);
    return -1;
  }

  return scm_module_find_sym_eval(module, sym, gloc);
}

int
scm_capi_define_global_var(ScmObj module, ScmObj sym, ScmObj val, bool export)
{
  SCM_STACK_FRAME_PUSH(&module, &sym, &val);

  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to define global variable: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to define global variable: invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("failed to define global variable: invalid argument", 0);
    return -1;
  }

  return scm_module_define_eval(module, sym, val, export);
}

int
scm_capi_define_global_syx(ScmObj module, ScmObj sym, ScmObj syx, bool export)
{
  SCM_STACK_FRAME_PUSH(&module, &sym, &syx);

  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to define syntax: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to define syntax: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("failed to define syntax: invalid argument", 0);
    return -1;
  }

  return scm_module_define_cmpl(module, sym, syx, export);
}

int
scm_capi_global_var_ref(ScmObj module, ScmObj sym, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module, &sym,
                       &gloc, &v);

  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to get a value of global variable:"
                   " invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to get a value of global variable:"
                   " invalid argument", 0);
    return -1;
  }
  else if (val == NULL) {
    scm_capi_error("failed to get a value of global variable:"
                   " invalid argument", 0);
    return -1;
  }

  rslt = scm_module_find_sym_eval(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc))
    v = scm_gloc_value(gloc);
  else
    v = SCM_OBJ_NULL;

  scm_csetter_setq(val, v);

  return 0;
}

int
scm_capi_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module, &sym,
                       &gloc, &v);


  if (!scm_capi_module_p(module)) {
    scm_capi_error("failed to get a syntax: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_symbol_p(sym)) {
    scm_capi_error("failed to get a syntax: invalid argument", 0);
    return -1;
  }
  else if (syx == NULL) {
    scm_capi_error("failed to get a syntax: invalid argument", 0);
    return -1;
  }

  rslt = scm_module_find_sym_cmpl(module, sym, SCM_CSETTER_L(gloc));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(gloc))
    v = scm_gloc_value(gloc);
  else
    v = SCM_OBJ_NULL;

  scm_csetter_setq(syx, v);

  return 0;
}


/*******************************************************************/
/*  Return Value                                                   */
/*******************************************************************/

int
scm_capi_return_val(const ScmObj *val, int vc)
{
  if (vc < 0) {
    scm_capi_error("failed to setup return value: invalid argument", 0);
    return -1;
  }
  else if (vc > 0 && val == NULL) {
    scm_capi_error("failed to setup return value: invalid argument", 0);
    return -1;
  }

  return scm_vm_set_val_reg(scm_vm_current_vm(), val, vc);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

ScmObj
scm_capi_capture_cont(void)
{
  ScmObj cap = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&cap);

  cap = scm_vm_capture_cont(scm_vm_current_vm());
  if (scm_obj_null_p(cap)) return SCM_OBJ_NULL;

  return scm_cont_new(SCM_MEM_HEAP, cap);
}

bool
scm_capi_continuation_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return scm_obj_type_p(obj, &SCM_CONTINUATION_TYPE_INFO);
}

ScmObj
scm_capi_cont_capture_obj(ScmObj cont)
{
  if (!scm_capi_continuation_p(cont)) {
    scm_capi_error("faild to get capture object from continuation: "
                   "invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_cont_content(cont);
}


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int
scm_capi_trampolining(ScmObj proc, ScmObj args,
                      ScmObj postproc, ScmObj handover)
{
  if (!scm_capi_subrutine_p(proc) && !scm_capi_closure_p(proc)) {
    scm_capi_error("", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_pair_p(args) && !scm_capi_nil_p(args)) {
    scm_capi_error("", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_not_null_p(postproc) && !scm_capi_procedure_p(postproc)) {
    scm_capi_error("", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vm_setup_stat_trmp(scm_vm_current_vm(), proc, args,
                                postproc, handover);
}


/*******************************************************************/
/*  Install Exception Handler                                      */
/*******************************************************************/

int
scm_capi_push_exception_handler(ScmObj handler)
{
  if (!scm_capi_subrutine_p(handler) && !scm_capi_closure_p(handler)) {
    scm_capi_error("can not install exception handler: invalid argument", 0);
    return -1;
  }

  return scm_vm_push_exception_handler(scm_vm_current_vm(), handler);
}


/*******************************************************************/
/*  Exit                                                           */
/*******************************************************************/

ScmObj
scm_api_exit(ScmObj obj)
{
  /* TODO: obj の内容に応じた VM の終了ステータスの設定*/

  scm_vm_setup_stat_halt(scm_vm_current_vm());

  return SCM_UNDEF_OBJ;
}


/*******************************************************************/
/*  System Environment                                             */
/*******************************************************************/

ScmEncoding *
scm_capi_system_encoding(void)
{
  return scm_bedrock_encoding(scm_bedrock_current_br());
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

  if (scm_obj_not_null_p(ev->vm)) {
    scm_vm_end(ev->vm);
    ev->vm = SCM_OBJ_NULL;
  }
  free(ev);
}

int
scm_capi_run_repl(ScmEvaluator *ev)
{
  ScmObj port = SCM_OBJ_INIT, asmbl = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT;
  int ret;

  ret = -1;

  if (ev == NULL) return ret;

  ev->vm = scm_vm_new();
  if (scm_obj_null_p(ev->vm)) return ret; /* [ERR]: [through] */

  scm_vm_change_current_vm(ev->vm);

  SCM_STACK_FRAME_PUSH(&port, &asmbl, &iseq);

  scm_vm_setup_system(ev->vm);

  port = scm_capi_open_input_string_cstr("("
                                         " (label loop)"
                                         "   (frame)"
                                         "   (immval \"> \")"
                                         "   (push)"
                                         "   (gref display main)"
                                         "   (call 1)"
                                         "   (arity 1)"
                                         "   (cframe)"
                                         "   (gref flush-output-port main)"
                                         "   (call 0)"
                                         "   (arity 1)"
                                         "   (frame)"
                                         "   (frame)"
                                         "   (cframe)"
                                         "   (gref read main)"
                                         "   (call 0)"
                                         "   (arity 1)"
                                         "   (push)"
                                         "   (gref eval main)"
                                         "   (call 1)"
                                         "   (arity 1)"
                                         "   (push)"
                                         "   (gref write main)"
                                         "   (call 1)"
                                         "   (arity 1)"
                                         "   (cframe)"
                                         "   (gref newline main)"
                                         "   (call 0)"
                                         "   (arity 1)"
                                         "   (cframe)"
                                         "   (gref flush-output-port main)"
                                         "   (call 0)"
                                         "   (arity 1)"
                                         "   (jmp loop)"
                                         ")",
                                         SCM_ENC_UTF8);
  if (scm_obj_null_p(port)) goto end;

  asmbl = scm_api_read(port);
  if (scm_obj_null_p(asmbl)) goto end;

  port = scm_api_close_input_port(port);
  if (scm_obj_null_p(port)) goto end;

  port = SCM_OBJ_NULL;

  iseq = scm_api_assemble(asmbl);
  if (scm_obj_null_p(iseq)) goto end;

  asmbl = SCM_OBJ_NULL;

  scm_vm_run(ev->vm, iseq);

  ret = 0;

 end:
  scm_vm_end(ev->vm);
  ev->vm = SCM_OBJ_NULL;

  return ret;
}


#ifdef SCM_UNIT_TEST

/* unit test 用 api ********************************************************/

void
scm_capi_ut_setup_current_vm(ScmEvaluator *ev)
{
  if (ev == NULL) return;

  ev->vm = scm_vm_new();
  if (scm_obj_null_p(ev->vm)) return;

  scm_vm_change_current_vm(ev->vm);
  scm_vm_setup_system(ev->vm);
}

ScmObj
scm_capi_ut_eval(ScmEvaluator *ev, ScmObj exp)
{
  ScmObj code = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&exp,
                       &code);

  code = scm_api_compile(exp, SCM_OBJ_NULL);
  if (scm_obj_null_p(code)) return SCM_OBJ_NULL;

  code = scm_api_assemble(code);
  if (scm_obj_null_p(code)) return SCM_OBJ_NULL;

  rslt = scm_capi_iseq_push_opfmt_noarg(code, SCM_OPCODE_HALT);
  if (rslt < 0) return SCM_OBJ_NULL;

  scm_vm_run(ev->vm, code);

  return scm_vm_register_val(ev->vm);
}

void
scm_capi_ut_clear_compiler_label_id(void)
{
  scm_cmpl_ut_clear_label_id();
}

#endif



