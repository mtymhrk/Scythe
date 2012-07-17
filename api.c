#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "reference.h"
#include "char.h"
#include "string.h"
#include "symbol.h"
#include "procedure.h"
#include "gloc.h"
#include "numeric.h"
#include "pair.h"
#include "vector.h"
#include "port.h"
#include "parser.h"
#include "syntax.h"
#include "iseq.h"
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

extern inline ScmObj
scm_capi_mem_alloc_heap(ScmTypeInfo *type)
{
  if (type == NULL) {
    scm_capi_fatal("memory allocation error: invalid object type");
    return SCM_OBJ_NULL;
  }

  return scm_mem_alloc_heap(scm_vm_current_mm(), type);
}

extern inline ScmObj
scm_capi_mem_alloc_root(ScmTypeInfo *type)
{
  if (type == NULL) {
    scm_capi_fatal("memory allocation error: invalid object type");
    return SCM_OBJ_NULL;
  }

  return scm_mem_alloc_root(scm_vm_current_mm(), type);
}

extern inline ScmObj
scm_capi_mem_alloc(ScmTypeInfo *otype, SCM_MEM_TYPE_T mtype)
{
  switch(mtype) {
  case SCM_MEM_HEAP:
    return scm_capi_mem_alloc_heap(otype);
    break;
  case SCM_MEM_ROOT:
    return scm_capi_mem_alloc_root(otype);
    break;
  default:
    scm_capi_fatal("memory allocation error: invalid memory type");
    return SCM_OBJ_NULL;          /* provisional implemntation */
    break;
  };
}

extern inline ScmObj
scm_capi_mem_free_root(ScmObj obj)
{
  if (obj == SCM_OBJ_NULL) return SCM_OBJ_NULL;
  return scm_mem_free_root(scm_vm_current_mm(), obj);
}

extern inline ScmRef
scm_capi_mem_register_extra_rfrn(ScmRef ref)
{
  if (ref == SCM_REF_NULL) return ref;
  return scm_mem_register_extra_rfrn(scm_vm_current_mm(), ref);
}

extern inline void
scm_capi_gc_start(void)
{
  scm_mem_gc_start(scm_vm_current_mm());
}

extern inline void
scm_capi_gc_enable(void)
{
  scm_mem_enable_gc(scm_vm_current_mm());
}

extern inline void
scm_capi_gc_disable(void)
{
  scm_mem_disable_gc(scm_vm_current_mm());
}


/*******************************************************************/
/*  Equivalence                                                    */
/*******************************************************************/

/* 述語関数について、C の bool 方を返すものは _p を関数名の後ろに付与する。
 * Scheme の #t/#f を返すものは _P を関数名の後ろに付与する。
 */

extern inline bool
scm_capi_null_value_p(ScmObj obj)
{
  return scm_obj_null_p(obj);
}

extern inline bool
scm_capi_eq_p(ScmObj obj1, ScmObj obj2)
{
  return scm_obj_same_instance_p(obj1, obj2);
}

extern inline ScmObj
scm_api_eq_P(ScmObj obj1, ScmObj obj2)
{
  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("eq?: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  return (scm_obj_same_instance_p(obj1, obj2) ?
          scm_api_bool_true() : scm_api_bool_false());
}

ScmObj
scm_api_eqv_P(ScmObj obj1, ScmObj obj2)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj1, &obj2,
                       &str1, &str2);

  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("eqv?: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  if (scm_capi_eq_p(obj1, obj2))
    return scm_api_bool_true();

  if (scm_capi_number_p(obj1)) {
    if (scm_capi_number_p(obj2))
      return scm_api_num_eq_P(obj1, obj2);
    else
      return scm_api_bool_false();
  }

  if (!scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2)))
    return scm_api_bool_false();

  if (scm_capi_char_p(obj1)) {
    return scm_api_char_eq_P(obj1, obj2);
  }
  else if (scm_capi_symbol_p(obj1)) {
    str1 = scm_api_symbol_to_string(obj1);
    if (scm_obj_null_p(str1)) return SCM_OBJ_NULL;

    str2 = scm_api_symbol_to_string(obj2);
    if (scm_obj_null_p(str2)) return SCM_OBJ_NULL;

    return scm_api_string_eq_P(str1, str2);
  }

  return scm_api_bool_false();
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
 * Memo: equal? precedure の動作について
 * 以下の 2 つの循環リストはいずれも、シンボル a、b が交互に表われるリストであ
 * るが、これらの equal? は実装上、偽となる。R6RS ではどうすべきかはよくわから
 * なかった。
 *
 *   1. #0=(a b a b . #0#)
 *   2. (a b . #0=(a b . #0#))
 *
 *  (equal? '#0=(a b a b . #0#) '(a b . #0=(a b . #0#)))  ; => #f
 *
 */

static ScmObj
scm_api_equal_aux_P(ScmObj obj1, ScmObj obj2, ScmObj stack1, ScmObj stack2)
{
  ScmObj rslt = SCM_OBJ_INIT, elm1 = SCM_OBJ_INIT, elm2 = SCM_OBJ_INIT;
  int cir;

  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));
  scm_assert(scm_capi_nil_p(stack1) || scm_capi_pair_p(stack1));
  scm_assert(scm_capi_nil_p(stack2) || scm_capi_pair_p(stack2));

  SCM_STACK_FRAME_PUSH(&obj1, &obj2, &stack1, &stack2,
                       &rslt, &elm1, &elm2);

  rslt = scm_api_eqv_P(obj1, obj2);
  if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;

  if (scm_capi_false_p(rslt)
      && scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2))
      && (scm_capi_pair_p(obj1) || scm_capi_vector_p(obj1))) {
    if (scm_api_equal_check_circular(obj1, obj2, stack1, stack2, &cir) < 0)
      return SCM_OBJ_NULL;

    if (cir == SAME_CIRCULATION)
      return scm_api_bool_true();
    else if (cir == DIFFERENT_CIRCULATION)
      return scm_api_bool_false();

    stack1 = scm_api_cons(obj1, stack1);
    if (scm_obj_null_p(stack1)) return SCM_OBJ_NULL;

    stack2 = scm_api_cons(obj2, stack2);
    if (scm_obj_null_p(stack2)) return SCM_OBJ_NULL;

    if (scm_capi_pair_p(obj1)) {
      elm1 = scm_api_car(obj1);
      if (scm_obj_null_p(elm1)) return SCM_OBJ_NULL;

      elm2 = scm_api_car(obj2);
      if (scm_obj_null_p(elm2)) return SCM_OBJ_NULL;

      rslt = scm_api_equal_aux_P(elm1, elm2, stack1, stack2);
      if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
      if (scm_capi_false_p(rslt)) return rslt;

      elm1 = scm_api_cdr(obj1);
      if (scm_obj_null_p(elm1)) return SCM_OBJ_NULL;

      elm2 = scm_api_cdr(obj2);
      if (scm_obj_null_p(elm2)) return SCM_OBJ_NULL;

      return scm_api_equal_aux_P(elm1, elm2, stack1, stack2);
    }
    else {
      ssize_t len1 = scm_capi_vector_length(obj1);
      ssize_t len2 = scm_capi_vector_length(obj2);

      if (len1 < 0 || len2 < 0) return SCM_OBJ_NULL;
      if (len1 != len2) return scm_api_bool_false();

      for (ssize_t i = 0; i < len1; i++) {
        elm1 = scm_capi_vector_ref(obj1, (size_t)i);
        if (scm_obj_null_p(elm1)) return SCM_OBJ_NULL;

        elm2 = scm_capi_vector_ref(obj2, (size_t)i);
        if (scm_obj_null_p(elm2)) return SCM_OBJ_NULL;

        rslt = scm_api_equal_aux_P(elm1, elm2, stack1, stack2);
        if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
        if (scm_capi_false_p(rslt)) return rslt;
      }

      return scm_api_bool_true();
    }
  }

  return rslt;
}

ScmObj
scm_api_equal_P(ScmObj obj1, ScmObj obj2)
{
  ScmObj stack1 = SCM_OBJ_INIT, stack2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj1, &obj2,
                       &stack1, &stack2);

  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2)) {
    scm_capi_error("equal?: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  stack1 = stack2 = scm_api_nil();
  if (scm_obj_null_p(stack1)) return SCM_OBJ_NULL;

  return scm_api_equal_aux_P(obj1, obj2, stack1, stack2);
}


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

extern inline ScmObj
scm_api_nil(void)
{
  return scm_vm_nil(scm_vm_current_vm());
}

extern inline bool
scm_capi_nil_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_nil());
}


/*******************************************************************/
/*  boolean                                                        */
/*******************************************************************/

extern inline ScmObj
scm_api_bool_true(void)
{
  return scm_vm_true(scm_vm_current_vm());
}

extern inline ScmObj
scm_api_bool_false(void)
{
  return scm_vm_false(scm_vm_current_vm());
}

extern inline bool
scm_capi_true_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_bool_true());
}

extern inline bool
scm_capi_false_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_bool_false());
}


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

extern inline ScmObj
scm_api_eof(void)
{
  return scm_vm_eof(scm_vm_current_vm());
}

extern inline bool
scm_capi_eof_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_eof());
}


/*******************************************************************/
/*  undef                                                          */
/*******************************************************************/

extern inline ScmObj
scm_api_undef(void)
{
  return scm_vm_undef(scm_vm_current_vm());
}

extern inline bool
scm_capi_undef_object_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_undef());
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
  return (scm_capi_raise(obj) < 0) ? SCM_OBJ_NULL : scm_api_undef();
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

  return scm_api_undef();
}

extern inline bool
scm_capi_error_object_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return scm_obj_type_p(obj, &SCM_EXCEPTION_TYPE_INFO);
}

/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

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

ScmObj
scm_api_set_car(ScmObj pair, ScmObj elm)
{
  if (scm_obj_null_p(pair) || scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  if (scm_pair_set_car(pair, elm) < 0) return SCM_OBJ_NULL;

  return scm_api_undef();
}

ScmObj
scm_api_set_cdr(ScmObj pair, ScmObj elm)
{
  if (scm_obj_null_p(pair) || scm_obj_null_p(elm)) {
    scm_capi_error("set-car!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO)) {
    scm_capi_error("set-car!: pair required, but got", 1, pair);
    return SCM_OBJ_NULL;
  }

  if (scm_pair_set_cdr(pair, elm) < 0) return SCM_OBJ_NULL;

  return scm_api_undef();
}

extern inline bool
scm_capi_pair_p(ScmObj pair)
{
  if (scm_obj_null_p(pair)) return false;
  return (scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO) ? true : false);
}

extern inline ScmObj
scm_api_pair_P(ScmObj pair)
{
  if (scm_obj_null_p(pair)) {
    scm_capi_error("pair?: invalid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  return scm_capi_pair_p(pair) ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_list(unsigned int n, ...)
{
  ScmObj args[n];
  ScmObj lst = SCM_OBJ_INIT;
  va_list ap;

  SCM_STACK_FRAME_PUSH(&lst);

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++) {
    args[i] = va_arg(ap, ScmObj);
    if (scm_obj_null_p(args[i])) {
      scm_capi_error("list: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
  }
  va_end(ap);

  for (unsigned int i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  lst = scm_api_nil();
  for (unsigned int i = n; i > 0; i--) {
    lst = scm_api_cons(args[i - 1], lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL; /* provisional implemntation */
  }

  return lst;
}

ScmObj
scm_capi_list_ref(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(l))
      l = scm_api_cdr(l);
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
scm_api_list_P(ScmObj lst)
{
  ScmObj rabbit = SCM_OBJ_INIT, tortoise = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &rabbit, &tortoise);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("list?: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_nil_p(lst))
    return scm_api_bool_true();
  else if (!scm_capi_pair_p(lst))
    return scm_api_bool_false();

  rabbit = tortoise = lst;

  do {
    tortoise = scm_api_cdr(tortoise);
    if (scm_obj_null_p(tortoise))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(lst))
      return scm_api_bool_true();
    else if (!scm_capi_pair_p(lst))
      return scm_api_bool_false();

    rabbit = scm_api_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(rabbit))
      return scm_api_bool_true();
    else if (!scm_capi_pair_p(lst))
      return scm_api_bool_false();

    rabbit = scm_api_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_capi_nil_p(rabbit))
      return scm_api_bool_true();
    else if (!scm_capi_pair_p(lst))
      return scm_api_bool_false();
  } while (!scm_capi_eq_p(tortoise, rabbit));

  return scm_api_bool_false();
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


/*******************************************************************/
/*  Numeric                                                        */
/*******************************************************************/

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

extern inline bool
scm_capi_number_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_NUM) ? true : false;
}

extern inline bool
scm_capi_integer_p(ScmObj obj)
{
  if (!scm_capi_number_p(obj)) return false;

  return SCM_NUM_CALL_VFUNC(obj, integer_p);
}

extern inline bool
scm_capi_fixnum_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_FIXNUM_TYPE_INFO) ? true : false;
}

extern inline bool
scm_capi_bignum_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_BIGNUM_TYPE_INFO) ? true : false;
}

int
scm_capi_num_to_sword(ScmObj num, scm_sword_t *w)
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
scm_capi_num_to_size_t(ScmObj num, size_t *s)
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

static int
scm_capi_num_cop_fold(const char *op, int (*func)(ScmObj x, ScmObj y, bool *r),
                      size_t argc, ScmObj *argv, bool *rslt)
{
  bool cmp;
  int err;
  char err_msg[64];

  if (argc <= 1) {
    snprintf(err_msg, sizeof(err_msg), "%s: too few arguments", op);
    scm_capi_error(err_msg, 0);
    return -1;
  }

  if (argv == NULL) {
    snprintf(err_msg, sizeof(err_msg), "%s: invalid argument", op);
    scm_capi_error(err_msg, 0);
    return -1;
  }

  if (rslt != NULL) {
    *rslt = true;
    for (size_t i = 1; i < argc; i++) {
      err = func(argv[i - 1], argv[i], &cmp);
      if (err < 0) return -1;

      if (!cmp) {
        *rslt = false;
        return 0;
      }
    }
  }

  return 0;
}

static int
scm_capi_num_cop_va(const char *op,
                    int (*func)(size_t argc, ScmObj *argv, bool *r),
                    size_t n, va_list ap, bool *rslt)
{
  ScmObj ary[n];
  char err_msg[64];

  SCM_STACK_FRAME;

  scm_assert(op != NULL);

  if (n == 0) {
    snprintf(err_msg, sizeof(err_msg), "%s: too few arguments", op);
    scm_capi_error(err_msg, 0);
    return SCM_OBJ_NULL;
  }

  for (size_t i = 0; i < n; i++) {
    ary[i] = va_arg(ap, ScmObj);
    SCM_STACK_PUSH(ary + i);
  }

  return func(n, ary, rslt);
}


int
scm_capi_num_eq(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

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

  err = SCM_NUM_CALL_VFUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp == 0) ? true : false;

  return 0;
}

int
scm_capi_num_eq_ary(size_t argc, ScmObj *argv, bool *rslt)
{
  return scm_capi_num_cop_fold("=", scm_capi_num_eq, argc, argv, rslt);
}

int
scm_capi_num_eq_v(bool *rslt, size_t n, ...)
{
  int err;
  va_list ap;

  va_start(ap, n);
  err = scm_capi_num_cop_va("=", scm_capi_num_eq_ary, n, ap, rslt);
  va_end(ap);

  return err;
}

ScmObj
scm_api_num_eq_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_eq(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_num_eq_ary_P(size_t argc, ScmObj *argv)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_cop_fold("=", scm_capi_num_eq, argc, argv, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

int
scm_capi_num_lt(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

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

  err = SCM_NUM_CALL_VFUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp < 0) ? true : false;

  return 0;
}

int
scm_capi_num_lt_ary(size_t argc, ScmObj *argv, bool *rslt)
{
  return scm_capi_num_cop_fold("<", scm_capi_num_lt, argc, argv, rslt);
}

int
scm_capi_num_lt_v(bool *rslt, size_t n, ...)
{
  int err;
  va_list ap;

  va_start(ap, n);
  err = scm_capi_num_cop_va("<", scm_capi_num_lt_ary, n, ap, rslt);
  va_end(ap);

  return err;
}

ScmObj
scm_api_num_lt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_lt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_num_lt_ary_P(size_t argc, ScmObj *argv)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_cop_fold("<", scm_capi_num_lt, argc, argv, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

int
scm_capi_num_gt(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

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

  err = SCM_NUM_CALL_VFUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp > 0) ? true : false;

  return 0;
}

int
scm_capi_num_gt_ary(size_t argc, ScmObj *argv, bool *rslt)
{
  return scm_capi_num_cop_fold(">", scm_capi_num_gt, argc, argv, rslt);
}

int
scm_capi_num_gt_v(bool *rslt, size_t n, ...)
{
  int err;
  va_list ap;

  va_start(ap, n);
  err = scm_capi_num_cop_va(">", scm_capi_num_gt_ary, n, ap, rslt);
  va_end(ap);

  return err;
}

ScmObj
scm_api_num_gt_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_gt(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_num_gt_ary_P(size_t argc, ScmObj *argv)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_cop_fold(">", scm_capi_num_gt, argc, argv, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

int
scm_capi_num_le(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

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

  err = SCM_NUM_CALL_VFUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp <= 0) ? true : false;

  return 0;
}

int
scm_capi_num_le_ary(size_t argc, ScmObj *argv, bool *rslt)
{
  return scm_capi_num_cop_fold("<=", scm_capi_num_le, argc, argv, rslt);
}

int
scm_capi_num_le_v(bool *rslt, size_t n, ...)
{
  int err;
  va_list ap;

  va_start(ap, n);
  err = scm_capi_num_cop_va("<=", scm_capi_num_le_ary, n, ap, rslt);
  va_end(ap);

  return err;
}

ScmObj
scm_api_num_le_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_le(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_num_le_ary_P(size_t argc, ScmObj *argv)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_cop_fold("<=", scm_capi_num_le, argc, argv, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

int
scm_capi_num_ge(ScmObj n1, ScmObj n2, bool *rslt)
{
  int err, cmp;

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

  err = SCM_NUM_CALL_VFUNC(n1, cmp, n2, &cmp);
  if (err < 0) return -1;

  if (rslt != NULL)
    *rslt = (cmp >= 0) ? true : false;

  return 0;
}

int
scm_capi_num_ge_ary(size_t argc, ScmObj *argv, bool *rslt)
{
  return scm_capi_num_cop_fold(">=", scm_capi_num_ge, argc, argv, rslt);
}

int
scm_capi_num_ge_v(bool *rslt, size_t n, ...)
{
  int err;
  va_list ap;

  va_start(ap, n);
  err = scm_capi_num_cop_va(">=", scm_capi_num_ge_ary, n, ap, rslt);
  va_end(ap);

  return err;
}

ScmObj
scm_api_num_ge_P(ScmObj n1, ScmObj n2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_ge(n1, n2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

ScmObj
scm_capi_num_ge_ary_P(size_t argc, ScmObj *argv)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_num_cop_fold(">=", scm_capi_num_ge, argc, argv, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}

static ScmObj
scm_capi_num_bop_fold(const char *op, ScmObj (*func)(ScmObj x, ScmObj y),
                      ScmObj ini, size_t argc, ScmObj *argv)
{
  ScmObj rslt = SCM_OBJ_INIT;
  char err_msg[64];

  SCM_STACK_FRAME_PUSH(&ini, &rslt);

  scm_assert(op != NULL);

  if (argv == NULL) {
    snprintf(err_msg, sizeof(err_msg), "%s: invalid argument", op);
    scm_capi_error(err_msg, 0);
    return SCM_OBJ_NULL;
  }

  rslt = ini;
  for (size_t i = 0; i < argc; i++) {
    rslt = func(rslt, argv[i]);
    if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
  }

  return rslt;
}

static ScmObj
scm_capi_num_bop_va(const char *op, ScmObj (*func)(size_t argc, ScmObj *argv),
                    size_t n, va_list ap)
{
  ScmObj ary[n];
  char err_msg[64];

  SCM_STACK_FRAME;

  scm_assert(op != NULL);

  if (n == 0) {
    snprintf(err_msg, sizeof(err_msg), "%s: too few arguments", op);
    scm_capi_error(err_msg, 0);
    return SCM_OBJ_NULL;
  }

  for (size_t i = 0; i < n; i++) {
    ary[i] = va_arg(ap, ScmObj);
    SCM_STACK_PUSH(ary + i);
  }

  return func(n, ary);
}


ScmObj
scm_api_plus(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("+: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(x)) {
    scm_capi_error("+: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("+: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_VFUNC(x, plus, y);
}

ScmObj
scm_capi_plus_ary(size_t argc, ScmObj *argv)
{
  if (argc == 0) {
    scm_capi_error("+: too few arguments", 0);
    return SCM_OBJ_NULL;
  }

  if (argv == NULL) {
    scm_capi_error("+: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_num_bop_fold("+", scm_api_plus,
                               argv[0], argc - 1, argv + 1);
}

ScmObj
scm_capi_plus_v(size_t n, ...)
{
  ScmObj rslt = SCM_OBJ_INIT;
  va_list ap;

  va_start(ap, n);
  rslt = scm_capi_num_bop_va("+", scm_capi_plus_ary, n, ap);
  va_end(ap);

  return rslt;
}

ScmObj
scm_api_minus(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("-: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(x)) {
    scm_capi_error("-: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("-: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_VFUNC(x, minus, y);
}

ScmObj
scm_capi_minus_ary(size_t argc, ScmObj *argv)
{
  if (argc == 0) {
    scm_capi_error("-: too few arguments", 0);
    return SCM_OBJ_NULL;
  }

  if (argv == NULL) {
    scm_capi_error("-: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (argc == 1)
    return scm_capi_num_bop_fold("-", scm_api_minus,
                                 SCM_FIXNUM_ZERO, argc, argv);
  else
    return scm_capi_num_bop_fold("-", scm_api_minus,
                                 argv[0], argc - 1, argv + 1);
}

ScmObj
scm_capi_minus_v(size_t n, ...)
{
  ScmObj rslt = SCM_OBJ_INIT;
  va_list ap;

  va_start(ap, n);
  rslt = scm_capi_num_bop_va("-", scm_capi_minus_ary, n, ap);
  va_end(ap);

  return rslt;
}

ScmObj
scm_api_mul(ScmObj x, ScmObj y)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("*: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(x)) {
    scm_capi_error("*: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("*: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_VFUNC(x, mul, y);
}

ScmObj
scm_capi_mul_ary(size_t argc, ScmObj *argv)
{
  if (argc == 0) {
    scm_capi_error("*: too few arguments", 0);
    return SCM_OBJ_NULL;
  }

  if (argv == NULL) {
    scm_capi_error("*: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_capi_num_bop_fold("*", scm_api_mul,
                               argv[0], argc - 1, argv + 1);
}

ScmObj
scm_capi_mul_v(size_t n, ...)
{
  ScmObj rslt = SCM_OBJ_INIT;
  va_list ap;

  va_start(ap, n);
  rslt = scm_capi_num_bop_va("*", scm_capi_mul_ary, n, ap);
  va_end(ap);

  return rslt;
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

  return SCM_NUM_CALL_VFUNC(x, floor_div, y, q, r);
}

ScmObj
scm_capi_floor_quo(ScmObj x, ScmObj y)
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

  rslt = SCM_NUM_CALL_VFUNC(x, floor_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_capi_floor_rem(ScmObj x, ScmObj y)
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

  rslt = SCM_NUM_CALL_VFUNC(x, floor_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}

int
scm_capi_ceiling_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r)
{
  SCM_STACK_FRAME_PUSH(&x, &y);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("ceiling/: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("ceiling/: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("fceiling/: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  return SCM_NUM_CALL_VFUNC(x, ceiling_div, y, q, r);
}

ScmObj
scm_capi_ceiling_quo(ScmObj x, ScmObj y)
{
  ScmObj q = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &q);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("ceiling-quotient: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(x)) {
    scm_capi_error("ceiling-quotient: number required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_number_p(y)) {
    scm_capi_error("ceiling-quotient: number required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_VFUNC(x, ceiling_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_capi_ceiling_rem(ScmObj x, ScmObj y)
{
  ScmObj r = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&x, &y, &r);

  if (scm_obj_null_p(x) || scm_obj_null_p(y)) {
    scm_capi_error("ceiling-remainder: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(x)) {
    scm_capi_error("ceiling-remainder: integer required, but got", 1, x);
    return SCM_OBJ_NULL;
  }

  if (!scm_capi_integer_p(y)) {
    scm_capi_error("ceiling-remainder: integer required, but got", 1, y);
    return SCM_OBJ_NULL;
  }

  rslt = SCM_NUM_CALL_VFUNC(x, ceiling_div, y, NULL, SCM_CSETTER_L(r));
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

  return SCM_NUM_CALL_VFUNC(x, truncate_div, y, q, r);
}

ScmObj
scm_capi_truncate_quo(ScmObj x, ScmObj y)
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

  rslt = SCM_NUM_CALL_VFUNC(x, truncate_div, y, SCM_CSETTER_L(q), NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return q;
}

ScmObj
scm_capi_truncate_rem(ScmObj x, ScmObj y)
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

  rslt = SCM_NUM_CALL_VFUNC(x, truncate_div, y, NULL, SCM_CSETTER_L(r));
  if (rslt < 0) return SCM_OBJ_NULL;

  return r;
}


/*******************************************************************/
/*  charactor                                                      */
/*******************************************************************/

ScmObj
scm_capi_make_char(scm_char_t chr, SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not make character object: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }
  else if (!SCM_ENCODING_VFUNC_VALID_CHAR_P(enc)(chr)) {
    scm_capi_error("can not make character object: invalid sequence", 0);
    return SCM_OBJ_NULL;          /* provisional implemntation */
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_char_new(SCM_MEM_ALLOC_HEAP, chr, enc);
}

ScmObj
scm_api_make_char_newline(SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not make character object: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_LF_CHR(enc),
                      enc);
}

ScmObj
scm_api_make_char_space(SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not make character object: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_SP_CHR(enc),
                      enc);
}

extern inline bool
scm_capi_char_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_CHAR_TYPE_INFO) ? true : false;
}

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

  return SCM_ENCODING_VFUNC_CHAR_WIDTH(scm_char_encoding(chr))(c.bytes,
                                                               sizeof(c));
}

int
scm_capi_char_encoding(ScmObj chr, SCM_ENC_T *enc)
{
  if (scm_obj_null_p(chr)) {
    scm_capi_error("char-encoding: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_char_p(chr)) {
    scm_capi_error("char-encoding: character required, but got", 1, chr);
    return -1;
  }

  if (enc != NULL)
    *enc = scm_char_encoding(chr);

  return 0;
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
    scm_capi_error("char=?: number required, but got", 1, chr1);
    return -1;
  }
  else if (!scm_capi_char_p(chr2)) {
    scm_capi_error("char=?: number required, but got", 1, chr2);
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

ScmObj
scm_api_char_eq_P(ScmObj chr1, ScmObj chr2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_char_eq(chr1, chr2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}


/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

ScmObj
scm_capi_make_string_from_cstr(const char *str, SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not make string object: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
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
scm_capi_make_string_from_bin(const void *data, size_t size, SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not make string object: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
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

extern inline bool
scm_capi_string_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_STRING_TYPE_INFO) ? true : false;
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

int
scm_capi_string_encoding(ScmObj str, SCM_ENC_T *enc)
{
  if (scm_obj_null_p(str)) {
    scm_capi_error("string-encoding: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_string_p(str)) {
    scm_capi_error("string-encoding: string required, but got", 1, str);
    return -1;
  }

  if (enc != NULL)
    *enc = scm_string_encoding(str);

  return 0;
}


ssize_t
scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  ssize_t n;

  if (!scm_capi_string_p(str)) {
    scm_capi_error("can not get byte sequence from string: invalid argument", 0);
    return -1;
  }

  if (cstr == NULL || size == 0) return 0;

  n = (ssize_t)scm_string_bytesize(str);
  if (n < 0) return -1;

  if (size - 1 < (size_t)n) n = (ssize_t)size - 1;

  memcpy(cstr, scm_string_content(str), (size_t)n);
  cstr[n] = '\0';

  return n;
}

int
scm_capi_string_push(ScmObj str, scm_char_t chr, SCM_ENC_T enc)
{
  SCM_ENC_T s_enc;
  int rslt;

  if (!scm_capi_string_p(str)) {
    scm_capi_error("can not push character into string: invalid argument", 0);
    return -1;
  }

  rslt = scm_capi_string_encoding(str, &s_enc);
  if (rslt < 0) return -1;

  if (s_enc != enc) {
    scm_capi_error("can not push character into string: encoding mismatch", 0);
    return -1;
  }

  str = scm_string_push(str, chr);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  return 0;
}

ScmObj
scm_api_string_push(ScmObj str, ScmObj c)
{
  SCM_ENC_T s_enc, c_enc;
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

  rslt = scm_capi_string_encoding(str, &s_enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  rslt = scm_capi_char_encoding(str, &c_enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (s_enc != c_enc) {
    scm_capi_error("string-push: encoding mismatch", 0);
    return SCM_OBJ_NULL;
  }

  cv = scm_char_value(c);

  str = scm_string_push(str, cv);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  return str;
}

int
scm_capi_string_eq(ScmObj s1, ScmObj s2, bool *rslt)
{
  int err, cmp;

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

ScmObj
scm_api_string_eq_P(ScmObj s1, ScmObj s2)
{
  bool cmp;
  int rslt;

  rslt = scm_capi_string_eq(s1, s2, &cmp);
  if (rslt < 0) return SCM_OBJ_NULL;

  return cmp ? scm_api_bool_true() : scm_api_bool_false();
}


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

ScmObj
scm_capi_make_vector(size_t len, ScmObj fill)
{
  if (len > SSIZE_MAX) {
    scm_capi_error("make-vector: too long", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(fill))
    return scm_vector_new(SCM_MEM_ALLOC_HEAP, len, scm_api_undef());
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

  r = scm_capi_num_to_size_t(len, &sz);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_capi_make_vector(sz, fill);
}

extern inline bool
scm_capi_vector_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_VECTOR_TYPE_INFO) ? true : false;
}

ScmObj
scm_capi_vector_set(ScmObj vec, size_t idx, ScmObj obj)
{
  if (scm_obj_null_p(vec) || scm_obj_null_p(obj)) {
    scm_capi_error("vector-set!: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(vec, &SCM_VECTOR_TYPE_INFO)) {
    scm_capi_error("vector-set!: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (idx >= scm_vector_length(vec)) {
    scm_capi_error("vector-set!: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_set(vec, idx, obj);
}

ScmObj
scm_capi_vector_ref(ScmObj vec, size_t idx)
{
  if (scm_obj_null_p(vec)) {
    scm_capi_error("vector-ref: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(vec, &SCM_VECTOR_TYPE_INFO)) {
    scm_capi_error("vector-ref: vector required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }
  else if (idx >= scm_vector_length(vec)) {
    scm_capi_error("vector-ref: argument out of range", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vector_ref(vec, idx);
}

ssize_t
scm_capi_vector_length(ScmObj vec)
{
    if (scm_obj_null_p(vec)) {
    scm_capi_error("vector-length: invalid argument", 0);
    return -1;
  }
  else if (!scm_obj_type_p(vec, &SCM_VECTOR_TYPE_INFO)) {
    scm_capi_error("vector-length: vector required, but got", 1, vec);
    return -1;
  }

  return (ssize_t)scm_vector_length(vec);
}

ScmObj
scm_api_list_to_vector(ScmObj lst)
{
  ScmObj vec = SCM_OBJ_INIT, node = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t i;
  ssize_t n;

  SCM_STACK_FRAME_PUSH(&lst,
                       &vec, &node, &elm);

  if (scm_obj_null_p(lst)) {
    scm_capi_error("list->vector: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!(scm_capi_pair_p(lst) || scm_capi_nil_p(lst))) {
    scm_capi_error("list->vector: proper list required, but got", 1, vec);
    return SCM_OBJ_NULL;
  }

  n = scm_capi_length(lst);
  if (n < 0) return SCM_OBJ_NULL;;

  vec = scm_capi_make_vector((size_t)n, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  for (node = lst, i = 0;
       scm_capi_pair_p(node);
       node = scm_api_cdr(node), i++) {
    elm = scm_api_car(node);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    elm = scm_capi_vector_set(vec, i, elm);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(node)) {
    return SCM_OBJ_NULL;
  }
  else if (scm_capi_nil_p(node)) {
    return vec;
  }
  else {
    scm_capi_error("list->vector: improper list is passed", 0);
    return SCM_OBJ_NULL;
  }
}


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

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
  SCM_ENC_T enc;
  int rslt;

  SCM_STACK_FRAME_PUSH(&str);

  if (scm_obj_null_p(str)) {
    scm_capi_error("string->symbol: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if  (!scm_capi_string_p(str)) {
    scm_capi_error("string->symbol: string required, but got", 1, str);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_string_encoding(str, &enc);
  if (rslt < 0) return SCM_OBJ_NULL; /* provisional implemntation */

  if (enc != scm_capi_system_encoding())
    str = scm_string_encode(str, scm_capi_system_encoding());

  return scm_symtbl_symbol(scm_vm_symtbl(scm_vm_current_vm()), str);
}

ScmObj
scm_capi_make_symbol_from_cstr(const char *str, SCM_ENC_T enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&s);

  s = scm_capi_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s))
    return SCM_OBJ_NULL;        /* provisional implemntation */

  if (enc != SCM_ENC_SYS && enc != scm_capi_system_encoding()) {
    s = scm_string_encode(s, scm_capi_system_encoding());
    if (scm_obj_null_p(s))
      return SCM_OBJ_NULL;        /* provisional implemntation */
  }

  return scm_api_string_to_symbol(s);
}

ScmObj
scm_capi_make_symbol_from_bin(const void *data, size_t size, SCM_ENC_T enc)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&s);

  s = scm_capi_make_string_from_bin(data, size, enc);
  if (scm_obj_null_p(s))
    return SCM_OBJ_NULL;        /* provisional implemntation */

  if (enc != SCM_ENC_SYS && enc != scm_capi_system_encoding()) {
    s = scm_string_encode(s, scm_capi_system_encoding());
    if (scm_obj_null_p(s))
      return SCM_OBJ_NULL;        /* provisional implemntation */
  }

  return scm_api_string_to_symbol(s);
}

extern inline bool
scm_capi_symbol_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

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

extern inline ssize_t
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
/*  Port                                                           */
/*******************************************************************/

ScmObj
scm_capi_open_input_fd(int fd, SCM_PORT_BUF_T mode, SCM_ENC_T enc)
{
  if (fd < 0) {
    scm_capi_error("open-input-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }
  else if (mode >= SCM_PORT_NR_BUF_MODE) {
    scm_capi_error("open-input-fd: unknown buffering mode", 0);
    return SCM_OBJ_NULL;
  }
  else if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("open-input-fd: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_port_open_input_fd(fd, mode, enc);
}

ScmObj
scm_capi_open_output_fd(int fd, SCM_PORT_BUF_T mode, SCM_ENC_T enc)
{
  if (fd < 0) {
    scm_capi_error("open-output-fd: invalid file descriptor", 0);
    return SCM_OBJ_NULL;
  }
  else if (mode >= SCM_PORT_NR_BUF_MODE) {
    scm_capi_error("open-output-fd: unknown buffering mode", 0);
    return SCM_OBJ_NULL;
  }
  else if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("open-output-fd: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_port_open_output_fd(fd, mode, enc);
}

ScmObj
scm_capi_open_input_string_from_cstr(const char *str, SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("can not open input string port: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_port_open_input_string(str,
                                    (str == NULL)? 0 : strlen(str),
                                    enc);
}

ScmObj
scm_capi_open_output_string(SCM_ENC_T enc)
{
  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("open-output-string: unknown encoding", 0);
    return SCM_OBJ_NULL;
  }

  if (enc == SCM_ENC_SYS)
    enc = scm_capi_system_encoding();

  return scm_port_open_output_string(enc);
}

extern inline bool
scm_capi_input_port_p(ScmObj port)
{
  if (scm_obj_null_p(port))
    return false;

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_readable_p(port))
    return true;
  else
    return false;
}

extern inline ScmObj
scm_api_input_port_P(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("input-port?: invaid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_readable_p(port))
    return scm_api_bool_true();
  else
    return scm_api_bool_false();
}

extern inline bool
scm_capi_output_port_p(ScmObj port)
{
  if (scm_obj_null_p(port))
    return false;

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_writable_p(port))
    return true;
  else
    return false;
}

extern inline ScmObj
scm_api_output_port_P(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("output-port?: invaid argument", 0);
    return SCM_OBJ_NULL;         /* provisional implemntation */
  }

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_writable_p(port))
    return scm_api_bool_true();
  else
    return scm_api_bool_false();
}

int
scm_capi_port_encoding(ScmObj port, SCM_ENC_T *enc)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("port-encoding: invalid argument", 0);
    return -1;
  }
  else if (!scm_obj_type_p(port, &SCM_PORT_TYPE_INFO)) {
    scm_capi_error("port-encoding: port required, but got", 1, port);
    return -1;                  /* provisional implemntation */
  }

  *enc = scm_port_encoding(port);
  return 0;
}

int
scm_api_close_input_port(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("close-input-port: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("close-input-port: input-port required, but got", 1, port);
    return -1;
  }

  return scm_port_close(port);
}

int
scm_api_close_output_port(ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("close-output-port: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("close-output-port: output-port required, but got", 1, port);
    return -1;         /* provisional implemntation */
  }

  return scm_port_close(port);
}

ssize_t
scm_capi_read_raw(void *buf, size_t size, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }
  else if (buf == NULL) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }
  else if (size > SSIZE_MAX) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }

  return scm_port_read(port, buf, size);
}

ssize_t
scm_capi_read_char(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }
  else if (chr == NULL) {
    scm_capi_error("read error: invalid argument", 0);
    return -1;
  }

  return scm_port_read_char(port, chr);
}

ssize_t
scm_capi_unread_raw(const void *buf, size_t size, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }
  else if (buf == NULL) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }
  else if (size > SSIZE_MAX) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }

  return scm_port_pushback(port, buf, size);
}

ssize_t
scm_capi_unread_char(const scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }
  else if (chr == NULL) {
    scm_capi_error("unread error: invalid argument", 0);
    return -1;
  }

  return scm_port_pushback_char(port, chr);
}

ssize_t
scm_capi_peek_raw(void *buf, size_t size, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }
  else if (buf == NULL) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }
  else if (size > SSIZE_MAX) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }

  return scm_port_peek(port, buf, size);
}

ssize_t
scm_capi_peek_char(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }
  else if (chr == NULL) {
    scm_capi_error("peek error: invalid argument", 0);
    return -1;
  }

  return scm_port_peek_char(port, chr);
}

ScmObj
scm_api_read(ScmObj port)
{
  ScmLexer *lexer;
  ScmParser *parser;

  if (scm_obj_null_p(port)) {
    port = scm_api_current_input_port();
  }
  else if (!scm_capi_input_port_p(port)) {
    scm_capi_error("read: input-port requried, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  lexer = scm_lexer_new();
  if (lexer == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  parser = scm_parser_new(lexer);
  if (parser == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_parser_parse_expression(parser, port);
}

ssize_t
scm_capi_write_raw(const void *buf, size_t size, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  if (buf == NULL) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  if (size > SSIZE_MAX) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  return scm_port_write(port, buf, size);
}

int
scm_capi_write_cstr(const char *str, SCM_ENC_T enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &s);

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  s = scm_capi_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return -1;

  s = scm_api_write_string(s, port);
  if (scm_obj_null_p(s)) return -1;

  return 0;
}

int
scm_capi_write_bin(const void *buf, size_t size, SCM_ENC_T enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &s);

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  if (enc >= SCM_ENC_NR_ENC) {
    scm_capi_error("write error: invalid argument", 0);
    return -1;
  }

  s = scm_capi_make_string_from_bin(buf, size, enc);
  if (scm_obj_null_p(s)) return -1;

  s = scm_api_write_string(s, port);
  if (scm_obj_null_p(s)) return -1;

  return 0;
}

ScmObj
scm_api_write_char(ScmObj chr, ScmObj port)
{
  SCM_ENC_T p_enc, c_enc;
  ssize_t rslt;

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
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-char: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_port_encoding(port, &p_enc);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_capi_char_encoding(chr, &c_enc);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (p_enc != c_enc) {
    chr = scm_char_encode(chr, p_enc);
    if (scm_obj_null_p(chr)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  rslt = scm_port_write_char(port, scm_char_value(chr));
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  return scm_api_undef();
}

ScmObj
scm_api_write_string(ScmObj str, ScmObj port)
{
  SCM_ENC_T p_enc, s_enc;
  ssize_t rslt, size;
  void *buf;

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
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-string: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_port_encoding(port, &p_enc);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  rslt = scm_capi_string_encoding(str, &s_enc);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (p_enc != s_enc) {
    str = scm_string_encode(str, p_enc);
    if (scm_obj_null_p(str)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  size = scm_capi_string_bytesize(str);
  if (size < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  buf = scm_capi_malloc((size_t)size + 1);
  if (buf == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_capi_string_to_cstr(str, buf, (size_t)size + 1);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */

  rslt = scm_capi_write_raw(buf, (size_t)size, port);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_api_undef();
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

  SCM_STACK_FRAME_PUSH(&obj, &port);

  if (scm_obj_null_p(obj)) {
    scm_capi_error("write-simple: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("write-simple: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_obj_call_pp_func(obj, port, true);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_api_undef();
}

ScmObj
scm_api_display(ScmObj obj, ScmObj port)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port);

  if (scm_obj_null_p(obj)) {
    scm_capi_error("display: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("display: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_obj_call_pp_func(obj, port, false);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_api_undef();
}

ScmObj
scm_api_newline(ScmObj port)
{
  SCM_ENC_T enc;
  scm_char_t nl;
  ssize_t w;
  int rslt;

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("newline: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_port_encoding(port, &enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  nl = SCM_ENCODING_CONST_LF_CHR(enc);
  w = (ssize_t)SCM_ENCODING_CONST_LF_WIDTH(enc);

  rslt = scm_capi_write_bin(nl.bytes, (size_t)w, enc, port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_api_undef();
}

ScmObj
scm_api_flush_output_port(ScmObj port)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&port);

  if (scm_obj_null_p(port)) {
    port = scm_api_current_output_port();
  }
  else if (!scm_capi_output_port_p(port)) {
    scm_capi_error("display: output-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  rslt = scm_port_flush(port);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_api_undef();
}

ssize_t
scm_capi_get_output_raw(ScmObj port, void *buf, size_t size)
{
  const void *p;
  ssize_t s;

  if (!scm_capi_output_port_p(port) || !scm_port_string_port_p(port)) {
    scm_capi_error("can not get byte sequence from output-string-port: "
                   "invalid argument ", 0);
    return -1;
  }
  else if (buf == NULL) {
    scm_capi_error("can not get byte sequence from output-string-port: "
                   "invalid argument ", 0);
    return -1;
  }

  p = scm_port_string_buffer(port);
  if (p == NULL) return -1;     /* [ERR]: [through] */

  s = scm_port_string_buffer_length(port);
  if (s < 0) return -1;         /* [ERR]: [through] */

  if ((size_t)s > size)
    s = (ssize_t)size;

  memcpy(buf, p, (size_t)s);

  return s;
}

ScmObj
scm_api_get_output_string(ScmObj port)
{
  const void *p;
  ssize_t s;
  SCM_ENC_T e;

  if (scm_obj_null_p(port)) {
    scm_capi_error("get-output-string: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_capi_output_port_p(port) || !scm_port_string_port_p(port)) {
    scm_capi_error("get-output-string: "
                   "output-string-port required, but got", 1, port);
    return SCM_OBJ_NULL;
  }

  p = scm_port_string_buffer(port);
  if (p == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  s = scm_port_string_buffer_length(port);
  if (s < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  e = scm_port_encoding(port);

  return scm_capi_make_string_from_bin(p, (size_t)s, e);
}

extern inline ScmObj
scm_api_current_input_port(void)
{
  return scm_vm_current_input_port(scm_vm_current_vm());
}

extern inline ScmObj
scm_api_current_output_port(void)
{
  return scm_vm_current_output_port(scm_vm_current_vm());
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
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj
scm_capi_make_subrutine(ScmSubrFunc func)
{
  if (func == NULL) {
    scm_capi_error("can not make subrutine: invaild argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_subrutine_new(SCM_MEM_ALLOC_HEAP, func);
}

ScmObj
scm_api_call_subrutine(ScmObj subr, int argc, ScmObj *argv)
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


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmObj
scm_capi_make_closure(ScmObj iseq, size_t nr_free_vars, ScmObj *sp)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not make closure: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (nr_free_vars > 0 && sp == NULL) {
    scm_capi_error("can not make closure: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_closure_new(SCM_MEM_ALLOC_HEAP, iseq, nr_free_vars, sp);
}

ScmObj
scm_capi_iseq_to_closure(ScmObj iseq)
{
  return scm_closure_new(SCM_MEM_ALLOC_HEAP, iseq, 0, NULL);
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


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

extern inline ScmObj
scm_api_make_syntax(int id, const char *keyword)
{
  return scm_syntax_new(SCM_MEM_HEAP, id, keyword);
}

extern inline bool
scm_capi_syntax_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_SYNTAX_TYPE_INFO) ? true : false);
}

extern inline int
scm_capi_syntax_id(ScmObj syx)
{
  if (!scm_capi_syntax_p(syx)) {
    scm_capi_error("can not get syntax-id: invalid argument", 0);
    return -1;
  }

  return scm_syntax_id(syx);
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

uint8_t *
scm_capi_iseq_to_ip(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not get instruction pointer from iseq: "
                   "invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return SCM_ISEQ_SEQ(iseq);
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
  ssize_t rslt;
  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;      /* [ERR: [through] */

  return scm_iseq_push_uint8(iseq, 0);
}

ssize_t
scm_capi_iseq_push_opfmt_obj(ScmObj iseq, SCM_OPCODE_T op, ScmObj val)
{
  ssize_t rslt, immv_idx;

  SCM_STACK_FRAME_PUSH(&iseq, &val);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  if (scm_obj_null_p(val)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  immv_idx = scm_iseq_push_immval(iseq, val);
  if (immv_idx < 0) return -1;          /* [ERR: [through] */

  if (immv_idx > UINT32_MAX) {
    scm_capi_error("can not push instruction to iseq: "
                   "immediate value area overflow", 0);
    return -1;
  }

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return scm_iseq_push_uint32(iseq, (uint32_t)immv_idx);
}

ssize_t
scm_capi_iseq_push_opfmt_si(ScmObj iseq, SCM_OPCODE_T op, int32_t val)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_uint32(iseq, (uint32_t)val);
}

ssize_t
scm_capi_iseq_push_opfmt_si_si(ScmObj iseq, SCM_OPCODE_T op,
                               int32_t val1, int32_t val2)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not push instruction to iseq: invalid argument", 0);
    return -1;
  }

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, (uint32_t)val1);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_uint32(iseq, (uint32_t)val2);
}

ssize_t
scm_capi_iseq_push_opfmt_si_obj(ScmObj iseq, SCM_OPCODE_T op,
                                int32_t val, ScmObj obj)
{
  ssize_t rslt, immv_idx;

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

  immv_idx = scm_iseq_push_immval(iseq, obj);
  if (immv_idx < 0) return -1;          /* [ERR: [through] */

  if (immv_idx > UINT32_MAX) {
    scm_capi_error("can not push instruction to iseq: "
                   "immediate value area overflow", 0);
    return -1;
  }

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, (uint32_t)val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return scm_iseq_push_uint32(iseq, (uint32_t)immv_idx);
}

ssize_t
scm_capi_iseq_push_opfmt_iof(ScmObj iseq, SCM_OPCODE_T op, int32_t offset)
{
  return scm_capi_iseq_push_opfmt_si(iseq, op, offset);
}

ssize_t
scm_capi_iseq_set_obj(ScmObj iseq, size_t idx, ScmObj val)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &val);

  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not update immediate value in iseq: "
                   "invalid argument", 0);
    return -1;
  }
  else if (idx > SSIZE_MAX || (ssize_t)idx >= scm_iseq_nr_immv(iseq)) {
    scm_capi_error("can not update immediate value in iseq: "
                   "invalid argument", 0);
    return -1;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("can not update immediate value in iseq: "
                   "invalid argument", 0);
    return -1;   /* provisional implemntation */
  }

  rslt = scm_iseq_set_immval(iseq, idx, val);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return (ssize_t)idx;
}

ssize_t
scm_capi_iseq_set_si(ScmObj iseq, size_t idx, int32_t val)
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

  rslt = scm_iseq_set_uint32(iseq, idx, (uint32_t)val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return (ssize_t)idx;
}

ScmObj
scm_capi_iseq_ref_obj(ScmObj iseq, size_t idx)
{
  if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not get immediate value from iseq: "
                   "invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (idx > SSIZE_MAX || (ssize_t)idx >= scm_iseq_nr_immv(iseq)) {
    scm_capi_error("can not get immediate value from iseq: "
                   "invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_iseq_get_immval(iseq, idx);;
}

int
scm_capi_opcode_to_opfmt(int opcode)
{
  static int tbl[] = {
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_NOP */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_HALT */
    SCM_OPFMT_SI,               /* SCM_OPCODE_CALL */
    SCM_OPFMT_SI_SI,            /* SCM_OPCODE_TAIL_CALL */
    SCM_OPFMT_SI,               /* SCM_OPCODE_RETURN */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_FRAME */
    SCM_OPFMT_OBJ,              /* SCM_OPCODE_IMMVAL */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_PUSH */
    SCM_OPFMT_OBJ,              /* SCM_OPCODE_GREF */
    SCM_OPFMT_OBJ,              /* SCM_OPCODE_GDEF */
    SCM_OPFMT_OBJ,              /* SCM_OPCODE_GSET */
    SCM_OPFMT_SI,               /* SCM_OPCODE_SREF */
    SCM_OPFMT_SI,               /* SCM_OPCODE_SSET */
    SCM_OPFMT_SI,               /* SCM_OPCODE_CREF */
    SCM_OPFMT_SI,               /* SCM_OPCODE_CSET */
    SCM_OPFMT_IOF,              /* SCM_OPCODE_JMP */
    SCM_OPFMT_IOF,              /* SCM_OPCODE_JMPF */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_RAISE */
    SCM_OPFMT_SI,               /* SCM_OPCODE_BOX */
    SCM_OPFMT_NOOPD,            /* SCM_OPCODE_UNBOX */
    SCM_OPFMT_SI_OBJ,           /* SCM_OPCODE_CLOSE */
  };

  if (opcode < 0 || sizeof(tbl)/sizeof(tbl[0]) <= (size_t)opcode) {
    scm_capi_error("can not get opcode format id: invalid opcode", 0);
    return -1;
  }

  return tbl[opcode];
}

uint8_t *
scm_capi_inst_fetch_oprand_obj(uint8_t *ip, ScmObj iseq,
                               size_t *idx, scm_csetter_t *obj)
{
  ScmObj opr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq,
                       &opr);

  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (!scm_capi_iseq_p(iseq)) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (idx == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }
  else if (obj == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  SCM_CAPI_INST_FETCH_UINT32(ip, *idx);

  opr = scm_capi_iseq_ref_obj(iseq, *idx);
  if (scm_obj_null_p(opr)) return NULL;

  scm_csetter_setq(obj, opr);

  return ip;
}

uint8_t *
scm_capi_inst_fetch_oprand_si(uint8_t *ip, int32_t *si)
{
  if (ip == NULL) {
    scm_capi_error("can not fetch operands: invalid ip", 0);
    return NULL;
  }
  else if (si == NULL) {
    scm_capi_error("can not fetch operands: invalid argument", 0);
    return NULL;
  }

  SCM_CAPI_INST_FETCH_INT32(ip, *si);

  return ip;
}

uint8_t *
scm_capi_inst_fetch_oprand_si_si(uint8_t *ip, int32_t *si1, int32_t *si2)
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

  SCM_CAPI_INST_FETCH_INT32(ip, *si1);
  SCM_CAPI_INST_FETCH_INT32(ip, *si2);

  return ip;
}

uint8_t *
scm_capi_inst_fetch_oprand_iof(uint8_t *ip, int32_t *offset)
{
  return scm_capi_inst_fetch_oprand_si(ip, offset);
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

ScmObj
scm_api_compile(ScmObj exp)
{
  if (scm_obj_null_p(exp)) {
    scm_capi_error("compile: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  return scm_cmpl_compile(exp);
}


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

int
scm_capi_global_var_ref(ScmObj sym, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &gloc);

  if (scm_obj_null_p(sym)) {
    scm_capi_error("can not get value of global variable: invalid argument", 0);
    return -1;
  }
  else if (!scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO)) {
    scm_capi_error("can not get value of global variable: invalid argument", 0);
    return -1;
  }

  rslt = scm_gloctbl_find(scm_vm_gloctbl(scm_vm_current_vm()),
                          sym, SCM_CSETTER_L(gloc));
  if (rslt != 0) return -1; /* [ERR]: [through] */

  if (scm_obj_null_p(gloc))
    scm_csetter_setq(val, SCM_OBJ_NULL);
  else
    scm_csetter_setq(val, scm_gloc_value(gloc));

  return 0;
}

ScmObj
scm_api_global_var_ref(ScmObj sym)
{
  ScmObj val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &val);

  rslt = scm_capi_global_var_ref(sym, SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_capi_error("unbound variable", 1, sym);
    return SCM_OBJ_NULL;
  }

  return val;
}

bool
scm_capi_global_var_bound_p(ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &gloc);

  if (scm_obj_null_p(sym)) return false;
  if (!scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO)) return false;

  rslt = scm_gloctbl_find(scm_vm_gloctbl(scm_vm_current_vm()),
                          sym, SCM_CSETTER_L(gloc));
  if (rslt != 0) return false;

  return scm_obj_null_p(gloc) ? false : true;
}

ScmObj
scm_api_global_var_define(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  gloc = scm_gloctbl_bind(scm_vm_gloctbl(scm_vm_current_vm()), sym, val);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL;

  return scm_api_undef();
}

ScmObj
scm_api_global_var_set(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (!scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }
  else if (scm_obj_null_p(val)) {
    scm_capi_error("can not bind global variable: invalid argument", 0);
    return SCM_OBJ_NULL;
  }

  rslt = scm_gloctbl_find(scm_vm_gloctbl(scm_vm_current_vm()),
                          sym, SCM_CSETTER_L(gloc));
  if (rslt != 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_obj_null_p(gloc)) {
    scm_capi_error("unbound variable", 1, sym);
    return SCM_OBJ_NULL;
  }

  gloc = scm_gloctbl_bind(scm_vm_gloctbl(scm_vm_current_vm()), sym, val);
  if (scm_obj_null_p(gloc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return val;
}


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int
scm_capi_trampolining(ScmObj target, ScmObj args, int nr_arg_cf,
                      ScmObj (*callback)(int argc, ScmObj *argv))
{
  if ((!scm_capi_iseq_p(target) && !scm_capi_closure_p(target))) {
    scm_capi_error("", 0);
    return SCM_OBJ_NULL;
  }
  else if ((!scm_capi_pair_p(args) && !scm_capi_nil_p(args))) {
    scm_capi_error("", 0);
    return SCM_OBJ_NULL;
  }

  return scm_vm_setup_stat_trmp(scm_vm_current_vm(),
                                target, args, nr_arg_cf, callback);
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

  return scm_api_undef();
}


/*******************************************************************/
/*  System Environment                                             */
/*******************************************************************/

SCM_ENC_T
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
  int rslt, ret;

  ret = -1;

  if (ev == NULL) return ret;

  ev->vm = scm_vm_new();
  if (scm_obj_null_p(ev->vm)) return ret; /* [ERR]: [through] */

  scm_vm_change_current_vm(ev->vm);

  SCM_STACK_FRAME_PUSH(&port, &asmbl, &iseq);

  scm_vm_setup_system(ev->vm);

  port = scm_capi_open_input_string_from_cstr("("
                                              " (label loop)"
                                              "   (frame)"
                                              "   (immval \"> \")"
                                              "   (push)"
                                              "   (gref display)"
                                              "   (call 1)"
                                              "   (frame)"
                                              "   (gref flush-output-port)"
                                              "   (call 0)"
                                              "   (frame)"
                                              "   (frame)"
                                              "   (frame)"
                                              "   (gref read)"
                                              "   (call 0)"
                                              "   (push)"
                                              "   (gref eval-asm)"
                                              "   (call 1)"
                                              "   (push)"
                                              "   (gref write)"
                                              "   (call 1)"
                                              "   (frame)"
                                              "   (gref newline)"
                                              "   (call 0)"
                                              "   (frame)"
                                              "   (gref flush-output-port)"
                                              "   (call 0)"
                                              "   (jmp loop)"
                                              ")",
                                              SCM_ENC_UTF8);
  if (scm_obj_null_p(port)) goto end;

  asmbl = scm_api_read(port);
  if (scm_obj_null_p(asmbl)) goto end;

  rslt = scm_api_close_input_port(port);
  if (rslt < 0) goto end;

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


/* unit test 用 api */
void
scm_capi_setup_current_vm(ScmEvaluator *ev)
{
  if (ev == NULL) return;

  ev->vm = scm_vm_new();
  if (scm_obj_null_p(ev->vm)) return;

  scm_vm_change_current_vm(ev->vm);
  scm_vm_setup_system(ev->vm);
}
