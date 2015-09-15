#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/bedrock.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/number.h"
#include "scythe/vector.h"
#include "scythe/iseq.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"
#include "scythe/string.h"
#include "scythe/symbol.h"
#include "scythe/vector.h"


/***************************************************************************/
/*  Equivalence (interface)                                                */
/***************************************************************************/

bool
scm_eq_p(ScmObj obj1, ScmObj obj2)
{
  return scm_obj_same_instance_p(obj1, obj2);
}

ScmObj
scm_eq_P(ScmObj obj1, ScmObj obj2)
{
  return (scm_eq_p(obj1, obj2) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

int
scm_eqv(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  bool cmp;

  SCM_REFSTK_INIT_REG(&obj1, &obj2);

  if (scm_eq_p(obj1, obj2)) {
    cmp = true;
  }
  else if (scm_number_p(obj1)) {
    if (scm_number_p(obj2)) {
      int r = scm_num_eq(obj1, obj2, &cmp);
      if (r < 0) return -1;
    }
    else {
      cmp = false;
    }
  }
  else if (!scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2))) {
    cmp = false;
  }
  else if (scm_symbol_p(obj1)) {
    cmp = scm_symbol_eq_p(obj1, obj2);
  }
  else if (scm_char_p(obj1)) {
    int r = scm_char_eq(obj1, obj2, &cmp);
    if (r < 0) return -1;
  }
  else {
    cmp = false;
  }

  if (rslt != NULL) *rslt = cmp;
  return 0;
}

ScmObj
scm_eqv_P(ScmObj obj1, ScmObj obj2)
{
  bool cmp;
  int r;

  r = scm_eqv(obj1, obj2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

enum { NON_CIRCULATIVE,
       SAME_CIRCULATION,
       DIFFERENT_CIRCULATION };

static int
equal_check_circular(ScmObj obj1, ScmObj obj2,
                     ScmObj stack1, ScmObj stack2, int *rslt)
{
  ScmObj elm1 = SCM_OBJ_INIT, elm2 = SCM_OBJ_INIT;
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  size_t cnt1 = 0, cnt2 = 0;

  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));
  scm_assert(scm_nil_p(stack1) || scm_pair_p(stack1));
  scm_assert(scm_nil_p(stack2) || scm_pair_p(stack2));
  scm_assert(rslt != NULL);

  SCM_REFSTK_INIT_REG(&obj1, &obj2, &stack1, &stack2,
                      &elm1, &elm2,
                      &lst1, &lst2);

  for (lst1 = stack1, cnt1 = 0;
       scm_pair_p(lst1);
       lst1 = scm_cdr(lst1), cnt1++) {
    elm1 = scm_car(lst1);
    if (scm_obj_null_p(elm1)) return -1;

    if (scm_eq_p(obj1, elm1)) break;
  }
  if (scm_obj_null_p(lst1)) return -1;

  if (!scm_nil_p(lst1)) {     /* 循環構造がある */
    for (lst2 = stack2, cnt2 = 0;
         cnt2 <= cnt1 && scm_pair_p(lst2);
         lst2 = scm_cdr(lst2), cnt2++) {
      elm2 = scm_car(lst2);
      if (scm_obj_null_p(elm2)) return -1;

      if (scm_eq_p(obj2, elm2)) break;
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
equal_aux(ScmObj obj1, ScmObj obj2, ScmObj stack1, ScmObj stack2, bool *rslt)
{
  ScmObj elm1 = SCM_OBJ_INIT, elm2 = SCM_OBJ_INIT;
  bool cmp;
  int r, cir;

  scm_assert(scm_nil_p(stack1) || scm_pair_p(stack1));
  scm_assert(scm_nil_p(stack2) || scm_pair_p(stack2));

  SCM_REFSTK_INIT_REG(&obj1, &obj2, &stack1, &stack2,
                      &elm1, &elm2);

  r = scm_eqv(obj1, obj2, &cmp);
  if (r < 0) return -1;

  if (!cmp  && scm_type_info_same_p(scm_obj_type(obj1), scm_obj_type(obj2))) {
    if (scm_pair_p(obj1) || scm_vector_p(obj1)) {
      r = equal_check_circular(obj1, obj2, stack1, stack2, &cir);
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
        stack1 = scm_cons(obj1, stack1);
        if (scm_obj_null_p(stack1)) return -1;

        stack2 = scm_cons(obj2, stack2);
        if (scm_obj_null_p(stack2)) return -1;
      }
    }

    if (scm_string_p(obj1)) {
      r = scm_string_eq(obj1, obj2, &cmp);
      if (r < 0) return -1;
    }
    else if (scm_bytevector_p(obj1)) {
      cmp = (scm_bytevector_cmp(obj1, obj2) == 0) ? true : false;
    }
    else if (scm_pair_p(obj1)) {
      elm1 = scm_car(obj1);
      if (scm_obj_null_p(elm1)) return -1;

      elm2 = scm_car(obj2);
      if (scm_obj_null_p(elm2)) return -1;

      r = equal_aux(elm1, elm2, stack1, stack2, &cmp);
      if (r < 0) return -1;

      if (cmp) {
        elm1 = scm_cdr(obj1);
        if (scm_obj_null_p(elm1)) return -1;

        elm2 = scm_cdr(obj2);
        if (scm_obj_null_p(elm2)) return -1;

        r = equal_aux(elm1, elm2, stack1, stack2, &cmp);
        if (r < 0) return -1;
      }
    }
    else if (scm_vector_p(obj1)) {
      size_t len1 = scm_vector_length(obj1);
      size_t len2 = scm_vector_length(obj2);

      if (len1 != len2) {
        cmp = false;
      }
      else {
        cmp = true;
        for (size_t i = 0; i < len1; i++) {
          elm1 = scm_vector_ref(obj1, (size_t)i);
          if (scm_obj_null_p(elm1)) return SCM_OBJ_NULL;

          elm2 = scm_vector_ref(obj2, (size_t)i);
          if (scm_obj_null_p(elm2)) return SCM_OBJ_NULL;

          r = equal_aux(elm1, elm2, stack1, stack2, &cmp);
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
scm_equal(ScmObj obj1, ScmObj obj2, bool *rslt)
{
  return equal_aux(obj1, obj2, SCM_NIL_OBJ, SCM_NIL_OBJ, rslt);
}

ScmObj
scm_equal_P(ScmObj obj1, ScmObj obj2)
{
  bool cmp;
  int r;

  r = scm_equal(obj1, obj2, &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

