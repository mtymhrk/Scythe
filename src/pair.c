#include <sys/types.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/bedrock.h"
#include "scythe/refstk.h"
#include "scythe/equivalence.h"
#include "scythe/exception.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"


/****************************************************************************/
/*  Pair                                                                    */
/****************************************************************************/

ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  .name                = "pair",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_pair_obj_print,
  .obj_size            = sizeof(ScmPair),
  .gc_ini_func         = scm_pair_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_pair_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_pair_P(ScmObj pair)
{
  return scm_pair_p(pair) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr)
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  SCM_SLOT_SETQ(ScmPair, pair, car, car);
  SCM_SLOT_SETQ(ScmPair, pair, cdr, cdr);

  return 0;
}

ScmObj
scm_pair_new(scm_mem_type_t mtype, ScmObj car, ScmObj cdr)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  pair = scm_alloc_mem(&SCM_PAIR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  if (scm_pair_initialize(pair, car, cdr) < 0)
    return SCM_OBJ_NULL;

  return pair;
}

int
scm_pair_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  return SCM_OBJ_PRINT_HANDLER_PRINT_LIST(handler, obj, port, kind);
}

void
scm_pair_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  SCM_PAIR_CAR(obj) = SCM_OBJ_NULL;
  SCM_PAIR_CDR(obj) = SCM_OBJ_NULL;
}

int
scm_pair_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CAR(obj));
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CDR(obj));
  return rslt;
}


/****************************************************************************/
/*  Lists                                                                   */
/****************************************************************************/

ScmObj
scm_cxr(ScmObj pair, const char *dir)
{
  ScmObj x = SCM_OBJ_INIT;
  size_t i;

  SCM_REFSTK_INIT_REG(&pair,
                      &x);

  scm_assert(dir != NULL);

  x = pair;
  for (i = strlen(dir); i > 0; i--) {
    if (!scm_pair_p(x)) {
      scm_error("failed to execute cxr: pair required, but got", 1, x);
      return SCM_OBJ_NULL;
    }

    switch (dir[i - 1]) {
    case 'a':                   /* fall through */
    case 'A':
      x = scm_car(x);
      break;
    case 'd':                   /* fall through */
    case 'D':
      x = scm_cdr(x);
      break;
    default:
      scm_error("failed to execute cxr: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
  }

  return x;
}

ScmObj
scm_list_P(ScmObj lst)
{
  ScmObj rabbit = SCM_OBJ_INIT, tortoise = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &rabbit, &tortoise);

  if (scm_nil_p(lst))
    return SCM_TRUE_OBJ;
  else if (!scm_pair_p(lst))
    return SCM_FALSE_OBJ;

  rabbit = tortoise = lst;

  do {
    tortoise = scm_cdr(tortoise);
    if (scm_obj_null_p(tortoise))
      return SCM_OBJ_NULL;
    else if (scm_nil_p(lst))
      return SCM_TRUE_OBJ;
    else if (!scm_pair_p(lst))
      return SCM_FALSE_OBJ;

    rabbit = scm_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_pair_p(rabbit))
      return SCM_FALSE_OBJ;

    rabbit = scm_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_pair_p(rabbit))
      return SCM_FALSE_OBJ;
  } while (!scm_eq_p(tortoise, rabbit));

  return SCM_FALSE_OBJ;
}

ScmObj
scm_make_list(size_t n, ScmObj fill)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fill,
                      &lst);

  if (scm_obj_null_p(fill))
    fill = SCM_UNDEF_OBJ;

  lst = SCM_NIL_OBJ;
  for (size_t i = 0; i < n; i++) {
    lst = scm_cons(fill, lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_list_cv(const ScmObj *elm, size_t n)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  if (elm == NULL)
    return SCM_NIL_OBJ;

  lst = SCM_NIL_OBJ;
  for (size_t i = n; i > 0; i--) {
    if (scm_obj_null_p(elm[i - 1])) {
      scm_error("list: invalid argument", 1, elm[i - 1]);
      return SCM_OBJ_NULL;
    }
    lst = scm_cons(elm[i - 1], lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_list(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_list_cv(args, n);
}

ssize_t
scm_length(ScmObj lst)
{
  ScmObj node = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&node);

  scm_assert(scm_nil_p(lst) || scm_pair_p(lst));

  if (scm_nil_p(lst))
    return 0;

  for (node = lst, n = 0; scm_pair_p(node); node = scm_cdr(node), n++) {
    if (n > SSIZE_MAX) {
      scm_error("length: too long list", 0);
      return -1;
    }
  }

  if (scm_nil_p(node)) {
    return (ssize_t)n;
  }
  else {
    scm_error("lenght: improper list is passed", 0);
    return -1;
  }
}

ScmObj
scm_append_lst(ScmObj lst)
{
  ScmObj arg[] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &l);
  SCM_REFSTK_REG_ARY(arg, sizeof(arg)/sizeof(arg[0]));

  scm_assert(scm_obj_not_null_p(lst));

  lst = scm_reverse(lst);
  if (!scm_pair_p(lst))
    return SCM_NIL_OBJ;

  arg[1] = scm_car(lst);
  for (l = scm_cdr(lst); scm_pair_p(l); l = scm_cdr(l)) {
    arg[0] = scm_car(l);
    arg[1] = scm_append_cv(arg, 2);
    if (scm_obj_null_p(arg[1])) return SCM_OBJ_NULL;
  }

  return arg[1];
}

ScmObj
scm_append_cv(const ScmObj *lists, size_t n)
{
  ScmObj tail = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj new_lst = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, cur = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&tail, &lst, &elm,
                      &new_lst, &prv, &cur);

  if (lists == NULL || n == 0)
    return SCM_NIL_OBJ;

  if (scm_obj_null_p(lists[0])) {
    scm_error("failed to append list: invalid argument", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  if (n == 1)
    return lists[0];

  tail = scm_append_cv(lists + 1, n - 1);
  if (scm_obj_null_p(tail)) return SCM_OBJ_NULL;

  if (scm_nil_p(lists[0]))
    return tail;

  if (!scm_pair_p(lists[0])) {
    scm_error("failed to append list: list required, but got", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  new_lst = prv = SCM_OBJ_NULL;
  for (lst = lists[0]; scm_pair_p(lst); lst = scm_cdr(lst)) {
    elm = scm_car(lst);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    cur = scm_cons(elm, SCM_NIL_OBJ);
    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prv)) {
      scm_set_cdr(prv, cur);
    }
    else {
      new_lst = cur;
    }

    prv = cur;
  }

  if (!scm_nil_p(lst)) {
    scm_error("failed to append lists: invalid argument", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  scm_set_cdr(prv, tail);

  return new_lst;
}

ScmObj
scm_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_append_cv(args, n);
}

ScmObj
scm_reverse(ScmObj lst)
{
  ScmObj new_lst = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &new_lst, &cur, &elm);

  scm_assert(scm_obj_not_null_p(lst));

  new_lst = SCM_NIL_OBJ;
  for (cur = lst; scm_pair_p(cur); cur = scm_cdr(cur)) {
    elm = scm_car(cur);
    new_lst = scm_cons(elm, new_lst);
    if (scm_obj_null_p(new_lst)) return SCM_OBJ_NULL;
  }

  return new_lst;
}

ScmObj
scm_list_tail(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (!scm_pair_p(l)) {
      scm_error("list-tail: argument out of range", 0);
      return SCM_OBJ_NULL;
    }
    l = scm_cdr(l);
  }

  return l;
}

static ScmObj
nth_pair(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  l = scm_list_tail(lst, n);
  if (!scm_pair_p(l)) {
    scm_error("failed to get Nth pair: out of range", 1, lst);
    return SCM_OBJ_NULL;
  }

  return l;
}

ScmObj
scm_list_ref(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  l = nth_pair(lst, n);
  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return scm_car(l);
}

int
scm_list_set(ScmObj lst, size_t n, ScmObj obj)
{
  ScmObj l = SCM_OBJ_NULL;

  l = nth_pair(lst, n);
  if (scm_obj_null_p(l)) return -1;

  scm_set_car(l, obj);
  return 0;
}

static ScmObj
member_aux(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT, c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj, &lst,
                      &l, &e, &c);

  scm_assert(cmp != NULL);

  if (scm_obj_null_p(lst))
    return SCM_FALSE_OBJ;

  for (l = lst; scm_pair_p(l); l = scm_cdr(l)) {
    e = scm_car(l);
    c = cmp(e, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_true_p(c))
      return l;
  }

  return SCM_FALSE_OBJ;
}

ScmObj
scm_memq(ScmObj obj, ScmObj lst)
{
  return member_aux(obj, lst, scm_eq_P);
}

ScmObj
scm_memv(ScmObj obj, ScmObj lst)
{
  return member_aux(obj, lst, scm_eqv_P);
}

ScmObj
scm_member(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return member_aux(obj, lst, (cmp == NULL) ? scm_equal_P : cmp);
}

static ScmObj
assoc_aux(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  ScmObj l = SCM_OBJ_INIT, e = SCM_OBJ_INIT, k = SCM_OBJ_INIT, c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj, &alist,
                      &l, &e, &k, &c);

  scm_assert(cmp != NULL);

  if (scm_obj_null_p(alist))
    return SCM_FALSE_OBJ;

  for (l = alist; scm_pair_p(l); l = scm_cdr(l)) {
    e = scm_car(l);
    if (!scm_pair_p(e)) {
      scm_error("failed to oparate alist: pair required, but got", 1, e);
      return SCM_OBJ_NULL;
    }

    k = scm_car(e);
    if (scm_obj_null_p(k)) return SCM_OBJ_NULL;

    c = cmp(k, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_true_p(c))
      return e;
  }

  return SCM_FALSE_OBJ;
}

ScmObj
scm_assq(ScmObj obj, ScmObj alist)
{
  return assoc_aux(obj, alist, scm_eq_P);
}

ScmObj
scm_assv(ScmObj obj, ScmObj alist)
{
  return assoc_aux(obj, alist, scm_eqv_P);
}

ScmObj
scm_assoc(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return assoc_aux(obj, alist, (cmp == NULL) ? scm_equal_P : cmp);
}

ScmObj
scm_list_copy(ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, pair = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &cur, &elm, &nil,
                      &head, &pair, &prev,
                      &rslt);

  if (scm_nil_p(lst) || !scm_pair_p(lst))
    return lst;

  prev = SCM_OBJ_NULL;
  head = SCM_OBJ_NULL;
  for (cur = lst; scm_pair_p(cur); cur = scm_cdr(cur)) {
    elm = scm_car(cur);
    pair = scm_cons(elm, SCM_NIL_OBJ);
    if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prev))
      scm_set_cdr(prev, pair);
    else
      head = pair;

    prev = pair;
  }

  scm_set_cdr(prev, cur);
  return head;
}
