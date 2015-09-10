#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/fcd.h"
#include "scythe/pair.h"

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

int
scm_pair_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  return SCM_OBJ_PRINT_HANDLER_PRINT_LIST(handler, obj, port, kind);
}

void
scm_pair_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  SCM_PAIR_CAR(obj) = SCM_OBJ_NULL;
  SCM_PAIR_CDR(obj) = SCM_OBJ_NULL;
}


int
scm_pair_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CAR(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CDR(obj), mem);
  return rslt;
}


/****************************************************************************/
/*  Pair (interface)                                                        */
/****************************************************************************/

bool
scm_fcd_pair_p(ScmObj pair)
{
  return scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO);
}

ScmObj
scm_fcd_pair_P(ScmObj pair)
{
  return scm_fcd_pair_p(pair) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_pair_new(scm_mem_type_t mtype, ScmObj car, ScmObj cdr)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  pair = scm_fcd_mem_alloc(&SCM_PAIR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  if (scm_pair_initialize(pair, car, cdr) < 0)
    return SCM_OBJ_NULL;

  return pair;
}

ScmObj
scm_fcd_cons(ScmObj car, ScmObj cdr)
{
  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));
  return scm_fcd_pair_new(SCM_MEM_HEAP, car, cdr);
}

ScmObj
scm_fcd_car(ScmObj pair)
{
  scm_assert(scm_fcd_pair_p(pair));
  return scm_pair_car(pair);
}

ScmObj
scm_fcd_cdr(ScmObj pair)
{
  scm_assert(scm_fcd_pair_p(pair));
  return scm_pair_cdr(pair);
}

void
scm_fcd_set_car_i(ScmObj pair, ScmObj elm)
{
  scm_assert(scm_fcd_pair_p(pair));
  scm_assert(scm_obj_not_null_p(elm));
  scm_pair_set_car(pair, elm);
}

void
scm_fcd_set_cdr_i(ScmObj pair, ScmObj elm)
{
  scm_assert(scm_fcd_pair_p(pair));
  scm_assert(scm_obj_not_null_p(elm));
  scm_pair_set_cdr(pair, elm);
}

ScmObj
scm_fcd_cxr(ScmObj pair, const char *dir)
{
  ScmObj x = SCM_OBJ_INIT;
  size_t i;

  SCM_REFSTK_INIT_REG(&pair,
                      &x);

  scm_assert(dir != NULL);

  x = pair;
  for (i = strlen(dir); i > 0; i--) {
    if (!scm_fcd_pair_p(x)) {
      scm_fcd_error("failed to execute cxr: pair required, but got", 1, x);
      return SCM_OBJ_NULL;
    }

    switch (dir[i - 1]) {
    case 'a':                   /* fall through */
    case 'A':
      x = scm_fcd_car(x);
      break;
    case 'd':                   /* fall through */
    case 'D':
      x = scm_fcd_cdr(x);
      break;
    default:
      scm_fcd_error("failed to execute cxr: invalid argument", 0);
      return SCM_OBJ_NULL;
    }
  }

  return x;
}

ScmObj
scm_fcd_list_P(ScmObj lst)
{
  ScmObj rabbit = SCM_OBJ_INIT, tortoise = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &rabbit, &tortoise);

  if (scm_fcd_nil_p(lst))
    return SCM_TRUE_OBJ;
  else if (!scm_fcd_pair_p(lst))
    return SCM_FALSE_OBJ;

  rabbit = tortoise = lst;

  do {
    tortoise = scm_fcd_cdr(tortoise);
    if (scm_obj_null_p(tortoise))
      return SCM_OBJ_NULL;
    else if (scm_fcd_nil_p(lst))
      return SCM_TRUE_OBJ;
    else if (!scm_fcd_pair_p(lst))
      return SCM_FALSE_OBJ;

    rabbit = scm_fcd_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_fcd_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_fcd_pair_p(rabbit))
      return SCM_FALSE_OBJ;

    rabbit = scm_fcd_cdr(rabbit);
    if (scm_obj_null_p(rabbit))
      return SCM_OBJ_NULL;
    else if (scm_fcd_nil_p(rabbit))
      return SCM_TRUE_OBJ;
    else if (!scm_fcd_pair_p(rabbit))
      return SCM_FALSE_OBJ;
  } while (!scm_fcd_eq_p(tortoise, rabbit));

  return SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_make_list(size_t n, ScmObj fill)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&fill,
                      &lst);

  if (scm_obj_null_p(fill))
    fill = scm_fcd_undef();

  lst = scm_fcd_nil();
  for (size_t i = 0; i < n; i++) {
    lst = scm_fcd_cons(fill, lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_fcd_list_cv(const ScmObj *elm, size_t n)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst);

  if (elm == NULL)
    return scm_fcd_nil();

  lst = scm_fcd_nil();
  for (size_t i = n; i > 0; i--) {
    if (scm_obj_null_p(elm[i - 1])) {
      scm_fcd_error("list: invalid argument", 1, elm[i - 1]);
      return SCM_OBJ_NULL;
    }
    lst = scm_fcd_cons(elm[i - 1], lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

ScmObj
scm_fcd_list(size_t n, ...)
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
scm_fcd_length(ScmObj lst)
{
  ScmObj node = SCM_OBJ_INIT;
  size_t n;

  SCM_REFSTK_INIT_REG(&node);

  scm_assert(scm_fcd_nil_p(lst) || scm_fcd_pair_p(lst));

  if (scm_fcd_nil_p(lst))
    return 0;

  for (node = lst, n = 0; scm_fcd_pair_p(node); node = scm_fcd_cdr(node), n++) {
    if (n > SSIZE_MAX) {
      scm_fcd_error("length: too long list", 0);
      return -1;
    }
  }

  if (scm_fcd_nil_p(node)) {
    return (ssize_t)n;
  }
  else {
    scm_fcd_error("lenght: improper list is passed", 0);
    return -1;
  }
}

ScmObj
scm_fcd_append_lst(ScmObj lst)
{
  ScmObj arg[] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst, &l);
  SCM_REFSTK_REG_ARY(arg, sizeof(arg)/sizeof(arg[0]));

  scm_assert(scm_obj_not_null_p(lst));

  lst = scm_fcd_reverse(lst);
  if (!scm_fcd_pair_p(lst))
    return SCM_NIL_OBJ;

  arg[1] = scm_fcd_car(lst);
  for (l = scm_fcd_cdr(lst); scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    arg[0] = scm_fcd_car(l);
    arg[1] = scm_fcd_append_cv(arg, 2);
    if (scm_obj_null_p(arg[1])) return SCM_OBJ_NULL;
  }

  return arg[1];
}

ScmObj
scm_fcd_append_cv(const ScmObj *lists, size_t n)
{
  ScmObj tail = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  ScmObj new_lst = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, cur = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&tail, &lst, &elm,
                      &new_lst, &prv, &cur);

  if (lists == NULL || n == 0)
    return SCM_NIL_OBJ;

  if (scm_obj_null_p(lists[0])) {
    scm_fcd_error("failed to append list: invalid argument", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  if (n == 1)
    return lists[0];

  tail = scm_fcd_append_cv(lists + 1, n - 1);
  if (scm_obj_null_p(tail)) return SCM_OBJ_NULL;

  if (scm_fcd_nil_p(lists[0]))
    return tail;

  if (!scm_fcd_pair_p(lists[0])) {
    scm_fcd_error("failed to append list: list required, but got", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  new_lst = prv = SCM_OBJ_NULL;
  for (lst = lists[0]; scm_fcd_pair_p(lst); lst = scm_fcd_cdr(lst)) {
    elm = scm_fcd_car(lst);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    cur = scm_fcd_cons(elm, SCM_NIL_OBJ);
    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prv)) {
      scm_fcd_set_cdr_i(prv, cur);
    }
    else {
      new_lst = cur;
    }

    prv = cur;
  }

  if (!scm_fcd_nil_p(lst)) {
    scm_fcd_error("failed to append lists: invalid argument", 1, lists[0]);
    return SCM_OBJ_NULL;
  }

  scm_fcd_set_cdr_i(prv, tail);

  return new_lst;
}

ScmObj
scm_fcd_append(size_t n, ...)
{
  ScmObj args[n];
  va_list ap;

  SCM_REFSTK_INIT;

  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  SCM_REFSTK_REG_ARY(args, n);

  return scm_fcd_append_cv(args, n);
}

ScmObj
scm_fcd_reverse(ScmObj lst)
{
  ScmObj new_lst = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &new_lst, &cur, &elm);

  scm_assert(scm_obj_not_null_p(lst));

  new_lst = scm_fcd_nil();
  for (cur = lst; scm_fcd_pair_p(cur); cur = scm_fcd_cdr(cur)) {
    elm = scm_fcd_car(cur);
    new_lst = scm_fcd_cons(elm, new_lst);
    if (scm_obj_null_p(new_lst)) return SCM_OBJ_NULL;
  }

  return new_lst;
}

ScmObj
scm_fcd_list_tail(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (!scm_fcd_pair_p(l)) {
      scm_fcd_error("list-tail: argument out of range", 0);
      return SCM_OBJ_NULL;
    }
    l = scm_fcd_cdr(l);
  }

  return l;
}

static ScmObj
nth_pair(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  l = scm_fcd_list_tail(lst, n);
  if (!scm_fcd_pair_p(l)) {
    scm_fcd_error("failed to get Nth pair: out of range", 1, lst);
    return SCM_OBJ_NULL;
  }

  return l;
}

ScmObj
scm_fcd_list_ref(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  l = nth_pair(lst, n);
  if (scm_obj_null_p(l)) return SCM_OBJ_NULL;

  return scm_fcd_car(l);
}

int
scm_fcd_list_set_i(ScmObj lst, size_t n, ScmObj obj)
{
  ScmObj l = SCM_OBJ_NULL;

  l = nth_pair(lst, n);
  if (scm_obj_null_p(l)) return -1;

  scm_fcd_set_car_i(l, obj);
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

  for (l = lst; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    e = scm_fcd_car(l);
    c = cmp(e, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_fcd_true_p(c))
      return l;
  }

  return SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_memq(ScmObj obj, ScmObj lst)
{
  return member_aux(obj, lst, scm_fcd_eq_P);
}

ScmObj
scm_fcd_memv(ScmObj obj, ScmObj lst)
{
  return member_aux(obj, lst, scm_fcd_eqv_P);
}

ScmObj
scm_fcd_member(ScmObj obj, ScmObj lst, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return member_aux(obj, lst, (cmp == NULL) ? scm_fcd_equal_P : cmp);
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

  for (l = alist; scm_fcd_pair_p(l); l = scm_fcd_cdr(l)) {
    e = scm_fcd_car(l);
    if (!scm_fcd_pair_p(e)) {
      scm_fcd_error("failed to oparate alist: pair required, but got", 1, e);
      return SCM_OBJ_NULL;
    }

    k = scm_fcd_car(e);
    if (scm_obj_null_p(k)) return SCM_OBJ_NULL;

    c = cmp(k, obj);
    if (scm_obj_null_p(c)) return SCM_OBJ_NULL;

    if (scm_fcd_true_p(c))
      return e;
  }

  return SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_assq(ScmObj obj, ScmObj alist)
{
  return assoc_aux(obj, alist, scm_fcd_eq_P);
}

ScmObj
scm_fcd_assv(ScmObj obj, ScmObj alist)
{
  return assoc_aux(obj, alist, scm_fcd_eqv_P);
}

ScmObj
scm_fcd_assoc(ScmObj obj, ScmObj alist, ScmObj (*cmp)(ScmObj x, ScmObj y))
{
  return assoc_aux(obj, alist, (cmp == NULL) ? scm_fcd_equal_P : cmp);
}

ScmObj
scm_fcd_list_copy(ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, pair = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst,
                      &cur, &elm, &nil,
                      &head, &pair, &prev,
                      &rslt);

  if (scm_fcd_nil_p(lst) || !scm_fcd_pair_p(lst))
    return lst;

  prev = SCM_OBJ_NULL;
  head = SCM_OBJ_NULL;
  for (cur = lst; scm_fcd_pair_p(cur); cur = scm_fcd_cdr(cur)) {
    elm = scm_fcd_car(cur);
    pair = scm_fcd_cons(elm, SCM_NIL_OBJ);
    if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prev))
      scm_fcd_set_cdr_i(prev, pair);
    else
      head = pair;

    prev = pair;
  }

  scm_fcd_set_cdr_i(prev, cur);
  return head;
}
