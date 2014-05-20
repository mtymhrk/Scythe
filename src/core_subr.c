
#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"


static ssize_t
scm_subr_list_to_cv(ScmObj lst, ScmObj *ary, size_t n)
{
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&lst);

  len = 0;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(lst)) {
      ary[i] = scm_api_car(lst);
      if (scm_obj_null_p(ary[i])) return -1;

      lst = scm_api_cdr(lst);
      if (scm_obj_null_p(lst)) return -1;

      len++;
    }
    else {
      ary[i] = SCM_OBJ_NULL;
    }
  }

  return len;
}

/*******************************************************************/
/*  Equivalence predicates                                         */
/*******************************************************************/

int
scm_subr_func_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_eq_P(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eqv_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_eqv_P(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_equal_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_equal_P(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

int
scm_subr_func_number_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_number_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_complex_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_complex_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_real_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_real_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_rational_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_rational_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_integer_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_integer_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_exact_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_exact_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_inexact_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_inexact_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_exact_integer_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_exact_integer_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_finite_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_finite_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_infinite_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_infinite_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_nan_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_nan_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_num_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_num_lt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_num_gt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_num_le_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_num_ge_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_zero_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_zero_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_positive_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_positive_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_negative_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_negative_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_odd_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_odd_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_even_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_even_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_max(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_max_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_min(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_min_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_plus(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_plus_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_mul(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_mul_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_minus(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_minus_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_div(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("/: not implemented", 0);
  return -1;
}

int
scm_subr_func_abs(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_abs(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_floor_div(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  int r;

  SCM_STACK_FRAME_PUSH(&subr);
  SCM_STACK_PUSH_ARY(val, sizeof(val)/sizeof(val[0]));

  r = scm_capi_floor_div(argv[0], argv[1],
                         SCM_CSETTER_L(val[0]), SCM_CSETTER_L(val[1]));
  if (r < 0) return -1;

  return scm_capi_return_val(val, sizeof(val)/sizeof(val[0]));
}

int
scm_subr_func_floor_quo(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_floor_quo(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_floor_rem(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_floor_rem(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_truncate_div(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  int r;

  SCM_STACK_FRAME_PUSH(&subr);
  SCM_STACK_PUSH_ARY(val, sizeof(val)/sizeof(val[0]));

  r = scm_capi_truncate_div(argv[0], argv[1],
                         SCM_CSETTER_L(val[0]), SCM_CSETTER_L(val[1]));
  if (r < 0) return -1;

  return scm_capi_return_val(val, sizeof(val)/sizeof(val[0]));
}

int
scm_subr_func_truncate_quo(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_truncate_quo(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_truncate_rem(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_truncate_rem(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/

int
scm_subr_func_not(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_not(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_boolean_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_boolean_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

int
scm_subr_func_pair_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_pair_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cons(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_car(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_car(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cdr(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_cdr(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_car_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_set_car_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_cdr_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_set_cdr_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_nil_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &fill);

  if (argc > 2) {
    scm_capi_error("make-list: too many arguments", 0);
    return -1;
  }

  fill = (argc > 1) ? argv[1] : SCM_OBJ_NULL;
  val = scm_api_make_list(argv[0], fill);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_return_val(argv, 1);
}

int
scm_subr_func_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_reverse(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_reverse(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_tail(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_tail(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_set_i(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_memq(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("memq: not implemented", 0);
  return -1;
}

int
scm_subr_func_memv(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("memv: not implemented", 0);
  return -1;
}

int
scm_subr_func_member(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("member: not implemented", 0);
  return -1;
}

int
scm_subr_func_assq(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assq: not implemented", 0);
  return -1;
}

int
scm_subr_func_assv(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assv: not implemented", 0);
  return -1;
}

int
scm_subr_func_assoc(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assoc: not implemented", 0);
  return -1;
}

int
scm_subr_func_list_copy(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_copy(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

int
scm_subr_func_symbol_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_symbol_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_symbol_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_symbol_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_symbol_to_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_symbol_to_string(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_to_symbol(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_to_symbol(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

int
scm_subr_func_char_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_char_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_char_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_char_lt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_char_gt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_char_le_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_char_ge_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_ci_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-ci=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_ci_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-ci<?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_ci_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-ci>?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_ci_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-ci<=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_ci_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-ci>=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_alphabetic_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-alphabetic?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_numeric_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-numeric?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_whitespace_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-whitespace?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_upper_case_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-upper-case?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_lower_case_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-lower-case?: not implemented", 0);
  return -1;
}

int
scm_subr_func_digit_value(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("digit-value?: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_to_integer(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_char_to_integer(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_integer_to_char(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_integer_to_char(argv[0], NULL);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_upcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-upcase: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_downcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-downcase: not implemented", 0);
  return -1;
}

int
scm_subr_func_char_foldcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("char-foldcase: not implemented", 0);
  return -1;
}


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

int
scm_subr_func_string_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_string(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("make-string: not implemented", 0);
  return -1;
}

int
scm_subr_func_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_bytesize(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_bytesize(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_string_set_i(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_string_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ci_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-ci=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_string_lt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ci_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-ci<?: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_string_gt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ci_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-ci>?: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_string_le_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ci_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-ci<=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_capi_string_ge_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ci_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-ci>=?: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_upcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-upcase: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_downcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-downcase: not implemented", 0);
  return -1;
}

int
scm_subr_func_string_foldcase(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("string-foldcase: not implemented", 0);
  return -1;
}

int
scm_subr_func_substring(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_substring(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_string_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_to_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmObj start_end[2] = { SCM_OBJ_INIT,  SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_string_to_list(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_to_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_to_string(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_copy(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmObj start_end[2] = { SCM_OBJ_INIT,  SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_string_copy(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_copy_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmObj start_end[2] = { SCM_OBJ_INIT,  SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[3],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_string_copy_i(argv[0], argv[1],
                              argv[2], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_fill_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmObj start_end[2] = { SCM_OBJ_INIT,  SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[3],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_string_fill_i(argv[0], argv[1], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

int
scm_subr_func_vector_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_vector_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_vector(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &fill);

  r = scm_subr_list_to_cv(argv[1], &fill, 1);
  if (r < 0) return -1;

  val = scm_api_make_vector(argv[0], fill);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_vector_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_vector_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_vector_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_vector_set_i(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_to_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_to_list(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_to_vector(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_to_vector(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_to_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_to_string(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_to_vector(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_string_to_vector(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_copy(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[1],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_copy(argv[0], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_copy_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[3],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_copy_i(argv[0], argv[1],
                              argv[2], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_vector_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_fill_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);
  SCM_STACK_PUSH_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[2],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_fill_i(argv[0], argv[1], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Exceptions                                                     */
/*******************************************************************/

int
scm_subr_func_with_exception_handler_post(ScmObj subr,
                                          int argc, const ScmObj *argv)
{
  int rslt;

  rslt = scm_capi_pop_exception_handler();
  if (rslt < 0) return -1;

  return scm_capi_return_val(argv + 1, argc - 1);
}

int
scm_subr_func_with_exception_handler(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj module = SCM_OBJ_INIT, postproc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &module, &postproc);

  rslt = scm_capi_subrutine_module(subr, SCM_CSETTER_L(module));
  if (rslt < 0) return -1;

  postproc = scm_capi_make_subrutine(scm_subr_func_with_exception_handler_post,
                                     -2, SCM_PROC_ADJ_UNWISHED, module);
  if (scm_obj_null_p(postproc)) return -1;

  rslt = scm_capi_trampolining(argv[1], SCM_NIL_OBJ, postproc, SCM_OBJ_NULL);
  if (rslt < 0) return -1;

  rslt = scm_capi_push_exception_handler(argv[0]);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_subr_func_raise(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_raise_for_subr(argv[0]);
}

int
scm_subr_func_raise_continuable(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_raise_continuable_for_subr(argv[0]);
}

int
scm_subr_func_error(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_error_for_subr(argv[0], argv[1]);
}

int
scm_subr_func_error_object_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_error_object_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_error_object_message(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_error_object_message(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_error_object_irritants(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_error_object_irritants(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_read_error_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_read_error_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_file_error_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_file_error_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

static ScmObj
scm_get_current_port(ScmObj subr, const char *var)
{
  ScmObj mod = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT, prm = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &mod, &prm, &port);

  rslt = scm_capi_subrutine_module(subr, SCM_CSETTER_L(mod));
  if (rslt < 0) return SCM_OBJ_NULL;

  sym = scm_capi_make_symbol_from_cstr(var, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  rslt = scm_capi_global_var_ref(mod, sym, SCM_CSETTER_L(prm));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(prm)) {
    scm_capi_error("failed to get current port", 0);
    return SCM_OBJ_NULL;
  }

  port = scm_capi_parameter_value(prm);
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  return port;
}

int
scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-input-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("read: too many arugments", 0);
    return -1;
  }

  val = scm_api_read(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("write: too many arugments", 0);
    return -1;
  }

  val = scm_api_write(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_NULL;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("display: too many arugments", 0);
    return -1;
  }

  val = scm_api_display(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("newline: too many arugments", 0);
    return -1;
  }

  val = scm_api_newline(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("flush-output-port: too many arugments", 0);
    return -1;
  }

  val = scm_api_flush_output_port(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eof_object_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_eof_object_p(argv[0]) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

int
scm_subr_func_callcc(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj cont = SCM_OBJ_INIT, args = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &cont, &args);

  if (argc != 1) {
    scm_capi_error("call/cc: 1 argumetn is require, but got ", 0);
    return -1;
  }

  cont = scm_capi_capture_cont();
  if (scm_obj_null_p(cont)) return -1;

  args = scm_capi_list(1, cont);
  if (scm_obj_null_p(args)) return -1;

  return scm_capi_trampolining(argv[0], args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}


/*******************************************************************/
/*  Multiple Return Values                                         */
/*******************************************************************/

int
scm_subr_func_values(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_return_val(argv, argc);
}

const char *scm_clsr_code_call_with_values =
  "((eframe)"
  " (cframe)"
  " (sref 0 0)"
  " (call 0)"
  " (arity -1)"
  " (mvpush)"
  " (sref 1 0)"
  " (tapply)"
  ")";


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

int
scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj code = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  ssize_t i;

  SCM_STACK_FRAME_PUSH(&subr,
                       &code, &args);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("eval-asm: 1 argument is require, but got ", 0);
    return -1;
  }

  if (scm_capi_pair_p(argv[0])) {
    code = scm_api_assemble(argv[0], SCM_OBJ_NULL);
    if (scm_obj_null_p(code)) return -1;
  }
  else if (scm_capi_iseq_p(argv[0])) {
    code = argv[0];
  }
  else {
    scm_capi_error("eval-asm: argument is not pair or iseq", 1, argv[0]);
    return -1;
  }

  i = scm_capi_iseq_push_opfmt_noarg(code, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  code = scm_capi_make_closure(code, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(code)) return -1;

  args = SCM_NIL_OBJ;

  return scm_capi_trampolining(code, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}

int
scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj exp = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  ssize_t i;

  SCM_STACK_FRAME_PUSH(&subr, &exp, &args);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("eval-asm: 1 argument is require, but got ", 0);
    return -1;
  }

  exp = scm_capi_compile(argv[0], SCM_OBJ_NULL, false);
  if (scm_obj_null_p(exp)) return -1;

  i = scm_capi_iseq_push_opfmt_noarg(exp, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  exp = scm_capi_make_closure(exp, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(exp)) return -1;

  args = SCM_NIL_OBJ;

  return scm_capi_trampolining(exp, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

int
scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  val = SCM_OBJ_NULL;
  if (len == 1) {
    val = scm_api_car(argv[0]);
    if (scm_obj_null_p(val)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("exit: too many arugments", 0);
    return -1;
  }

  val = scm_api_exit(val);
  if (scm_obj_null_p(val)) return -1;

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

int
scm_subr_func_compile_file(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  port = scm_api_open_input_file(argv[0]);
  if (scm_obj_null_p(port)) return -1;

  val = scm_capi_compile_port(port, argv[1], true);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

int
scm_subr_func_format(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_format_lst(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}
