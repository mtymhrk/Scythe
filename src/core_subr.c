
#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/core_subr.h"


static ssize_t
scm_subr_list_to_cv(ScmObj lst, ScmObj *ary, size_t n)
{
  ssize_t len;

  SCM_REFSTK_INIT_REG(&lst);

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_eq_P(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eqv_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_eqv_P(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_equal_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_number_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_complex_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_complex_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_real_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_real_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_rational_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_rational_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_integer_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_integer_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_exact_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_exact_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_inexact_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_inexact_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_exact_integer_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_exact_integer_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_finite_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_finite_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_infinite_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_infinite_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_nan_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_nan_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_num_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_num_lt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_num_gt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_num_le_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_num_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_num_ge_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_zero_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_zero_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_positive_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_positive_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_negative_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_negative_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_odd_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_odd_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_even_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_even_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_max(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_max_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_min(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_min_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_plus(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_plus_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_mul(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_mul_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_minus(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj lst = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &lst, &val);

  lst = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_minus_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr);
  SCM_REFSTK_REG_ARY(val, sizeof(val)/sizeof(val[0]));

  r = scm_capi_floor_div(argv[0], argv[1],
                         SCM_CSETTER_L(val[0]), SCM_CSETTER_L(val[1]));
  if (r < 0) return -1;

  return scm_capi_return_val(val, sizeof(val)/sizeof(val[0]));
}

int
scm_subr_func_floor_quo(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_floor_quo(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_floor_rem(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr);
  SCM_REFSTK_REG_ARY(val, sizeof(val)/sizeof(val[0]));

  r = scm_capi_truncate_div(argv[0], argv[1],
                            SCM_CSETTER_L(val[0]), SCM_CSETTER_L(val[1]));
  if (r < 0) return -1;

  return scm_capi_return_val(val, sizeof(val)/sizeof(val[0]));
}

int
scm_subr_func_truncate_quo(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_truncate_quo(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_truncate_rem(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_not(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_boolean_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_pair_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cons(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_car(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_car(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cdr(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_cdr(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_car_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_set_car_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_cdr_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_set_cdr_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_nil_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_list_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_reverse(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_reverse(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_tail(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_list_tail(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_list_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_list_set_i(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_memq(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_memq(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_memv(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_memv(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
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
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_assq(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_assv(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_assv(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_symbol_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_symbol_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_symbol_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_symbol_to_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_symbol_to_string(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_to_symbol(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_char_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_char_eq_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_lt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_char_lt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_gt_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_char_gt_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_le_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_char_le_P_lst(lst);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_char_ge_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_char_ge_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_char_to_integer(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_integer_to_char(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_bytesize(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_bytesize(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_set_i(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_eq_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_string_eq_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_string_lt_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_string_gt_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_string_le_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &lst);

  lst = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_api_cons(argv[0], lst);
  if (scm_obj_null_p(lst)) return -1;

  val = scm_api_string_ge_P_lst(lst);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_substring(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_string_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_string_to_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmObj start_end[2] = { SCM_OBJ_INIT,  SCM_OBJ_INIT };
  ssize_t r;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end, sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_vector_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_vector_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_vector_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_set_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_vector_append_lst(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_vector_fill_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, start_end[2] = { SCM_OBJ_INIT, SCM_OBJ_INIT };
  ssize_t r;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);
  SCM_REFSTK_REG_ARY(start_end,  sizeof(start_end)/sizeof(start_end[0]));

  r = scm_subr_list_to_cv(argv[2],
                          start_end, sizeof(start_end)/sizeof(start_end[0]));
  if (r < 0) return -1;

  val = scm_api_vector_fill_i(argv[0], argv[1], start_end[0], start_end[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Control features                                               */
/*******************************************************************/

int
scm_subr_func_procedure_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_procedure_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_apply(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj arg = SCM_OBJ_INIT, prv = SCM_OBJ_INIT, cur = SCM_OBJ_INIT;
  ScmObj itr = SCM_OBJ_INIT, obj = SCM_OBJ_INIT, nxt = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &arg, &prv, &cur,
                      &itr, &obj, &nxt);

  if (!scm_capi_procedure_p(argv[0])) {
    scm_capi_error("apply: procedure required, but got", 1, argv[0]);
    return -1;
  }

  arg = scm_api_cons(argv[1], argv[2]);
  if (scm_obj_null_p(arg)) return -1;

  prv = SCM_OBJ_NULL;
  for (itr = arg; scm_capi_pair_p(itr); itr = nxt) {
    obj = scm_api_car(itr);
    if (scm_obj_null_p(obj)) return -1;

    nxt = scm_api_cdr(itr);
    if (scm_obj_null_p(nxt)) return -1;

    if (scm_capi_pair_p(nxt)) {
      cur = scm_api_cons(obj, SCM_NIL_OBJ);
      if (scm_obj_null_p(cur)) return -1;
    }
    else {
      if (!scm_capi_pair_p(obj)) {
        scm_capi_error("apply: list required, but got", 1, obj);
        return -1;
      }
      cur = obj;
    }

    if (scm_obj_null_p(prv)) {
      arg = cur;
    }
    else {
      int r = scm_capi_set_cdr_i(prv, cur);
      if (r < 0) return -1;
    }

    prv = cur;
  }

  if (scm_obj_null_p(itr)) return -1;

  return scm_capi_trampolining(argv[0], arg, SCM_OBJ_NULL, SCM_OBJ_NULL);
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

  return scm_capi_return_val(argv, argc);
}

int
scm_subr_func_with_exception_handler(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj module = SCM_OBJ_INIT, postproc = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&subr,
                      &module, &postproc);

  rslt = scm_capi_subrutine_module(subr, SCM_CSETTER_L(module));
  if (rslt < 0) return -1;

  postproc = scm_capi_make_subrutine(scm_subr_func_with_exception_handler_post,
                                     -1, SCM_PROC_ADJ_UNWISHED, module);
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
  return scm_capi_raise(argv[0]);
}

int
scm_subr_func_raise_continuable(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_raise_continuable(argv[0]);
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

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_error_object_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_error_object_message(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_error_object_message(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_error_object_irritants(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_error_object_irritants(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_read_error_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_read_error_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_file_error_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_file_error_P(argv[0]);

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Ports                                                          */
/*******************************************************************/

int
scm_subr_func_open_input_file(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_open_input_file(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/


int
scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &val);

  port = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[0])) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }

  val = scm_api_read(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &val);

  port = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[1])) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }

  val = scm_api_write(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &val);

  port = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[0])) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }

  val = scm_api_display(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &val);

  port = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[0])) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }

  val = scm_api_newline(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &val);

  port = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[0])) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }

  val = scm_api_flush_output_port(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eof_object_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
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

  SCM_REFSTK_INIT_REG(&subr,
                      &cont, &args);

  if (argc != 1) {
    scm_capi_error("call/cc: 1 argumetn is require, but got ", 0);
    return -1;
  }

  cont = scm_api_capture_cont();
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

int
scm_subr_func_call_with_values(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_trampolining(argv[0], SCM_NIL_OBJ, argv[1], SCM_OBJ_NULL);
}


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

int
scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj code = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  ssize_t i;

  SCM_REFSTK_INIT_REG(&subr,
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

  i = scm_capi_iseq_push_inst(code, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  code = scm_capi_make_closure(code, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(code)) return -1;

  args = SCM_NIL_OBJ;

  return scm_capi_trampolining(code, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}

static int
scm_subr_func_eval__post_compile(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj iseq = SCM_OBJ_INIT, proc = SCM_OBJ_INIT;
  ssize_t i;

  SCM_REFSTK_INIT_REG(&subr,
                      &iseq, &proc);

  iseq = scm_api_assemble(argv[0], SCM_OBJ_NULL);
  if (scm_obj_null_p(iseq)) return -1;

  i = scm_capi_iseq_push_inst(iseq, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  proc = scm_capi_make_closure(iseq, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(proc)) return -1;

  return scm_capi_trampolining(proc, SCM_NIL_OBJ, SCM_OBJ_NULL, SCM_OBJ_NULL);
}

int
scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj compile = SCM_OBJ_INIT, postproc = SCM_OBJ_INIT;
  ScmObj subr_mod = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  ScmObj cmpl = SCM_OBJ_INIT;
  int r;


  SCM_REFSTK_INIT_REG(&subr,
                      &compile, &postproc,
                      &subr_mod, &args,
                      &cmpl);


  r = scm_capi_subrutine_module(subr, SCM_CSETTER_L(subr_mod));
  if (r < 0) return -1;

  r = scm_capi_cached_global_var_ref(SCM_CACHED_GV_COMPILE,
                                     SCM_CSETTER_L(compile));
  if (r < 0) return -1;

  if (scm_obj_null_p(compile)) {
    scm_capi_error("unbound variable: compile", 0);
    return -1;
  }

  postproc = scm_capi_make_subrutine(scm_subr_func_eval__post_compile,
                                     1, 0, subr_mod);
  if (scm_obj_null_p(postproc)) return -1;

  if (scm_capi_nil_p(argv[1])) {
    cmpl = scm_api_make_compiler(SCM_OBJ_NULL);
    if (scm_obj_null_p(cmpl)) return -1;

    args = scm_capi_list(2, argv[0], cmpl);
  }
  else {
    args = scm_api_cons(argv[0], argv[1]);
  }

  if (scm_obj_null_p(args)) return -1;

  return scm_capi_trampolining(compile, args, postproc, SCM_OBJ_NULL);
}


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

int
scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_REFSTK_INIT_REG(&subr,
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
/*  format                                                         */
/*******************************************************************/

int
scm_subr_func_format(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_format_lst(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Modules                                                        */
/*******************************************************************/

int
scm_subr_func_module_name(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_module_name(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Internals (Compiler)                                           */
/*******************************************************************/

int
scm_subr_func_compiler_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_compiler_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_compiler(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val, &mod);

  mod = SCM_OBJ_NULL;
  if (scm_capi_pair_p(argv[0])) {
    mod = scm_api_car(argv[0]);
    if (scm_obj_null_p(mod)) return -1;
  }

  val = scm_api_make_compiler(mod);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_compiler_assign_label_id_i(ScmObj subr,
                                         int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  int id;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  id = scm_capi_compiler_assign_label_id_i(argv[0]);
  if (id < 0) return -1;

  val = scm_capi_make_number_from_sword(id);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_compiler_current_module(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_compiler_current_module(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_compiler_select_module_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_compiler_select_module_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_compiler_current_expr(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_compiler_current_expr(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_compiler_select_expr_i(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_compiler_select_expr_i(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_syntax_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_syntax_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_syntax(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_make_syntax(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_syntax_keyword(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_syntax_keyword(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_syntax_handler(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  val = scm_api_syntax_handler(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_global_syntax_bind(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj obj = SCM_OBJ_INIT;
  bool export;
  int r;

  SCM_REFSTK_INIT_REG(&subr,
                      &obj);

  export = false;
  if (scm_capi_pair_p(argv[3])) {
    obj = scm_api_car(argv[3]);
    if (scm_obj_null_p(obj)) return -1;

    if (!scm_capi_boolean_p(obj)) {
      scm_capi_error("global-syntax-bind: invalid argument", 1, obj);
      return -1;
    }

    export = scm_capi_true_object_p(obj);
  }

  r = scm_capi_define_global_syx(argv[0], argv[1], argv[2], export);
  if (r < 0) return -1;

  obj = SCM_NIL_OBJ;
  return scm_capi_return_val(&obj, 1);
}

int
scm_subr_func_global_syntax_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&subr,
                      &val);

  r = scm_capi_global_syx_ref(argv[0], argv[1], SCM_CSETTER_L(val));
  if (r < 0) return -1;

  if (scm_obj_null_p(val)) {
    if (scm_capi_nil_p(argv[2])) {
      scm_capi_error("unbound syntax", 1, argv[1]);
      return -1;
    }

    val = scm_api_car(argv[2]);
    if (scm_obj_null_p(val)) return -1;
  }

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Internals                                                      */
/*******************************************************************/

static int
scm_subr_func_eval_file__loop(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, cmpl = SCM_OBJ_INIT;
  ScmObj eval = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &cmpl,
                      &eval, &exp, &args);

  port = scm_api_car(argv[0]);
  if (scm_obj_null_p(port)) return -1;

  cmpl = scm_api_cdr(argv[0]);
  if (scm_obj_null_p(cmpl)) return -1;

  exp = scm_api_read(port);
  if (scm_obj_null_p(exp)) return -1;

  if (scm_capi_eof_object_p(exp))
    return scm_capi_return_val(argv + 1, argc - 1);

  r = scm_capi_cached_global_var_ref(SCM_CACHED_GV_EVAL, SCM_CSETTER_L(eval));
  if (r < 0) return -1;

  if (scm_obj_null_p(eval)) {
    scm_capi_error("unbound variable: eval", 0);
    return -1;
  }

  args = scm_capi_list(2, exp, cmpl);

  return scm_capi_trampolining(eval, args, subr, argv[0]);
}

int
scm_subr_func_eval_file(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, cmpl = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj loop = SCM_OBJ_INIT, args = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&subr,
                      &port, &cmpl, &mod,
                      &loop, &args);

  port = scm_api_open_input_file(argv[0]);
  if (scm_obj_null_p(port)) return -1;

  cmpl = scm_api_make_compiler(SCM_OBJ_NULL);
  if (scm_obj_null_p(cmpl)) return -1;

  r = scm_capi_subrutine_module(subr, SCM_CSETTER_L(mod));
  if (r < 0) return -1;

  args = scm_api_cons(port, cmpl);
  if (scm_obj_null_p(args)) return -1;

  args = scm_capi_list(2, args, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(args)) return -1;

  loop = scm_capi_make_subrutine(scm_subr_func_eval_file__loop,
                                 -2, SCM_PROC_ADJ_UNWISHED, mod);
  if (scm_obj_null_p(loop)) return -1;

  return scm_capi_trampolining(loop, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}


int
scm_subr_func_eval_string(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, exp = SCM_OBJ_INIT, eval = SCM_OBJ_INIT;
  ScmObj args = SCM_OBJ_INIT;
  int r;

  port = scm_api_open_input_string(argv[0]);
  if (scm_obj_null_p(port)) return -1;

  exp = scm_api_read(port);
  if (scm_obj_null_p(exp)) return -1;

  r = scm_capi_cached_global_var_ref(SCM_CACHED_GV_EVAL, SCM_CSETTER_L(eval));
  if (r < 0) return -1;

  if (scm_obj_null_p(eval)) {
    scm_capi_error("unbound variable: eval", 0);
    return -1;
  }

  args = scm_api_cons(exp, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  return scm_capi_trampolining(eval, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}
