#ifndef INCLUDE_CORE_SUBR_H__
#define INCLUDE_CORE_SUBR_H__

#include <stdint.h>

#include "scythe/object.h"


/*******************************************************************/
/*  Dynamic bindings                                               */
/*******************************************************************/

#define SCM_SUBR_ARITY_MAKE_PARAMETER -2

#define SCM_SUBR_FLAG_MAKE_PARAMETER 0

int scm_subr_func_make_parameter(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Equivalence predicates                                         */
/*******************************************************************/

#define SCM_SUBR_ARITY_EQ_P 2
#define SCM_SUBR_ARITY_EQV_P 2
#define SCM_SUBR_ARITY_EQUAL_P 2

#define SCM_SUBR_FLAG_EQ_P 0
#define SCM_SUBR_FLAG_EQV_P 0
#define SCM_SUBR_FLAG_EQUAL_P 0

int scm_subr_func_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eqv_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_equal_P(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

#define SCM_SUBR_ARITY_NUMBER_P 1
#define SCM_SUBR_ARITY_COMPLEX_P 1
#define SCM_SUBR_ARITY_REAL_P 1
#define SCM_SUBR_ARITY_RATIONAL_P 1
#define SCM_SUBR_ARITY_INTEGER_P 1
#define SCM_SUBR_ARITY_EXACT_P 1
#define SCM_SUBR_ARITY_INEXACT_P 1
#define SCM_SUBR_ARITY_EXACT_INTEGER_P 1
#define SCM_SUBR_ARITY_FINITE_P 1
#define SCM_SUBR_ARITY_INFINITE_P 1
#define SCM_SUBR_ARITY_NAN_P 1
#define SCM_SUBR_ARITY_NUM_EQ_P -3
#define SCM_SUBR_ARITY_NUM_LT_P -3
#define SCM_SUBR_ARITY_NUM_GT_P -3
#define SCM_SUBR_ARITY_NUM_LE_P -3
#define SCM_SUBR_ARITY_NUM_GE_P -3
#define SCM_SUBR_ARITY_ZERO_P 1
#define SCM_SUBR_ARITY_POSITIVE_P 1
#define SCM_SUBR_ARITY_NEGATIVE_P 1
#define SCM_SUBR_ARITY_ODD_P 1
#define SCM_SUBR_ARITY_EVEN_P 1
#define SCM_SUBR_ARITY_MAX -2
#define SCM_SUBR_ARITY_MIN -2
#define SCM_SUBR_ARITY_PLUS -1
#define SCM_SUBR_ARITY_MUL -1
#define SCM_SUBR_ARITY_MINUS -2
#define SCM_SUBR_ARITY_DIV -2
#define SCM_SUBR_ARITY_ABS 1
#define SCM_SUBR_ARITY_FLOOR_DIV 2
#define SCM_SUBR_ARITY_FLOOR_QUO 2
#define SCM_SUBR_ARITY_FLOOR_REM 2
#define SCM_SUBR_ARITY_TRUNCATE_DIV 2
#define SCM_SUBR_ARITY_TRUNCATE_QUO 2
#define SCM_SUBR_ARITY_TRUNCATE_REM 2

#define SCM_SUBR_FLAG_NUMBER_P 0
#define SCM_SUBR_FLAG_COMPLEX_P 0
#define SCM_SUBR_FLAG_REAL_P 0
#define SCM_SUBR_FLAG_RATIONAL_P 0
#define SCM_SUBR_FLAG_INTEGER_P 0
#define SCM_SUBR_FLAG_EXACT_P 0
#define SCM_SUBR_FLAG_INEXACT_P 0
#define SCM_SUBR_FLAG_EXACT_INTEGER_P 0
#define SCM_SUBR_FLAG_FINITE_P 0
#define SCM_SUBR_FLAG_INFINITE_P 0
#define SCM_SUBR_FLAG_NAN_P 0
#define SCM_SUBR_FLAG_NUM_EQ_P 0
#define SCM_SUBR_FLAG_NUM_LT_P 0
#define SCM_SUBR_FLAG_NUM_GT_P 0
#define SCM_SUBR_FLAG_NUM_LE_P 0
#define SCM_SUBR_FLAG_NUM_GE_P 0
#define SCM_SUBR_FLAG_ZERO_P 0
#define SCM_SUBR_FLAG_POSITIVE_P 0
#define SCM_SUBR_FLAG_NEGATIVE_P 0
#define SCM_SUBR_FLAG_ODD_P 0
#define SCM_SUBR_FLAG_EVEN_P 0
#define SCM_SUBR_FLAG_MAX 0
#define SCM_SUBR_FLAG_MIN 0
#define SCM_SUBR_FLAG_PLUS 0
#define SCM_SUBR_FLAG_MUL 0
#define SCM_SUBR_FLAG_MINUS 0
#define SCM_SUBR_FLAG_DIV 0
#define SCM_SUBR_FLAG_ABS 0
#define SCM_SUBR_FLAG_FLOOR_DIV 0
#define SCM_SUBR_FLAG_FLOOR_QUO 0
#define SCM_SUBR_FLAG_FLOOR_REM 0
#define SCM_SUBR_FLAG_TRUNCATE_DIV 0
#define SCM_SUBR_FLAG_TRUNCATE_QUO 0
#define SCM_SUBR_FLAG_TRUNCATE_REM 0

int scm_subr_func_number_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_complex_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_real_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_rational_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_integer_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_exact_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_inexact_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_exact_integer_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_finite_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_infinite_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_nan_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_num_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_num_lt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_num_gt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_num_le_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_num_ge_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_zero_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_positive_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_negative_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_odd_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_even_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_max(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_min(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_plus(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_mul(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_minus(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_div(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_abs(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_floor_div(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_floor_quo(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_floor_rem(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_truncate_div(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_truncate_quo(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_truncate_rem(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/

#define SCM_SUBR_ARITY_NOT 1
#define SCM_SUBR_ARITY_BOOLEAN_P 1

#define SCM_SUBR_FLAG_NOT 0
#define SCM_SUBR_FLAG_BOOLEAN_P 0

int scm_subr_func_not(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_boolean_P(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

#define SCM_SUBR_ARITY_PAIR_P 1
#define SCM_SUBR_ARITY_CONS 2
#define SCM_SUBR_ARITY_CAR 1
#define SCM_SUBR_ARITY_CDR 1
#define SCM_SUBR_ARITY_SET_CAR_I 2
#define SCM_SUBR_ARITY_SET_CDR_I 2
#define SCM_SUBR_ARITY_NULL_P 1
#define SCM_SUBR_ARITY_LIST_P 1
#define SCM_SUBR_ARITY_MAKE_LIST -2
#define SCM_SUBR_ARITY_LIST -1
#define SCM_SUBR_ARITY_LENGTH 1
#define SCM_SUBR_ARITY_APPEND -1
#define SCM_SUBR_ARITY_REVERSE 1
#define SCM_SUBR_ARITY_LIST_TAIL 2
#define SCM_SUBR_ARITY_LIST_REF 2
#define SCM_SUBR_ARITY_LIST_SET_I 3
#define SCM_SUBR_ARITY_MEMQ 2
#define SCM_SUBR_ARITY_MEMV 2
#define SCM_SUBR_ARITY_MEMBER -3
#define SCM_SUBR_ARITY_ASSQ 2
#define SCM_SUBR_ARITY_ASSV 2
#define SCM_SUBR_ARITY_ASSOC -3
#define SCM_SUBR_ARITY_LIST_COPY 1

#define SCM_SUBR_FLAG_PAIR_P 0
#define SCM_SUBR_FLAG_CONS 0
#define SCM_SUBR_FLAG_CONS 0
#define SCM_SUBR_FLAG_CAR 0
#define SCM_SUBR_FLAG_CDR 0
#define SCM_SUBR_FLAG_SET_CAR_I 0
#define SCM_SUBR_FLAG_SET_CDR_I 0
#define SCM_SUBR_FLAG_NULL_P 0
#define SCM_SUBR_FLAG_LIST_P 0
#define SCM_SUBR_FLAG_MAKE_LIST SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_LIST 0
#define SCM_SUBR_FLAG_LENGTH 0
#define SCM_SUBR_FLAG_APPEND 0
#define SCM_SUBR_FLAG_REVERSE 0
#define SCM_SUBR_FLAG_LIST_TAIL 0
#define SCM_SUBR_FLAG_LIST_REF 0
#define SCM_SUBR_FLAG_LIST_SET_I 0
#define SCM_SUBR_FLAG_MEMQ 0
#define SCM_SUBR_FLAG_MEMV 0
#define SCM_SUBR_FLAG_MEMBER 0
#define SCM_SUBR_FLAG_ASSQ 0
#define SCM_SUBR_FLAG_ASSV 0
#define SCM_SUBR_FLAG_ASSOC 0
#define SCM_SUBR_FLAG_LIST_COPY 0

int scm_subr_func_pair_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_cons(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_car(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_cdr(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_set_car_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_set_cdr_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_length(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_append(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_reverse(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_tail(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_ref(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_set_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_memq(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_memv(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_member(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_assq(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_assv(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_assoc(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_copy(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

#define SCM_SUBR_ARITY_SYMBOL_P 1
#define SCM_SUBR_ARITY_SYMBOL_EQ_P -3
#define SCM_SUBR_ARITY_SYMBOL_TO_STRING 1
#define SCM_SUBR_ARITY_STRING_TO_SYMBOL 1

#define SCM_SUBR_FLAG_SYMBOL_P 0
#define SCM_SUBR_FLAG_SYMBOL_EQ_P 0
#define SCM_SUBR_FLAG_SYMBOL_TO_STRING 0
#define SCM_SUBR_FLAG_STRING_TO_SYMBOL 0

int scm_subr_func_symbol_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_symbol_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_symbol_to_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_to_symbol(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

#define SCM_SUBR_ARITY_CHAR_P 1
#define SCM_SUBR_ARITY_CHAR_EQ_P -3
#define SCM_SUBR_ARITY_CHAR_LT_P -3
#define SCM_SUBR_ARITY_CHAR_GT_P -3
#define SCM_SUBR_ARITY_CHAR_LE_P -3
#define SCM_SUBR_ARITY_CHAR_GE_P -3
#define SCM_SUBR_ARITY_CHAR_CI_EQ_P -3
#define SCM_SUBR_ARITY_CHAR_CI_LT_P -3
#define SCM_SUBR_ARITY_CHAR_CI_GT_P -3
#define SCM_SUBR_ARITY_CHAR_CI_LE_P -3
#define SCM_SUBR_ARITY_CHAR_CI_GE_P -3
#define SCM_SUBR_ARITY_CHAR_ALPHABETIC_P 1
#define SCM_SUBR_ARITY_CHAR_NUMERIC_P 1
#define SCM_SUBR_ARITY_CHAR_WHITESPACE_P 1
#define SCM_SUBR_ARITY_CHAR_UPPER_CASE_P 1
#define SCM_SUBR_ARITY_CHAR_LOWER_CASE_P 1
#define SCM_SUBR_ARITY_DIGIT_VALUE 1
#define SCM_SUBR_ARITY_CHAR_TO_INTEGER 1
#define SCM_SUBR_ARITY_INTEGER_TO_CHAR 1
#define SCM_SUBR_ARITY_CHAR_UPCASE 1
#define SCM_SUBR_ARITY_CHAR_DOWNCASE 1
#define SCM_SUBR_ARITY_CHAR_FOLDCASE 1

#define SCM_SUBR_FLAG_CHAR_P 0
#define SCM_SUBR_FLAG_CHAR_EQ_P 0
#define SCM_SUBR_FLAG_CHAR_LT_P 0
#define SCM_SUBR_FLAG_CHAR_GT_P 0
#define SCM_SUBR_FLAG_CHAR_LE_P 0
#define SCM_SUBR_FLAG_CHAR_GE_P 0
#define SCM_SUBR_FLAG_CHAR_CI_EQ_P 0
#define SCM_SUBR_FLAG_CHAR_CI_LT_P 0
#define SCM_SUBR_FLAG_CHAR_CI_GT_P 0
#define SCM_SUBR_FLAG_CHAR_CI_LE_P 0
#define SCM_SUBR_FLAG_CHAR_CI_GE_P 0
#define SCM_SUBR_FLAG_CHAR_ALPHABETIC_P 0
#define SCM_SUBR_FLAG_CHAR_NUMERIC_P 0
#define SCM_SUBR_FLAG_CHAR_WHITESPACE_P 0
#define SCM_SUBR_FLAG_CHAR_UPPER_CASE_P 0
#define SCM_SUBR_FLAG_CHAR_LOWER_CASE_P 0
#define SCM_SUBR_FLAG_DIGIT_VALUE 0
#define SCM_SUBR_FLAG_CHAR_TO_INTEGER 0
#define SCM_SUBR_FLAG_INTEGER_TO_CHAR 0
#define SCM_SUBR_FLAG_CHAR_UPCASE 0
#define SCM_SUBR_FLAG_CHAR_DOWNCASE 0
#define SCM_SUBR_FLAG_CHAR_FOLDCASE 0

int scm_subr_func_char_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_lt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_gt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_le_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ge_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ci_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ci_lt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ci_gt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ci_le_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_ci_ge_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_alphabetic_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_numeric_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_whitespace_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_upper_case_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_lower_case_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_digit_value(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_to_integer(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_integer_to_char(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_upcase(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_downcase(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_char_foldcase(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

#define SCM_SUBR_ARITY_STRING_P 1
#define SCM_SUBR_ARITY_MAKE_STRING -2
#define SCM_SUBR_ARITY_STRING -1
#define SCM_SUBR_ARITY_STRING_LENGTH 1
#define SCM_SUBR_ARITY_STRING_BYTESIZE 1
#define SCM_SUBR_ARITY_STRING_REF 2
#define SCM_SUBR_ARITY_STRING_SET_I 3
#define SCM_SUBR_ARITY_STRING_EQ_P -3
#define SCM_SUBR_ARITY_STRING_CI_EQ_P -3
#define SCM_SUBR_ARITY_STRING_LT_P -3
#define SCM_SUBR_ARITY_STRING_CI_LT_P -3
#define SCM_SUBR_ARITY_STRING_GT_P -3
#define SCM_SUBR_ARITY_STRING_CI_GT_P -3
#define SCM_SUBR_ARITY_STRING_LE_P -3
#define SCM_SUBR_ARITY_STRING_CI_LE_P -3
#define SCM_SUBR_ARITY_STRING_GE_P -3
#define SCM_SUBR_ARITY_STRING_CI_GE_P -3
#define SCM_SUBR_ARITY_STRING_UPCASE 1
#define SCM_SUBR_ARITY_STRING_DOWNCASE 1
#define SCM_SUBR_ARITY_STRING_FOLDCASE 1
#define SCM_SUBR_ARITY_SUBSTRING 3
#define SCM_SUBR_ARITY_STRING_APPEND -1
#define SCM_SUBR_ARITY_STRING_TO_LIST -2
#define SCM_SUBR_ARITY_LIST_TO_STRING 1
#define SCM_SUBR_ARITY_STRING_COPY -2
#define SCM_SUBR_ARITY_STRING_COPY_I -4
#define SCM_SUBR_ARITY_STRING_FILL_I -3

#define SCM_SUBR_FLAG_STRING_P 0
#define SCM_SUBR_FLAG_MAKE_STRING 0
#define SCM_SUBR_FLAG_STRING 0
#define SCM_SUBR_FLAG_STRING_LENGTH 0
#define SCM_SUBR_FLAG_STRING_BYTESIZE 0
#define SCM_SUBR_FLAG_STRING_REF 0
#define SCM_SUBR_FLAG_STRING_SET_I 0
#define SCM_SUBR_FLAG_STRING_EQ_P 0
#define SCM_SUBR_FLAG_STRING_CI_EQ_P 0
#define SCM_SUBR_FLAG_STRING_LT_P 0
#define SCM_SUBR_FLAG_STRING_CI_LT_P 0
#define SCM_SUBR_FLAG_STRING_GT_P 0
#define SCM_SUBR_FLAG_STRING_CI_GT_P 0
#define SCM_SUBR_FLAG_STRING_LE_P 0
#define SCM_SUBR_FLAG_STRING_CI_LE_P 0
#define SCM_SUBR_FLAG_STRING_GE_P 0
#define SCM_SUBR_FLAG_STRING_CI_GE_P 0
#define SCM_SUBR_FLAG_STRING_UPCASE 0
#define SCM_SUBR_FLAG_STRING_DOWNCASE 0
#define SCM_SUBR_FLAG_STRING_FOLDCASE 0
#define SCM_SUBR_FLAG_SUBSTRING 0
#define SCM_SUBR_FLAG_STRING_APPEND 0
#define SCM_SUBR_FLAG_STRING_TO_LIST 0
#define SCM_SUBR_FLAG_LIST_TO_STRING 0
#define SCM_SUBR_FLAG_STRING_COPY 0
#define SCM_SUBR_FLAG_STRING_COPY_I 0
#define SCM_SUBR_FLAG_STRING_FILL_I 0

int scm_subr_func_string_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_length(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_bytesize(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ref(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_set_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ci_eq_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_lt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ci_lt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_gt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ci_gt_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_le_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ci_le_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ge_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_ci_ge_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_upcase(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_downcase(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_foldcase(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_substring(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_append(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_to_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_to_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_copy(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_copy_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_fill_i(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

#define SCM_SUBR_ARITY_VECTOR_P 1
#define SCM_SUBR_ARITY_MAKE_VECTOR -2
#define SCM_SUBR_ARITY_VECTOR -1
#define SCM_SUBR_ARITY_VECTOR_LENGTH 1
#define SCM_SUBR_ARITY_VECTOR_REF 2
#define SCM_SUBR_ARITY_VECTOR_SET_I 3
#define SCM_SUBR_ARITY_VECTOR_TO_LIST -2
#define SCM_SUBR_ARITY_LIST_TO_VECTOR 1
#define SCM_SUBR_ARITY_VECTOR_TO_STRING -2
#define SCM_SUBR_ARITY_STRING_TO_VECTOR -2
#define SCM_SUBR_ARITY_VECTOR_COPY -2
#define SCM_SUBR_ARITY_VECTOR_COPY_I -4
#define SCM_SUBR_ARITY_VECTOR_APPEND -1
#define SCM_SUBR_ARITY_VECTOR_FILL_I -3

#define SCM_SUBR_FLAG_VECTOR_P 0
#define SCM_SUBR_FLAG_MAKE_VECTOR 0
#define SCM_SUBR_FLAG_VECTOR 0
#define SCM_SUBR_FLAG_VECTOR_LENGTH 0
#define SCM_SUBR_FLAG_VECTOR_REF 0
#define SCM_SUBR_FLAG_VECTOR_SET_I 0
#define SCM_SUBR_FLAG_VECTOR_TO_LIST 0
#define SCM_SUBR_FLAG_LIST_TO_VECTOR 0
#define SCM_SUBR_FLAG_VECTOR_TO_STRING 0
#define SCM_SUBR_FLAG_STRING_TO_VECTOR 0
#define SCM_SUBR_FLAG_VECTOR_COPY 0
#define SCM_SUBR_FLAG_VECTOR_COPY_I 0
#define SCM_SUBR_FLAG_VECTOR_APPEND 0
#define SCM_SUBR_FLAG_VECTOR_FILL_I 0

int scm_subr_func_vector_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_vector(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_length(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_ref(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_set_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_to_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_to_vector(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_to_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_string_to_vector(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_copy(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_copy_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_append(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_vector_fill_i(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Control features                                               */
/*******************************************************************/

#define SCM_SUBR_ARITY_PROCEDURE_P 1
#define SCM_SUBR_ARITY_APPLY -3

#define SCM_SUBR_FLAG_PROCEDURE_P 0
#define SCM_SUBR_FLAG_APPLY 0

int scm_subr_func_procedure_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_apply(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Exceptions                                                     */
/*******************************************************************/

#define SCM_SUBR_ARITY_RAISE 1
#define SCM_SUBR_ARITY_RAISE_CONTINUABLE 1
#define SCM_SUBR_ARITY_ERROR -2
#define SCM_SUBR_ARITY_ERROR_OBJECT_P 1
#define SCM_SUBR_ARITY_ERROR_OBJECT_MESSAGE 1
#define SCM_SUBR_ARITY_ERROR_OBJECT_IRRITANTS 1
#define SCM_SUBR_ARITY_READ_ERROR_P 1
#define SCM_SUBR_ARITY_FILE_ERROR_P 1

#define SCM_SUBR_FLAG_RAISE 0
#define SCM_SUBR_FLAG_RAISE_CONTINUABLE 0
#define SCM_SUBR_FLAG_ERROR 0
#define SCM_SUBR_FLAG_ERROR_OBJECT_P 0
#define SCM_SUBR_FLAG_ERROR_OBJECT_MESSAGE 0
#define SCM_SUBR_FLAG_ERROR_OBJECT_IRRITANTS 0
#define SCM_SUBR_FLAG_READ_ERROR_P 0
#define SCM_SUBR_FLAG_FILE_ERROR_P 0

int scm_subr_func_raise(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_raise_continuable(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_error(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_error_object_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_error_object_message(ScmObj subr,
                                       int argc, const ScmObj *argv);
int scm_subr_func_error_object_irritants(ScmObj subr,
                                         int argc, const ScmObj *argv);
int scm_subr_func_read_error_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_file_error_P(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Ports                                                          */
/*******************************************************************/

#define SCM_SUBR_ARITY_OPEN_INPUT_FILE 1

#define SCM_SUBR_FLAG_OPEN_INPUT_FILE 0

int scm_subr_func_open_input_file(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

#define SCM_SUBR_ARITY_READ -1
#define SCM_SUBR_ARITY_WRITE -2
#define SCM_SUBR_ARITY_WRITE_SHARED -2
#define SCM_SUBR_ARITY_WRITE_SIMPLE -2
#define SCM_SUBR_ARITY_DISPLAY -2
#define SCM_SUBR_ARITY_NEWLINE -1
#define SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT -1
#define SCM_SUBR_ARITY_EOF_OBJECT_P 1

#define SCM_SUBR_FLAG_READ 0
#define SCM_SUBR_FLAG_WRITE 0
#define SCM_SUBR_FLAG_WRITE_SHARED 0
#define SCM_SUBR_FLAG_WRITE_SIMPLE 0
#define SCM_SUBR_FLAG_DISPLAY 0
#define SCM_SUBR_FLAG_NEWLINE 0
#define SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT 0
#define SCM_SUBR_FLAG_EOF_OBJECT_P 0

int scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_write_shared(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_write_simple(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eof_object_P(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

#define SCM_SUBR_ARITY_CALLCC 1

#define SCM_SUBR_FLAG_CALLCC 0

int scm_subr_func_callcc(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Multiple Return Values                                         */
/*******************************************************************/

#define SCM_SUBR_ARITY_VALUES -1
#define SCM_SUBR_ARITY_CALL_WITH_VALUES 2

#define SCM_SUBR_FLAG_VALUES SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_CALL_WITH_VALUES 0

int scm_subr_func_values(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_call_with_values(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

#define SCM_SUBR_ARITY_EVAL_ASM 1
#define SCM_SUBR_ARITY_EVAL__POST_COMPILE 1
#define SCM_SUBR_ARITY_EVAL -2

#define SCM_SUBR_FLAG_EVAL_ASM 0
#define SCM_SUBR_FLAG_EVAL__POST_COMPILE 0
#define SCM_SUBR_FLAG_EVAL 0

int scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eval__post_compile(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  System interface                                               */
/*******************************************************************/

#define SCM_SUBR_ARITY_EVAL_FILE__LOOP -2
#define SCM_SUBR_ARITY_LOAD -2
#define SCM_SUBR_ARITY_FILE_EXISTS_P 1
#define SCM_SUBR_ARITY_DELETE_FILE 1
#define SCM_SUBR_ARITY_EXIT -1

#define SCM_SUBR_FLAG_EVAL_FILE__LOOP SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_LOAD 0
#define SCM_SUBR_FLAG_FILE_EXISTS_P 0
#define SCM_SUBR_FLAG_DELETE_FILE 0
#define SCM_SUBR_FLAG_EXIT 0

int scm_subr_func_eval_file__loop(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_load(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_file_exists_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_delete_file(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

#define SCM_SUBR_ARITY_FORMAT -2

#define SCM_SUBR_FLAG_FORMAT 0

int scm_subr_func_format(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Modules                                                        */
/*******************************************************************/

#define SCM_SUBR_ARITY_MODULE_P 1
#define SCM_SUBR_ARITY_MODULE_NAME 1
#define SCM_SUBR_ARITY_MODULE_EXPORT 2

#define SCM_SUBR_FLAG_MODULE_P 0
#define SCM_SUBR_FLAG_MODULE_NAME 0
#define SCM_SUBR_FLAG_MODULE_EXPORT 0

int scm_subr_func_module_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_module_name(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_module_export(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Internals (Compiler)                                           */
/*******************************************************************/

#define SCM_SUBR_ARITY_MAKE_ASSEMBLER -1
#define SCM_SUBR_ARITY_ASSEMBLER_ASSIGN_LABEL_ID_I 1
#define SCM_SUBR_ARITY_ASSEMBLER_PUSH_I -3
#define SCM_SUBR_ARITY_ASSEMBLER_COMMIT_I 1
#define SCM_SUBR_ARITY_COMPILER_P 1
#define SCM_SUBR_ARITY_MAKE_COMPILER -1
#define SCM_SUBR_ARITY_COMPILER_BASE_ENV 1
#define SCM_SUBR_ARITY_COMPILER_SELECT_BASE_ENV_I 2
#define SCM_SUBR_ARITY_COMPILER_SELECT_MODULE_I 2
#define SCM_SUBR_ARITY_COMPILER_CURRENT_EXPR 1
#define SCM_SUBR_ARITY_COMPILER_SELECT_EXPR_I 2
#define SCM_SUBR_ARITY_MAKE_SYNTAX 2
#define SCM_SUBR_ARITY_SYNTAX_P 1
#define SCM_SUBR_ARITY_SYNTAX_KEYWORD 1
#define SCM_SUBR_ARITY_SYNTAX_HANDLER 1
#define SCM_SUBR_ARITY_MACRO_P 1
#define SCM_SUBR_ARITY_MAKE_MACRO 2
#define SCM_SUBR_ARITY_MACRO_ENV 1
#define SCM_SUBR_ARITY_MACRO_YIELD_TRANSFORMER 2
#define SCM_SUBR_ARITY_GLOBAL_VARIABLE_BIND -4
#define SCM_SUBR_ARITY_GLOBAL_SYNTAX_BIND -4
#define SCM_SUBR_ARITY_GLOBAL_SYNTAX_REF -3
#define SCM_SUBR_ARITY_COMPILE_QQ_TEMPLATE 1
#define SCM_SUBR_ARITY_SUBSTITUTE_QQ_TEMPLATE -2
#define SCM_SUBR_ARITY_QQ_TEMPLATE_NUM_OF_UNQUOTED 1
#define SCM_SUBR_ARITY_QQ_TEMPLATE_UNQUOTED 2
#define SCM_SUBR_ARITY_IDENTIFIER_P 1
#define SCM_SUBR_ARITY_MAKE_IDENTIFIER 2
#define SCM_SUBR_ARITY_IDENTIFIER_NAME 1
#define SCM_SUBR_ARITY_IDENTIFIER_ENV 1

#define SCM_SUBR_FLAG_MAKE_ASSEMBLER 0
#define SCM_SUBR_FLAG_ASSEMBLER_ASSIGN_LABEL_ID_I 0
#define SCM_SUBR_FLAG_ASSEMBLER_PUSH_I SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_ASSEMBLER_COMMIT_I 0
#define SCM_SUBR_FLAG_COMPILER_P 0
#define SCM_SUBR_FLAG_MAKE_COMPILER 0
#define SCM_SUBR_FLAG_COMPILER_BASE_ENV 0
#define SCM_SUBR_FLAG_COMPILER_SELECT_BASE_ENV_I 0
#define SCM_SUBR_FLAG_COMPILER_SELECT_MODULE_I 0
#define SCM_SUBR_FLAG_COMPILER_CURRENT_EXPR 0
#define SCM_SUBR_FLAG_COMPILER_SELECT_EXPR_I 0
#define SCM_SUBR_FLAG_MAKE_SYNTAX 0
#define SCM_SUBR_FLAG_SYNTAX_P 0
#define SCM_SUBR_FLAG_SYNTAX_KEYWORD 0
#define SCM_SUBR_FLAG_SYNTAX_HANDLER 0
#define SCM_SUBR_FLAG_MACRO_P 0
#define SCM_SUBR_FLAG_MAKE_MACRO 0
#define SCM_SUBR_FLAG_MACRO_ENV 0
#define SCM_SUBR_FLAG_MACRO_YIELD_TRANSFORMER 0
#define SCM_SUBR_FLAG_GLOBAL_VARIABLE_BIND 0
#define SCM_SUBR_FLAG_GLOBAL_SYNTAX_BIND 0
#define SCM_SUBR_FLAG_GLOBAL_SYNTAX_REF 0
#define SCM_SUBR_FLAG_COMPILE_QQ_TEMPLATE 0
#define SCM_SUBR_FLAG_SUBSTITUTE_QQ_TEMPLATE 0
#define SCM_SUBR_FLAG_QQ_TEMPLATE_NUM_OF_UNQUOTED 0
#define SCM_SUBR_FLAG_QQ_TEMPLATE_UNQUOTED 0
#define SCM_SUBR_FLAG_IDENTIFIER_P 0
#define SCM_SUBR_FLAG_MAKE_IDENTIFIER 0
#define SCM_SUBR_FLAG_IDENTIFIER_NAME 0
#define SCM_SUBR_FLAG_IDENTIFIER_ENV 0

int scm_subr_func_make_assembler(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_assembler_assign_label_id_i(ScmObj subr,
                                              int argc, const ScmObj *argv);
int scm_subr_func_assembler_push_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_assembler_commit_i(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_compiler_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_compiler(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_compiler_base_env(ScmObj subr,
                                    int argc, const ScmObj *argv);
int scm_subr_func_compiler_select_base_env_i(ScmObj subr,
                                             int argc, const ScmObj *argv);
int scm_subr_func_compiler_select_module_i(ScmObj subr,
                                           int argc, const ScmObj *argv);
int scm_subr_func_compiler_current_expr(ScmObj subr,
                                        int argc, const ScmObj *argv);
int scm_subr_func_compiler_select_expr_i(ScmObj subr,
                                         int argc, const ScmObj *argv);
int scm_subr_func_syntax_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_syntax(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_syntax_keyword(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_syntax_handler(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_macro_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_macro(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_macro_env(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_macro_yield_transformer(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_global_variable_bind(ScmObj subr,
                                       int argc, const ScmObj *argv);
int scm_subr_func_global_syntax_bind(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_global_syntax_ref(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_compile_qq_template(ScmObj subr,
                                      int argc, const ScmObj *argv);
int scm_subr_func_substitute_qq_template(ScmObj subr,
                                         int argc, const ScmObj *argv);
int scm_subr_func_qq_template_num_of_unquoted(ScmObj subr,
                                              int argc, const ScmObj *argv);
int scm_subr_func_qq_template_unquoted(ScmObj subr,
                                       int argc, const ScmObj *argv);
int scm_subr_func_identifier_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_identifier(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_identifier_name(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_identifier_env(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Internals (dynamic environment)                                */
/*******************************************************************/

#define SCM_SUBR_ARITY_PUSH_DYNAMIC_BINDINGS -1
#define SCM_SUBR_ARITY_POP_DYNAMIC_BINDINGS 0
#define SCM_SUBR_ARITY_PUSH_DYNAMIC_WIND_HANDLER 2
#define SCM_SUBR_ARITY_POP_DYNAMIC_WIND_HANDLER 0
#define SCM_SUBR_ARITY_PUSH_EXCEPTION_HANDLER 1
#define SCM_SUBR_ARITY_POP_EXCEPTION_HANDLER 0

#define SCM_SUBR_FLAG_PUSH_DYNAMIC_BINDINGS 0
#define SCM_SUBR_FLAG_POP_DYNAMIC_BINDINGS 0
#define SCM_SUBR_FLAG_PUSH_DYNAMIC_WIND_HANDLER 0
#define SCM_SUBR_FLAG_POP_DYNAMIC_WIND_HANDLER 0
#define SCM_SUBR_FLAG_PUSH_EXCEPTION_HANDLER 0
#define SCM_SUBR_FLAG_POP_EXCEPTION_HANDLER 0

int scm_subr_func_push_exception_handler(ScmObj subr,
                                         int argc, const ScmObj *argv);
int scm_subr_func_pop_exception_handler(ScmObj subr,
                                        int argc, const ScmObj *argv);
int scm_subr_func_push_dynamic_bindings(ScmObj subr,
                                        int argc, const ScmObj *argv);
int scm_subr_func_pop_dynamic_bindings(ScmObj subr,
                                       int argc, const ScmObj *argv);
int scm_subr_func_push_dynamic_wind_handler(ScmObj subr,
                                            int argc, const ScmObj *argv);
int scm_subr_func_pop_dynamic_wind_handler(ScmObj subr,
                                           int argc, const ScmObj *argv);


/*******************************************************************/
/*  Internals                                                      */
/*******************************************************************/

#define SCM_SUBR_ARITY_REPL 0
#define SCM_SUBR_ARITY_EXEC_FILE -2
#define SCM_SUBR_ARITY_EVAL_STRING 1
#define SCM_SUBR_ARITY_COMPILE_FILE -2

#define SCM_SUBR_FLAG_REPL 0
#define SCM_SUBR_FLAG_EXEC_FILE 0
#define SCM_SUBR_FLAG_EVAL_STRING 0
#define SCM_SUBR_FLAG_COMPILE_FILE 0

int scm_subr_func_repl(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_exec_file(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eval_string(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_compile_file(ScmObj subr, int argc, const ScmObj *argv);


#endif /* INCLUDE_CORE_SUBR_H__ */
