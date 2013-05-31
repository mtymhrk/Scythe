#ifndef INCLUDE_CORE_SUBR_H__
#define INCLUDE_CORE_SUBR_H__

#include <stdint.h>

#include "object.h"

/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

#define SCM_SUBR_ARITY_NULL_P 1

#define SCM_SUBR_FLAG_NULL_P 0

int scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv);


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
/*  Pair and Lists                                                 */
/*******************************************************************/

#define SCM_SUBR_ARITY_PAIR_P 1
#define SCM_SUBR_ARITY_CONS 2
#define SCM_SUBR_ARITY_CAR 1
#define SCM_SUBR_ARITY_CDR 1
#define SCM_SUBR_ARITY_SET_CAR_I 2
#define SCM_SUBR_ARITY_SET_CDR_I 2
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
/*  Input Output                                                   */
/*******************************************************************/

#define SCM_SUBR_ARITY_READ -1
#define SCM_SUBR_ARITY_WRITE -2
#define SCM_SUBR_ARITY_DISPLAY -2
#define SCM_SUBR_ARITY_NEWLINE -1
#define SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT -1

#define SCM_SUBR_FLAG_READ 0
#define SCM_SUBR_FLAG_WRITE 0
#define SCM_SUBR_FLAG_DISPLAY 0
#define SCM_SUBR_FLAG_NEWLINE 0
#define SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT 0

int scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv);


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

#define SCM_SUBR_FLAG_VALUES SCM_PROC_ADJ_UNWISHED

int scm_subr_func_values(ScmObj subr, int argc, const ScmObj *argv);

extern const char *scm_clsr_code_call_with_values;


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

#define SCM_SUBR_ARITY_EVAL_ASM 1
#define SCM_SUBR_ARITY_EVAL 1

#define SCM_SUBR_FLAG_EVAL_ASM 0
#define SCM_SUBR_FLAG_EVAL 0

int scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

#define SCM_SUBR_ARITY_EXIT -1

#define SCM_SUBR_FLAG_EXIT 0

int scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Default Exception Handler                                      */
/*******************************************************************/

#define SCM_SUBR_ARITY_DEFAULT_EXCEPTION_HANDLER 1

#define SCM_SUBR_FLAG_DEFAULT_EXCEPTION_HANDLER 0

int scm_subr_func_default_exception_handler(ScmObj subr,
                                            int argc, const ScmObj *argv);


#endif /* INCLUDE_CORE_SUBR_H__ */
