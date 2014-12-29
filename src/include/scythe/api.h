#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/vminst.h"
#include "scythe/fcd.h"


/*******************************************************************/
/*  Equivalence predicates                                         */
/*******************************************************************/

#define scm_api_eq_P scm_fcd_eq_P
#define scm_api_eqv_P scm_fcd_eqv_P
#define scm_api_equal_P scm_fcd_equal_P


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

#define scm_api_nil_P scm_fcd_nil_P


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/

#define scm_api_boolean_P scm_fcd_boolean_P
#define scm_api_not scm_fcd_not


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

#define scm_api_eof_object_P scm_fcd_eof_object_P


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

#define scm_api_pair_P scm_fcd_pair_P
#define scm_api_list_P scm_fcd_list_P
#define scm_api_memq scm_fcd_memq
#define scm_api_memv scm_fcd_memv
#define scm_api_assq scm_fcd_assq
#define scm_api_assv scm_fcd_assv
#define scm_api_list_copy scm_fcd_list_copy

ScmObj scm_api_cons(ScmObj car, ScmObj cdr);
ScmObj scm_api_car(ScmObj pair);
ScmObj scm_api_cdr(ScmObj pair);
ScmObj scm_api_set_car_i(ScmObj pair, ScmObj elm);
ScmObj scm_api_set_cdr_i(ScmObj pair, ScmObj elm);
ScmObj scm_api_make_list(ScmObj n, ScmObj fill);
ssize_t scm_capi_length(ScmObj lst);
ScmObj scm_api_length(ScmObj lst);
ScmObj scm_api_append_lst(ScmObj lst);
ScmObj scm_api_reverse(ScmObj lst);
ScmObj scm_api_list_tail(ScmObj lst, ScmObj n);
ScmObj scm_api_list_ref(ScmObj lst, ScmObj n);
ScmObj scm_api_list_set_i(ScmObj lst, ScmObj n, ScmObj obj);


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

#define scm_api_number_P scm_fcd_number_P
#define scm_api_complex_P scm_fcd_complex_P
#define scm_api_real_P scm_fcd_real_P
#define scm_api_rational_P scm_fcd_rational_P
#define scm_api_integer_P scm_fcd_integer_P
#define scm_api_exact_P scm_fcd_exact_P
#define scm_api_inexact_P scm_fcd_inexact_P
#define scm_api_exact_integer_P scm_fcd_exact_integer_P
#define scm_api_finite_P scm_fcd_finite_P
#define scm_api_infinite_P scm_fcd_infinite_P
#define scm_api_nan_P scm_fcd_nan_P

ScmObj scm_api_num_eq_P_lst(ScmObj lst);
ScmObj scm_api_num_lt_P_lst(ScmObj lst);
ScmObj scm_api_num_gt_P_lst(ScmObj lst);
ScmObj scm_api_num_le_P_lst(ScmObj lst);
ScmObj scm_api_num_ge_P_lst(ScmObj lst);

ScmObj scm_api_zero_P(ScmObj num);
ScmObj scm_api_positive_P(ScmObj num);
ScmObj scm_api_negative_P(ScmObj num);
ScmObj scm_api_odd_P(ScmObj num);
ScmObj scm_api_even_P(ScmObj num);

ScmObj scm_api_max_lst(ScmObj lst);
ScmObj scm_api_min_lst(ScmObj lst);

ScmObj scm_api_plus_lst(ScmObj lst);
ScmObj scm_api_mul_lst(ScmObj lst);
ScmObj scm_api_minus_lst(ScmObj lst);

int scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_api_floor_quo(ScmObj x, ScmObj y);
ScmObj scm_api_floor_rem(ScmObj x, ScmObj y);
int scm_capi_truncate_div(ScmObj x, ScmObj y,
                          scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_api_truncate_quo(ScmObj x, ScmObj y);
ScmObj scm_api_truncate_rem(ScmObj x, ScmObj y);


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

#define scm_api_symbol_P scm_fcd_symbol_P

ScmObj scm_api_symbol_eq_P_lst(ScmObj lst);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

#define scm_api_char_P scm_fcd_char_P

ScmObj scm_api_char_eq_P_lst(ScmObj lst);
ScmObj scm_api_char_lt_P_lst(ScmObj lst);
ScmObj scm_api_char_gt_P_lst(ScmObj lst);
ScmObj scm_api_char_le_P_lst(ScmObj lst);
ScmObj scm_api_char_ge_P_lst(ScmObj lst);
ScmObj scm_api_char_to_integer(ScmObj chr);
ScmObj scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc);


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

#define scm_api_string_P scm_fcd_string_P
#define scm_api_string_append_lst scm_fcd_string_append_lst
#define scm_api_list_to_string scm_fcd_list_to_string

ScmObj scm_api_string_lst(ScmObj lst);
ScmObj scm_api_string_length(ScmObj str);
ScmObj scm_api_string_bytesize(ScmObj str);
ScmObj scm_api_string_ref(ScmObj str, ScmObj pos);
ScmObj scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr);
ScmObj scm_api_string_eq_P_lst(ScmObj lst);
ScmObj scm_api_string_lt_P_lst(ScmObj lst);
ScmObj scm_api_string_gt_P_lst(ScmObj lst);
ScmObj scm_api_string_le_P_lst(ScmObj lst);
ScmObj scm_api_string_ge_P_lst(ScmObj lst);
ScmObj scm_api_substring(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_api_string_to_list(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_api_string_copy(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_api_string_copy_i(ScmObj to, ScmObj at,
                             ScmObj from, ScmObj start, ScmObj end);
ScmObj scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end);


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

#define scm_api_vector_P scm_fcd_vector_P
#define scm_api_list_to_vector scm_fcd_list_to_vector
#define scm_api_vector_append_lst scm_fcd_vector_append_lst

ScmObj scm_api_make_vector(ScmObj len, ScmObj fill);
ScmObj scm_api_vector_lst(ScmObj lst);
ScmObj scm_api_vector_length(ScmObj vec);
ScmObj scm_api_vector_ref(ScmObj vec, ScmObj idx);
ScmObj scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj);
ScmObj scm_api_vector_to_list(ScmObj vec, ScmObj start, ScmObj end);
ScmObj scm_api_vector_to_string(ScmObj vec, ScmObj start, ScmObj end);
ScmObj scm_api_string_to_vector(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_api_vector_copy(ScmObj vec, ScmObj start, ScmObj end);
ScmObj scm_api_vector_copy_i(ScmObj to, ScmObj at,
                             ScmObj from, ScmObj start, ScmObj end);
ScmObj scm_api_vector_fill_i(ScmObj vec, ScmObj fill, ScmObj start, ScmObj end);


/*******************************************************************/
/*  Bytevectors                                                    */
/*******************************************************************/

#define scm_api_bytevector_P scm_fcd_bytevector_P


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

#define scm_capi_pop_exception_handler scm_fcd_pop_exception_handler
#define scm_api_error_object_P scm_fcd_error_object_P
#define scm_api_read_error_P scm_fcd_read_error_P
#define scm_api_file_error_P scm_fcd_file_error_P

#define scm_capi_error scm_fcd_error
#define scm_capi_read_error scm_fcd_read_error
#define scm_capi_file_error scm_fcd_file_error

int scm_capi_raise(ScmObj obj);
int scm_capi_raise_continuable(ScmObj obj);
int scm_capi_push_exception_handler(ScmObj handler);
ScmObj scm_api_error_lst(ScmObj msg, ScmObj irris);
ScmObj scm_api_error_object_message(ScmObj obj);
ScmObj scm_api_error_object_irritants(ScmObj obj);


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

#define scm_api_port_P scm_fcd_port_P
#define scm_api_input_port_P scm_fcd_input_port_P
#define scm_api_output_port_P scm_fcd_output_port_P
#define scm_api_textual_port_P scm_fcd_textual_port_P
#define scm_api_binary_port_P scm_fcd_bignum_P
#define scm_api_input_port_open_P scm_fcd_input_port_open_P
#define scm_api_output_port_open_P scm_fcd_output_port_open_P
#define scm_api_open_output_string scm_fcd_open_output_string
#define scm_api_open_output_bytevector scm_fcd_open_output_bytevector

ScmObj scm_api_open_input_file(ScmObj path);
ScmObj scm_api_open_binary_input_file(ScmObj path);
ScmObj scm_api_open_output_file(ScmObj path);
ScmObj scm_api_close_port(ScmObj port);
ScmObj scm_api_close_input_port(ScmObj port);
ScmObj scm_api_close_output_port(ScmObj port);
ScmObj scm_api_open_input_string(ScmObj str);
ScmObj scm_api_get_output_string(ScmObj port);
ScmObj scm_api_open_input_bytevector(ScmObj vec);
ScmObj scm_api_get_output_bytevector(ScmObj port);


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj scm_api_read(ScmObj port);
ScmObj scm_api_read_char(ScmObj port);
ScmObj scm_api_peek_char(ScmObj port);
ScmObj scm_api_read_line(ScmObj port);
ScmObj scm_api_char_ready_P(ScmObj port);
ScmObj scm_api_read_string(ScmObj n, ScmObj port);


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

ScmObj scm_api_write_shared(ScmObj obj, ScmObj port);
ScmObj scm_api_write(ScmObj obj, ScmObj port);
ScmObj scm_api_write_simple(ScmObj obj, ScmObj port);
ScmObj scm_api_display(ScmObj obj, ScmObj port);
ScmObj scm_api_newline(ScmObj port);
ScmObj scm_api_write_char(ScmObj chr, ScmObj port);
ScmObj scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end);
ScmObj scm_api_flush_output_port(ScmObj port);


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

#define scm_api_procedure_P scm_fcd_procedure_P


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmObj scm_api_make_parameter(ScmObj conv);


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

#define scm_api_syntax_P scm_fcd_syntax_P

ScmObj scm_api_make_syntax(ScmObj keyword, ScmObj handler);
ScmObj scm_api_syntax_keyword(ScmObj syx);
ScmObj scm_api_syntax_handler(ScmObj syx);


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj scm_api_assemble(ScmObj lst, ScmObj iseq);


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

#define scm_api_compiler_P scm_fcd_compiler_P
#define scm_api_make_compiler scm_fcd_make_compiler

ScmObj scm_api_compiler_current_module(ScmObj cmpl);
ScmObj scm_api_compiler_current_expr(ScmObj cmpl);
ScmObj scm_api_compiler_select_module_i(ScmObj cmpl, ScmObj mod);
ScmObj scm_api_compiler_select_expr_i(ScmObj cmpl, ScmObj expr);
int scm_capi_compiler_assign_label_id_i(ScmObj cmpl);


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

ScmObj scm_api_module_name(ScmObj module);
int scm_capi_define_global_syx(ScmObj module,
                               ScmObj sym, ScmObj syx, bool export);
int scm_capi_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx);


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

#define scm_api_capture_cont scm_fcd_capture_cont


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int scm_capi_trampolining(ScmObj proc, ScmObj arg,
                          ScmObj postproc, ScmObj handover);


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

ScmObj scm_api_format_lst(ScmObj fmt, ScmObj lst);


/*******************************************************************/
/*  Exit                                                           */
/*******************************************************************/

ScmObj scm_api_exit(ScmObj obj);


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

ScmEvaluator *scm_capi_evaluator(void);
void scm_capi_evaluator_end(ScmEvaluator *ev);
int scm_capi_evaluator_make_vm(ScmEvaluator *ev);
int scm_capi_evaluator_load_core(ScmEvaluator *ev);
int scm_capi_evaluator_delete_vm(ScmEvaluator *ev);
int scm_capi_run_repl(ScmEvaluator *ev);
int scm_capi_exec_file(const char *path, ScmEvaluator *ev);
int scm_capi_exec_cstr(const char *expr, ScmEvaluator *ev);
int scm_capi_compile_file(const char *path, ScmEvaluator *ev);


/*******************************************************************/
/*  XXX                                                            */
/*******************************************************************/

int scm_capi_load_iseq(ScmObj iseq);


#endif /* INCLUDE_API_H__ */
