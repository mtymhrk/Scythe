#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "object.h"
#include "encoding.h"
#include "api_enum.h"
#include "api_type.h"


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

void scm_capi_fatal(const char *msg);
void scm_capi_fatalf(const char *fmt, ...);
bool scm_capi_fatal_p(void);


/*******************************************************************/
/*  C Stack                                                        */
/*******************************************************************/

#define SCM_STACK_FRAME                                                 \
  __attribute__((__cleanup__(scm_capi_ref_stack_restore)))                  \
  ScmRefStackInfo SCM_CONCAT_SYMBOL__(scm_ref_stack_frame__, __LINE__)  \
  = { NULL, NULL };                                                     \
  scm_capi_ref_stack_save(&SCM_CONCAT_SYMBOL__(scm_ref_stack_frame__, __LINE__));

#define SCM_STACK_PUSH(...)                                             \
  scm_capi_ref_stack_push(0, __VA_ARGS__, NULL)

#define SCM_STACK_PUSH_ARY(ary, n) \
  scm_capi_ref_stack_push_ary(ary, n)

#define SCM_STACK_FRAME_PUSH(...) \
  SCM_STACK_FRAME; SCM_STACK_PUSH(__VA_ARGS__);

void scm_capi_ref_stack_push_ary(ScmObj *ary, size_t n);
void scm_capi_ref_stack_push(int dummy, ...);
void scm_capi_ref_stack_save(ScmRefStackInfo *info);
void scm_capi_ref_stack_restore(ScmRefStackInfo *info);


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

inline void *
scm_capi_malloc(size_t size)
{
  void *p = malloc(size);
  if (p == NULL) scm_capi_fatal("memory allocation error");
  return p;
}

inline void *
scm_capi_free(void *ptr)
{
  free(ptr);
  return NULL;
}

inline void *
scm_capi_realloc(void *ptr, size_t size)
{
  void *p = realloc(ptr, size);
  if (p == NULL) scm_capi_fatal("memory allocation error");
  return p;
}

ScmObj scm_capi_mem_alloc_heap(ScmTypeInfo *type, size_t add_size);
ScmObj scm_capi_mem_alloc_root(ScmTypeInfo *type, size_t add_size);
ScmObj scm_capi_mem_alloc(ScmTypeInfo *otype, size_t add_size,
                          SCM_MEM_TYPE_T mtype);
ScmObj scm_capi_mem_free_root(ScmObj obj);
ScmRef scm_capi_mem_register_extra_rfrn(ScmRef ref);

void scm_capi_gc_start(void);
void scm_capi_gc_enable(void);
void scm_capi_gc_disable(void);


/*******************************************************************/
/*  NULL Value                                                     */
/*******************************************************************/

bool scm_capi_null_value_p(ScmObj obj);


/*******************************************************************/
/*  Equivalence predicates                                         */
/*******************************************************************/

bool scm_capi_eq_p(ScmObj obj1, ScmObj obj2);
int scm_capi_eq(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_api_eq_P(ScmObj obj1, ScmObj obj2);
int scm_capi_eqv(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_api_eqv_P(ScmObj obj1, ScmObj obj2);
int scm_capi_equal(ScmObj obj1, ScmObj obj2, bool *rslt);
ScmObj scm_api_equal_P(ScmObj obj1, ScmObj obj2);


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

ScmObj scm_api_nil(void);
bool scm_capi_nil_p(ScmObj obj);
ScmObj scm_api_nil_P(ScmObj obj);

#define SCM_NIL_OBJ scm_api_nil()


/*******************************************************************/
/*  Booleans                                                       */
/*******************************************************************/

bool scm_capi_boolean_p(ScmObj obj);
ScmObj scm_api_boolean_P(ScmObj obj);
ScmObj scm_api_true(void);
ScmObj scm_api_false(void);
bool scm_capi_true_object_p(ScmObj obj);
bool scm_capi_false_object_p(ScmObj obj);
bool scm_capi_true_p(ScmObj obj);
bool scm_capi_false_p(ScmObj obj);
ScmObj scm_api_not(ScmObj obj);

#define SCM_TRUE_OBJ scm_api_true()
#define SCM_FALSE_OBJ scm_api_false()


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

ScmObj scm_api_eof(void);
bool scm_capi_eof_object_p(ScmObj obj);

#define SCM_EOF_OBJ scm_api_eof()


/*******************************************************************/
/*  undef                                                          */
/*******************************************************************/

ScmObj scm_api_undef(void);
bool scm_capi_undef_object_p(ScmObj obj);

#define SCM_UNDEF_OBJ scm_api_undef()


/*******************************************************************/
/*  Landmine                                                       */
/*******************************************************************/

bool scm_capi_landmine_object_p(ScmObj obj);


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

bool scm_capi_pair_p(ScmObj pair);
ScmObj scm_api_pair_P(ScmObj pair);
ScmObj scm_api_cons(ScmObj car, ScmObj cdr);
ScmObj scm_api_car(ScmObj pair);
ScmObj scm_api_cdr(ScmObj pair);
int scm_capi_set_car_i(ScmObj pair, ScmObj elm);
ScmObj scm_api_set_car_i(ScmObj pair, ScmObj elm);
int scm_capi_set_cdr_i(ScmObj pair, ScmObj elm);
ScmObj scm_api_set_cdr_i(ScmObj pair, ScmObj elm);
ScmObj scm_capi_cxr(ScmObj pair, const char *dir);
ScmObj scm_api_list_P(ScmObj lst);
ScmObj scm_capi_make_list(size_t n, ScmObj fill);
ScmObj scm_api_make_list(ScmObj n, ScmObj fill);
ScmObj scm_capi_list_cv(const ScmObj *elm, size_t n);
ScmObj scm_capi_list(size_t n, ...);
ssize_t scm_capi_length(ScmObj lst);
ScmObj scm_api_length(ScmObj lst);
ScmObj scm_capi_append_lst(ScmObj lst);
ScmObj scm_capi_append_cv(const ScmObj *lists, size_t n);
ScmObj scm_capi_append(size_t n, ...);
ScmObj scm_api_reverse(ScmObj lst);
ScmObj scm_capi_list_tail(ScmObj lst, size_t n);
ScmObj scm_api_list_tail(ScmObj lst, ScmObj n);
ScmObj scm_capi_list_ref(ScmObj lst, size_t n);
ScmObj scm_api_list_ref(ScmObj lst, ScmObj n);
int scm_capi_list_set_i(ScmObj lst, size_t n, ScmObj obj);
ScmObj scm_api_list_set_i(ScmObj lst, ScmObj n, ScmObj obj);
ScmObj scm_capi_memq(ScmObj obj, ScmObj lst);
ScmObj scm_capi_memv(ScmObj obj, ScmObj lst);
ScmObj scm_capi_member(ScmObj obj, ScmObj lst,
                       ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_capi_assq(ScmObj obj, ScmObj alist);
ScmObj scm_capi_assv(ScmObj obj, ScmObj alist);
ScmObj scm_capi_assoc(ScmObj obj, ScmObj alist,
                      ScmObj (*cmp)(ScmObj x, ScmObj y));
ScmObj scm_api_list_copy(ScmObj lst);


/*******************************************************************/
/*  Numbers                                                        */
/*******************************************************************/

bool scm_capi_fixnum_p(ScmObj obj);
ScmObj scm_api_fixnum_P(ScmObj obj);
bool scm_capi_bignum_p(ScmObj obj);
ScmObj scm_api_bignum_P(ScmObj obj);

bool scm_capi_number_p(ScmObj obj);
ScmObj scm_api_number_P(ScmObj obj);
bool scm_capi_complex_p(ScmObj obj);
ScmObj scm_api_complex_P(ScmObj obj);
bool scm_capi_real_p(ScmObj obj);
ScmObj scm_api_real_P(ScmObj obj);
bool scm_capi_rational_p(ScmObj obj);
ScmObj scm_api_rational_P(ScmObj obj);
bool scm_capi_integer_p(ScmObj obj);
ScmObj scm_api_integer_P(ScmObj obj);

bool scm_capi_exact_p(ScmObj obj);
ScmObj scm_api_exact_P(ScmObj obj);
bool scm_capi_inexact_p(ScmObj obj);
ScmObj scm_api_inexact_P(ScmObj obj);

bool scm_capi_exact_integer_p(ScmObj obj);
ScmObj scm_api_exact_integer_P(ScmObj obj);

bool scm_capi_finite_p(ScmObj obj);
ScmObj scm_api_finite_P(ScmObj obj);
bool scm_capi_infinite_p(ScmObj obj);
ScmObj scm_api_infinite_P(ScmObj obj);
bool scm_capi_nan_p(ScmObj obj);
ScmObj scm_api_nan_P(ScmObj obj);

ScmObj scm_capi_make_number_from_literal(const char *literal, size_t size);
ScmObj scm_capi_make_number_from_sword(scm_sword_t num);
ScmObj scm_capi_make_number_from_size_t(size_t num);

int scm_capi_num_eq(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_capi_num_eq_P_lst(ScmObj lst);
ScmObj scm_api_num_eq_P(ScmObj n1, ScmObj n2);
int scm_capi_num_lt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_capi_num_lt_P_lst(ScmObj lst);
ScmObj scm_api_num_lt_P(ScmObj n1, ScmObj n2);
int scm_capi_num_gt(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_capi_num_gt_P_lst(ScmObj lst);
ScmObj scm_api_num_gt_P(ScmObj n1, ScmObj n2);
int scm_capi_num_le(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_capi_num_le_P_lst(ScmObj lst);
ScmObj scm_api_num_le_P(ScmObj n1, ScmObj n2);
int scm_capi_num_ge(ScmObj n1, ScmObj n2, bool *rslt);
ScmObj scm_capi_num_ge_P_lst(ScmObj lst);
ScmObj scm_api_num_ge_P(ScmObj n1, ScmObj n2);

bool scm_capi_zero_p(ScmObj num);
ScmObj scm_api_zero_P(ScmObj num);
bool scm_capi_positive_p(ScmObj num);
ScmObj scm_api_positive_P(ScmObj num);
bool scm_capi_negative_p(ScmObj num);
ScmObj scm_api_negative_P(ScmObj num);
bool scm_capi_odd_p(ScmObj num);
ScmObj scm_api_odd_P(ScmObj num);
bool scm_capi_even_p(ScmObj num);
ScmObj scm_api_even_P(ScmObj num);

ScmObj scm_api_max(ScmObj n1, ScmObj n2);
ScmObj scm_capi_max_lst(ScmObj lst);
ScmObj scm_api_min(ScmObj n1, ScmObj n2);
ScmObj scm_capi_min_lst(ScmObj lst);

ScmObj scm_api_plus(ScmObj x, ScmObj y);
ScmObj scm_capi_plus_lst(ScmObj lst);
ScmObj scm_api_mul(ScmObj x, ScmObj y);
ScmObj scm_capi_mul_lst(ScmObj lst);
ScmObj scm_api_minus(ScmObj x, ScmObj y);
ScmObj scm_capi_minus_lst(ScmObj lst);

ScmObj scm_api_abs(ScmObj num);

int scm_capi_floor_div(ScmObj x, ScmObj y, scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_api_floor_quo(ScmObj x, ScmObj y);
ScmObj scm_api_floor_rem(ScmObj x, ScmObj y);
int scm_capi_truncate_div(ScmObj x, ScmObj y,
                          scm_csetter_t *q, scm_csetter_t *r);
ScmObj scm_api_truncate_quo(ScmObj x, ScmObj y);
ScmObj scm_api_truncate_rem(ScmObj x, ScmObj y);

ScmObj scm_api_exact(ScmObj num);
ScmObj scm_api_inexact(ScmObj num);

int scm_capi_integer_to_sword(ScmObj num, scm_sword_t *w);
int scm_capi_integer_to_size_t(ScmObj num, size_t *s);


/*******************************************************************/
/*  Symbols                                                        */
/*******************************************************************/

bool scm_capi_symbol_p(ScmObj obj);
ScmObj scm_api_symbol_P(ScmObj obj);
int scm_capi_symbol_eq(ScmObj sym1, ScmObj sym2, bool *rslt);
ScmObj scm_capi_symbol_eq_P_lst(ScmObj lst);
ScmObj scm_api_symbol_eq_P(ScmObj sym1, ScmObj sym2);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);
ScmObj scm_capi_make_symbol_from_cstr(const char *str, ScmEncoding *enc);
ScmObj scm_capi_make_symbol_from_bin(const void *data,
                                     size_t size, ScmEncoding *enc);
ssize_t scm_capi_symbol_bytesize(ScmObj sym);
char *scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size);
size_t scm_capi_symbol_hash_value(ScmObj sym);


/*******************************************************************/
/*  Characters                                                     */
/*******************************************************************/

ScmObj scm_capi_make_char(const scm_char_t *chr, ScmEncoding *enc);
ScmObj scm_api_make_char_newline(ScmEncoding *enc);
ScmObj scm_api_make_char_space(ScmEncoding *enc);
bool scm_capi_char_p(ScmObj obj);
ScmObj scm_api_char_P(ScmObj obj);
int scm_capi_char_eq(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_capi_char_eq_P_lst(ScmObj lst);
ScmObj scm_api_char_eq_P(ScmObj chr1, ScmObj chr2);
int scm_capi_char_lt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_capi_char_lt_P_lst(ScmObj lst);
ScmObj scm_api_char_lt_P(ScmObj chr1, ScmObj chr2);
int scm_capi_char_gt(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_capi_char_gt_P_lst(ScmObj lst);
ScmObj scm_api_char_gt_P(ScmObj chr1, ScmObj chr2);
int scm_capi_char_le(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_capi_char_le_P_lst(ScmObj lst);
ScmObj scm_api_char_le_P(ScmObj chr1, ScmObj chr2);
int scm_capi_char_ge(ScmObj chr1, ScmObj chr2, bool *rslt);
ScmObj scm_capi_char_ge_P_lst(ScmObj lst);
ScmObj scm_api_char_ge_P(ScmObj chr1, ScmObj chr2);
ScmObj scm_api_char_to_integer(ScmObj chr);
ScmObj scm_capi_integer_to_char(ScmObj num, ScmEncoding *enc);
ssize_t scm_capi_char_to_cchr(ScmObj chr, scm_char_t *cp);
ScmEncoding *scm_capi_char_encoding(ScmObj chr);


/*******************************************************************/
/*  Strings                                                        */
/*******************************************************************/

bool scm_capi_string_p(ScmObj obj);
ScmObj scm_api_string_P(ScmObj obj);
ScmObj scm_capi_make_string(size_t n, ScmObj chr);
ScmObj scm_api_make_string(ScmObj n, ScmObj chr);
ScmObj scm_capi_make_string_from_cstr(const char *str, ScmEncoding *enc);
ScmObj scm_capi_make_string_from_bin(const void *data,
                                     size_t size, ScmEncoding *enc);
ScmObj scm_api_string_lst(ScmObj lst);
ScmObj scm_capi_string_cv(const ScmObj *chr, size_t n);
size_t scm_capi_string(size_t n, ...);
ssize_t scm_capi_string_length(ScmObj str);
ScmObj scm_api_string_length(ScmObj str);
ssize_t scm_capi_string_bytesize(ScmObj str);
ScmObj scm_api_string_bytesize(ScmObj str);
ScmObj scm_capi_string_ref(ScmObj str, size_t pos);
ScmObj scm_api_string_ref(ScmObj str, ScmObj pos);
int scm_capi_string_set_i(ScmObj str, size_t pos, ScmObj chr);
ScmObj scm_api_string_set_i(ScmObj str, ScmObj pos, ScmObj chr);
int scm_capi_string_eq(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_capi_string_eq_P_lst(ScmObj lst);
ScmObj scm_api_string_eq_P(ScmObj s1, ScmObj s2);
int scm_capi_string_lt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_capi_string_lt_P_lst(ScmObj lst);
ScmObj scm_api_string_lt_P(ScmObj s1, ScmObj s2);
int scm_capi_string_gt(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_capi_string_gt_P_lst(ScmObj lst);
ScmObj scm_api_string_gt_P(ScmObj s1, ScmObj s2);
int scm_capi_string_le(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_capi_string_le_P_lst(ScmObj lst);
ScmObj scm_api_string_le_P(ScmObj s1, ScmObj s2);
int scm_capi_string_ge(ScmObj s1, ScmObj s2, bool *rslt);
ScmObj scm_capi_string_ge_P_lst(ScmObj lst);
ScmObj scm_api_string_ge_P(ScmObj s1, ScmObj s2);
ScmObj scm_api_string_upcase(ScmObj str);
ScmObj scm_api_string_downcase(ScmObj str);
ScmObj scm_capi_substring(ScmObj str, size_t start, size_t end);
ScmObj scm_api_substring(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_capi_string_append_lst(ScmObj lst);
ScmObj scm_capi_string_append_cv(ScmObj *ary, size_t n);
ScmObj scm_capi_string_append(size_t n, ...);
ScmObj scm_capi_string_to_list(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_api_string_to_list(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_api_list_to_string(ScmObj lst);
ScmObj scm_capi_string_copy(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_api_string_copy(ScmObj str, ScmObj start, ScmObj end);
int scm_capi_string_copy_i(ScmObj to, size_t at,
                           ScmObj from, ssize_t start, ssize_t end);
ScmObj scm_api_string_copy_i(ScmObj to, ScmObj at,
                             ScmObj from, ScmObj start, ScmObj end);
int scm_capi_string_fill_i(ScmObj str, ScmObj fill, ssize_t start, ssize_t end);
ScmObj scm_api_string_fill_i(ScmObj str, ScmObj fill, ScmObj start, ScmObj end);
ScmEncoding *scm_capi_string_encoding(ScmObj str);
char *scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size);
int scm_capi_string_push(ScmObj str, scm_char_t chr, ScmEncoding *enc);
ScmObj scm_api_string_push(ScmObj str, ScmObj c);


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

bool scm_capi_vector_p(ScmObj obj);
ScmObj scm_api_vector_P(ScmObj obj);
ScmObj scm_capi_make_vector(size_t len, ScmObj fill);
ScmObj scm_api_make_vector(ScmObj len, ScmObj fill);
ScmObj scm_capi_vector_lst(ScmObj lst);
ScmObj scm_capi_vector_cv(const ScmObj *elm, size_t n);
ScmObj scm_capi_vector(size_t n, ...);
ssize_t scm_capi_vector_length(ScmObj vec);
ScmObj scm_api_vector_length(ScmObj vec);
ScmObj scm_capi_vector_ref(ScmObj vec, size_t idx);
ScmObj scm_api_vector_ref(ScmObj vec, ScmObj idx);
int scm_capi_vector_set_i(ScmObj vec, size_t idx, ScmObj obj);
ScmObj scm_api_vector_set_i(ScmObj vec, ScmObj idx, ScmObj obj);
ScmObj scm_capi_vector_to_list(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_api_vector_to_list(ScmObj vec, ScmObj start, ScmObj end);
ScmObj scm_api_list_to_vector(ScmObj lst);
ScmObj scm_capi_vector_to_string(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_api_vector_to_string(ScmObj vec, ScmObj start, ScmObj end);
ScmObj scm_capi_string_to_vector(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_api_string_to_vector(ScmObj str, ScmObj start, ScmObj end);
ScmObj scm_capi_vector_copy(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_api_vector_copy(ScmObj vec, ScmObj start, ScmObj end);
int scm_capi_vector_copy_i(ScmObj to, size_t at,
                           ScmObj from, ssize_t start, ssize_t end);
ScmObj scm_api_vector_copy_i(ScmObj to, ScmObj at,
                             ScmObj from, ScmObj start, ScmObj end);
ScmObj scm_capi_vector_append_lst(ScmObj lst);
ScmObj scm_capi_vector_append_cv(ScmObj *ary, size_t n);
ScmObj scm_capi_vector_append(size_t n, ...);
int scm_capi_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end);
ScmObj scm_api_vector_fill_i(ScmObj vec, ScmObj fill, ScmObj start, ScmObj end);


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

int scm_capi_raise(ScmObj obj);
int scm_capi_raise_for_subr(ScmObj obj);
int scm_capi_raise_continuable_for_subr(ScmObj obj);
bool scm_capi_raised_p(void);
ScmObj scm_capi_raised_obj(void);
void scm_capi_discard_raised_obj(void);
int scm_capi_push_exception_handler(ScmObj handler);
int scm_capi_pop_exception_handler(void);
int scm_capi_error(const char *msg, size_t n, ...);
int scm_capi_read_error(const char *msg, size_t n, ...);
int scm_capi_file_error(const char *msg, size_t n, ...);
int scm_capi_error_for_subr(ScmObj msg, ScmObj irris);
bool scm_capi_error_object_p(ScmObj obj);
ScmObj scm_api_error_object_P(ScmObj obj);
ScmObj scm_api_error_object_message(ScmObj obj);
ScmObj scm_api_error_object_irritants(ScmObj obj);
ScmObj scm_api_read_error_P(ScmObj obj);
ScmObj scm_api_file_error_P(ScmObj obj);


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

bool scm_capi_port_p(ScmObj obj);
ScmObj scm_api_port_P(ScmObj obj);
bool scm_capi_input_port_p(ScmObj obj);
ScmObj scm_api_input_port_P(ScmObj obj);
bool scm_capi_output_port_p(ScmObj obj);
ScmObj scm_api_output_port_P(ScmObj obj);
bool scm_capi_textual_port_p(ScmObj obj);
ScmObj scm_api_textual_port_P(ScmObj obj);
bool scm_capi_binary_port_p(ScmObj obj);
ScmObj scm_api_binary_port_P(ScmObj obj);
int scm_capi_input_port_open_p(ScmObj port, bool *rslt);
ScmObj scm_api_input_port_open_P(ScmObj port);
int scm_capi_output_port_open_p(ScmObj port, bool *rslt);
ScmObj scm_api_output_port_open_P(ScmObj port);

ScmObj scm_capi_open_input_fd(int fd, const char *enc);
ScmObj scm_capi_open_output_fd(int fd, const char *enc);
ScmObj scm_capi_open_input_file(const char *path, const char *enc);
ScmObj scm_api_open_input_file(ScmObj path);
ScmObj scm_capi_open_output_file(const char *path, const char *enc);
ScmObj scm_api_open_output_file(ScmObj path);
ScmObj scm_api_close_port(ScmObj port);
ScmObj scm_api_close_input_port(ScmObj port);
ScmObj scm_api_close_output_port(ScmObj port);
ScmObj scm_capi_open_input_string_cstr(const char *str, const char *enc);
ScmObj scm_api_open_input_string(ScmObj str);
ScmObj scm_api_open_output_string();
ScmObj scm_api_get_output_string(ScmObj port);
const char *scm_capi_port_encoding(ScmObj port);
ScmEncoding *scm_capi_port_internal_encoding(ScmObj port);


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj scm_api_read(ScmObj port);
ssize_t scm_capi_read_cchr(scm_char_t *chr, ScmObj port);
ScmObj scm_api_read_char(ScmObj port);
ssize_t scm_capi_peek_cchr(scm_char_t *chr, ScmObj port);
ScmObj scm_api_peek_char(ScmObj port);
ScmObj scm_api_read_line(ScmObj port);
int scm_capi_char_ready(ScmObj port, bool *rslt);
ScmObj scm_api_char_ready_P(ScmObj port);
ScmObj scm_api_read_string(ScmObj n, ScmObj port);


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

ScmObj scm_api_write(ScmObj obj, ScmObj port);
ScmObj scm_api_write_simple(ScmObj obj, ScmObj port);
ScmObj scm_api_display(ScmObj obj, ScmObj port);
ScmObj scm_api_newline(ScmObj port);
int scm_capi_write_cchr(scm_char_t chr, ScmEncoding *enc, ScmObj port);
ScmObj scm_api_write_char(ScmObj chr, ScmObj port);
int scm_capi_write_cstr(const char *str, ScmEncoding *enc, ScmObj port);
ssize_t scm_capi_write_string(ScmObj str, ScmObj port,
                              ssize_t start, ssize_t end);
ScmObj scm_api_write_string(ScmObj str, ScmObj port, ScmObj start, ScmObj end);
ScmObj scm_api_flush_output_port(ScmObj port);


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

bool scm_capi_procedure_p(ScmObj proc);
int scm_capi_arity(ScmObj proc, int *arity);
int scm_capi_procedure_flg_set_p(ScmObj proc, SCM_PROC_FLG_T flg, bool *rslt);


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj scm_capi_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags,
                               ScmObj module);
int scm_api_call_subrutine(ScmObj subr, int argc, const ScmObj *argv);
bool scm_capi_subrutine_p(ScmObj obj);
int scm_capi_subrutine_module(ScmObj subr, scm_csetter_t *mod);


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

ScmObj scm_capi_make_closure(ScmObj iseq, ScmObj env, int arity);
bool scm_capi_closure_p(ScmObj obj);
ScmObj scm_capi_closure_to_iseq(ScmObj clsr);
scm_byte_t *scm_capi_closure_to_ip(ScmObj clsr);
int scm_capi_closure_env(ScmObj clsr, scm_csetter_t *env);


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

ScmObj scm_capi_make_parameter(ScmObj conv);
bool scm_capi_parameter_p(ScmObj obj);
int scm_capi_parameter_init_val(ScmObj prm, scm_csetter_t *val);
int scm_capi_parameter_converter(ScmObj prm, scm_csetter_t *conv);
int scm_capi_parameter_set_init_val(ScmObj prm, ScmObj val);
ScmObj scm_capi_parameter_value(ScmObj prm);


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

ScmObj scm_capi_make_syntax(ScmObj keyword, ScmSyntaxHandlerFunc handler);
bool scm_capi_syntax_p(ScmObj obj);
ScmObj scm_api_syntax_keyword(ScmObj syx);
int scm_capi_syntax_handler(ScmObj syx, ScmSyntaxHandlerFunc *handler);


/*******************************************************************/
/*  ISeq                                                           */
/*******************************************************************/

ScmObj scm_api_make_iseq(void);
bool scm_capi_iseq_p(ScmObj obj);
scm_byte_t *scm_capi_iseq_to_ip(ScmObj iseq);
ssize_t scm_capi_iseq_length(ScmObj iseq);
ssize_t scm_capi_iseq_push_opfmt_noarg(ScmObj iseq, SCM_OPCODE_T op);
ssize_t scm_capi_iseq_push_opfmt_obj(ScmObj iseq, SCM_OPCODE_T op, ScmObj val);
ssize_t scm_capi_iseq_push_opfmt_obj_obj(ScmObj iseq, SCM_OPCODE_T op,
                                         ScmObj val1, ScmObj val2);
ssize_t scm_capi_iseq_push_opfmt_si(ScmObj iseq, SCM_OPCODE_T op, int val);
ssize_t scm_capi_iseq_push_opfmt_si_si(ScmObj iseq, SCM_OPCODE_T op,
                                       int val1, int val2);
ssize_t scm_capi_iseq_push_opfmt_si_si_obj(ScmObj iseq, SCM_OPCODE_T op,
                                           int val1, int val2, ScmObj obj);
ssize_t scm_capi_iseq_push_opfmt_iof(ScmObj iseq,
                                     SCM_OPCODE_T op, int offset);
ssize_t scm_capi_iseq_set_si(ScmObj iseq, size_t idx, int val);

int scm_capi_opcode_to_opfmt(int opcode);

#define SCM_CAPI_INST_FETCH_OP(ip, op)                  \
  do {                                                  \
    (op) = *(unsigned short *)(ip);                      \
    (ip) = (scm_byte_t *)(ip) + sizeof(unsigned short); \
  } while (0)

scm_byte_t *scm_capi_inst_fetch_oprand_obj(scm_byte_t *ip, scm_csetter_t *obj);
scm_byte_t *scm_capi_inst_fetch_oprand_obj_obj(scm_byte_t *ip,
                                               scm_csetter_t *obj1,
                                               scm_csetter_t *obj2);
scm_byte_t *scm_capi_inst_fetch_oprand_si(scm_byte_t *ip, int *si);
scm_byte_t *scm_capi_inst_fetch_oprand_si_si(scm_byte_t *ip,
                                             int *si1, int *si2);
scm_byte_t *scm_capi_inst_fetch_oprand_si_si_obj(scm_byte_t *ip,
                                                 int *si1, int *si2,
                                                 scm_csetter_t *obj);
scm_byte_t *scm_capi_inst_fetch_oprand_iof(scm_byte_t *ip, int *offset);

int scm_capi_inst_update_oprand_obj(scm_byte_t *ip, ScmObj clsr, ScmObj obj);


/*******************************************************************/
/*  Assembler                                                      */
/*******************************************************************/

ScmObj scm_api_assemble(ScmObj lst, ScmObj iseq);


/*******************************************************************/
/*  Compiler                                                       */
/*******************************************************************/

bool scm_capi_compiler_p(ScmObj obj);
ScmObj scm_api_compiler_P(ScmObj obj);
ScmObj scm_api_make_compiler(ScmObj mod);
ScmObj scm_api_compiler_current_module(ScmObj cmpl);
ScmObj scm_api_compiler_current_expr(ScmObj cmpl);
ScmObj scm_api_compiler_select_module_i(ScmObj cmpl, ScmObj mod);
ScmObj scm_api_compiler_select_expr_i(ScmObj cmpl, ScmObj expr);
int scm_capi_compiler_assign_label_id_i(ScmObj cmpl);


/*******************************************************************/
/*  Module                                                         */
/*******************************************************************/

bool scm_capi_gloc_p(ScmObj obj);
int scm_capi_gloc_value(ScmObj gloc, scm_csetter_t *val);
int scm_capi_gloc_symbol(ScmObj gloc, scm_csetter_t *sym);
int scm_capi_gloc_bind(ScmObj gloc, ScmObj val);
ScmObj scm_api_make_module(ScmObj name);
bool scm_capi_module_p(ScmObj obj);
ScmObj scm_api_module_name(ScmObj module);
int scm_capi_import(ScmObj module, ScmObj imported, bool unexport);
int scm_capi_find_module(ScmObj name, scm_csetter_t *mod);
ScmObj scm_api_get_gloc(ScmObj module, ScmObj sym);
int scm_capi_find_gloc(ScmObj module, ScmObj sym, scm_csetter_t *gloc);
int scm_capi_define_global_var(ScmObj module,
                               ScmObj sym, ScmObj val, bool export);
int scm_capi_define_global_syx(ScmObj module,
                               ScmObj sym, ScmObj syx, bool export);
int scm_capi_global_var_ref(ScmObj module, ScmObj sym, scm_csetter_t *val);
int scm_capi_global_syx_ref(ScmObj module, ScmObj sym, scm_csetter_t *syx);
int scm_capi_cached_global_var_ref(int kind, scm_csetter_t *val);


/*******************************************************************/
/*  Return Value                                                   */
/*******************************************************************/

int scm_capi_return_val(const ScmObj *val, int vc);


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

ScmObj scm_capi_capture_cont(void);
bool scm_capi_continuation_p(ScmObj obj);
ScmObj scm_capi_cont_capture_obj(ScmObj cont);


/*******************************************************************/
/*  Setup Trampolining                                             */
/*******************************************************************/

int scm_capi_trampolining(ScmObj proc, ScmObj arg,
                          ScmObj postproc, ScmObj handover);


/*******************************************************************/
/*  Exit                                                           */
/*******************************************************************/

ScmObj scm_api_exit(ScmObj obj);


/*******************************************************************/
/*  System Environment                                             */
/*******************************************************************/

ScmEncoding *scm_capi_system_encoding(void);


/*******************************************************************/
/*  format                                                         */
/*******************************************************************/

int scm_capi_pformat_lst(ScmObj port, ScmObj fmt, ScmObj lst);
int scm_capi_pformat_cv(ScmObj port, ScmObj fmt, ScmObj *obj, size_t n);
ScmObj scm_api_pformat(ScmObj port, ScmObj fmt, ...);
int scm_capi_pformat_cstr(ScmObj port, const char *fmt, ...);

ScmObj scm_capi_format_lst(ScmObj fmt, ScmObj lst);
ScmObj scm_capi_format_cv(ScmObj fmt, ScmObj *obj, size_t n);
ScmObj scm_api_format(ScmObj fmt, ...);
ScmObj scm_capi_format_cstr(const char *fmt, ...);


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
int scm_capi_exec_cstr(const char *expr, const char *enc, ScmEvaluator *ev);


/*******************************************************************/
/*  XXX                                                            */
/*******************************************************************/

int scm_capi_load_iseq(ScmObj iseq);


#ifdef SCM_UNIT_TEST

ScmObj scm_capi_ut_compile(ScmEvaluator *ev, ScmObj exp);
ScmObj scm_capi_ut_eval(ScmEvaluator *ev, ScmObj exp);
void scm_capi_ut_clear_compiler_label_id(void);

#endif

#endif /* INCLUDE_API_H__ */
