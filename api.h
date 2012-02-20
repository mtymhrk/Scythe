#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include <stddef.h>
#include <stdbool.h>

#include "object.h"
#include "encoding.h"

/*******************************************************************/
/*  Predicate                                                      */
/*******************************************************************/

bool scm_capi_null_value_p(ScmObj obj);
bool scm_capi_eq_p(ScmObj obj1, ScmObj obj2);
ScmObj scm_api_eq_P(ScmObj obj1, ScmObj obj2);


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

ScmObj scm_api_nil(void);


/*******************************************************************/
/*  boolean                                                        */
/*******************************************************************/


ScmObj scm_api_bool_true(void);
ScmObj scm_api_bool_false(void);


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

ScmObj scm_api_eof(void);


/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

ScmObj scm_api_cons(ScmObj car, ScmObj cdr);
ScmObj scm_api_car(ScmObj pair);
ScmObj scm_api_cdr(ScmObj pair);
bool scm_capi_pair_p(ScmObj pair);
ScmObj scm_api_pair_P(ScmObj pair);


/*******************************************************************/
/*  numeric                                                        */
/*******************************************************************/

ScmObj scm_capi_make_fixnum(scm_sword_t num);


/*******************************************************************/
/*  charactor                                                      */
/*******************************************************************/

ScmObj scm_capi_make_char(scm_char_t chr);
ScmObj scm_api_make_char_newline(void);
ScmObj scm_api_make_char_space(void);

/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

ScmObj scm_capi_make_string_from_cstr(const char *str);
ScmObj scm_capi_make_string_from_bin(const void *data, size_t size);


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

ScmObj scm_capi_make_vector(size_t len);
ScmObj scm_capi_make_vector_fill(size_t len, ScmObj fill);
ScmObj scm_capi_vector_set(ScmObj vec, size_t idx, ScmObj obj);


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

ScmObj scm_capi_make_symbol_from_cstr(const char *str);
ScmObj scm_capi_make_symbol_from_bin(const void *data, size_t size);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

ScmObj scm_api_open_input_fd_port(int fd);
ScmObj scm_api_open_output_fd_port(int fd);
bool scm_capi_input_port_p(ScmObj port);
ScmObj scm_api_input_port_P(ScmObj port);
bool scm_capi_output_port_p(ScmObj port);
ScmObj scm_api_output_port_P(ScmObj port);
int scm_api_close_input_port(ScmObj port);
int scm_api_close_output_port(ScmObj port);
ssize_t scm_capi_read_raw(ScmObj port, void *buf, size_t size);
ssize_t scm_capi_unread_raw(ScmObj port, void *buf, size_t size);
ssize_t scm_capi_peek_raw(ScmObj port, void *buf, size_t size);


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj scm_capi_make_subrutine(ScmSubrFunc func);


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

ScmObj scm_api_global_var_ref(ScmObj sym);
bool scm_capi_global_var_bound_p(ScmObj sym);
ScmObj scm_api_global_var_bound_P(ScmObj sym);
ScmObj scm_api_global_var_define(ScmObj sym, ScmObj val);
ScmObj scm_api_global_var_set(ScmObj sym, ScmObj val);


/*******************************************************************/
/*  Access to Argument of Function                                 */
/*******************************************************************/

int scm_capi_get_nr_func_arg(void);
ScmObj scm_capi_get_func_arg(int nth);

#endif /* INCLUDE_API_H__ */
