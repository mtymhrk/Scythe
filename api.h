#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include <stddef.h>
#include <stdbool.h>

#include "object.h"

/*******************************************************************/
/*  Predicate                                                      */
/*******************************************************************/

bool scm_capi_eq_p(ScmObj obj1, ScmObj obj2);
ScmObj scm_api_eq_P(ScmObj obj1, ScmObj obj2);


/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

ScmObj scm_api_make_string_ascii(const char *str);


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

ScmObj scm_api_make_symbol_ascii(const char *str);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);


/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

ScmObj scm_api_cons(ScmObj car, ScmObj cdr);
ScmObj scm_api_car(ScmObj pair);
ScmObj scm_api_cdr(ScmObj pair);
bool scm_api_pair_p(ScmObj pair);
ScmObj scm_api_pair_P(ScmObj pair);


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

/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj scm_api_make_subrutine(ScmSubrFunc func);


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

ScmObj scm_api_global_var_ref(ScmObj sym);
ScmObj scm_api_global_var_bound_p(ScmObj sym);
ScmObj scm_api_global_var_define(ScmObj sym, ScmObj val);
ScmObj scm_api_global_var_set(ScmObj sym, ScmObj val);


/*******************************************************************/
/*  Access to Argument of Function                                 */
/*******************************************************************/

int scm_api_get_nr_func_arg(void);
ScmObj scm_api_get_func_arg(int nth);

#endif /* INCLUDE_API_H__ */
