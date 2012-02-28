#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include <stddef.h>
#include <stdbool.h>

#include "object.h"
#include "reference.h"
#include "encoding.h"
#include "api_enum.h"

/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

void scm_capi_fatal(const char *msg);
void scm_capi_fatalf(const char *fmt, ...);
bool scm_capi_fatal_p(void);
bool scm_capi_error_p(void);


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

#define SCM_STACK_FRAME_PUSH(...) \
  SCM_STACK_FRAME; SCM_STACK_PUSH(__VA_ARGS__);

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

ScmObj scm_capi_mem_alloc_heap(ScmTypeInfo *type);
ScmObj scm_capi_mem_alloc_root(ScmTypeInfo *type);
ScmObj scm_capi_mem_alloc(ScmTypeInfo *otype, SCM_CAPI_MEM_TYPE_T mtype);
ScmObj scm_capi_mem_free_root(ScmObj obj);
ScmRef scm_capi_mem_register_extra_rfrn(ScmRef ref);

void scm_capi_gc_start(void);
void scm_capi_gc_enable(void);
void scm_capi_gc_disable(void);


/*******************************************************************/
/*  Equivalence                                                    */
/*******************************************************************/

bool scm_capi_null_value_p(ScmObj obj);
bool scm_capi_eq_p(ScmObj obj1, ScmObj obj2);
ScmObj scm_api_eq_P(ScmObj obj1, ScmObj obj2);


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

ScmObj scm_api_nil(void);
bool scm_capi_nil_p(ScmObj obj);


/*******************************************************************/
/*  boolean                                                        */
/*******************************************************************/

ScmObj scm_api_bool_true(void);
ScmObj scm_api_bool_false(void);
bool scm_capi_true_p(ScmObj obj);
bool scm_capi_false_p(ScmObj obj);


/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

ScmObj scm_api_eof(void);
bool scm_capi_eof_p(ScmObj obj);


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
bool scm_capi_fixnum_p(ScmObj obj);
long scm_capi_fixnum_to_clong(ScmObj fn);


/*******************************************************************/
/*  charactor                                                      */
/*******************************************************************/

ScmObj scm_capi_make_char(scm_char_t chr);
ScmObj scm_api_make_char_newline(void);
ScmObj scm_api_make_char_space(void);
bool scm_capi_char_p(ScmObj obj);
ssize_t scm_capi_char_to_cchar(ScmObj chr, scm_char_t *c);


/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

ScmObj scm_capi_make_string_from_cstr(const char *str);
ScmObj scm_capi_make_string_from_bin(const void *data, size_t size);
bool scm_capi_string_p(ScmObj obj);
ssize_t scm_capi_string_length(ScmObj str);
ssize_t scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size);


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

ScmObj scm_capi_make_vector(size_t len);
ScmObj scm_capi_make_vector_fill(size_t len, ScmObj fill);
bool scm_capi_vector_p(ScmObj obj);
ScmObj scm_capi_vector_set(ScmObj vec, size_t idx, ScmObj obj);
ScmObj scm_capi_vector_ref(ScmObj vec, size_t idx);
ssize_t scm_capi_vector_length(ScmObj vec);


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

ScmObj scm_capi_make_symbol_from_cstr(const char *str);
ScmObj scm_capi_make_symbol_from_bin(const void *data, size_t size);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);
bool scm_capi_symbol_p(ScmObj obj);
ssize_t scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size);
size_t scm_capi_symbol_hash_value(ScmObj sym);


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

ScmObj scm_capi_open_input_fd_port(int fd,
                                   SCM_PORT_BUF_T mode, SCM_ENC_T enc);
ScmObj scm_capi_open_output_fd_port(int fd,
                                    SCM_PORT_BUF_T mode, SCM_ENC_T enc);
ScmObj scm_capi_open_input_string_port_from_cstr(const char *str,
                                                 SCM_ENC_T enc);
bool scm_capi_input_port_p(ScmObj port);
ScmObj scm_api_input_port_P(ScmObj port);
bool scm_capi_output_port_p(ScmObj port);
ScmObj scm_api_output_port_P(ScmObj port);
int scm_capi_port_encoding(ScmObj port, SCM_ENC_T *enc);
int scm_api_close_input_port(ScmObj port);
int scm_api_close_output_port(ScmObj port);
ssize_t scm_capi_read_raw(ScmObj port, void *buf, size_t size);
ssize_t scm_capi_read_char(ScmObj port, scm_char_t *chr);
ssize_t scm_capi_unread_raw(ScmObj port, const void *buf, size_t size);
ssize_t scm_capi_unread_char(ScmObj port, const scm_char_t *chr);
ssize_t scm_capi_peek_raw(ScmObj port, void *buf, size_t size);
ssize_t scm_capi_peek_char(ScmObj port, scm_char_t *chr);


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
