#include <stddef.h>
#include <stdbool.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "char.h"
#include "string.h"
#include "symbol.h"
#include "procedure.h"
#include "gloc.h"
#include "numeric.h"
#include "pair.h"
#include "vector.h"
#include "port.h"

#include "encoding.h"

#include "api.h"


/*******************************************************************/
/*  Equivalence                                                    */
/*******************************************************************/

/* 述語関数について、C の bool 方を返すものは _p を関数名の後ろに付与する。
 * Scheme の #t/#f を返すものは _P を関数名の後ろに付与する。
 */

extern inline bool
scm_capi_null_value_p(ScmObj obj)
{
  return scm_obj_null_p(obj);
}

extern inline bool
scm_capi_eq_p(ScmObj obj1, ScmObj obj2)
{
  return scm_obj_same_instance_p(obj1, obj2);
}

extern inline ScmObj
scm_api_eq_P(ScmObj obj1, ScmObj obj2)
{
  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2))
      return SCM_OBJ_NULL;         /* provisional implemntation */

  return (scm_obj_same_instance_p(obj1, obj2) ?
          scm_vm_bool_true_instance() : scm_vm_bool_false_instance());
}


/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

extern inline ScmObj
scm_api_nil(void)
{
  return scm_vm_nil_instance();
}

extern inline bool
scm_capi_nil_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_nil());
}

/*******************************************************************/
/*  boolean                                                        */
/*******************************************************************/

extern inline ScmObj
scm_api_bool_true(void)
{
  return scm_vm_nil_instance();
}

extern inline ScmObj
scm_api_bool_false(void)
{
  return scm_vm_nil_instance();
}

extern inline bool
scm_capi_true_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_bool_true());
}

extern inline bool
scm_capi_false_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_bool_false());
}

/*******************************************************************/
/*  eof                                                           */
/*******************************************************************/

extern inline ScmObj
scm_api_eof(void)
{
  return scm_vm_eof_instance();
}

extern inline bool
scm_capi_eof_p(ScmObj obj)
{
  return scm_capi_eq_p(obj, scm_api_eof());
}

/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

extern inline ScmObj
scm_api_cons(ScmObj car, ScmObj cdr)
{
  if (scm_obj_null_p(car) || scm_obj_null_p(cdr))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr);
}

extern inline ScmObj
scm_api_car(ScmObj pair)
{
  if (scm_obj_null_p(pair) || !scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_car(pair);
}

extern inline ScmObj
scm_api_cdr(ScmObj pair)
{
  if (scm_obj_null_p(pair) || !scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_cdr(pair);
}

extern inline bool
scm_capi_pair_p(ScmObj pair)
{
  if (scm_obj_null_p(pair)) return false;
  return (scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO) ? true : false);
}

extern inline ScmObj
scm_api_pair_P(ScmObj pair)
{
  if (scm_obj_null_p(pair))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return (scm_capi_pair_p(pair) ?
          scm_vm_bool_true_instance() : scm_vm_bool_false_instance());
}


/*******************************************************************/
/*  numeric                                                        */
/*******************************************************************/

int
scm_capi_perse_numric_literal(const void *data,
                              size_t size/*, struct liteinfo *rslt*/)
                              /* struct liteinfo は仮の構造体名 */
{
  /* TODO: write me */
  return -1;
}

ScmObj
scm_api_make_numeric(/* struct liteinfo *rslt */)
                     /* struct liteinfo は仮の構造体名 */

{
  /* TODO: write me */
  return SCM_OBJ_NULL;
}

/* XXX; 一時的な API
 * scm_capi_perse_numric_literal と scm_api_make_numeric が実装され
 * れば廃止する。
 */
extern inline ScmObj
scm_capi_make_fixnum(scm_sword_t num)
{
  if (num < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < num)
    return SCM_OBJ_NULL;

  return scm_fixnum_new(num);
}

extern inline bool
scm_capi_fixnum_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_FIXNUM_TYPE_INFO) ? true : false;
}

extern inline long
scm_capi_fixnum_to_clong(ScmObj fn)
{
  scm_sword_t v;
  long ret;

  if (!scm_capi_fixnum_p(fn)) {
    errno = EINVAL;           /* provisional implemntation */
    return 0;
  }

  v = scm_fixnum_value(fn);
  if (v > LONG_MAX) {
    errno = ERANGE; /* provisional implemntation */
    ret = LONG_MAX;
  }
  else if (v < LONG_MIN) {
    errno = ERANGE; /* provisional implemntation */
    ret = LONG_MIN;
  }
  else {
    ret = (long)v;
  }

  return ret;
}

/*******************************************************************/
/*  charactor                                                      */
/*******************************************************************/

extern inline ScmObj
scm_capi_make_char(scm_char_t chr)
{
  if (!SCM_ENCODING_VFUNC_VALID_P(SCM_ENCODING_ASCII)(chr))
    return SCM_OBJ_NULL;          /* provisional implemntation */

  return scm_char_new(SCM_MEM_ALLOC_HEAP, chr, SCM_ENCODING_ASCII);
}

extern inline ScmObj
scm_api_make_char_newline(void)
{
  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_LF_CHAR(SCM_ENCODING_ASCII),
                      SCM_ENCODING_ASCII);
}

extern inline ScmObj
scm_api_make_char_space(void)
{
  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_SPACE_CHAR(SCM_ENCODING_ASCII),
                      SCM_ENCODING_ASCII);
}

extern inline bool
scm_capi_char_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_CHAR_TYPE_INFO) ? true : false;
}

extern inline ssize_t
scm_capi_char_to_cchar(ScmObj chr, scm_char_t *c)
{
  if (!scm_capi_char_p(chr))
    return -1;          /* provisional implemntation */

  *c = scm_char_value(chr);

  return SCM_ENCODING_VFUNC_CHAR_WIDTH(scm_char_encoding(chr))(c->bytes,
                                                               sizeof(*c));
}

/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

extern inline ScmObj
scm_capi_make_string_from_cstr(const char *str)
{
  if (str == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_string_new(SCM_MEM_ALLOC_HEAP,
                        str, strlen(str), SCM_ENCODING_ASCII);
}

extern inline ScmObj
scm_capi_make_string_from_bin(const void *data, size_t size)
{
  if (data == NULL)
    return SCM_OBJ_NULL;

  return scm_string_new(SCM_MEM_ALLOC_HEAP,
                        data, size, SCM_ENCODING_ASCII);
}

extern inline bool
scm_capi_string_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_STRING_TYPE_INFO) ? true : false;
}

extern inline ssize_t
scm_capi_string_length(ScmObj str)
{
  if (!scm_capi_string_p(str))
    return -1;                  /* provisional implementation */

  return (ssize_t)scm_string_length(str);
}

extern inline ssize_t
scm_capi_string_bytesize(ScmObj str)
{
  if (!scm_capi_string_p(str))
    return -1;                  /* provisional implementation */

  return (ssize_t)scm_string_bytesize(str);
}

extern inline ssize_t
scm_capi_string_to_cstr(ScmObj str, char *cstr, size_t size)
{
  ssize_t n;

  if (!scm_capi_string_p(str))
    return -1;                  /* provisional implementation */

  if (cstr == NULL || size == 0) return 0;

  n = (ssize_t)scm_string_bytesize(str);
  if (n < 0) return -1;

  if (size - 1 < (size_t)n) n = (ssize_t)size - 1;

  memcpy(cstr, scm_string_content(str), (size_t)n);
  cstr[n] = '\0';

  return n;
}

/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

extern inline ScmObj
scm_capi_make_vector(size_t len)
{
  if (len > SSIZE_MAX)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_vector_new(SCM_MEM_ALLOC_HEAP, len, scm_api_nil());
}

extern inline ScmObj
scm_capi_make_vector_fill(size_t len, ScmObj fill)
{
  if (scm_obj_null_p(fill) || len > SSIZE_MAX)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_vector_new(SCM_MEM_ALLOC_HEAP, len, fill);
}

extern inline bool
scm_capi_vector_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_VECTOR_TYPE_INFO) ? true : false;
}

extern inline ScmObj
scm_capi_vector_set(ScmObj vec, size_t idx, ScmObj obj)
{
  if (scm_obj_null_p(vec)
      || !scm_obj_type_p(vec, &SCM_VECTOR_TYPE_INFO)
      || idx >= scm_vector_length(vec)
      || scm_obj_null_p(obj))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_vector_set(vec, idx, obj);
}

extern inline ScmObj
scm_capi_vector_ref(ScmObj vec, size_t idx)
{
  if (!scm_capi_vector_p(vec)
      || idx >= scm_vector_length(vec))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_vector_ref(vec, idx);
}

extern inline ssize_t
scm_capi_vector_length(ScmObj vec)
{
  if (!scm_capi_vector_p(vec))
    return -1;                   /* provisional implementation */

  return (ssize_t)scm_vector_length(vec);
}

/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

extern inline ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    /* TODO: ランタイムエラーをどう処理するか。*/
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return SCM_SYMBOL_STR(sym);
}

extern inline ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if (scm_obj_null_p(str) || !scm_obj_type_p(str, &SCM_STRING_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_symtbl_symbol(scm_vm_current_symtbl(), str);
}

extern inline ScmObj
scm_capi_make_symbol_from_cstr(const char *str)
{
  if (str == NULL)
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return scm_api_string_to_symbol(scm_capi_make_string_from_cstr(str));
}

extern inline ScmObj
scm_capi_make_symbol_from_bin(const void *data, size_t size)
{
  if (data == NULL)
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return scm_api_string_to_symbol(scm_capi_make_string_from_bin(data, size));
}

extern inline bool
scm_capi_symbol_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

extern inline ssize_t
scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size)
{
  if (!scm_capi_symbol_p(sym))
    return -1;                  /* provisional implementation */

  return scm_capi_string_to_cstr(scm_api_symbol_to_string(sym),
                                 cstr, size);
}

/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

extern inline ScmObj
scm_capi_open_input_fd_port(int fd) /* TODO: バッファ種別を指定できるようにする*/
{
  if (fd < 0) return SCM_OBJ_NULL; /* provisional implemntation */
  return scm_port_open_input_fd(fd, SCM_PORT_BUF_DEFAULT);
}

extern inline ScmObj
scm_capi_open_output_fd_port(int fd)/* TODO: バッファ種別を指定できるようにする*/
{
  if (fd < 0) return SCM_OBJ_NULL; /* provisional implemntation */
  return scm_port_open_output_fd(fd, SCM_PORT_BUF_DEFAULT);
}

extern inline ScmObj
scm_capi_open_input_string_port_from_cstr(const char *str)
{
  return scm_port_open_input_string(str, (str == NULL)? 0 : strlen(str));
}

extern inline bool
scm_capi_input_port_p(ScmObj port)
{
  if (scm_obj_null_p(port))
    return false;

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_readable_p(port))
    return true;
  else
    return false;
}

extern inline ScmObj
scm_api_input_port_P(ScmObj port)
{
  if (scm_obj_null_p(port))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_readable_p(port))
    return scm_vm_bool_true_instance();
  else
    return scm_vm_bool_false_instance();
}

extern inline bool
scm_capi_output_port_p(ScmObj port)
{
  if (scm_obj_null_p(port))
    return false;

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_writable_p(port))
    return true;
  else
    return false;
}

extern inline ScmObj
scm_api_output_port_P(ScmObj port)
{
  if (scm_obj_null_p(port))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  if (scm_obj_type_p(port, &SCM_PORT_TYPE_INFO) && scm_port_writable_p(port))
    return scm_vm_bool_true_instance();
  else
    return scm_vm_bool_false_instance();
}

extern inline int
scm_api_close_input_port(ScmObj port)
{
  if (scm_obj_null_p(port) || scm_capi_input_port_p(port))
    return -1;         /* provisional implemntation */

  return scm_port_close(port);
}

extern inline int
scm_api_close_output_port(ScmObj port)
{
  if (scm_obj_null_p(port) || scm_capi_output_port_p(port))
    return -1;         /* provisional implemntation */

  return scm_port_close(port);
}

extern inline ssize_t
scm_capi_read_raw(ScmObj port, void *buf, size_t size)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || buf == NULL
      || size > SSIZE_MAX)
    return -1;         /* provisional implemntation */

  return scm_port_read(port, buf, size);
}

extern inline ssize_t
scm_capi_unread_raw(ScmObj port, void *buf, size_t size)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || buf == NULL
      || size > SSIZE_MAX)
    return -1;         /* provisional implemntation */

  return scm_port_pushback(port, buf, size);
}

extern inline ssize_t
scm_capi_peek_raw(ScmObj port, void *buf, size_t size)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || buf == NULL
      || size > SSIZE_MAX)
    return -1;         /* provisional implemntation */

  return scm_port_peek(port, buf, size);
}

/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

extern inline ScmObj
scm_capi_make_subrutine(ScmSubrFunc func)
{
  if (func == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_subrutine_new(SCM_MEM_ALLOC_HEAP, func);
}


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

ScmObj
scm_api_global_var_ref(ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  rslt = scm_gloctbl_find(scm_vm_current_gloctbl(), sym, SCM_CSETTER_L(gloc));
  if (rslt != 0) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  /* 未束縛変数の参照の場合は SCM_OBJ_NULL を返す */
  return (scm_obj_null_p(gloc) ?  SCM_OBJ_NULL : scm_gloc_value(gloc));
}

extern inline bool
scm_capi_global_var_bound_p(ScmObj sym)
{
  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_obj_null_p(scm_api_global_var_ref(sym)) ? false : true;
}

ScmObj
scm_api_global_var_bound_P(ScmObj sym)
{
  return (scm_capi_global_var_bound_p(sym) ?
          scm_api_bool_true() : scm_api_bool_false());
}

ScmObj
scm_api_global_var_define(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */
  if (scm_obj_null_p(val))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  gloc = scm_gloctbl_bind(scm_vm_current_gloctbl(), sym, val);
  if (scm_obj_null_p(gloc)) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  return val;
}

ScmObj
scm_api_global_var_set(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */
  if (scm_obj_null_p(val))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  /* 未束縛変数の参照の場合は SCM_OBJ_NULL を返す */
  if (scm_obj_same_instance_p(scm_api_global_var_bound_P(sym),
                               scm_vm_bool_false_instance()))
    return SCM_OBJ_NULL;

  gloc = scm_gloctbl_bind(scm_vm_current_gloctbl(), sym, val);
  if (scm_obj_null_p(gloc)) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  return val;
}


/*******************************************************************/
/*  Access to Argument of Function                                 */
/*******************************************************************/

extern inline int
scm_capi_get_nr_func_arg(void)
{
  /* 2012.02.07: 今のところスタックフレームが全く無い状態でこの api を
     呼ばれると assertion に引っ掛って落ちる */
  return scm_vm_nr_local_var(scm_vm_current_vm());
}

extern inline ScmObj
scm_capi_get_func_arg(int nth)
{
  if (nth >= scm_vm_nr_local_var(scm_vm_current_vm()))
    return SCM_OBJ_NULL;                  /* provisional implemntation */

  return scm_vm_refer_local_var(scm_vm_current_vm(), nth);
}


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

extern inline void
scm_capi_fatal(const char *msg)
{
  scm_vm_fatal(scm_vm_current_vm(), msg);
}

extern inline bool
scm_capi_fatal_p(void)
{
  return scm_vm_fatal_p(scm_vm_current_vm());
}

extern inline bool
scm_capi_error_p(void)
{
  return scm_vm_error_p(scm_vm_current_vm());
}
