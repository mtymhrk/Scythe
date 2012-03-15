#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "reference.h"
#include "char.h"
#include "string.h"
#include "symbol.h"
#include "procedure.h"
#include "gloc.h"
#include "numeric.h"
#include "pair.h"
#include "vector.h"
#include "port.h"
#include "parser.h"
#include "iseq.h"
#include "assembler.h"

#include "encoding.h"
#include "impl_utils.h"

#include "api_enum.h"
#include "api.h"


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

extern inline void
scm_capi_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_bedrock_current_br(), msg);
}

extern inline void
scm_capi_fatalf(const char *fmt, ...)
{
}

extern inline bool
scm_capi_fatal_p(void)
{
  return scm_bedrock_fatal_p(scm_bedrock_current_br());
}

extern inline bool
scm_capi_error_p(void)
{
  return scm_bedrock_error_p(scm_bedrock_current_br());
}


/*******************************************************************/
/*  C Stack                                                        */
/*******************************************************************/

void
scm_capi_ref_stack_push(int dummy, ...)
{
  va_list ap;

  va_start(ap, dummy);
  scm_ref_stack_push_va(scm_bedrock_current_br()->ref_stack, ap);
  va_end(ap);
}

void
scm_capi_ref_stack_save(ScmRefStackInfo *info)
{
  scm_ref_stack_save(scm_bedrock_current_br()->ref_stack, info);
}

void
scm_capi_ref_stack_restore(ScmRefStackInfo *info)
{
  scm_ref_stack_restore(scm_bedrock_current_br()->ref_stack, info);
}


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

extern inline ScmObj
scm_capi_mem_alloc_heap(ScmTypeInfo *type)
{
  if (type == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_mem_alloc_heap(scm_vm_current_mm(), type);
}

extern inline ScmObj
scm_capi_mem_alloc_root(ScmTypeInfo *type)
{
  if (type == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_mem_alloc_root(scm_vm_current_mm(), type);
}

extern inline ScmObj
scm_capi_mem_alloc(ScmTypeInfo *otype, SCM_MEM_TYPE_T mtype)
{
  switch(mtype) {
  case SCM_MEM_HEAP:
    return scm_capi_mem_alloc_heap(otype);
    break;
  case SCM_MEM_ROOT:
    return scm_capi_mem_alloc_root(otype);
    break;
  default:
    return SCM_OBJ_NULL;          /* provisional implemntation */
    break;
  };
}

extern inline ScmObj
scm_capi_mem_free_root(ScmObj obj)
{
  if (obj == SCM_OBJ_NULL) return SCM_OBJ_NULL;
  return scm_mem_free_root(scm_vm_current_mm(), obj);
}

extern inline ScmRef
scm_capi_mem_register_extra_rfrn(ScmRef ref)
{
  if (ref == SCM_REF_NULL) return ref;
  return scm_mem_register_extra_rfrn(scm_vm_current_mm(), ref);
}

extern inline void
scm_capi_gc_start(void)
{
  scm_mem_gc_start(scm_vm_current_mm());
}

extern inline void
scm_capi_gc_enable(void)
{
  scm_mem_enable_gc(scm_vm_current_mm());
}

extern inline void
scm_capi_gc_disable(void)
{
  scm_mem_disable_gc(scm_vm_current_mm());
}


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
scm_capi_eof_object_p(ScmObj obj)
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

ScmObj
scm_capi_list(unsigned int n, ...)
{
  ScmObj args[n];
  ScmObj lst = SCM_OBJ_INIT;
  va_list ap;

  SCM_STACK_FRAME_PUSH(&lst);

  va_start(ap, n);
  for (unsigned int i = 0; i < n; i++)
    args[i] = va_arg(ap, ScmObj);
  va_end(ap);

  for (unsigned int i = 0; i < n; i++)
    SCM_STACK_PUSH(args + i);

  lst = scm_api_nil();
  for (unsigned int i = n; i > 0; i--) {
    lst = scm_api_cons(args[i - 1], lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL; /* provisional implemntation */
  }

  return lst;
}

ScmObj
scm_capi_list_ref(ScmObj lst, size_t n)
{
  ScmObj l = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&l);

  l = lst;
  for (size_t i = 0; i < n; i++) {
    if (scm_capi_pair_p(l))
      l = scm_api_cdr(l);
    else
      return SCM_OBJ_NULL;
  }

  return scm_api_car(l);
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
  if (!SCM_ENCODING_VFUNC_VALID_P(SCM_ENC_ASCII)(chr))
    return SCM_OBJ_NULL;          /* provisional implemntation */

  return scm_char_new(SCM_MEM_ALLOC_HEAP, chr, SCM_ENC_ASCII);
}

extern inline ScmObj
scm_api_make_char_newline(void)
{
  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_LF_CHAR(SCM_ENC_ASCII),
                      SCM_ENC_ASCII);
}

extern inline ScmObj
scm_api_make_char_space(void)
{
  return scm_char_new(SCM_MEM_ALLOC_HEAP,
                      SCM_ENCODING_CONST_SPACE_CHAR(SCM_ENC_ASCII),
                      SCM_ENC_ASCII);
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
scm_capi_make_string_from_cstr(const char *str, SCM_ENC_T enc)
{
  if (str == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_string_new(SCM_MEM_ALLOC_HEAP, str, strlen(str), enc);
}

extern inline ScmObj
scm_capi_make_string_from_bin(const void *data, size_t size, SCM_ENC_T enc)
{
  if (data == NULL)
    return SCM_OBJ_NULL;

  return scm_string_new(SCM_MEM_ALLOC_HEAP, data, size, enc);
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
scm_capi_make_symbol_from_cstr(const char *str, SCM_ENC_T enc)
{
  if (str == NULL)
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return scm_api_string_to_symbol(scm_capi_make_string_from_cstr(str, enc));
}

extern inline ScmObj
scm_capi_make_symbol_from_bin(const void *data, size_t size, SCM_ENC_T enc)
{
  if (data == NULL)
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return scm_api_string_to_symbol(scm_capi_make_string_from_bin(data, size, enc));
}

extern inline bool
scm_capi_symbol_p(ScmObj obj)
{
  if (scm_capi_null_value_p(obj)) return false;

  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

extern inline ssize_t
scm_capi_symbol_bytesize(ScmObj sym)
{
  if (!scm_capi_symbol_p(sym))
    return -1;                  /* provisional implementation */

  return scm_capi_string_bytesize(scm_api_symbol_to_string(sym));
}

extern inline ssize_t
scm_capi_symbol_to_cstr(ScmObj sym, char *cstr, size_t size)
{
  if (!scm_capi_symbol_p(sym))
    return -1;                  /* provisional implementation */

  return scm_capi_string_to_cstr(scm_api_symbol_to_string(sym),
                                 cstr, size);
}

extern inline size_t
scm_capi_symbol_hash_value(ScmObj sym)
{
  if (!scm_capi_symbol_p(sym))
    return SIZE_MAX;                  /* provisional implementation */

  return scm_symbol_hash_value(sym);
}


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

extern inline ScmObj
scm_capi_open_input_fd_port(int fd, SCM_PORT_BUF_T mode, SCM_ENC_T enc)
{
  if (fd < 0) return SCM_OBJ_NULL; /* provisional implemntation */
  return scm_port_open_input_fd(fd, mode, enc);
}

extern inline ScmObj
scm_capi_open_output_fd_port(int fd, SCM_PORT_BUF_T mode, SCM_ENC_T enc)
{
  if (fd < 0) return SCM_OBJ_NULL; /* provisional implemntation */
  return scm_port_open_output_fd(fd, mode, enc);
}

extern inline ScmObj
scm_capi_open_input_string_port_from_cstr(const char *str, SCM_ENC_T enc)
{
  return scm_port_open_input_string(str,
                                    (str == NULL)? 0 : strlen(str),
                                    enc);
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
scm_capi_port_encoding(ScmObj port, SCM_ENC_T *enc)
{
  if (scm_obj_null_p(port)
      || !scm_obj_type_p(port, &SCM_PORT_TYPE_INFO))
    return -1;                  /* provisional implemntation */

  *enc = scm_port_encoding(port);
  return 0;
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
scm_capi_read_char(ScmObj port, scm_char_t *chr)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || chr == NULL)
    return -1;         /* provisional implemntation */

  return scm_port_read_char(port, chr);
}

extern inline ssize_t
scm_capi_unread_raw(ScmObj port, const void *buf, size_t size)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || buf == NULL
      || size > SSIZE_MAX)
    return -1;         /* provisional implemntation */

  return scm_port_pushback(port, buf, size);
}

extern inline ssize_t
scm_capi_unread_char(ScmObj port, const scm_char_t *chr)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || chr == NULL)
    return -1;

  return scm_port_pushback_char(port, chr);
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

extern inline ssize_t
scm_capi_peek_char(ScmObj port, scm_char_t *chr)
{
  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port)
      || chr == NULL)
    return -1;         /* provisional implemntation */

  return scm_port_peek_char(port, chr);
}

ScmObj
scm_api_read(ScmObj port)
{
  ScmLexer *lexer;
  ScmParser *parser;

  if (scm_obj_null_p(port)
      || !scm_capi_input_port_p(port))
      return SCM_OBJ_NULL;         /* provisional implementation */

  lexer = scm_lexer_new();
  if (lexer == NULL)
    return SCM_OBJ_NULL;         /* provisional implementation */

  parser = scm_parser_new(lexer);
  if (parser == NULL)
    return SCM_OBJ_NULL;         /* provisional implementation */

  return scm_parser_parse_expression(parser, port);
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

extern inline ScmObj
scm_api_call_subrutine(ScmObj subr, int argc, ScmObj *argv)
{
  if (!scm_capi_subrutine_p(subr))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_subrutine_call(subr, argc, argv);
}

extern inline bool
scm_capi_subrutine_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_SUBRUTINE_TYPE_INFO) ? true : false);
}


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

extern inline ScmObj
scm_capi_make_closure(ScmObj iseq, size_t nr_free_vars, ScmObj *sp)
{
  if (!scm_capi_iseq_p(iseq))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_closure_new(SCM_MEM_ALLOC_HEAP, iseq, nr_free_vars, sp);
}

extern inline ScmObj
scm_capi_iseq_to_closure(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_closure_new(SCM_MEM_ALLOC_HEAP, iseq, 0, NULL);
}

extern inline bool
scm_capi_closure_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_CLOSURE_TYPE_INFO) ? true : false);
}

extern inline ScmObj
scm_capi_closure_to_iseq(ScmObj clsr)
{
  if (!scm_capi_closure_p(clsr))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_closure_body(clsr);
}


/*******************************************************************/
/*  ISeq                                                           */
/*******************************************************************/

extern inline ScmObj
scm_api_make_iseq(void)
{
  return scm_iseq_new(SCM_MEM_HEAP);
}

extern inline bool
scm_capi_iseq_p(ScmObj obj)
{
  if (scm_obj_null_p(obj)) return false;
  return (scm_obj_type_p(obj, &SCM_ISEQ_TYPE_INFO) ? true : false);
}

extern inline uint8_t *
scm_capi_iseq_to_ip(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return SCM_ISEQ_SEQ(iseq);
}

extern inline ssize_t
scm_capi_iseq_length(ScmObj iseq)
{
  if (!scm_capi_iseq_p(iseq))
    return -1;         /* provisional implemntation */

  return scm_iseq_length(iseq);
}

ssize_t
scm_capi_iseq_push_op(ScmObj iseq, SCM_OPCODE_T op)
{
  ssize_t rslt, idx;
  SCM_STACK_FRAME_PUSH(&iseq);

  if (!scm_capi_iseq_p(iseq))
    return -1;

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */
  idx = rslt;

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return idx;
}

ssize_t
scm_capi_iseq_push_op_immval(ScmObj iseq, SCM_OPCODE_T op, ScmObj val)
{
  ssize_t rslt, idx, immv_idx;

  SCM_STACK_FRAME_PUSH(&iseq, &val);

  if (!scm_capi_iseq_p(iseq))
    return -1;

  immv_idx = scm_iseq_push_immval(iseq, val);
  if (immv_idx < 0) return -1;   /* provisional implemntation */
  if (immv_idx > UINT32_MAX) return -1; /* provisional implemntation */

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */
  idx = rslt;

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, (uint32_t)immv_idx);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return idx;
}

ssize_t
scm_capi_iseq_push_op_cval(ScmObj iseq, SCM_OPCODE_T op, uint32_t val)
{
  ssize_t rslt, idx;

  SCM_STACK_FRAME_PUSH(&iseq);

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */
  idx = rslt;

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return idx;
}

ssize_t
scm_capi_iseq_push_op_cval_cval(ScmObj iseq, SCM_OPCODE_T op,
                                uint32_t val1, uint32_t val2)
{
  ssize_t rslt, idx;

  SCM_STACK_FRAME_PUSH(&iseq);

  rslt = scm_iseq_push_uint8(iseq, op);
  if (rslt < 0) return -1;   /* provisional implemntation */
  idx = rslt;

  rslt = scm_iseq_push_uint8(iseq, 0);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, val1);
  if (rslt < 0) return -1;   /* provisional implemntation */

  rslt = scm_iseq_push_uint32(iseq, val2);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return idx;
}

ssize_t
scm_capi_iseq_set_immval(ScmObj iseq, size_t idx, ScmObj val)
{
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&iseq, &val);

  if (!scm_capi_iseq_p(iseq)
      || idx > SSIZE_MAX
      || (ssize_t)idx >= scm_iseq_nr_immv(iseq))
    return -1;   /* provisional implemntation */

  rslt = scm_iseq_set_immval(iseq, idx, val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return (ssize_t)idx;
}

ssize_t
scm_capi_iseq_set_cval(ScmObj iseq, size_t idx, uint32_t val)
{
  ssize_t rslt;

  if (!scm_capi_iseq_p(iseq)
      || idx > SSIZE_MAX
      || (ssize_t)idx > scm_iseq_length(iseq) - 4)
    return -1;   /* provisional implemntation */

  rslt = scm_iseq_set_uint32(iseq, idx, val);
  if (rslt < 0) return -1;   /* provisional implemntation */

  return (ssize_t)idx;
}

extern inline ScmObj
scm_capi_iseq_ref_immval(ScmObj iseq, size_t idx)
{
  if (!scm_capi_iseq_p(iseq)
      || idx > SSIZE_MAX
      || (ssize_t)idx >= scm_iseq_nr_immv(iseq))
    return SCM_OBJ_NULL;

  return scm_iseq_get_immval(iseq, idx);;
}

extern inline ScmObj
scm_api_assemble(ScmObj lst)
{
  if (!scm_capi_pair_p(lst))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_asm_assemble(lst);
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
/*  Setup Trampolining                                             */
/*******************************************************************/

extern inline int
scm_capi_trampolining(ScmObj target, ScmObj args, int nr_arg_cf,
                      ScmObj (*callback)(int argc, ScmObj *argv))
{
  if ((!scm_capi_iseq_p(target) && !scm_capi_closure_p(target))
      || scm_capi_pair_p(args))
    return SCM_OBJ_NULL;                  /* provisional implemntation */

  return scm_vm_setup_trampolining(scm_vm_current_vm(),
                                   target, args, nr_arg_cf, callback);
}
