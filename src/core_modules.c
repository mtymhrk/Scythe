#include <unistd.h>
#include <stdint.h>

#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/core_subr.h"
#include "scythe/impl_utils.h"

struct subr_data {
  const char *name;
  int arity;
  unsigned int flag;
  ScmSubrFunc func;
  bool export;
};

#define STRARY(...) (const char *[]){__VA_ARGS__}


static ScmObj
scm_make_module_name(const char * const *name_str, size_t n)
{
  ScmObj str[n];
  ScmObj name = SCM_OBJ_INIT;

  for (size_t i = 0; i < n; i++) str[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&name);
  SCM_REFSTK_REG_ARY(str, n);

  scm_assert(n > 0);

  for (size_t i = 0; i < n; i++) {
    str[i] = scm_capi_make_symbol_from_cstr(name_str[i], SCM_ENC_SRC);
    if (scm_obj_null_p(str[i])) return SCM_OBJ_NULL;
  }

  name = SCM_NIL_OBJ;

  for (size_t i = n; i > 0; i--) {
    name = scm_api_cons(str[i - 1], name);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
  }

  return name;
}

static int
scm_define_subr(ScmObj module, const struct subr_data *data, size_t n)
{
  ScmObj sym  = SCM_OBJ_INIT, subr = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module,
                      &sym, &subr);

  for (size_t i = 0; i < n; i++) {
    sym = scm_capi_make_symbol_from_cstr(data[i].name, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;

    subr = scm_capi_make_subrutine(data[i].func, data[i].arity, data[i].flag,
                                   module);
    if (scm_obj_null_p(subr)) return -1;

    rslt = scm_capi_define_global_var(module, sym, subr, data[i].export);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_load_module(const char * const *name_str, size_t n,
                int (*load_func)(ScmObj mod))
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&name, &mod);

  name = scm_make_module_name(name_str, n);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(mod));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(mod))
    return 0;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  return load_func(mod);
}


static int scm_load_module_scheme_base(void);
static int scm_load_module_scheme_char(void);
static int scm_load_module_scythe_internal_compile(void);
static int scm_load_module_scythe_internal_repl(void);
static int scm_load_module_scythe_internal_command(void);
static int scm_load_module_scythe_base(void);
static int scm_load_module_main(void);


/*******************************************************************/
/*  (scheme base)                                                  */
/*******************************************************************/

static int
scm_define_scheme_base_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Equivalence predicates                                         */
    /*******************************************************************/
    { "eq?", SCM_SUBR_ARITY_EQ_P, SCM_SUBR_FLAG_EQ_P, scm_subr_func_eq_P, true },
    { "eqv?", SCM_SUBR_ARITY_EQV_P, SCM_SUBR_FLAG_EQV_P, scm_subr_func_eqv_P, true },
    { "equal?", SCM_SUBR_ARITY_EQUAL_P, SCM_SUBR_FLAG_EQUAL_P, scm_subr_func_equal_P, true },

    /*******************************************************************/
    /*  Numbers                                                        */
    /*******************************************************************/
    { "number?", SCM_SUBR_ARITY_NUMBER_P, SCM_SUBR_FLAG_NUMBER_P, scm_subr_func_number_P, true },
    { "complex?", SCM_SUBR_ARITY_COMPLEX_P, SCM_SUBR_FLAG_COMPLEX_P, scm_subr_func_complex_P, true },
    { "real?", SCM_SUBR_ARITY_REAL_P, SCM_SUBR_FLAG_REAL_P, scm_subr_func_real_P, true },
    { "rational?", SCM_SUBR_ARITY_RATIONAL_P, SCM_SUBR_FLAG_RATIONAL_P, scm_subr_func_rational_P, true },
    { "integer?", SCM_SUBR_ARITY_INTEGER_P, SCM_SUBR_FLAG_INTEGER_P, scm_subr_func_integer_P, true },
    { "exact?", SCM_SUBR_ARITY_EXACT_P, SCM_SUBR_FLAG_EXACT_P, scm_subr_func_exact_P, true },
    { "inexact?", SCM_SUBR_ARITY_INEXACT_P, SCM_SUBR_FLAG_INEXACT_P, scm_subr_func_inexact_P, true },
    { "exact-integer?", SCM_SUBR_ARITY_EXACT_INTEGER_P, SCM_SUBR_FLAG_EXACT_INTEGER_P, scm_subr_func_exact_integer_P, true },
    { "finite?", SCM_SUBR_ARITY_FINITE_P, SCM_SUBR_FLAG_FINITE_P, scm_subr_func_finite_P, true },
    { "infinite?", SCM_SUBR_ARITY_INFINITE_P, SCM_SUBR_FLAG_INFINITE_P, scm_subr_func_infinite_P, true },
    { "nan?", SCM_SUBR_ARITY_NAN_P, SCM_SUBR_FLAG_NAN_P, scm_subr_func_nan_P, true },
    { "=", SCM_SUBR_ARITY_NUM_EQ_P, SCM_SUBR_FLAG_NUM_EQ_P, scm_subr_func_num_eq_P, true },
    { "<", SCM_SUBR_ARITY_NUM_LT_P, SCM_SUBR_FLAG_NUM_LT_P, scm_subr_func_num_lt_P, true },
    { ">", SCM_SUBR_ARITY_NUM_GT_P, SCM_SUBR_FLAG_NUM_GT_P, scm_subr_func_num_gt_P, true },
    { "<=", SCM_SUBR_ARITY_NUM_LE_P, SCM_SUBR_FLAG_NUM_LE_P, scm_subr_func_num_le_P, true },
    { ">=", SCM_SUBR_ARITY_NUM_GE_P, SCM_SUBR_FLAG_NUM_GE_P, scm_subr_func_num_ge_P, true },
    { "zero?", SCM_SUBR_ARITY_ZERO_P, SCM_SUBR_FLAG_ZERO_P, scm_subr_func_zero_P, true },
    { "positive?", SCM_SUBR_ARITY_POSITIVE_P, SCM_SUBR_FLAG_POSITIVE_P, scm_subr_func_positive_P, true },
    { "negative?", SCM_SUBR_ARITY_NEGATIVE_P, SCM_SUBR_FLAG_NEGATIVE_P, scm_subr_func_negative_P, true },
    { "odd?", SCM_SUBR_ARITY_ODD_P, SCM_SUBR_FLAG_ODD_P, scm_subr_func_odd_P, true },
    { "even?", SCM_SUBR_ARITY_EVEN_P, SCM_SUBR_FLAG_EVEN_P, scm_subr_func_even_P, true },
    { "max", SCM_SUBR_ARITY_MAX, SCM_SUBR_FLAG_MAX, scm_subr_func_max, true },
    { "min", SCM_SUBR_ARITY_MIN, SCM_SUBR_FLAG_MIN, scm_subr_func_min, true },
    { "+", SCM_SUBR_ARITY_PLUS, SCM_SUBR_FLAG_PLUS, scm_subr_func_plus, true },
    { "*", SCM_SUBR_ARITY_MUL, SCM_SUBR_FLAG_MUL, scm_subr_func_mul, true },
    { "-", SCM_SUBR_ARITY_MINUS, SCM_SUBR_FLAG_MINUS, scm_subr_func_minus, true },
    { "/", SCM_SUBR_ARITY_DIV, SCM_SUBR_FLAG_DIV, scm_subr_func_div, true },
    { "floor/", SCM_SUBR_ARITY_FLOOR_DIV, SCM_SUBR_FLAG_FLOOR_DIV, scm_subr_func_floor_div, true },
    { "floor-quotient", SCM_SUBR_ARITY_FLOOR_QUO, SCM_SUBR_FLAG_FLOOR_QUO, scm_subr_func_floor_quo, true },
    { "floor-remainder", SCM_SUBR_ARITY_FLOOR_REM, SCM_SUBR_FLAG_FLOOR_REM, scm_subr_func_floor_rem, true },
    { "truncate/", SCM_SUBR_ARITY_TRUNCATE_DIV, SCM_SUBR_FLAG_TRUNCATE_DIV, scm_subr_func_truncate_div, true },
    { "truncate-quotient", SCM_SUBR_ARITY_TRUNCATE_QUO, SCM_SUBR_FLAG_TRUNCATE_QUO, scm_subr_func_truncate_quo, true },
    { "truncate-remainder", SCM_SUBR_ARITY_TRUNCATE_REM, SCM_SUBR_FLAG_TRUNCATE_REM, scm_subr_func_truncate_rem, true },
    { "quotient", SCM_SUBR_ARITY_TRUNCATE_QUO, SCM_SUBR_FLAG_TRUNCATE_QUO, scm_subr_func_truncate_quo, true },
    { "remainder", SCM_SUBR_ARITY_TRUNCATE_REM, SCM_SUBR_FLAG_TRUNCATE_REM, scm_subr_func_truncate_rem, true },
    { "modulo", SCM_SUBR_ARITY_FLOOR_REM, SCM_SUBR_FLAG_FLOOR_REM, scm_subr_func_floor_rem, true },

    /*******************************************************************/
    /*  Booleans                                                       */
    /*******************************************************************/
    { "not", SCM_SUBR_ARITY_NOT, SCM_SUBR_FLAG_NOT, scm_subr_func_not, true },
    { "boolean?", SCM_SUBR_ARITY_BOOLEAN_P, SCM_SUBR_FLAG_BOOLEAN_P, scm_subr_func_boolean_P, true },

    /*******************************************************************/
    /*  Pair and Lists                                                 */
    /*******************************************************************/
    { "null?", SCM_SUBR_ARITY_NULL_P, SCM_SUBR_FLAG_NULL_P, scm_subr_func_null_P, true },
    { "pair?", SCM_SUBR_ARITY_PAIR_P, SCM_SUBR_FLAG_PAIR_P, scm_subr_func_pair_P, true },
    { "cons", SCM_SUBR_ARITY_CONS, SCM_SUBR_FLAG_CONS, scm_subr_func_cons, true },
    { "car", SCM_SUBR_ARITY_CAR, SCM_SUBR_FLAG_CAR, scm_subr_func_car, true },
    { "cdr", SCM_SUBR_ARITY_CDR, SCM_SUBR_FLAG_CDR, scm_subr_func_cdr, true },
    { "set-car!", SCM_SUBR_ARITY_SET_CAR_I, SCM_SUBR_FLAG_SET_CAR_I, scm_subr_func_set_car_i, true },
    { "set-cdr!", SCM_SUBR_ARITY_SET_CDR_I, SCM_SUBR_FLAG_SET_CDR_I, scm_subr_func_set_cdr_i, true },
    { "list?", SCM_SUBR_ARITY_LIST_P, SCM_SUBR_FLAG_LIST_P, scm_subr_func_list_P, true },
    { "make-list", SCM_SUBR_ARITY_MAKE_LIST, SCM_SUBR_FLAG_MAKE_LIST, scm_subr_func_make_list, true },
    { "list", SCM_SUBR_ARITY_LIST, SCM_SUBR_FLAG_LIST, scm_subr_func_list, true },
    { "length", SCM_SUBR_ARITY_LENGTH, SCM_SUBR_FLAG_LENGTH, scm_subr_func_length, true },
    { "append", SCM_SUBR_ARITY_APPEND, SCM_SUBR_FLAG_APPEND, scm_subr_func_append, true },
    { "reverse", SCM_SUBR_ARITY_REVERSE, SCM_SUBR_FLAG_REVERSE, scm_subr_func_reverse, true },
    { "list-tail", SCM_SUBR_ARITY_LIST_TAIL, SCM_SUBR_FLAG_LIST_TAIL, scm_subr_func_list_tail, true },
    { "list-ref", SCM_SUBR_ARITY_LIST_REF, SCM_SUBR_FLAG_LIST_REF, scm_subr_func_list_ref, true },
    { "list-set!", SCM_SUBR_ARITY_LIST_SET_I, SCM_SUBR_FLAG_LIST_SET_I, scm_subr_func_list_set_i, true },
    { "memq", SCM_SUBR_ARITY_MEMQ, SCM_SUBR_FLAG_MEMQ, scm_subr_func_memq, true },
    { "memv", SCM_SUBR_ARITY_MEMV, SCM_SUBR_FLAG_MEMV, scm_subr_func_memv, true },
    { "member", SCM_SUBR_ARITY_MEMBER, SCM_SUBR_FLAG_MEMBER, scm_subr_func_member, true },
    { "assq", SCM_SUBR_ARITY_ASSQ, SCM_SUBR_FLAG_ASSQ, scm_subr_func_assq, true },
    { "assv", SCM_SUBR_ARITY_ASSV, SCM_SUBR_FLAG_ASSV, scm_subr_func_assv, true },
    { "assoc", SCM_SUBR_ARITY_ASSOC, SCM_SUBR_FLAG_ASSOC, scm_subr_func_assoc, true },
    { "list-copy", SCM_SUBR_ARITY_LIST_COPY, SCM_SUBR_FLAG_LIST_COPY, scm_subr_func_list_copy, true },

    /*******************************************************************/
    /*  Symbols                                                        */
    /*******************************************************************/
    { "symbol?", SCM_SUBR_ARITY_SYMBOL_P, SCM_SUBR_FLAG_SYMBOL_P, scm_subr_func_symbol_P, true },
    { "symbol=?", SCM_SUBR_ARITY_SYMBOL_EQ_P, SCM_SUBR_FLAG_SYMBOL_EQ_P, scm_subr_func_symbol_eq_P, true },
    { "symbol->string", SCM_SUBR_ARITY_SYMBOL_TO_STRING, SCM_SUBR_FLAG_SYMBOL_TO_STRING, scm_subr_func_symbol_to_string, true },
    { "string->symbol", SCM_SUBR_ARITY_STRING_TO_SYMBOL, SCM_SUBR_FLAG_STRING_TO_SYMBOL, scm_subr_func_string_to_symbol, true },

    /*******************************************************************/
    /*  Characters                                                     */
    /*******************************************************************/
    { "char?", SCM_SUBR_ARITY_CHAR_P, SCM_SUBR_FLAG_CHAR_P, scm_subr_func_char_P, true },
    { "char=?", SCM_SUBR_ARITY_CHAR_EQ_P, SCM_SUBR_FLAG_CHAR_EQ_P, scm_subr_func_char_eq_P, true },
    { "char<?", SCM_SUBR_ARITY_CHAR_LT_P, SCM_SUBR_FLAG_CHAR_LT_P, scm_subr_func_char_lt_P, true },
    { "char>?", SCM_SUBR_ARITY_CHAR_GT_P, SCM_SUBR_FLAG_CHAR_GT_P, scm_subr_func_char_gt_P, true },
    { "char<=?", SCM_SUBR_ARITY_CHAR_LE_P, SCM_SUBR_FLAG_CHAR_LE_P, scm_subr_func_char_le_P, true },
    { "char>=?", SCM_SUBR_ARITY_CHAR_GE_P, SCM_SUBR_FLAG_CHAR_GE_P, scm_subr_func_char_ge_P, true },
    { "char->integer", SCM_SUBR_ARITY_CHAR_TO_INTEGER, SCM_SUBR_FLAG_CHAR_TO_INTEGER, scm_subr_func_char_to_integer, true },
    { "integer->char", SCM_SUBR_ARITY_INTEGER_TO_CHAR, SCM_SUBR_FLAG_INTEGER_TO_CHAR, scm_subr_func_integer_to_char, true },

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    { "string?", SCM_SUBR_ARITY_STRING_P, SCM_SUBR_FLAG_STRING_P, scm_subr_func_string_P, true },
    { "make-string", SCM_SUBR_ARITY_MAKE_STRING, SCM_SUBR_FLAG_MAKE_STRING, scm_subr_func_make_string, true },
    { "string", SCM_SUBR_ARITY_STRING, SCM_SUBR_FLAG_STRING, scm_subr_func_string, true },
    { "string-length", SCM_SUBR_ARITY_STRING_LENGTH, SCM_SUBR_FLAG_STRING_LENGTH, scm_subr_func_string_length, true },
    { "string-bytesize", SCM_SUBR_ARITY_STRING_BYTESIZE, SCM_SUBR_FLAG_STRING_BYTESIZE, scm_subr_func_string_bytesize, true },
    { "string-ref", SCM_SUBR_ARITY_STRING_REF, SCM_SUBR_FLAG_STRING_REF, scm_subr_func_string_ref, true },
    { "string-set!", SCM_SUBR_ARITY_STRING_SET_I, SCM_SUBR_FLAG_STRING_SET_I, scm_subr_func_string_set_i, true },
    { "string=?", SCM_SUBR_ARITY_STRING_EQ_P, SCM_SUBR_FLAG_STRING_EQ_P, scm_subr_func_string_eq_P, true },
    { "string<?", SCM_SUBR_ARITY_STRING_LT_P, SCM_SUBR_FLAG_STRING_LT_P, scm_subr_func_string_lt_P, true },
    { "string>?", SCM_SUBR_ARITY_STRING_GT_P, SCM_SUBR_FLAG_STRING_GT_P, scm_subr_func_string_gt_P, true },
    { "string<=?", SCM_SUBR_ARITY_STRING_LE_P, SCM_SUBR_FLAG_STRING_LE_P, scm_subr_func_string_le_P, true },
    { "string>=?", SCM_SUBR_ARITY_STRING_GE_P, SCM_SUBR_FLAG_STRING_GE_P, scm_subr_func_string_ge_P, true },
    { "substring", SCM_SUBR_ARITY_SUBSTRING, SCM_SUBR_FLAG_SUBSTRING, scm_subr_func_substring, true },
    { "string-append", SCM_SUBR_ARITY_STRING_APPEND, SCM_SUBR_FLAG_STRING_APPEND, scm_subr_func_string_append, true },
    { "string->list", SCM_SUBR_ARITY_STRING_TO_LIST, SCM_SUBR_FLAG_STRING_TO_LIST, scm_subr_func_string_to_list, true },
    { "list->string", SCM_SUBR_ARITY_LIST_TO_STRING, SCM_SUBR_FLAG_LIST_TO_STRING, scm_subr_func_list_to_string, true },
    { "string-copy", SCM_SUBR_ARITY_STRING_COPY, SCM_SUBR_FLAG_STRING_COPY, scm_subr_func_string_copy, true },
    { "string-copy!", SCM_SUBR_ARITY_STRING_COPY_I, SCM_SUBR_FLAG_STRING_COPY_I, scm_subr_func_string_copy_i, true },
    { "string-fill!", SCM_SUBR_ARITY_STRING_FILL_I, SCM_SUBR_FLAG_STRING_FILL_I, scm_subr_func_string_fill_i, true },

    /*******************************************************************/
    /*  Vectors                                                        */
    /*******************************************************************/
    { "vector?", SCM_SUBR_ARITY_VECTOR_P, SCM_SUBR_FLAG_VECTOR_P, scm_subr_func_vector_P, true },
    { "make-vector", SCM_SUBR_ARITY_MAKE_VECTOR, SCM_SUBR_FLAG_MAKE_VECTOR, scm_subr_func_make_vector, true },
    { "vector", SCM_SUBR_ARITY_VECTOR, SCM_SUBR_FLAG_VECTOR, scm_subr_func_vector, true },
    { "vector-length", SCM_SUBR_ARITY_VECTOR_LENGTH, SCM_SUBR_FLAG_VECTOR_LENGTH, scm_subr_func_vector_length, true },
    { "vector-ref", SCM_SUBR_ARITY_VECTOR_REF, SCM_SUBR_FLAG_VECTOR_REF, scm_subr_func_vector_ref, true },
    { "vector-set!", SCM_SUBR_ARITY_VECTOR_SET_I, SCM_SUBR_FLAG_VECTOR_SET_I, scm_subr_func_vector_set_i, true },
    { "vector->list", SCM_SUBR_ARITY_VECTOR_TO_LIST, SCM_SUBR_FLAG_VECTOR_TO_LIST, scm_subr_func_vector_to_list, true },
    { "list->vector", SCM_SUBR_ARITY_LIST_TO_VECTOR, SCM_SUBR_FLAG_LIST_TO_VECTOR, scm_subr_func_list_to_vector, true },
    { "vector->string", SCM_SUBR_ARITY_VECTOR_TO_STRING, SCM_SUBR_FLAG_VECTOR_TO_STRING, scm_subr_func_vector_to_string, true },
    { "string->vector", SCM_SUBR_ARITY_STRING_TO_VECTOR, SCM_SUBR_FLAG_STRING_TO_VECTOR, scm_subr_func_string_to_vector, true },
    { "vector-copy", SCM_SUBR_ARITY_VECTOR_COPY, SCM_SUBR_FLAG_VECTOR_COPY, scm_subr_func_vector_copy, true },
    { "vector-copy!", SCM_SUBR_ARITY_VECTOR_COPY_I, SCM_SUBR_FLAG_VECTOR_COPY_I, scm_subr_func_vector_copy_i, true },
    { "vector-append", SCM_SUBR_ARITY_VECTOR_APPEND, SCM_SUBR_FLAG_VECTOR_APPEND, scm_subr_func_vector_append, true },
    { "vector-fill!", SCM_SUBR_ARITY_VECTOR_FILL_I, SCM_SUBR_FLAG_VECTOR_FILL_I, scm_subr_func_vector_fill_i, true },

    /*******************************************************************/
    /*  Control features                                               */
    /*******************************************************************/
    { "procedure?", SCM_SUBR_ARITY_PROCEDURE_P, SCM_SUBR_FLAG_PROCEDURE_P, scm_subr_func_procedure_P, true },
    { "apply", SCM_SUBR_ARITY_APPLY, SCM_SUBR_FLAG_APPLY, scm_subr_func_apply, true },

    /*******************************************************************/
    /*  Exceptions                                                     */
    /*******************************************************************/
    { "with-exception-handler", SCM_SUBR_ARITY_WITH_EXCEPTION_HANDLER, SCM_SUBR_FLAG_WITH_EXCEPTION_HANDLER, scm_subr_func_with_exception_handler, true },
    { "raise", SCM_SUBR_ARITY_RAISE, SCM_SUBR_FLAG_RAISE, scm_subr_func_raise, true },
    { "raise-continuable", SCM_SUBR_ARITY_RAISE_CONTINUABLE, SCM_SUBR_FLAG_RAISE_CONTINUABLE, scm_subr_func_raise_continuable, true },
    { "error", SCM_SUBR_ARITY_ERROR, SCM_SUBR_FLAG_ERROR, scm_subr_func_error, true },
    { "error-object?", SCM_SUBR_ARITY_ERROR_OBJECT_P, SCM_SUBR_FLAG_ERROR_OBJECT_P, scm_subr_func_error_object_P, true },
    { "error-object-message", SCM_SUBR_ARITY_ERROR_OBJECT_MESSAGE, SCM_SUBR_FLAG_ERROR_OBJECT_MESSAGE, scm_subr_func_error_object_message, true },
    { "error-object-irritants", SCM_SUBR_ARITY_ERROR_OBJECT_IRRITANTS, SCM_SUBR_FLAG_ERROR_OBJECT_IRRITANTS, scm_subr_func_error_object_irritants, true },
    { "read-error?", SCM_SUBR_ARITY_READ_ERROR_P, SCM_SUBR_FLAG_READ_ERROR_P, scm_subr_func_read_error_P, true },
    { "file-error?", SCM_SUBR_ARITY_FILE_ERROR_P, SCM_SUBR_FLAG_FILE_ERROR_P, scm_subr_func_file_error_P, true },

    /*******************************************************************/
    /*  Ports                                                          */
    /*******************************************************************/
    { "open-input-file", SCM_SUBR_ARITY_OPEN_INPUT_FILE, SCM_SUBR_FLAG_OPEN_INPUT_FILE, scm_subr_func_open_input_file, true },

    /*******************************************************************/
    /*  Input Output                                                   */
    /*******************************************************************/
    { "read", SCM_SUBR_ARITY_READ, SCM_SUBR_FLAG_READ, scm_subr_func_read, true },
    { "write", SCM_SUBR_ARITY_WRITE, SCM_SUBR_FLAG_WRITE, scm_subr_func_write, true },
    { "display", SCM_SUBR_ARITY_DISPLAY, SCM_SUBR_FLAG_DISPLAY, scm_subr_func_display, true },
    { "newline", SCM_SUBR_ARITY_NEWLINE, SCM_SUBR_FLAG_NEWLINE, scm_subr_func_newline, true },
    { "flush-output-port", SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT, SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT, scm_subr_func_flush_output_port, true },
    { "eof-object?", SCM_SUBR_ARITY_EOF_OBJECT_P, SCM_SUBR_FLAG_EOF_OBJECT_P, scm_subr_func_eof_object_P, true },

    { "call/cc", SCM_SUBR_ARITY_CALLCC, SCM_SUBR_FLAG_CALLCC, scm_subr_func_callcc, true },
    { "values", SCM_SUBR_ARITY_VALUES, SCM_SUBR_FLAG_VALUES, scm_subr_func_values, true },
    { "call-with-values", SCM_SUBR_ARITY_CALL_WITH_VALUES, SCM_SUBR_FLAG_CALL_WITH_VALUES, scm_subr_func_call_with_values, true },
    { "eval-asm", SCM_SUBR_ARITY_EVAL_ASM, SCM_SUBR_FLAG_EVAL_ASM, scm_subr_func_eval_asm, true },
    { "eval", SCM_SUBR_ARITY_EVAL, SCM_SUBR_FLAG_EVAL, scm_subr_func_eval, true },
    { "exit", SCM_SUBR_ARITY_EXIT, SCM_SUBR_FLAG_EXIT, scm_subr_func_exit, true },
  };

  ScmObj sym  = SCM_OBJ_INIT, subr = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&module,
                      &sym, &subr);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_define_scheme_base_current_port(ScmObj module)
{
  struct {
    const char *name;
    int fd;
    ScmObj (*func)(int fd, const char *enc);
  } const data[] = {
    { "current-input-port", 0, scm_capi_open_input_fd },
    { "current-output-port", 1, scm_capi_open_output_fd },
    { "current-error-port", 2, scm_capi_open_output_fd },
  };

  ScmObj sym = SCM_OBJ_INIT, port = SCM_OBJ_INIT, prm = SCM_OBJ_INIT;
  int rslt, fd;

  SCM_REFSTK_INIT_REG(&module,
                      &sym, &port, &prm);

  for (size_t i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
    sym = scm_capi_make_symbol_from_cstr(data[i].name, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;

    prm = scm_api_make_parameter(SCM_OBJ_NULL);
    if (scm_obj_null_p(prm)) return -1;

    SCM_SYSCALL(fd, dup(data[i].fd));
    if (fd < 0) {
      scm_capi_error("system call error: dup", 0);
      return -1;
    }

    port = data[i].func(fd, NULL);
    if (scm_obj_null_p(port)) {
      close(fd);
      return -1;
    }

    rslt = scm_capi_parameter_set_init_val(prm, port);
    if (rslt < 0) return -1;

    rslt = scm_capi_define_global_var(module, sym, prm, true);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_load_module_func_scheme_base(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &name);


  /*
   * define global variables
   */

  rslt = scm_define_scheme_base_subr(mod);
  if (rslt < 0) return -1;

  rslt = scm_define_scheme_base_current_port(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_base(void)
{
  return scm_load_module((const char *[]){"scheme", "base"}, 2,
                         scm_load_module_func_scheme_base);
}


/*******************************************************************/
/*  (scheme char)                                                  */
/*******************************************************************/

static int
scm_define_scheme_char_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Characters                                                     */
    /*******************************************************************/
    { "char-ci=?", SCM_SUBR_ARITY_CHAR_CI_EQ_P, SCM_SUBR_FLAG_CHAR_CI_EQ_P, scm_subr_func_char_ci_eq_P, true },
    { "char-ci<?", SCM_SUBR_ARITY_CHAR_CI_LT_P, SCM_SUBR_FLAG_CHAR_CI_LT_P, scm_subr_func_char_ci_lt_P, true },
    { "char-ci>?", SCM_SUBR_ARITY_CHAR_CI_GT_P, SCM_SUBR_FLAG_CHAR_CI_GT_P, scm_subr_func_char_ci_gt_P, true },
    { "char-ci<=?", SCM_SUBR_ARITY_CHAR_CI_LE_P, SCM_SUBR_FLAG_CHAR_CI_LE_P, scm_subr_func_char_ci_le_P, true },
    { "char-ci>=?", SCM_SUBR_ARITY_CHAR_CI_GE_P, SCM_SUBR_FLAG_CHAR_CI_GE_P, scm_subr_func_char_ci_ge_P, true },
    { "char-alphabetic?", SCM_SUBR_ARITY_CHAR_ALPHABETIC_P, SCM_SUBR_FLAG_CHAR_ALPHABETIC_P, scm_subr_func_char_alphabetic_P, true },
    { "char-numeric?", SCM_SUBR_ARITY_CHAR_NUMERIC_P, SCM_SUBR_FLAG_CHAR_NUMERIC_P, scm_subr_func_char_numeric_P, true },
    { "char-whitespace?", SCM_SUBR_ARITY_CHAR_WHITESPACE_P, SCM_SUBR_FLAG_CHAR_WHITESPACE_P, scm_subr_func_char_whitespace_P, true },
    { "char-upper-case?", SCM_SUBR_ARITY_CHAR_UPPER_CASE_P, SCM_SUBR_FLAG_CHAR_UPPER_CASE_P, scm_subr_func_char_upper_case_P, true },
    { "char-lower-case?", SCM_SUBR_ARITY_CHAR_LOWER_CASE_P, SCM_SUBR_FLAG_CHAR_LOWER_CASE_P, scm_subr_func_char_lower_case_P, true },
    { "digit_value", SCM_SUBR_ARITY_DIGIT_VALUE, SCM_SUBR_FLAG_DIGIT_VALUE, scm_subr_func_digit_value, true },
    { "char-upcase", SCM_SUBR_ARITY_CHAR_UPCASE, SCM_SUBR_FLAG_CHAR_UPCASE, scm_subr_func_char_upcase, true },
    { "char-downcase", SCM_SUBR_ARITY_CHAR_DOWNCASE, SCM_SUBR_FLAG_CHAR_DOWNCASE, scm_subr_func_char_downcase, true },
    { "char-foldcase", SCM_SUBR_ARITY_CHAR_FOLDCASE, SCM_SUBR_FLAG_CHAR_FOLDCASE, scm_subr_func_char_foldcase, true },

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    { "string-ci=?", SCM_SUBR_ARITY_STRING_CI_EQ_P, SCM_SUBR_FLAG_STRING_CI_EQ_P, scm_subr_func_string_ci_eq_P, true },
    { "string-ci<?", SCM_SUBR_ARITY_STRING_CI_LT_P, SCM_SUBR_FLAG_STRING_CI_LT_P, scm_subr_func_string_ci_lt_P, true },
    { "string-ci>?", SCM_SUBR_ARITY_STRING_CI_GT_P, SCM_SUBR_FLAG_STRING_CI_GT_P, scm_subr_func_string_ci_gt_P, true },
    { "string-ci<=?", SCM_SUBR_ARITY_STRING_CI_LE_P, SCM_SUBR_FLAG_STRING_CI_LE_P, scm_subr_func_string_ci_le_P, true },
    { "string-ci>=?", SCM_SUBR_ARITY_STRING_CI_GE_P, SCM_SUBR_FLAG_STRING_CI_GE_P, scm_subr_func_string_ci_ge_P, true },
  };

  int rslt;

  SCM_REFSTK_INIT_REG(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_char(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &name);


  /*
   * define global variables
   */

  rslt = scm_define_scheme_char_subr(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_char(void)
{
  return scm_load_module(STRARY("scheme", "char"), 2,
                         scm_load_module_func_scheme_char);
}


/*******************************************************************/
/*  (scythe internal compile)                                      */
/*******************************************************************/

static int
scm_define_scythe_internal_compile_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Compiler                                                       */
    /*******************************************************************/
    { "compiler?", SCM_SUBR_ARITY_COMPILER_P, SCM_SUBR_FLAG_COMPILER_P, scm_subr_func_compiler_P, false },
    { "make-compiler", SCM_SUBR_ARITY_MAKE_COMPILER, SCM_SUBR_FLAG_MAKE_COMPILER, scm_subr_func_make_compiler, false },
    { "compiler-assign-label-id!", SCM_SUBR_ARITY_COMPILER_ASSIGN_LABEL_ID_I, SCM_SUBR_FLAG_COMPILER_ASSIGN_LABEL_ID_I, scm_subr_func_compiler_assign_label_id_i, false },
    { "compiler-current-module", SCM_SUBR_ARITY_COMPILER_CURRENT_MODULE, SCM_SUBR_FLAG_COMPILER_CURRENT_MODULE, scm_subr_func_compiler_current_module, false },
    { "compiler-select-module!", SCM_SUBR_ARITY_COMPILER_SELECT_MODULE_I, SCM_SUBR_FLAG_COMPILER_SELECT_MODULE_I, scm_subr_func_compiler_select_module_i, false },
    { "compiler-current-expr", SCM_SUBR_ARITY_COMPILER_CURRENT_EXPR, SCM_SUBR_FLAG_COMPILER_CURRENT_EXPR, scm_subr_func_compiler_current_expr, false },
    { "compiler-select-expr!", SCM_SUBR_ARITY_COMPILER_SELECT_EXPR_I, SCM_SUBR_FLAG_COMPILER_SELECT_EXPR_I, scm_subr_func_compiler_select_expr_i, false },

    /*******************************************************************/
    /*  Syntax                                                         */
    /*******************************************************************/
    { "syntax?", SCM_SUBR_ARITY_SYNTAX_P, SCM_SUBR_FLAG_SYNTAX_P, scm_subr_func_syntax_P, false },
    { "make-syntax", SCM_SUBR_ARITY_MAKE_SYNTAX, SCM_SUBR_FLAG_MAKE_SYNTAX, scm_subr_func_make_syntax, false },
    { "syntax-keyword", SCM_SUBR_ARITY_SYNTAX_KEYWORD, SCM_SUBR_FLAG_SYNTAX_KEYWORD, scm_subr_func_syntax_keyword, false },
    { "syntax-handler", SCM_SUBR_ARITY_SYNTAX_HANDLER, SCM_SUBR_FLAG_SYNTAX_HANDLER, scm_subr_func_syntax_handler, false },

    /*******************************************************************/
    /*  Global Variables                                               */
    /*******************************************************************/
    { "global-syntax-bind", SCM_SUBR_ARITY_GLOBAL_SYNTAX_BIND, SCM_SUBR_FLAG_GLOBAL_SYNTAX_BIND, scm_subr_func_global_syntax_bind, false },
    { "global-syntax-ref", SCM_SUBR_ARITY_GLOBAL_SYNTAX_REF, SCM_SUBR_FLAG_GLOBAL_SYNTAX_REF, scm_subr_func_global_syntax_ref, false },
  };

  int rslt;

  SCM_REFSTK_INIT_REG(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

#include "scythe/compiler_code.h"

static int
scm_define_scythe_internal_compile_closure(ScmObj mod)
{
  ScmObj port = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &port, &lst, &iseq);

  port = scm_capi_open_input_string_cstr(compiler_code,
                                         SCM_ENC_NAME_SRC);
  if (scm_obj_null_p(port)) return -1;

  lst = scm_api_read(port);
  if (scm_obj_null_p(lst)) return -1;

  iseq = scm_api_assemble(lst, SCM_OBJ_NULL);
  if (scm_obj_null_p(iseq)) return -1;

  rslt = scm_capi_load_iseq(iseq);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_compile(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  /* TODO: compile モジュールのロードが非常に重いので㷀然する
   *       (compiler のコードのデシリアライズが重い)
   */

  SCM_REFSTK_INIT_REG(&name, &mod);

  /*
   * load (scythe base) module and import it
   */

  rslt = scm_load_module_scythe_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(STRARY("scythe", "base"), 2);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_import(mod, name, true);
  if (rslt < 0) return -1;


  /*
   * define global variables
   */

  rslt = scm_define_scythe_internal_compile_subr(mod);
  if (rslt < 0) return -1;

  rslt = scm_define_scythe_internal_compile_closure(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_compile(void)
{
  return scm_load_module(STRARY("scythe", "internal", "compile"), 3,
                         scm_load_module_func_scythe_internal_compile);
}


/*******************************************************************/
/*  (scythe internal repl)                                         */
/*******************************************************************/

#include "scythe/repl_code.h"

static int
scm_define_scythe_internal_repl_closure(ScmObj mod)
{
  ScmObj port = SCM_OBJ_INIT, lst = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &port, &lst, &iseq);

  port = scm_capi_open_input_string_cstr(repl_code,
                                         SCM_ENC_NAME_SRC);
  if (scm_obj_null_p(port)) return -1;

  lst = scm_api_read(port);
  if (scm_obj_null_p(lst)) return -1;

  iseq = scm_api_assemble(lst, SCM_OBJ_NULL);
  if (scm_obj_null_p(iseq)) return -1;

  rslt = scm_capi_load_iseq(iseq);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_repl(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&name, &mod);

  /*
   * load (scythe base) module and import it
   */

  rslt = scm_load_module_scythe_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(STRARY("scythe", "base"), 2);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_import(mod, name, true);
  if (rslt < 0) return -1;

  rslt = scm_define_scythe_internal_repl_closure(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_repl(void)
{
  return scm_load_module(STRARY("scythe", "internal", "repl"), 3,
                         scm_load_module_func_scythe_internal_repl);
}


/*******************************************************************/
/*  (scythe internal command)                                      */
/*******************************************************************/

static int
scm_define_scythe_internal_command_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    { "eval-file", SCM_SUBR_ARITY_EVAL_FILE, SCM_SUBR_FLAG_EVAL_FILE, scm_subr_func_eval_file, false },
    { "eval-string", SCM_SUBR_ARITY_EVAL_STRING, SCM_SUBR_FLAG_EVAL_STRING, scm_subr_func_eval_string, false },
  };

  int rslt;

  SCM_REFSTK_INIT_REG(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_command(ScmObj mod)
{
  int rslt;

  /*
   * define global variables
   */

  rslt = scm_define_scythe_internal_command_subr(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_command(void)
{
  return scm_load_module(STRARY("scythe", "internal", "command"), 3,
                         scm_load_module_func_scythe_internal_command);
}


/*******************************************************************/
/*  (scythe base)                                                  */
/*******************************************************************/

static int
scm_define_scythe_base_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  format                                                         */
    /*******************************************************************/
    { "format", SCM_SUBR_ARITY_FORMAT, SCM_SUBR_FLAG_FORMAT, scm_subr_func_format, true },

    /*******************************************************************/
    /*  Modules                                                        */
    /*******************************************************************/
    { "module-name", SCM_SUBR_ARITY_MODULE_NAME, SCM_SUBR_FLAG_MODULE_NAME, scm_subr_func_module_name, true },
  };

  int rslt;

  SCM_REFSTK_INIT_REG(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}


static int
scm_load_module_func_scythe_base(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &name);

  /*
   * load (scheme base) module and import it
   */

  rslt = scm_load_module_scheme_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(STRARY("scheme", "base"), 2);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_import(mod, name, false);
  if (rslt < 0) return -1;


  /*
   * load (scheme char) module and import it
   */

  rslt = scm_load_module_scheme_char();
  if (rslt < 0) return -1;

  name = scm_make_module_name(STRARY("scheme", "char"), 2);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_import(mod, name, false);
  if (rslt < 0) return -1;


  /*
   * define global variables
   */

  rslt = scm_define_scythe_base_subr(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_base(void)
{
  return scm_load_module(STRARY("scythe", "base"), 2,
                         scm_load_module_func_scythe_base);
}


/*******************************************************************/
/*  (main)                                                         */
/*******************************************************************/

static int
scm_load_module_func_main(ScmObj mod)
{
  ScmObj name = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&mod,
                      &name);

  /*
   * load (scythe base) module and import it
   */

  rslt = scm_load_module_scythe_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(STRARY("scythe", "base"), 2);
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_import(mod, name, false);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_main(void)
{
  return scm_load_module(STRARY("main"), 1,
                         scm_load_module_func_main);
}


int
scm_load_core_modules(void)
{
  int (*func[])(void) = {
    scm_load_module_scheme_base,
    scm_load_module_scheme_char,
    scm_load_module_scythe_internal_compile,
    scm_load_module_scythe_internal_repl,
    scm_load_module_scythe_internal_command,
    scm_load_module_scythe_base,
    scm_load_module_main,
  };

  for (size_t i = 0; i < sizeof(func)/sizeof(func[0]); i++) {
    int r = func[i]();
    if (r < 0) return -1;
  }

  return 0;
}
