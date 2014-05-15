#include <unistd.h>
#include <stdint.h>
#include <stdarg.h>

#include "object.h"
#include "api.h"
#include "compiler.h"
#include "core_subr.h"
#include "impl_utils.h"

struct subr_data {
  const char *name;
  int arity;
  unsigned int flag;
  ScmSubrFunc func;
};

static ScmObj
scm_make_module_name(int n, ScmEncoding *enc, ...)
{
  ScmObj str[n];
  ScmObj name = SCM_OBJ_INIT;
  const char *args[n];
  va_list ap;

  SCM_STACK_FRAME_PUSH(&name);

  scm_assert(n > 0);

  for (int i = 0; i < n; i++) {
    str[i] = SCM_OBJ_INIT;
    SCM_STACK_PUSH(&str[i]);
  }

  va_start(ap, enc);
  for (int i = 0; i < n; i++)
    args[i] = va_arg(ap, const char *);
  va_end(ap);

  for (int i = 0; i < n; i++) {
    str[i] = scm_capi_make_symbol_from_cstr(args[i], enc);
    if (scm_obj_null_p(str[i])) return SCM_OBJ_NULL;
  }

  name = SCM_NIL_OBJ;

  for (int i = n; i > 0; i--) {
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

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &subr);

  for (size_t i = 0; i < n; i++) {
    sym = scm_capi_make_symbol_from_cstr(data[i].name, SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    subr = scm_capi_make_subrutine(data[i].func, data[i].arity, data[i].flag,
                                   module);
    if (scm_obj_null_p(subr)) return -1;

    rslt = scm_capi_define_global_var(module, sym, subr, true);
    if (rslt < 0) return -1;                   /* [ERR]: [through] */
  }

  return 0;
}


/*******************************************************************/
/*  (scheme base)                                                  */
/*******************************************************************/

static int
scm_load_module_scheme_base_syntax(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod);

  name = scm_make_module_name(3, SCM_ENC_ASCII, "scheme", "base", "syntax");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  rslt = scm_cmpl_define_syntax(mod);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_define_scheme_base_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Equivalence predicates                                         */
    /*******************************************************************/
    { "eq?", SCM_SUBR_ARITY_EQ_P, SCM_SUBR_FLAG_EQ_P, scm_subr_func_eq_P },
    { "eqv?", SCM_SUBR_ARITY_EQV_P, SCM_SUBR_FLAG_EQV_P, scm_subr_func_eqv_P },
    { "equal?", SCM_SUBR_ARITY_EQUAL_P, SCM_SUBR_FLAG_EQUAL_P, scm_subr_func_equal_P },

    /*******************************************************************/
    /*  Numbers                                                        */
    /*******************************************************************/
    { "number?", SCM_SUBR_ARITY_NUMBER_P, SCM_SUBR_FLAG_NUMBER_P, scm_subr_func_number_P },
    { "complex?", SCM_SUBR_ARITY_COMPLEX_P, SCM_SUBR_FLAG_COMPLEX_P, scm_subr_func_complex_P },
    { "real?", SCM_SUBR_ARITY_REAL_P, SCM_SUBR_FLAG_REAL_P, scm_subr_func_real_P },
    { "rational?", SCM_SUBR_ARITY_RATIONAL_P, SCM_SUBR_FLAG_RATIONAL_P, scm_subr_func_rational_P },
    { "integer?", SCM_SUBR_ARITY_INTEGER_P, SCM_SUBR_FLAG_INTEGER_P, scm_subr_func_integer_P },
    { "exact?", SCM_SUBR_ARITY_EXACT_P, SCM_SUBR_FLAG_EXACT_P, scm_subr_func_exact_P },
    { "inexact?", SCM_SUBR_ARITY_INEXACT_P, SCM_SUBR_FLAG_INEXACT_P, scm_subr_func_inexact_P },
    { "exact-integer?", SCM_SUBR_ARITY_EXACT_INTEGER_P, SCM_SUBR_FLAG_EXACT_INTEGER_P, scm_subr_func_exact_integer_P },
    { "finite?", SCM_SUBR_ARITY_FINITE_P, SCM_SUBR_FLAG_FINITE_P, scm_subr_func_finite_P },
    { "infinite?", SCM_SUBR_ARITY_INFINITE_P, SCM_SUBR_FLAG_INFINITE_P, scm_subr_func_infinite_P },
    { "nan?", SCM_SUBR_ARITY_NAN_P, SCM_SUBR_FLAG_NAN_P, scm_subr_func_nan_P },
    { "=", SCM_SUBR_ARITY_NUM_EQ_P, SCM_SUBR_FLAG_NUM_EQ_P, scm_subr_func_num_eq_P },
    { "<", SCM_SUBR_ARITY_NUM_LT_P, SCM_SUBR_FLAG_NUM_LT_P, scm_subr_func_num_lt_P },
    { ">", SCM_SUBR_ARITY_NUM_GT_P, SCM_SUBR_FLAG_NUM_GT_P, scm_subr_func_num_gt_P },
    { "<=", SCM_SUBR_ARITY_NUM_LE_P, SCM_SUBR_FLAG_NUM_LE_P, scm_subr_func_num_le_P },
    { ">=", SCM_SUBR_ARITY_NUM_GE_P, SCM_SUBR_FLAG_NUM_GE_P, scm_subr_func_num_ge_P },
    { "zero?", SCM_SUBR_ARITY_ZERO_P, SCM_SUBR_FLAG_ZERO_P, scm_subr_func_zero_P },
    { "positive?", SCM_SUBR_ARITY_POSITIVE_P, SCM_SUBR_FLAG_POSITIVE_P, scm_subr_func_positive_P },
    { "negative?", SCM_SUBR_ARITY_NEGATIVE_P, SCM_SUBR_FLAG_NEGATIVE_P, scm_subr_func_negative_P },
    { "odd?", SCM_SUBR_ARITY_ODD_P, SCM_SUBR_FLAG_ODD_P, scm_subr_func_odd_P },
    { "even?", SCM_SUBR_ARITY_EVEN_P, SCM_SUBR_FLAG_EVEN_P, scm_subr_func_even_P },
    { "max", SCM_SUBR_ARITY_MAX, SCM_SUBR_FLAG_MAX, scm_subr_func_max },
    { "min", SCM_SUBR_ARITY_MIN, SCM_SUBR_FLAG_MIN, scm_subr_func_min },
    { "+", SCM_SUBR_ARITY_PLUS, SCM_SUBR_FLAG_PLUS, scm_subr_func_plus },
    { "*", SCM_SUBR_ARITY_MUL, SCM_SUBR_FLAG_MUL, scm_subr_func_mul },
    { "-", SCM_SUBR_ARITY_MINUS, SCM_SUBR_FLAG_MINUS, scm_subr_func_minus },
    { "/", SCM_SUBR_ARITY_DIV, SCM_SUBR_FLAG_DIV, scm_subr_func_div },
    { "floor/", SCM_SUBR_ARITY_FLOOR_DIV, SCM_SUBR_FLAG_FLOOR_DIV, scm_subr_func_floor_div },
    { "floor-quotient", SCM_SUBR_ARITY_FLOOR_QUO, SCM_SUBR_FLAG_FLOOR_QUO, scm_subr_func_floor_quo },
    { "floor-remainder", SCM_SUBR_ARITY_FLOOR_REM, SCM_SUBR_FLAG_FLOOR_REM, scm_subr_func_floor_rem },
    { "truncate/", SCM_SUBR_ARITY_TRUNCATE_DIV, SCM_SUBR_FLAG_TRUNCATE_DIV, scm_subr_func_truncate_div },
    { "truncate-quotient", SCM_SUBR_ARITY_TRUNCATE_QUO, SCM_SUBR_FLAG_TRUNCATE_QUO, scm_subr_func_truncate_quo },
    { "truncate-remainder", SCM_SUBR_ARITY_TRUNCATE_REM, SCM_SUBR_FLAG_TRUNCATE_REM, scm_subr_func_truncate_rem },
    { "quotient", SCM_SUBR_ARITY_TRUNCATE_QUO, SCM_SUBR_FLAG_TRUNCATE_QUO, scm_subr_func_truncate_quo },
    { "remainder", SCM_SUBR_ARITY_TRUNCATE_REM, SCM_SUBR_FLAG_TRUNCATE_REM, scm_subr_func_truncate_rem },
    { "modulo", SCM_SUBR_ARITY_FLOOR_REM, SCM_SUBR_FLAG_FLOOR_REM, scm_subr_func_floor_rem },

    /*******************************************************************/
    /*  Booleans                                                       */
    /*******************************************************************/
    { "not", SCM_SUBR_ARITY_NOT, SCM_SUBR_FLAG_NOT, scm_subr_func_not },
    { "boolean?", SCM_SUBR_ARITY_BOOLEAN_P, SCM_SUBR_FLAG_BOOLEAN_P, scm_subr_func_boolean_P },

    /*******************************************************************/
    /*  Pair and Lists                                                 */
    /*******************************************************************/
    { "null?", SCM_SUBR_ARITY_NULL_P, SCM_SUBR_FLAG_NULL_P, scm_subr_func_null_P },
    { "pair?", SCM_SUBR_ARITY_PAIR_P, SCM_SUBR_FLAG_PAIR_P, scm_subr_func_pair_P },
    { "cons", SCM_SUBR_ARITY_CONS, SCM_SUBR_FLAG_CONS, scm_subr_func_cons },
    { "car", SCM_SUBR_ARITY_CAR, SCM_SUBR_FLAG_CAR, scm_subr_func_car },
    { "cdr", SCM_SUBR_ARITY_CDR, SCM_SUBR_FLAG_CDR, scm_subr_func_cdr },
    { "set-car!", SCM_SUBR_ARITY_SET_CAR_I, SCM_SUBR_FLAG_SET_CAR_I, scm_subr_func_set_car_i },
    { "set-cdr!", SCM_SUBR_ARITY_SET_CDR_I, SCM_SUBR_FLAG_SET_CDR_I, scm_subr_func_set_cdr_i },
    { "list?", SCM_SUBR_ARITY_LIST_P, SCM_SUBR_FLAG_LIST_P, scm_subr_func_list_P },
    { "make-list", SCM_SUBR_ARITY_MAKE_LIST, SCM_SUBR_FLAG_MAKE_LIST, scm_subr_func_make_list },
    { "list", SCM_SUBR_ARITY_LIST, SCM_SUBR_FLAG_LIST, scm_subr_func_list },
    { "length", SCM_SUBR_ARITY_LENGTH, SCM_SUBR_FLAG_LENGTH, scm_subr_func_length },
    { "append", SCM_SUBR_ARITY_APPEND, SCM_SUBR_FLAG_APPEND, scm_subr_func_append },
    { "reverse", SCM_SUBR_ARITY_REVERSE, SCM_SUBR_FLAG_REVERSE, scm_subr_func_reverse },
    { "list-tail", SCM_SUBR_ARITY_LIST_TAIL, SCM_SUBR_FLAG_LIST_TAIL, scm_subr_func_list_tail },
    { "list-ref", SCM_SUBR_ARITY_LIST_REF, SCM_SUBR_FLAG_LIST_REF, scm_subr_func_list_ref },
    { "list-set!", SCM_SUBR_ARITY_LIST_SET_I, SCM_SUBR_FLAG_LIST_SET_I, scm_subr_func_list_set_i },
    { "memq", SCM_SUBR_ARITY_MEMQ, SCM_SUBR_FLAG_MEMQ, scm_subr_func_memq },
    { "memv", SCM_SUBR_ARITY_MEMV, SCM_SUBR_FLAG_MEMV, scm_subr_func_memv },
    { "member", SCM_SUBR_ARITY_MEMBER, SCM_SUBR_FLAG_MEMBER, scm_subr_func_member },
    { "assq", SCM_SUBR_ARITY_ASSQ, SCM_SUBR_FLAG_ASSQ, scm_subr_func_assq },
    { "assv", SCM_SUBR_ARITY_ASSV, SCM_SUBR_FLAG_ASSV, scm_subr_func_assv },
    { "assoc", SCM_SUBR_ARITY_ASSOC, SCM_SUBR_FLAG_ASSOC, scm_subr_func_assoc },
    { "list-copy", SCM_SUBR_ARITY_LIST_COPY, SCM_SUBR_FLAG_LIST_COPY, scm_subr_func_list_copy },

    /*******************************************************************/
    /*  Symbols                                                        */
    /*******************************************************************/
    { "symbol?", SCM_SUBR_ARITY_SYMBOL_P, SCM_SUBR_FLAG_SYMBOL_P, scm_subr_func_symbol_P },
    { "symbol=?", SCM_SUBR_ARITY_SYMBOL_EQ_P, SCM_SUBR_FLAG_SYMBOL_EQ_P, scm_subr_func_symbol_eq_P },
    { "symbol->string", SCM_SUBR_ARITY_SYMBOL_TO_STRING, SCM_SUBR_FLAG_SYMBOL_TO_STRING, scm_subr_func_symbol_to_string },
    { "string->symbol", SCM_SUBR_ARITY_STRING_TO_SYMBOL, SCM_SUBR_FLAG_STRING_TO_SYMBOL, scm_subr_func_string_to_symbol },

    /*******************************************************************/
    /*  Characters                                                     */
    /*******************************************************************/
    { "char?", SCM_SUBR_ARITY_CHAR_P, SCM_SUBR_FLAG_CHAR_P, scm_subr_func_char_P },
    { "char=?", SCM_SUBR_ARITY_CHAR_EQ_P, SCM_SUBR_FLAG_CHAR_EQ_P, scm_subr_func_char_eq_P },
    { "char<?", SCM_SUBR_ARITY_CHAR_LT_P, SCM_SUBR_FLAG_CHAR_LT_P, scm_subr_func_char_lt_P },
    { "char>?", SCM_SUBR_ARITY_CHAR_GT_P, SCM_SUBR_FLAG_CHAR_GT_P, scm_subr_func_char_gt_P },
    { "char<=?", SCM_SUBR_ARITY_CHAR_LE_P, SCM_SUBR_FLAG_CHAR_LE_P, scm_subr_func_char_le_P },
    { "char>=?", SCM_SUBR_ARITY_CHAR_GE_P, SCM_SUBR_FLAG_CHAR_GE_P, scm_subr_func_char_ge_P },
    { "char->integer", SCM_SUBR_ARITY_CHAR_TO_INTEGER, SCM_SUBR_FLAG_CHAR_TO_INTEGER, scm_subr_func_char_to_integer },
    { "integer->char", SCM_SUBR_ARITY_INTEGER_TO_CHAR, SCM_SUBR_FLAG_INTEGER_TO_CHAR, scm_subr_func_integer_to_char },

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    { "string?", SCM_SUBR_ARITY_STRING_P, SCM_SUBR_FLAG_STRING_P, scm_subr_func_string_P },
    { "make-string", SCM_SUBR_ARITY_MAKE_STRING, SCM_SUBR_FLAG_MAKE_STRING, scm_subr_func_make_string },
    { "string", SCM_SUBR_ARITY_STRING, SCM_SUBR_FLAG_STRING, scm_subr_func_string },
    { "string-length", SCM_SUBR_ARITY_STRING_LENGTH, SCM_SUBR_FLAG_STRING_LENGTH, scm_subr_func_string_length },
    { "string-bytesize", SCM_SUBR_ARITY_STRING_BYTESIZE, SCM_SUBR_FLAG_STRING_BYTESIZE, scm_subr_func_string_bytesize },
    { "string-ref", SCM_SUBR_ARITY_STRING_REF, SCM_SUBR_FLAG_STRING_REF, scm_subr_func_string_ref },
    { "string-set!", SCM_SUBR_ARITY_STRING_SET_I, SCM_SUBR_FLAG_STRING_SET_I, scm_subr_func_string_set_i },
    { "string=?", SCM_SUBR_ARITY_STRING_EQ_P, SCM_SUBR_FLAG_STRING_EQ_P, scm_subr_func_string_eq_P },
    { "string<?", SCM_SUBR_ARITY_STRING_LT_P, SCM_SUBR_FLAG_STRING_LT_P, scm_subr_func_string_lt_P },
    { "string>?", SCM_SUBR_ARITY_STRING_GT_P, SCM_SUBR_FLAG_STRING_GT_P, scm_subr_func_string_gt_P },
    { "string<=?", SCM_SUBR_ARITY_STRING_LE_P, SCM_SUBR_FLAG_STRING_LE_P, scm_subr_func_string_le_P },
    { "string>=?", SCM_SUBR_ARITY_STRING_GE_P, SCM_SUBR_FLAG_STRING_GE_P, scm_subr_func_string_ge_P },
    { "substring", SCM_SUBR_ARITY_SUBSTRING, SCM_SUBR_FLAG_SUBSTRING, scm_subr_func_substring },
    { "string-append", SCM_SUBR_ARITY_STRING_APPEND, SCM_SUBR_FLAG_STRING_APPEND, scm_subr_func_string_append },
    { "string->list", SCM_SUBR_ARITY_STRING_TO_LIST, SCM_SUBR_FLAG_STRING_TO_LIST, scm_subr_func_string_to_list },
    { "list->string", SCM_SUBR_ARITY_LIST_TO_STRING, SCM_SUBR_FLAG_LIST_TO_STRING, scm_subr_func_list_to_string },
    { "string-copy", SCM_SUBR_ARITY_STRING_COPY, SCM_SUBR_FLAG_STRING_COPY, scm_subr_func_string_copy },
    { "string-copy!", SCM_SUBR_ARITY_STRING_COPY_I, SCM_SUBR_FLAG_STRING_COPY_I, scm_subr_func_string_copy_i },
    { "string-fill!", SCM_SUBR_ARITY_STRING_FILL_I, SCM_SUBR_FLAG_STRING_FILL_I, scm_subr_func_string_fill_i },

    /*******************************************************************/
    /*  Vectors                                                        */
    /*******************************************************************/
    { "vector?", SCM_SUBR_ARITY_VECTOR_P, SCM_SUBR_FLAG_VECTOR_P, scm_subr_func_vector_P },
    { "make-vector", SCM_SUBR_ARITY_MAKE_VECTOR, SCM_SUBR_FLAG_MAKE_VECTOR, scm_subr_func_make_vector },
    { "vector", SCM_SUBR_ARITY_VECTOR, SCM_SUBR_FLAG_VECTOR, scm_subr_func_vector },
    { "vector-length", SCM_SUBR_ARITY_VECTOR_LENGTH, SCM_SUBR_FLAG_VECTOR_LENGTH, scm_subr_func_vector_length },
    { "vector-ref", SCM_SUBR_ARITY_VECTOR_REF, SCM_SUBR_FLAG_VECTOR_REF, scm_subr_func_vector_ref },
    { "vector-set!", SCM_SUBR_ARITY_VECTOR_SET_I, SCM_SUBR_FLAG_VECTOR_SET_I, scm_subr_func_vector_set_i },
    { "vector->list", SCM_SUBR_ARITY_VECTOR_TO_LIST, SCM_SUBR_FLAG_VECTOR_TO_LIST, scm_subr_func_vector_to_list },
    { "list->vector", SCM_SUBR_ARITY_LIST_TO_VECTOR, SCM_SUBR_FLAG_LIST_TO_VECTOR, scm_subr_func_list_to_vector },
    { "vector->string", SCM_SUBR_ARITY_VECTOR_TO_STRING, SCM_SUBR_FLAG_VECTOR_TO_STRING, scm_subr_func_vector_to_string },
    { "string->vector", SCM_SUBR_ARITY_STRING_TO_VECTOR, SCM_SUBR_FLAG_STRING_TO_VECTOR, scm_subr_func_string_to_vector },
    { "vector-copy", SCM_SUBR_ARITY_VECTOR_COPY, SCM_SUBR_FLAG_VECTOR_COPY, scm_subr_func_vector_copy },
    { "vector-copy!", SCM_SUBR_ARITY_VECTOR_COPY_I, SCM_SUBR_FLAG_VECTOR_COPY_I, scm_subr_func_vector_copy_i },
    { "vector-append", SCM_SUBR_ARITY_VECTOR_APPEND, SCM_SUBR_FLAG_VECTOR_APPEND, scm_subr_func_vector_append },
    { "vector-fill!", SCM_SUBR_ARITY_VECTOR_FILL_I, SCM_SUBR_FLAG_VECTOR_FILL_I, scm_subr_func_vector_fill_i },

    /*******************************************************************/
    /*  Exceptions                                                     */
    /*******************************************************************/
    { "with-exception-handler", SCM_SUBR_ARITY_WITH_EXCEPTION_HANDLER, SCM_SUBR_FLAG_WITH_EXCEPTION_HANDLER, scm_subr_func_with_exception_handler },
    { "raise", SCM_SUBR_ARITY_RAISE, SCM_SUBR_FLAG_RAISE, scm_subr_func_raise },
    { "raise-continuable", SCM_SUBR_ARITY_RAISE_CONTINUABLE, SCM_SUBR_FLAG_RAISE_CONTINUABLE, scm_subr_func_raise_continuable },
    { "error", SCM_SUBR_ARITY_ERROR, SCM_SUBR_FLAG_ERROR, scm_subr_func_error },
    { "error-object?", SCM_SUBR_ARITY_ERROR_OBJECT_P, SCM_SUBR_FLAG_ERROR_OBJECT_P, scm_subr_func_error_object_P },
    { "error-object-message", SCM_SUBR_ARITY_ERROR_OBJECT_MESSAGE, SCM_SUBR_FLAG_ERROR_OBJECT_MESSAGE, scm_subr_func_error_object_message },
    { "error-object-irritants", SCM_SUBR_ARITY_ERROR_OBJECT_IRRITANTS, SCM_SUBR_FLAG_ERROR_OBJECT_IRRITANTS, scm_subr_func_error_object_irritants },
    { "read-error?", SCM_SUBR_ARITY_READ_ERROR_P, SCM_SUBR_FLAG_READ_ERROR_P, scm_subr_func_read_error_P },
    { "file-error?", SCM_SUBR_ARITY_FILE_ERROR_P, SCM_SUBR_FLAG_FILE_ERROR_P, scm_subr_func_file_error_P },

    /*******************************************************************/
    /*  Input Output                                                   */
    /*******************************************************************/
    { "read", SCM_SUBR_ARITY_READ, SCM_SUBR_FLAG_READ, scm_subr_func_read },
    { "write", SCM_SUBR_ARITY_WRITE, SCM_SUBR_FLAG_WRITE, scm_subr_func_write },
    { "display", SCM_SUBR_ARITY_DISPLAY, SCM_SUBR_FLAG_DISPLAY, scm_subr_func_display },
    { "newline", SCM_SUBR_ARITY_NEWLINE, SCM_SUBR_FLAG_NEWLINE, scm_subr_func_newline },
    { "flush-output-port", SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT, SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT, scm_subr_func_flush_output_port },

    { "call/cc", SCM_SUBR_ARITY_CALLCC, SCM_SUBR_FLAG_CALLCC, scm_subr_func_callcc },
    { "values", SCM_SUBR_ARITY_VALUES, SCM_SUBR_FLAG_VALUES, scm_subr_func_values },
    { "eval-asm", SCM_SUBR_ARITY_EVAL_ASM, SCM_SUBR_FLAG_EVAL_ASM, scm_subr_func_eval_asm },
    { "eval", SCM_SUBR_ARITY_EVAL, SCM_SUBR_FLAG_EVAL, scm_subr_func_eval },
    { "exit", SCM_SUBR_ARITY_EXIT, SCM_SUBR_FLAG_EXIT, scm_subr_func_exit },
  };

  ScmObj sym  = SCM_OBJ_INIT, subr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &subr);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_define_scheme_base_clsr(ScmObj module)
{
  const char *syms[] = { "call-with-values" };
  int arities[] = { 2 };
  const char *codes[] = { scm_clsr_code_call_with_values };
  ScmObj sym  = SCM_OBJ_INIT, code = SCM_OBJ_INIT, clsr = SCM_OBJ_INIT;
  ScmObj port = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &code, &clsr,
                       &port);

  for (size_t i = 0; i < sizeof(syms)/sizeof(syms[0]); i++) {
    port = scm_capi_open_input_string_cstr(codes[i], SCM_ENC_ASCII);
    if (scm_obj_null_p(port)) return -1;

    code = scm_api_read(port);
    if (scm_obj_null_p(code)) return -1;

    code = scm_api_assemble(code, SCM_OBJ_NULL);
    if (scm_obj_null_p(code)) return -1;

    sym = scm_capi_make_symbol_from_cstr(syms[i], SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    clsr = scm_capi_make_closure(code, SCM_OBJ_NULL, arities[i]);
    if (scm_obj_null_p(code)) return -1;

    rslt = scm_capi_define_global_var(module, sym, clsr, true);
    if (rslt < 0) return -1;
  }

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

  SCM_STACK_FRAME_PUSH(&module,
                       &sym, &port, &prm);

  for (size_t i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
    sym = scm_capi_make_symbol_from_cstr(data[i].name, SCM_ENC_ASCII);
    if (scm_obj_null_p(sym)) return -1;

    prm = scm_capi_make_parameter(SCM_OBJ_NULL);
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
scm_load_module_scheme_base(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod, &imp);

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "base");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  /*
   * load (scheme base syntax) module and import it
   */

  rslt = scm_load_module_scheme_base_syntax();
  if (rslt < 0) return -1;

  name = scm_make_module_name(3, SCM_ENC_ASCII, "scheme", "base", "syntax");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;

  /*
   * define global variables
   */

  rslt = scm_define_scheme_base_subr(mod);
  if (rslt < 0) return -1;

  rslt = scm_define_scheme_base_clsr(mod);
  if (rslt < 0) return -1;

  rslt = scm_define_scheme_base_current_port(mod);
  if (rslt < 0) return -1;

  return 0;
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
    { "char-ci=?", SCM_SUBR_ARITY_CHAR_CI_EQ_P, SCM_SUBR_FLAG_CHAR_CI_EQ_P, scm_subr_func_char_ci_eq_P },
    { "char-ci<?", SCM_SUBR_ARITY_CHAR_CI_LT_P, SCM_SUBR_FLAG_CHAR_CI_LT_P, scm_subr_func_char_ci_lt_P },
    { "char-ci>?", SCM_SUBR_ARITY_CHAR_CI_GT_P, SCM_SUBR_FLAG_CHAR_CI_GT_P, scm_subr_func_char_ci_gt_P },
    { "char-ci<=?", SCM_SUBR_ARITY_CHAR_CI_LE_P, SCM_SUBR_FLAG_CHAR_CI_LE_P, scm_subr_func_char_ci_le_P },
    { "char-ci>=?", SCM_SUBR_ARITY_CHAR_CI_GE_P, SCM_SUBR_FLAG_CHAR_CI_GE_P, scm_subr_func_char_ci_ge_P },
    { "char-alphabetic?", SCM_SUBR_ARITY_CHAR_ALPHABETIC_P, SCM_SUBR_FLAG_CHAR_ALPHABETIC_P, scm_subr_func_char_alphabetic_P },
    { "char-numeric?", SCM_SUBR_ARITY_CHAR_NUMERIC_P, SCM_SUBR_FLAG_CHAR_NUMERIC_P, scm_subr_func_char_numeric_P },
    { "char-whitespace?", SCM_SUBR_ARITY_CHAR_WHITESPACE_P, SCM_SUBR_FLAG_CHAR_WHITESPACE_P, scm_subr_func_char_whitespace_P },
    { "char-upper-case?", SCM_SUBR_ARITY_CHAR_UPPER_CASE_P, SCM_SUBR_FLAG_CHAR_UPPER_CASE_P, scm_subr_func_char_upper_case_P },
    { "char-lower-case?", SCM_SUBR_ARITY_CHAR_LOWER_CASE_P, SCM_SUBR_FLAG_CHAR_LOWER_CASE_P, scm_subr_func_char_lower_case_P },
    { "digit_value", SCM_SUBR_ARITY_DIGIT_VALUE, SCM_SUBR_FLAG_DIGIT_VALUE, scm_subr_func_digit_value },
    { "char-upcase", SCM_SUBR_ARITY_CHAR_UPCASE, SCM_SUBR_FLAG_CHAR_UPCASE, scm_subr_func_char_upcase },
    { "char-downcase", SCM_SUBR_ARITY_CHAR_DOWNCASE, SCM_SUBR_FLAG_CHAR_DOWNCASE, scm_subr_func_char_downcase },
    { "char-foldcase", SCM_SUBR_ARITY_CHAR_FOLDCASE, SCM_SUBR_FLAG_CHAR_FOLDCASE, scm_subr_func_char_foldcase },

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    { "string-ci=?", SCM_SUBR_ARITY_STRING_CI_EQ_P, SCM_SUBR_FLAG_STRING_CI_EQ_P, scm_subr_func_string_ci_eq_P },
    { "string-ci<?", SCM_SUBR_ARITY_STRING_CI_LT_P, SCM_SUBR_FLAG_STRING_CI_LT_P, scm_subr_func_string_ci_lt_P },
    { "string-ci>?", SCM_SUBR_ARITY_STRING_CI_GT_P, SCM_SUBR_FLAG_STRING_CI_GT_P, scm_subr_func_string_ci_gt_P },
    { "string-ci<=?", SCM_SUBR_ARITY_STRING_CI_LE_P, SCM_SUBR_FLAG_STRING_CI_LE_P, scm_subr_func_string_ci_le_P },
    { "string-ci>=?", SCM_SUBR_ARITY_STRING_CI_GE_P, SCM_SUBR_FLAG_STRING_CI_GE_P, scm_subr_func_string_ci_ge_P },
  };

  int rslt;

  SCM_STACK_FRAME_PUSH(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_char(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod);

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "char");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  rslt = scm_define_scheme_char_subr(mod);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************************/
/*  (scythe format)                                                  */
/*******************************************************************/

static int
scm_define_scythe_format_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  format                                                         */
    /*******************************************************************/
    { "format", SCM_SUBR_ARITY_FORMAT, SCM_SUBR_FLAG_FORMAT, scm_subr_func_format },
  };

  int rslt;

  SCM_STACK_FRAME_PUSH(&module);

  rslt = scm_define_subr(module, data, sizeof(data)/sizeof(data[0]));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_format(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod);

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scythe", "format");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  rslt = scm_define_scythe_format_subr(mod);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************************/
/*  (main)                                                         */
/*******************************************************************/

static int
scm_load_module_main(void)
{
  ScmObj name = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, imp = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&name, &mod, &imp);

  name = scm_make_module_name(1, SCM_ENC_ASCII, "main");
  if (scm_obj_null_p(name)) return -1;

  mod = scm_api_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  /*
   * load (scheme base) module and import it
   */

  rslt = scm_load_module_scheme_base();
  if (rslt < 0) return -1;

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "base");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;


  /*
   * load (scheme char) module and import it
   */

  rslt = scm_load_module_scheme_char();
  if (rslt < 0) return -1;

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scheme", "char");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;


  /*
   * load (scythe format) module and import it
   */

  rslt = scm_load_module_scythe_format();
  if (rslt < 0) return -1;

  name = scm_make_module_name(2, SCM_ENC_ASCII, "scythe", "format");
  if (scm_obj_null_p(name)) return -1;

  rslt = scm_capi_find_module(name, SCM_CSETTER_L(imp));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(imp)) {
    scm_capi_error("faild to import module", 1, name);
    return -1;
  }

  rslt = scm_capi_import(mod, imp);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_load_core_modules(void)
{
  return scm_load_module_main();
}
