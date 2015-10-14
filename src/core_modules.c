#include <unistd.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/core_subr.h"
#include "scythe/impl_utils.h"
#include "scythe/bedrock.h"
#include "scythe/vm.h"
#include "scythe/refstk.h"
#include "scythe/assembler.h"
#include "scythe/file.h"
#include "scythe/number.h"
#include "scythe/exception.h"
#include "scythe/iseq.h"
#include "scythe/marshal.h"
#include "scythe/module.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/procedure.h"
#include "scythe/string.h"
#include "scythe/symbol.h"

struct subr_data {
  const char *name;
  int arity;
  unsigned int flag;
  ScmSubrFunc func;
  bool export;
};

struct const_num_data {
  const char *name;
  scm_sword_t val;
  bool export;
};

struct alias_data {
  const char *alias;
  const char *src;
  bool export;
};

struct import_data {
  const char *name[10];
  size_t n;
  int (*load_func)(void);
  bool restrictive;
};

#define STRARY(...) (const char *[]){__VA_ARGS__}
#define SUBR_DATA_TERMINATE { NULL, 0, 0, NULL, false }
#define CONST_NUM_DATA_TERMINATE { NULL, 0, false }
#define ALIAS_SAME_VAR(var, exp) { var, var, exp }
#define ALIAS_DATA_TERMINATE { NULL, NULL, false }
#define IMPORT_DATA_TERMINATE { {}, 0, NULL, false }

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
    str[i] = scm_make_symbol_from_cstr(name_str[i], SCM_ENC_SRC);
    if (scm_obj_null_p(str[i])) return SCM_OBJ_NULL;
  }

  name = SCM_NIL_OBJ;

  for (size_t i = n; i > 0; i--) {
    name = scm_cons(str[i - 1], name);
    if (scm_obj_null_p(name)) return SCM_OBJ_NULL;
  }

  return name;
}

static int
scm_define_var(ScmObj module, const char *var, ScmObj val, bool export)
{
  ScmObj sym = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module, &val);

  sym = scm_make_symbol_from_cstr(var, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return -1;

  r = scm_define_global_var(module, sym, val, export);
  if (r < 0) return -1;

  return 0;
}

static int
scm_define_subr(ScmObj module, const struct subr_data *data)
{
  ScmObj subr = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module,
                      &subr);

  for (const struct subr_data *p = data; p->name != NULL; p++) {
    subr = scm_make_subrutine(p->func, p->arity, p->flag,
                              module);
    if (scm_obj_null_p(subr)) return -1;

    r = scm_define_var(module, p->name, subr, p->export);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_define_const_num(ScmObj module, const struct const_num_data *data)
{
  ScmObj num = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module,
                      &num);

  for (const struct const_num_data *p = data; p->name != NULL; p++) {
    num = scm_make_number_from_sword(p->val);
    if (scm_obj_null_p(num)) return -1;

    r = scm_define_var(module, p->name, num, p->export);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_define_alias(ScmObj module, const struct alias_data *data)
{
  ScmObj sym = SCM_OBJ_INIT, ali = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module,
                      &sym, &ali);

  for (const struct alias_data *p = data; p->alias != NULL; p++) {
    ali = scm_make_symbol_from_cstr(p->alias, SCM_ENC_SRC);
    if (scm_obj_null_p(ali)) return -1;

    sym = scm_make_symbol_from_cstr(p->src, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;

    r = scm_define_global_alias(module, ali, sym, p->export);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_exec_compiled_data(ScmObj mod, const unsigned char *data)
{
  ScmObj unmarshal = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&mod,
                      &unmarshal, &iseq);

  unmarshal = scm_make_unmarshal(data);
  if (scm_obj_null_p(unmarshal)) return -1;

  iseq = scm_unmarshal_ref(unmarshal, 0);
  if (scm_obj_null_p(iseq)) return -1;
  scm_assert(scm_iseq_p(iseq));

  r = scm_exec_iseq(iseq);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_modules_and_import_them(ScmObj module, const struct import_data *data)
{
  ScmObj name = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&module,
                      &name);

  for (const struct import_data *p = data; p->load_func != NULL; p++) {
    r = p->load_func();
    if (r < 0) return -1;

    name = scm_make_module_name(p->name, p->n);
    if (scm_obj_null_p(name)) return -1;

    r = scm_module_import(module, name, p->restrictive);
    if (r < 0) return -1;
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

  rslt = scm_find_module(name, SCM_CSETTER_L(mod));
  if (rslt < 0) return -1;

  if (scm_obj_not_null_p(mod))
    return 0;

  mod = scm_make_module(name);
  if (scm_obj_null_p(mod)) return -1;

  return load_func(mod);
}


static int scm_load_module_scythe_internal_core_public(void);
static int scm_load_module_scythe_internal_core_private(void);
static int scm_load_module_scythe_internal_identifier(void);
static int scm_load_module_scythe_internal_cmplenv(void);
static int scm_load_module_scythe_internal_dynamicenv(void);
static int scm_load_module_scythe_internal_compile(void);
static int scm_load_module_scythe_internal_macro(void);
static int scm_load_module_scythe_internal_record(void);
static int scm_load_module_scythe_internal_multipleval(void);
static int scm_load_module_scythe_base(void);
static int scm_load_module_scythe_repl(void);
static int scm_load_module_scythe_internal_command(void);
static int scm_load_module_main(void);
static int scm_load_module_scheme_base(void);
static int scm_load_module_scheme_char(void);
static int scm_load_module_scheme_eval(void);
static int scm_load_module_scheme_file(void);
static int scm_load_module_scheme_load(void);
static int scm_load_module_scheme_processcontext(void);
static int scm_load_module_scheme_read(void);
static int scm_load_module_scheme_write(void);


/*******************************************************************/
/*  (scythe internal core public)                                  */
/*******************************************************************/

static int
scm_define_scythe_internal_core_public_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Dynamic bindings                                               */
    /*******************************************************************/
    { "make-parameter", SCM_SUBR_ARITY_MAKE_PARAMETER, SCM_SUBR_FLAG_MAKE_PARAMETER, scm_subr_func_make_parameter, true },

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
   { "call/cc", SCM_SUBR_ARITY_CALLCC, SCM_SUBR_FLAG_CALLCC, scm_subr_func_callcc, true },
    { "values", SCM_SUBR_ARITY_VALUES, SCM_SUBR_FLAG_VALUES, scm_subr_func_values, true },
    { "call-with-values", SCM_SUBR_ARITY_CALL_WITH_VALUES, SCM_SUBR_FLAG_CALL_WITH_VALUES, scm_subr_func_call_with_values, true },
    { "eval-asm", SCM_SUBR_ARITY_EVAL_ASM, SCM_SUBR_FLAG_EVAL_ASM, scm_subr_func_eval_asm, true },

    /*******************************************************************/
    /*  Exceptions                                                     */
    /*******************************************************************/
    { "raise", SCM_SUBR_ARITY_RAISE, SCM_SUBR_FLAG_RAISE, scm_subr_func_raise, true },
    { "raise-continuable", SCM_SUBR_ARITY_RAISE_CONTINUABLE, SCM_SUBR_FLAG_RAISE_CONTINUABLE, scm_subr_func_raise_continuable, true },
    { "error", SCM_SUBR_ARITY_ERROR, SCM_SUBR_FLAG_ERROR, scm_subr_func_error, true },
    { "error-object?", SCM_SUBR_ARITY_ERROR_OBJECT_P, SCM_SUBR_FLAG_ERROR_OBJECT_P, scm_subr_func_error_object_P, true },
    { "error-object-message", SCM_SUBR_ARITY_ERROR_OBJECT_MESSAGE, SCM_SUBR_FLAG_ERROR_OBJECT_MESSAGE, scm_subr_func_error_object_message, true },
    { "error-object-irritants", SCM_SUBR_ARITY_ERROR_OBJECT_IRRITANTS, SCM_SUBR_FLAG_ERROR_OBJECT_IRRITANTS, scm_subr_func_error_object_irritants, true },
    { "read-error?", SCM_SUBR_ARITY_READ_ERROR_P, SCM_SUBR_FLAG_READ_ERROR_P, scm_subr_func_read_error_P, true },
    { "file-error?", SCM_SUBR_ARITY_FILE_ERROR_P, SCM_SUBR_FLAG_FILE_ERROR_P, scm_subr_func_file_error_P, true },

    /*******************************************************************/
    /*  Input Output                                                   */
    /*******************************************************************/
    { "newline", SCM_SUBR_ARITY_NEWLINE, SCM_SUBR_FLAG_NEWLINE, scm_subr_func_newline, true },
    { "flush-output-port", SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT, SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT, scm_subr_func_flush_output_port, true },
    { "eof-object?", SCM_SUBR_ARITY_EOF_OBJECT_P, SCM_SUBR_FLAG_EOF_OBJECT_P, scm_subr_func_eof_object_P, true },

    /*******************************************************************/
    /*  Char (characters)                                              */
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
    /*  Char (strings)                                                 */
    /*******************************************************************/
    { "string-ci=?", SCM_SUBR_ARITY_STRING_CI_EQ_P, SCM_SUBR_FLAG_STRING_CI_EQ_P, scm_subr_func_string_ci_eq_P, true },
    { "string-ci<?", SCM_SUBR_ARITY_STRING_CI_LT_P, SCM_SUBR_FLAG_STRING_CI_LT_P, scm_subr_func_string_ci_lt_P, true },
    { "string-ci>?", SCM_SUBR_ARITY_STRING_CI_GT_P, SCM_SUBR_FLAG_STRING_CI_GT_P, scm_subr_func_string_ci_gt_P, true },
    { "string-ci<=?", SCM_SUBR_ARITY_STRING_CI_LE_P, SCM_SUBR_FLAG_STRING_CI_LE_P, scm_subr_func_string_ci_le_P, true },
    { "string-ci>=?", SCM_SUBR_ARITY_STRING_CI_GE_P, SCM_SUBR_FLAG_STRING_CI_GE_P, scm_subr_func_string_ci_ge_P, true },

    /*******************************************************************/
    /*  Eval                                                           */
    /*******************************************************************/
    { "eval", SCM_SUBR_ARITY_EVAL, SCM_SUBR_FLAG_EVAL, scm_subr_func_eval, true },

    /*******************************************************************/
    /*  File                                                           */
    /*******************************************************************/
    { "delete-file", SCM_SUBR_ARITY_DELETE_FILE, SCM_SUBR_FLAG_DELETE_FILE, scm_subr_func_delete_file, true },
    { "file-exists?", SCM_SUBR_ARITY_FILE_EXISTS_P, SCM_SUBR_FLAG_FILE_EXISTS_P, scm_subr_func_file_exists_P, true },
    { "open-input-file", SCM_SUBR_ARITY_OPEN_INPUT_FILE, SCM_SUBR_FLAG_OPEN_INPUT_FILE, scm_subr_func_open_input_file, true },

    /*******************************************************************/
    /*  Load                                                           */
    /*******************************************************************/
    { "load", SCM_SUBR_ARITY_LOAD, SCM_SUBR_FLAG_LOAD, scm_subr_func_load, true },

    /*******************************************************************/
    /*  Process Context                                                */
    /*******************************************************************/
    { "exit", SCM_SUBR_ARITY_EXIT, SCM_SUBR_FLAG_EXIT, scm_subr_func_exit, true },

    /*******************************************************************/
    /*  Read                                                           */
    /*******************************************************************/
    { "read", SCM_SUBR_ARITY_READ, SCM_SUBR_FLAG_READ, scm_subr_func_read, true },

    /*******************************************************************/
    /*  Write                                                           */
    /*******************************************************************/
    { "display", SCM_SUBR_ARITY_DISPLAY, SCM_SUBR_FLAG_DISPLAY, scm_subr_func_display, true },
    { "write", SCM_SUBR_ARITY_WRITE, SCM_SUBR_FLAG_WRITE, scm_subr_func_write, true },
    { "write-shared", SCM_SUBR_ARITY_WRITE_SHARED, SCM_SUBR_FLAG_WRITE_SHARED, scm_subr_func_write_shared, true },
    { "write-simple", SCM_SUBR_ARITY_WRITE_SIMPLE, SCM_SUBR_FLAG_WRITE_SIMPLE, scm_subr_func_write_simple, true },

    /*******************************************************************/
    /*  Format                                                         */
    /*******************************************************************/
    { "format", SCM_SUBR_ARITY_FORMAT, SCM_SUBR_FLAG_FORMAT, scm_subr_func_format, true },

    /*******************************************************************/
    /*  Modules                                                        */
    /*******************************************************************/
    { "module?", SCM_SUBR_ARITY_MODULE_P, SCM_SUBR_FLAG_MODULE_P, scm_subr_func_module_P, true },
    { "module-name", SCM_SUBR_ARITY_MODULE_NAME, SCM_SUBR_FLAG_MODULE_NAME, scm_subr_func_module_name, true },
    { "module-export", SCM_SUBR_ARITY_MODULE_EXPORT, SCM_SUBR_FLAG_MODULE_EXPORT, scm_subr_func_module_export, true },

    SUBR_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_subr(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_define_scythe_internal_core_public_current_port(ScmObj module)
{
  struct {
    const char *name;
    int fd;
    ScmObj (*func)(int fd, const char *enc);
  } const data[] = {
    { "current-input-port", 0, scm_open_input_fd },
    { "current-output-port", 1, scm_open_output_fd },
    { "current-error-port", 2, scm_open_output_fd },
  };

  ScmObj sym = SCM_OBJ_INIT, port = SCM_OBJ_INIT, prm = SCM_OBJ_INIT;
  int rslt, fd;

  SCM_REFSTK_INIT_REG(&module,
                      &sym, &port, &prm);

  for (size_t i = 0; i < sizeof(data)/sizeof(data[0]); i++) {
    sym = scm_make_symbol_from_cstr(data[i].name, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;

    SCM_SYSCALL(fd, dup(data[i].fd));
    if (fd < 0) {
      scm_error("system call error: dup", 0);
      return -1;
    }

    port = data[i].func(fd, NULL);
    if (scm_obj_null_p(port)) {
      close(fd);
      return -1;
    }

    prm = scm_make_parameter(port, SCM_OBJ_NULL);
    if (scm_obj_null_p(prm)) return -1;

    rslt = scm_define_global_var(module, sym, prm, true);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_define_scythe_internal_core_public_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    { "call-with-current-continuation", "call/cc", true },
    ALIAS_DATA_TERMINATE
  };
  int r;

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static ScmObj
eary_to_scheme_list(const EArray *ary)
{
  ScmObj lst = SCM_OBJ_INIT, s = SCM_OBJ_INIT;
  size_t idx;
  char **ptr;

  SCM_REFSTK_INIT_REG(&lst, &s);

  lst = SCM_NIL_OBJ;
  EARY_FOR_EACH(ary, idx, ptr) {
    s = scm_make_string_from_external(*ptr, strlen(*ptr), NULL);
    if (scm_obj_null_p(s)) return SCM_OBJ_NULL;

    lst = scm_cons(s, lst);
    if (scm_obj_null_p(lst)) return SCM_OBJ_NULL;
  }

  return lst;
}

static int
scm_define_load_path_and_suffixes(ScmObj mod)
{
  ScmObj val = SCM_OBJ_INIT;
  int r;

  val = eary_to_scheme_list(scm_initial_load_path());
  if (scm_obj_null_p(val)) return -1;

  r = scm_define_var(mod, SCM_LOAD_PATH_VARIABLE_NAME, val, true);
  if (r < 0) return -1;

  val = eary_to_scheme_list(scm_initial_load_suffixes());
  if (scm_obj_null_p(val)) return -1;

  r = scm_define_var(mod, SCM_LOAD_SUFFIXES_VARIABLE_NAME, val, true);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_core_public(ScmObj mod)
{
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_define_scythe_internal_core_public_subr(mod);
  if (r < 0) return -1;

  r = scm_define_scythe_internal_core_public_current_port(mod);
  if (r < 0) return -1;

  r = scm_define_scythe_internal_core_public_alias(mod);
  if (r < 0) return -1;

  r = scm_define_load_path_and_suffixes(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_core_public(void)
{
  return scm_load_module((const char *[]){"scythe", "internal", "core", "public"},
                         4,
                         scm_load_module_func_scythe_internal_core_public);
}


/*******************************************************************/
/*  (scythe internal core private)                                 */
/*******************************************************************/

static int
scm_define_scythe_internal_core_private_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    /*******************************************************************/
    /*  Exception Handler                                              */
    /*******************************************************************/
    { "push-exception-handler", SCM_SUBR_ARITY_PUSH_EXCEPTION_HANDLER, SCM_SUBR_FLAG_PUSH_EXCEPTION_HANDLER, scm_subr_func_push_exception_handler, true },
    { "pop-exception-handler", SCM_SUBR_ARITY_POP_EXCEPTION_HANDLER, SCM_SUBR_FLAG_POP_EXCEPTION_HANDLER, scm_subr_func_pop_exception_handler, true },

    /*******************************************************************/
    /*  Dynamic bindings                                               */
    /*******************************************************************/
    { "push-dynamic-bindings", SCM_SUBR_ARITY_PUSH_DYNAMIC_BINDINGS, SCM_SUBR_FLAG_PUSH_DYNAMIC_BINDINGS, scm_subr_func_push_dynamic_bindings, true },
    { "pop-dynamic-bindings", SCM_SUBR_ARITY_POP_DYNAMIC_BINDINGS, SCM_SUBR_FLAG_POP_DYNAMIC_BINDINGS, scm_subr_func_pop_dynamic_bindings, true },

    /*******************************************************************/
    /*  Dynamic Wind                                                   */
    /*******************************************************************/
    { "push-dynamic-wind-handler", SCM_SUBR_ARITY_PUSH_DYNAMIC_WIND_HANDLER, SCM_SUBR_FLAG_PUSH_DYNAMIC_WIND_HANDLER, scm_subr_func_push_dynamic_wind_handler, true },
    { "pop-dynamic-wind-handler", SCM_SUBR_ARITY_POP_DYNAMIC_WIND_HANDLER, SCM_SUBR_FLAG_POP_DYNAMIC_WIND_HANDLER, scm_subr_func_pop_dynamic_wind_handler, true },

    /*******************************************************************/
    /*  Global Variables                                               */
    /*******************************************************************/
    { "global-variable-bind", SCM_SUBR_ARITY_GLOBAL_VARIABLE_BIND, SCM_SUBR_FLAG_GLOBAL_VARIABLE_BIND, scm_subr_func_global_variable_bind, true },
    { "global-syntax-bind", SCM_SUBR_ARITY_GLOBAL_SYNTAX_BIND, SCM_SUBR_FLAG_GLOBAL_SYNTAX_BIND, scm_subr_func_global_syntax_bind, true },
    { "global-syntax-ref", SCM_SUBR_ARITY_GLOBAL_SYNTAX_REF, SCM_SUBR_FLAG_GLOBAL_SYNTAX_REF, scm_subr_func_global_syntax_ref, true },

    /*******************************************************************/
    /*  Assembler                                                      */
    /*******************************************************************/
    { "make-assembler", SCM_SUBR_ARITY_MAKE_ASSEMBLER, SCM_SUBR_FLAG_MAKE_ASSEMBLER, scm_subr_func_make_assembler, true },
    { "assembler-assign-label-id!", SCM_SUBR_ARITY_ASSEMBLER_ASSIGN_LABEL_ID_I, SCM_SUBR_FLAG_ASSEMBLER_ASSIGN_LABEL_ID_I, scm_subr_func_assembler_assign_label_id_i, true },
    { "assembler-push!", SCM_SUBR_ARITY_ASSEMBLER_PUSH_I, SCM_SUBR_FLAG_ASSEMBLER_PUSH_I, scm_subr_func_assembler_push_i, true },
    { "assembler-commit!", SCM_SUBR_ARITY_ASSEMBLER_COMMIT_I, SCM_SUBR_FLAG_ASSEMBLER_COMMIT_I, scm_subr_func_assembler_commit_i, true },

    /*******************************************************************/
    /*  Compiler                                                       */
    /*******************************************************************/
    { "compiler?", SCM_SUBR_ARITY_COMPILER_P, SCM_SUBR_FLAG_COMPILER_P, scm_subr_func_compiler_P, true },
    { "make-compiler", SCM_SUBR_ARITY_MAKE_COMPILER, SCM_SUBR_FLAG_MAKE_COMPILER, scm_subr_func_make_compiler, true },
    { "compiler-base-env", SCM_SUBR_ARITY_COMPILER_BASE_ENV, SCM_SUBR_FLAG_COMPILER_BASE_ENV, scm_subr_func_compiler_base_env, true },
    { "compiler-select-base-env!", SCM_SUBR_ARITY_COMPILER_SELECT_BASE_ENV_I, SCM_SUBR_FLAG_COMPILER_SELECT_BASE_ENV_I, scm_subr_func_compiler_select_base_env_i, true },
    { "compiler-select-module!", SCM_SUBR_ARITY_COMPILER_SELECT_MODULE_I, SCM_SUBR_FLAG_COMPILER_SELECT_MODULE_I, scm_subr_func_compiler_select_module_i, true },
    { "compiler-current-expr", SCM_SUBR_ARITY_COMPILER_CURRENT_EXPR, SCM_SUBR_FLAG_COMPILER_CURRENT_EXPR, scm_subr_func_compiler_current_expr, true },
    { "compiler-select-expr!", SCM_SUBR_ARITY_COMPILER_SELECT_EXPR_I, SCM_SUBR_FLAG_COMPILER_SELECT_EXPR_I, scm_subr_func_compiler_select_expr_i, true },

    /*******************************************************************/
    /*  Syntax                                                         */
    /*******************************************************************/
    { "syntax?", SCM_SUBR_ARITY_SYNTAX_P, SCM_SUBR_FLAG_SYNTAX_P, scm_subr_func_syntax_P, true },
    { "make-syntax", SCM_SUBR_ARITY_MAKE_SYNTAX, SCM_SUBR_FLAG_MAKE_SYNTAX, scm_subr_func_make_syntax, true },
    { "syntax-keyword", SCM_SUBR_ARITY_SYNTAX_KEYWORD, SCM_SUBR_FLAG_SYNTAX_KEYWORD, scm_subr_func_syntax_keyword, true },
    { "syntax-handler", SCM_SUBR_ARITY_SYNTAX_HANDLER, SCM_SUBR_FLAG_SYNTAX_HANDLER, scm_subr_func_syntax_handler, true },

    /*******************************************************************/
    /*  Macro                                                          */
    /*******************************************************************/
    { "macro?", SCM_SUBR_ARITY_MACRO_P, SCM_SUBR_FLAG_MACRO_P, scm_subr_func_macro_P, true },
    { "make-macro", SCM_SUBR_ARITY_MAKE_MACRO, SCM_SUBR_FLAG_MAKE_MACRO, scm_subr_func_make_macro, true },
    { "macro-env", SCM_SUBR_ARITY_MACRO_ENV, SCM_SUBR_FLAG_MACRO_ENV, scm_subr_func_macro_env, true },
    { "macro-yield-transformer", SCM_SUBR_ARITY_MACRO_YIELD_TRANSFORMER, SCM_SUBR_FLAG_MACRO_YIELD_TRANSFORMER, scm_subr_func_macro_yield_transformer, true },

    /*******************************************************************/
    /*  Quasiquatation                                                 */
    /*******************************************************************/
    { "compile-qq-template", SCM_SUBR_ARITY_COMPILE_QQ_TEMPLATE, SCM_SUBR_FLAG_COMPILE_QQ_TEMPLATE, scm_subr_func_compile_qq_template, true },
    { "substitute-qq-template", SCM_SUBR_ARITY_SUBSTITUTE_QQ_TEMPLATE, SCM_SUBR_FLAG_SUBSTITUTE_QQ_TEMPLATE, scm_subr_func_substitute_qq_template, true },
    { "qq-template-num-of-unquoted", SCM_SUBR_ARITY_QQ_TEMPLATE_NUM_OF_UNQUOTED, SCM_SUBR_FLAG_QQ_TEMPLATE_NUM_OF_UNQUOTED, scm_subr_func_qq_template_num_of_unquoted, true },
    { "qq-template-unquoted", SCM_SUBR_ARITY_QQ_TEMPLATE_UNQUOTED, SCM_SUBR_FLAG_QQ_TEMPLATE_UNQUOTED, scm_subr_func_qq_template_unquoted, true },

    /*******************************************************************/
    /*  Record                                                         */
    /*******************************************************************/
    { "record?", SCM_SUBR_ARITY_RECORD_P, SCM_SUBR_FLAG_RECORD_P, scm_subr_func_record_P, true },
    { "make-record-type", SCM_SUBR_ARITY_MAKE_RECORD_TYPE, SCM_SUBR_FLAG_MAKE_RECORD_TYPE, scm_subr_func_make_record_type, true },
    { "make-record", SCM_SUBR_ARITY_MAKE_RECORD, SCM_SUBR_FLAG_MAKE_RECORD, scm_subr_func_make_record, true },
    { "record-type", SCM_SUBR_ARITY_RECORD_TYPE, SCM_SUBR_FLAG_RECORD_TYPE_OF_P, scm_subr_func_record_type, true },
    { "record-ref", SCM_SUBR_ARITY_RECORD_REF, SCM_SUBR_FLAG_RECORD_REF, scm_subr_func_record_ref, true },
    { "record-set!", SCM_SUBR_ARITY_RECORD_SET_I, SCM_SUBR_FLAG_RECORD_SET_I, scm_subr_func_record_set_i, true },

    SUBR_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_subr(module, data);
  if (r < 0) return -1;

  return 0;
}

#define const_num_asm_inst_record(x, y) \
  { "+asm-inst-" #x "+", SCM_OPCODE_##y, true }
#define const_num_asm_pinst_record(x, y) \
  { "+asm-inst-" #x "+", SCM_ASM_PI_##y, true }

static int
scm_define_scythe_internal_core_private_constant_number(ScmObj module)
{
  static const struct const_num_data data[] = {
    const_num_asm_inst_record(nop, NOP),
    const_num_asm_inst_record(halt, HALT),
    const_num_asm_inst_record(int, INT),
    const_num_asm_inst_record(cframe, CFRAME),
    const_num_asm_inst_record(eframe, EFRAME),
    const_num_asm_inst_record(epop, EPOP),
    const_num_asm_inst_record(eshift, ESHIFT),
    const_num_asm_inst_record(immval, IMMVAL),
    const_num_asm_inst_record(push, PUSH),
    const_num_asm_inst_record(mvpush, MVPUSH),
    const_num_asm_inst_record(return, RETURN),
    const_num_asm_inst_record(pcall, PCALL),
    const_num_asm_inst_record(call, CALL),
    const_num_asm_inst_record(tcall, TAIL_CALL),
    const_num_asm_inst_record(gref, GREF),
    const_num_asm_inst_record(gdef, GDEF),
    const_num_asm_inst_record(gset, GSET),
    const_num_asm_inst_record(sref, SREF),
    const_num_asm_inst_record(sset, SSET),
    const_num_asm_inst_record(jmp, JMP),
    const_num_asm_inst_record(jmpt, JMPT),
    const_num_asm_inst_record(jmpf, JMPF),
    const_num_asm_inst_record(box, BOX),
    const_num_asm_inst_record(close, CLOSE),
    const_num_asm_inst_record(demine, DEMINE),
    const_num_asm_inst_record(emine, EMINE),
    const_num_asm_inst_record(edemine, EDEMINE),
    const_num_asm_inst_record(mrvc, MRVC),
    const_num_asm_inst_record(mrve, MRVE),
    const_num_asm_inst_record(module, MODULE),

    const_num_asm_pinst_record(label, LABEL),

    CONST_NUM_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_const_num(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_core_private(ScmObj mod)
{
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_define_scythe_internal_core_private_subr(mod);
  if (r < 0) return -1;

  r = scm_define_scythe_internal_core_private_constant_number(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_core_private(void)
{
  return scm_load_module((const char *[]){"scythe", "internal", "core", "private"},
                         4,
                         scm_load_module_func_scythe_internal_core_private);
}


/*******************************************************************/
/*  (scythe internal identifier)                                   */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_identifier[];

static int
scm_load_module_func_scythe_internal_identifier(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_identifier);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_identifier(void)
{
  return scm_load_module(STRARY("scythe", "internal", "identifier"), 3,
                         scm_load_module_func_scythe_internal_identifier);
}


/*******************************************************************/
/*  (scythe internal cmpl-env)                                     */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_cmplenv[];

static int
scm_load_module_func_scythe_internal_cmplenv(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    { { "scythe", "internal", "identifier" }, 3,
      scm_load_module_scythe_internal_identifier, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_cmplenv);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_cmplenv(void)
{
  return scm_load_module(STRARY("scythe", "internal", "cmpl-env"), 3,
                         scm_load_module_func_scythe_internal_cmplenv);
}


/*******************************************************************/
/*  (scythe internal dynamic-env)                                  */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_dynamicenv[];

static int
scm_load_module_func_scythe_internal_dynamicenv(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_dynamicenv);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_dynamicenv(void)
{
  return scm_load_module(STRARY("scythe", "internal", "dynamic-env"), 3,
                         scm_load_module_func_scythe_internal_dynamicenv);
}


/*******************************************************************/
/*  (scythe internal compile)                                      */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_compile[];

static int
scm_load_module_func_scythe_internal_compile(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    { { "scythe", "internal", "identifier" }, 3,
      scm_load_module_scythe_internal_identifier, true },
    { { "scythe", "internal", "cmpl-env" }, 3,
      scm_load_module_scythe_internal_cmplenv, true },
    { { "scythe", "internal", "dynamic-env" }, 3,
      scm_load_module_scythe_internal_dynamicenv, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_compile);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_compile(void)
{
  return scm_load_module(STRARY("scythe", "internal", "compile"), 3,
                         scm_load_module_func_scythe_internal_compile);
}


/*******************************************************************/
/*  (scythe internal macro)                                        */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_macro[];

static int
scm_load_module_func_scythe_internal_macro(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    { { "scythe", "internal", "identifier" }, 3,
      scm_load_module_scythe_internal_identifier, true },
    { { "scythe", "internal", "cmpl-env" }, 3,
      scm_load_module_scythe_internal_cmplenv, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_macro);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_macro(void)
{
  return scm_load_module(STRARY("scythe", "internal", "macro"), 3,
                         scm_load_module_func_scythe_internal_macro);

}


/*******************************************************************/
/*  (scythe internal record)                                       */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_record[];

static int
scm_load_module_func_scythe_internal_record(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    { { "scythe", "internal", "macro" }, 3,
      scm_load_module_scythe_internal_macro, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod, scm_compiled_data_scythe_internal_record);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_record(void)
{
  return scm_load_module(STRARY("scythe", "internal", "record"), 3,
                         scm_load_module_func_scythe_internal_record);
}


/*******************************************************************/
/*  (scythe internal multiple-val)                                 */
/*******************************************************************/

extern const unsigned char scm_compiled_data_scythe_internal_multipleval[];

static int
scm_load_module_func_scythe_internal_multipleval(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "core", "private" }, 4,
      scm_load_module_scythe_internal_core_private, true },
    { { "scythe", "internal", "macro" }, 3,
      scm_load_module_scythe_internal_macro, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_exec_compiled_data(mod,
                             scm_compiled_data_scythe_internal_multipleval);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_multipleval(void)
{
  return scm_load_module(STRARY("scythe", "internal", "multiple-val"), 3,
                         scm_load_module_func_scythe_internal_multipleval);
}


/*******************************************************************/
/*  (scythe base)                                                  */
/*******************************************************************/

static int
scm_load_module_func_scythe_base(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, false },
    { { "scythe", "internal", "dynamic-env" }, 3,
      scm_load_module_scythe_internal_dynamicenv, false },
    { { "scythe", "internal", "macro" }, 3,
      scm_load_module_scythe_internal_macro, false },
    { { "scythe", "internal", "record" }, 3,
      scm_load_module_scythe_internal_record, false },
    { { "scythe", "internal", "multiple-val" }, 3,
      scm_load_module_scythe_internal_multipleval, false },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_base(void)
{
  return scm_load_module(STRARY("scythe", "base"), 2,
                         scm_load_module_func_scythe_base);
}


/*******************************************************************/
/*  (scythe repl)                                                  */
/*******************************************************************/

static int
scm_load_module_func_scythe_repl(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "base" }, 2, scm_load_module_scythe_base, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_repl(void)
{
  return scm_load_module(STRARY("scythe", "repl"), 2,
                         scm_load_module_func_scythe_repl);
}


/*******************************************************************/
/*  (scythe internal command)                                      */
/*******************************************************************/

static int
scm_define_scythe_internal_command_subr(ScmObj module)
{
  static const struct subr_data data[] = {
    { "repl", SCM_SUBR_ARITY_REPL, SCM_SUBR_FLAG_REPL, scm_subr_func_repl, false },
    { "exec-file", SCM_SUBR_ARITY_EXEC_FILE, SCM_SUBR_FLAG_EXEC_FILE, scm_subr_func_exec_file, false },
    { "eval-string", SCM_SUBR_ARITY_EVAL_STRING, SCM_SUBR_FLAG_EVAL_STRING, scm_subr_func_eval_string, false },
    { "compile-file", SCM_SUBR_ARITY_COMPILE_FILE, SCM_SUBR_FLAG_COMPILE_FILE, scm_subr_func_compile_file, false },

    SUBR_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_subr(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scythe_internal_command(ScmObj mod)
{
  int r;

  /*
   * define global variables
   */

  r = scm_define_scythe_internal_command_subr(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scythe_internal_command(void)
{
  return scm_load_module(STRARY("scythe", "internal", "command"), 3,
                         scm_load_module_func_scythe_internal_command);
}


/*******************************************************************/
/*  (main)                                                         */
/*******************************************************************/

static int
scm_load_module_func_main(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "base" }, 2, scm_load_module_scythe_base, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_main(void)
{
  return scm_load_module(STRARY("main"), 1,
                         scm_load_module_func_main);
}


/*******************************************************************/
/*  (scheme base)                                                  */
/*******************************************************************/

static int
scm_define_scheme_base_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    /*******************************************************************/
    /*  Dynamic bindings                                               */
    /*******************************************************************/
    ALIAS_SAME_VAR("make-parameter", true),

    /*******************************************************************/
    /*  Equivalence predicates                                         */
    /*******************************************************************/
    ALIAS_SAME_VAR("eq?", true),
    ALIAS_SAME_VAR("eqv?", true),
    ALIAS_SAME_VAR("equal?", true),

    /*******************************************************************/
    /*  Numbers                                                        */
    /*******************************************************************/
    ALIAS_SAME_VAR("number?", true),
    ALIAS_SAME_VAR("complex?", true),
    ALIAS_SAME_VAR("real?", true),
    ALIAS_SAME_VAR("rational?", true),
    ALIAS_SAME_VAR("integer?", true),
    ALIAS_SAME_VAR("exact?", true),
    ALIAS_SAME_VAR("inexact?", true),
    ALIAS_SAME_VAR("exact-integer?", true),
    ALIAS_SAME_VAR("finite?", true),
    ALIAS_SAME_VAR("infinite?", true),
    ALIAS_SAME_VAR("nan?", true),
    ALIAS_SAME_VAR("=", true),
    ALIAS_SAME_VAR("<", true),
    ALIAS_SAME_VAR(">", true),
    ALIAS_SAME_VAR("<=", true),
    ALIAS_SAME_VAR(">=", true),
    ALIAS_SAME_VAR("zero?", true),
    ALIAS_SAME_VAR("positive?", true),
    ALIAS_SAME_VAR("negative?", true),
    ALIAS_SAME_VAR("odd?", true),
    ALIAS_SAME_VAR("even?", true),
    ALIAS_SAME_VAR("max", true),
    ALIAS_SAME_VAR("min", true),
    ALIAS_SAME_VAR("+", true),
    ALIAS_SAME_VAR("*", true),
    ALIAS_SAME_VAR("-", true),
    ALIAS_SAME_VAR("/", true),
    ALIAS_SAME_VAR("floor/", true),
    ALIAS_SAME_VAR("floor-quotient", true),
    ALIAS_SAME_VAR("floor-remainder", true),
    ALIAS_SAME_VAR("truncate/", true),
    ALIAS_SAME_VAR("truncate-quotient", true),
    ALIAS_SAME_VAR("truncate-remainder", true),
    ALIAS_SAME_VAR("quotient", true),
    ALIAS_SAME_VAR("remainder", true),
    ALIAS_SAME_VAR("modulo", true),

    /*******************************************************************/
    /*  Booleans                                                       */
    /*******************************************************************/
    ALIAS_SAME_VAR("not", true),
    ALIAS_SAME_VAR("boolean?", true),

    /*******************************************************************/
    /*  Pair and Lists                                                 */
    /*******************************************************************/
    ALIAS_SAME_VAR("null?", true),
    ALIAS_SAME_VAR("pair?", true),
    ALIAS_SAME_VAR("cons", true),
    ALIAS_SAME_VAR("car", true),
    ALIAS_SAME_VAR("cdr", true),
    ALIAS_SAME_VAR("set-car!", true),
    ALIAS_SAME_VAR("set-cdr!", true),
    ALIAS_SAME_VAR("list?", true),
    ALIAS_SAME_VAR("make-list", true),
    ALIAS_SAME_VAR("list", true),
    ALIAS_SAME_VAR("length", true),
    ALIAS_SAME_VAR("append", true),
    ALIAS_SAME_VAR("reverse", true),
    ALIAS_SAME_VAR("list-tail", true),
    ALIAS_SAME_VAR("list-ref", true),
    ALIAS_SAME_VAR("list-set!", true),
    ALIAS_SAME_VAR("memq", true),
    ALIAS_SAME_VAR("memv", true),
    ALIAS_SAME_VAR("member", true),
    ALIAS_SAME_VAR("assq", true),
    ALIAS_SAME_VAR("assv", true),
    ALIAS_SAME_VAR("assoc", true),
    ALIAS_SAME_VAR("list-copy", true),

    /*******************************************************************/
    /*  Symbols                                                        */
    /*******************************************************************/
    ALIAS_SAME_VAR("symbol?", true),
    ALIAS_SAME_VAR("symbol=?", true),
    ALIAS_SAME_VAR("symbol->string", true),
    ALIAS_SAME_VAR("string->symbol", true),

    /*******************************************************************/
    /*  Characters                                                     */
    /*******************************************************************/
    ALIAS_SAME_VAR("char?", true),
    ALIAS_SAME_VAR("char=?", true),
    ALIAS_SAME_VAR("char<?", true),
    ALIAS_SAME_VAR("char>?", true),
    ALIAS_SAME_VAR("char<=?", true),
    ALIAS_SAME_VAR("char>=?", true),
    ALIAS_SAME_VAR("char->integer", true),
    ALIAS_SAME_VAR("integer->char", true),

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    ALIAS_SAME_VAR("string?", true),
    ALIAS_SAME_VAR("make-string", true),
    ALIAS_SAME_VAR("string", true),
    ALIAS_SAME_VAR("string-length", true),
    ALIAS_SAME_VAR("string-bytesize", true),
    ALIAS_SAME_VAR("string-ref", true),
    ALIAS_SAME_VAR("string-set!", true),
    ALIAS_SAME_VAR("string=?", true),
    ALIAS_SAME_VAR("string<?", true),
    ALIAS_SAME_VAR("string>?", true),
    ALIAS_SAME_VAR("string<=?", true),
    ALIAS_SAME_VAR("string>=?", true),
    ALIAS_SAME_VAR("substring", true),
    ALIAS_SAME_VAR("string-append", true),
    ALIAS_SAME_VAR("string->list", true),
    ALIAS_SAME_VAR("list->string", true),
    ALIAS_SAME_VAR("string-copy", true),
    ALIAS_SAME_VAR("string-copy!", true),
    ALIAS_SAME_VAR("string-fill!", true),

    /*******************************************************************/
    /*  Vectors                                                        */
    /*******************************************************************/
    ALIAS_SAME_VAR("vector?", true),
    ALIAS_SAME_VAR("make-vector", true),
    ALIAS_SAME_VAR("vector", true),
    ALIAS_SAME_VAR("vector-length", true),
    ALIAS_SAME_VAR("vector-ref", true),
    ALIAS_SAME_VAR("vector-set!", true),
    ALIAS_SAME_VAR("vector->list", true),
    ALIAS_SAME_VAR("list->vector", true),
    ALIAS_SAME_VAR("vector->string", true),
    ALIAS_SAME_VAR("string->vector", true),
    ALIAS_SAME_VAR("vector-copy", true),
    ALIAS_SAME_VAR("vector-copy!", true),
    ALIAS_SAME_VAR("vector-append", true),
    ALIAS_SAME_VAR("vector-fill!", true),

    /*******************************************************************/
    /*  Control features                                               */
    /*******************************************************************/
    ALIAS_SAME_VAR("procedure?", true),
    ALIAS_SAME_VAR("apply", true),
    ALIAS_SAME_VAR("call/cc", true),
    ALIAS_SAME_VAR("call-with-current-continuation", true),
    ALIAS_SAME_VAR("values", true),
    ALIAS_SAME_VAR("call-with-values", true),
    ALIAS_SAME_VAR("dynamic-wind", true),

    /*******************************************************************/
    /*  Exceptions                                                     */
    /*******************************************************************/
    ALIAS_SAME_VAR("with-exception-handler", true),
    ALIAS_SAME_VAR("raise", true),
    ALIAS_SAME_VAR("raise-continuable", true),
    ALIAS_SAME_VAR("error", true),
    ALIAS_SAME_VAR("error-object?", true),
    ALIAS_SAME_VAR("error-object-message", true),
    ALIAS_SAME_VAR("error-object-irritants", true),
    ALIAS_SAME_VAR("read-error?", true),
    ALIAS_SAME_VAR("file-error?", true),

    /*******************************************************************/
    /*  Input Output                                                   */
    /*******************************************************************/
    ALIAS_SAME_VAR("current-input-port", true),
    ALIAS_SAME_VAR("current-output-port", true),
    ALIAS_SAME_VAR("current-error-port", true),
    ALIAS_SAME_VAR("newline", true),
    ALIAS_SAME_VAR("flush-output-port", true),
    ALIAS_SAME_VAR("eof-object?", true),

    /*******************************************************************/
    /*  Syntax                                                         */
    /*******************************************************************/
    ALIAS_SAME_VAR("define", true),
    ALIAS_SAME_VAR("begin", true),
    ALIAS_SAME_VAR("quote", true),
    ALIAS_SAME_VAR("lambda", true),
    ALIAS_SAME_VAR("set!", true),
    ALIAS_SAME_VAR("if", true),
    ALIAS_SAME_VAR("cond", true),
    ALIAS_SAME_VAR("and", true),
    ALIAS_SAME_VAR("or", true),
    ALIAS_SAME_VAR("when", true),
    ALIAS_SAME_VAR("unless", true),
    ALIAS_SAME_VAR("let", true),
    ALIAS_SAME_VAR("let*", true),
    ALIAS_SAME_VAR("letrec", true),
    ALIAS_SAME_VAR("letrec*", true),
    ALIAS_SAME_VAR("do", true),
    ALIAS_SAME_VAR("let-values", true),
    ALIAS_SAME_VAR("let*-values", true),
    ALIAS_SAME_VAR("parameterize", true),
    ALIAS_SAME_VAR("quasiquote", true),
    ALIAS_SAME_VAR("define-syntax", true),
    ALIAS_SAME_VAR("let-syntax", true),
    ALIAS_SAME_VAR("letrec-syntax", true),

    ALIAS_DATA_TERMINATE
  };
  int r;

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_base(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    { { "scythe", "internal", "dynamic-env" }, 3,
      scm_load_module_scythe_internal_dynamicenv, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_base_alias(mod);
  if (r < 0) return -1;

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
scm_define_scheme_char_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    /*******************************************************************/
    /*  Characters                                                     */
    /*******************************************************************/
    ALIAS_SAME_VAR("char-ci=?", true),
    ALIAS_SAME_VAR("char-ci<?", true),
    ALIAS_SAME_VAR("char-ci>?", true),
    ALIAS_SAME_VAR("char-ci<=?", true),
    ALIAS_SAME_VAR("char-ci>=?", true),
    ALIAS_SAME_VAR("char-alphabetic?", true),
    ALIAS_SAME_VAR("char-numeric?", true),
    ALIAS_SAME_VAR("char-whitespace?", true),
    ALIAS_SAME_VAR("char-upper-case?", true),
    ALIAS_SAME_VAR("char-lower-case?", true),
    ALIAS_SAME_VAR("digit_value", true),
    ALIAS_SAME_VAR("char-upcase", true),
    ALIAS_SAME_VAR("char-downcase", true),
    ALIAS_SAME_VAR("char-foldcase", true),

    /*******************************************************************/
    /*  Strings                                                        */
    /*******************************************************************/
    ALIAS_SAME_VAR("string-ci=?", true),
    ALIAS_SAME_VAR("string-ci<?", true),
    ALIAS_SAME_VAR("string-ci>?", true),
    ALIAS_SAME_VAR("string-ci<=?", true),
    ALIAS_SAME_VAR("string-ci>=?", true),

    ALIAS_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_char(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_char_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_char(void)
{
  return scm_load_module(STRARY("scheme", "char"), 2,
                         scm_load_module_func_scheme_char);
}


/*******************************************************************/
/*  (scheme eval)                                                  */
/*******************************************************************/

static int
scm_define_scheme_eval_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("eval", true),
    ALIAS_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_eval(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_eval_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_eval(void)
{
  return scm_load_module(STRARY("scheme", "eval"), 2,
                         scm_load_module_func_scheme_eval);
}


/*******************************************************************/
/*  (scheme file)                                                  */
/*******************************************************************/

static int
scm_define_scheme_file_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("delete-file", true),
    ALIAS_SAME_VAR("file-exists?", true),
    ALIAS_SAME_VAR("open-input-file", true),
    ALIAS_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_file(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_file_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_file(void)
{
  return scm_load_module(STRARY("scheme", "file"), 2,
                         scm_load_module_func_scheme_file);
}


/*******************************************************************/
/*  (scheme load)                                                  */
/*******************************************************************/

static int
scm_define_scheme_load_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("load", true),
    ALIAS_SAME_VAR(SCM_LOAD_PATH_VARIABLE_NAME, true),
    ALIAS_DATA_TERMINATE
  };

  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_load(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_load_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_load(void)
{
  return scm_load_module(STRARY("scheme", "load"), 2,
                         scm_load_module_func_scheme_load);
}


/*******************************************************************/
/*  (scheme process-context)                                       */
/*******************************************************************/

static int
scm_define_scheme_processcontext_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("exit", true),
    ALIAS_DATA_TERMINATE
  };

  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_processcontext(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_processcontext_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_processcontext(void)
{
  return scm_load_module(STRARY("scheme", "process-context"), 2,
                         scm_load_module_func_scheme_processcontext);
}


/*******************************************************************/
/*  (scheme read)                                                  */
/*******************************************************************/

static int
scm_define_scheme_read_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("read", true),
    ALIAS_DATA_TERMINATE
  };

  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_read(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_read_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_read(void)
{
  return scm_load_module(STRARY("scheme", "read"), 2,
                         scm_load_module_func_scheme_read);
}

/*******************************************************************/
/*  (scheme write)                                                  */
/*******************************************************************/

static int
scm_define_scheme_write_alias(ScmObj module)
{
  static const struct alias_data data[] = {
    ALIAS_SAME_VAR("display", true),
    ALIAS_SAME_VAR("write", true),
    ALIAS_SAME_VAR("write-shared", true),
    ALIAS_SAME_VAR("write-simple", true),
    ALIAS_DATA_TERMINATE
  };

  int r;

  SCM_REFSTK_INIT_REG(&module);

  r = scm_define_alias(module, data);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_func_scheme_write(ScmObj mod)
{
  static const struct import_data data[] = {
    { { "scythe", "internal", "core", "public" }, 4,
      scm_load_module_scythe_internal_core_public, true },
    IMPORT_DATA_TERMINATE
  };
  int r;

  SCM_REFSTK_INIT_REG(&mod);

  r = scm_load_modules_and_import_them(mod, data);
  if (r < 0) return -1;

  r = scm_define_scheme_write_alias(mod);
  if (r < 0) return -1;

  return 0;
}

static int
scm_load_module_scheme_write(void)
{
  return scm_load_module(STRARY("scheme", "write"), 2,
                         scm_load_module_func_scheme_write);
}

int
scm_load_core_modules(void)
{
  int (*func[])(void) = {
    scm_load_module_scythe_internal_core_public,
    scm_load_module_scythe_internal_core_private,
    scm_load_module_scythe_internal_identifier,
    scm_load_module_scythe_internal_cmplenv,
    scm_load_module_scythe_internal_dynamicenv,
    scm_load_module_scythe_internal_compile,
    scm_load_module_scythe_internal_macro,
    scm_load_module_scythe_internal_record,
    scm_load_module_scythe_internal_multipleval,
    scm_load_module_scythe_base,
    scm_load_module_scythe_repl,
    scm_load_module_scythe_internal_command,
    scm_load_module_main,
    NULL,

    /*
     *  XXX: 現状、不要なので実行しない(Module を作成しない)。後々は Scheme
     *       ファイルへ切り出し、必要になったタイミングでロードされるように
     *       したい
     */
    scm_load_module_scheme_base,
    scm_load_module_scheme_char,
    scm_load_module_scheme_eval,
    scm_load_module_scheme_file,
    scm_load_module_scheme_load,
    scm_load_module_scheme_processcontext,
    scm_load_module_scheme_read,
    scm_load_module_scheme_write,
    NULL,
  };
  int (**p)(void);

  for (p = func; *p != NULL; p++) {
    int r = ((int (*)(void))*p)();
    if (r < 0) return -1;
  }

  return 0;
}
