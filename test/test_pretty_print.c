#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "integer.h"
#include "string.h"
#include "symbol.h"
#include "pair.h"
#include "vector.h"
#include "char.h"
#include "port.h"
#include "bool.h"
#include "miscobjects.h"
#include "obuffer.h"

void
test_pretty_print_nil(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());

  scm_obuffer_pretty_print_scm_obj(obuffer, nil, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("()", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_integer(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj integer = SCM_OBJ(scm_integer_new(100));

  scm_obuffer_pretty_print_scm_obj(obuffer, integer, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("100", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_string(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj string = SCM_OBJ(scm_string_new("abc\\def",
                                               sizeof("abc\\def") - 1,
                                               SCM_ENCODING_ASCII));

  scm_obuffer_pretty_print_scm_obj(obuffer, string, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("\"abc\\\\def\"", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_symbol(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj symbol = SCM_OBJ(scm_symbol_instance("abc\\def"));

  scm_obuffer_pretty_print_scm_obj(obuffer, symbol, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("abc\\def", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_perfect_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_nil_instance());
  ScmObj pair = SCM_OBJ(scm_pair_new(car, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("(abc)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_imperfect_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair = SCM_OBJ(scm_pair_new(car, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("(abc . def)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_nesting_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair1 = SCM_OBJ(scm_pair_new(car, nil));
  ScmObj pair2 = SCM_OBJ(scm_pair_new(pair1, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair2, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("((abc) . def)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_vector(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmVector *vector = scm_vector_new(5);
  ScmObj e0 = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj e1 = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj e2 = SCM_OBJ(scm_integer_new(123));
  ScmObj e3 = SCM_OBJ(scm_string_new("ghi", 3, SCM_ENCODING_ASCII));
  ScmObj e4 = SCM_OBJ(scm_nil_instance());

  scm_vector_set(vector, 0, e0);
  scm_vector_set(vector, 1, e1);
  scm_vector_set(vector, 2, e2);
  scm_vector_set(vector, 3, e3);
  scm_vector_set(vector, 4, e4);

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(vector), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#(abc def 123 \"ghi\" ())",
                          scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_eof(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmEOF *eof = scm_eof_instance();

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(eof), SCM_OBUFFER_MODE_CLEAR);
  
  cut_assert_equal_string("#<eof>", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_char_printable(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  scm_char_t c;
  ScmChar *chr;

  SCM_CHR_SET_ASCII(c, 'a');
  chr = scm_char_new(c, SCM_ENCODING_ASCII);
  
  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(chr), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#\\a", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_char_newline(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmChar *chr = scm_char_new_newline(SCM_ENCODING_ASCII);
  
  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(chr), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#\\newline", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_char_space(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmChar *chr = scm_char_new_space(SCM_ENCODING_ASCII);
  
  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(chr), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#\\space", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_char_control(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  scm_char_t c;
  ScmChar *chr2d;

  SCM_CHR_SET_ASCII(c, 0x0b);
  chr2d = scm_char_new(c, SCM_ENCODING_ASCII);

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(chr2d), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#\\0x0b", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_input_string_port(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmPort *port = scm_port_open_input_string("", 1);

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(port), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#<port: readable string>",
                          scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_output_string_port(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmPort *port = scm_port_open_output_string();

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(port), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#<port: writable string>",
                          scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_bool_true(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmBool *bool_true = scm_bool_new(true);

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(bool_true), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#t", scm_obuffer_buffer(obuffer));  
}

void
test_pretty_print_bool_false(void)
{
  ScmOBuffer *obuffer = scm_obuffer_new(stdout);
  ScmBool *bool_false = scm_bool_new(false);

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(bool_false), SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("#f", scm_obuffer_buffer(obuffer));  
}
