#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "integer.h"
#include "string.h"
#include "symbol.h"
#include "pair.h"
#include "vector.h"
#include "miscobjects.h"
#include "printer.h"

void
test_pretty_print_nil(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());

  scm_obuffer_pretty_print_scm_obj(obuffer, nil, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("()", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_integer(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj integer = SCM_OBJ(scm_integer_construct(100));

  scm_obuffer_pretty_print_scm_obj(obuffer, integer, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("100", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_string(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj string = SCM_OBJ(scm_string_construct("abc\\def"));

  scm_obuffer_pretty_print_scm_obj(obuffer, string, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("\"abc\\\\def\"", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_symbol(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj symbol = SCM_OBJ(scm_symbol_instance("abc\\def"));

  scm_obuffer_pretty_print_scm_obj(obuffer, symbol, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("abc\\def", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_perfect_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_nil_instance());
  ScmObj pair = SCM_OBJ(scm_pair_construct(car, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("(abc)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_imperfect_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair = SCM_OBJ(scm_pair_construct(car, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("(abc . def)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_nesting_list(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair1 = SCM_OBJ(scm_pair_construct(car, nil));
  ScmObj pair2 = SCM_OBJ(scm_pair_construct(pair1, cdr));

  scm_obuffer_pretty_print_scm_obj(obuffer, pair2, SCM_OBUFFER_MODE_CLEAR);
  cut_assert_equal_string("((abc) . def)", scm_obuffer_buffer(obuffer));
}

void
test_pretty_print_vector(void)
{
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmVector *vector = scm_vector_construct(5);
  ScmObj e0 = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj e1 = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj e2 = SCM_OBJ(scm_integer_construct(123));
  ScmObj e3 = SCM_OBJ(scm_string_construct("ghi"));
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
  ScmOBuffer *obuffer = scm_obuffer_construct(stdout);
  ScmEOF *eof = scm_eof_instance();

  scm_obuffer_pretty_print_scm_obj(obuffer,
                                   SCM_OBJ(eof), SCM_OBUFFER_MODE_CLEAR);
  
  cut_assert_equal_string("#<eof>", scm_obuffer_buffer(obuffer));
}
