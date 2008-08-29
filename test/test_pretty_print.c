#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "integer.h"
#include "string.h"
#include "symbol.h"
#include "pair.h"
#include "printer.h"

void
test_pretty_print_nil(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());

  scm_printer_pretty_print_scm_obj(printer, nil, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("()", scm_printer_buffer(printer));
}

void
test_pretty_print_integer(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj integer = SCM_OBJ(scm_integer_construct(100));

  scm_printer_pretty_print_scm_obj(printer, integer, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("100", scm_printer_buffer(printer));
}

void
test_pretty_print_string(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj string = SCM_OBJ(scm_string_construct("abc\\def"));

  scm_printer_pretty_print_scm_obj(printer, string, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("\"abc\\\\def\"", scm_printer_buffer(printer));
}

void
test_pretty_print_symbol(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj symbol = SCM_OBJ(scm_symbol_instance("abc\\def"));

  scm_printer_pretty_print_scm_obj(printer, symbol, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("abc\\def", scm_printer_buffer(printer));
}

void
test_pretty_print_perfect_list(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_nil_instance());
  ScmObj pair = SCM_OBJ(scm_pair_construct(car, cdr));

  scm_printer_pretty_print_scm_obj(printer, pair, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("(abc)", scm_printer_buffer(printer));
}

void
test_pretty_print_imperfect_list(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair = SCM_OBJ(scm_pair_construct(car, cdr));

  scm_printer_pretty_print_scm_obj(printer, pair, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("(abc . def)", scm_printer_buffer(printer));
}

void
test_pretty_print_nesting_list(void)
{
  ScmPrinter *printer = scm_printer_construct(stdout);
  ScmObj nil = SCM_OBJ(scm_nil_instance());
  ScmObj car = SCM_OBJ(scm_symbol_instance("abc"));
  ScmObj cdr = SCM_OBJ(scm_symbol_instance("def"));
  ScmObj pair1 = SCM_OBJ(scm_pair_construct(car, nil));
  ScmObj pair2 = SCM_OBJ(scm_pair_construct(pair1, cdr));

  scm_printer_pretty_print_scm_obj(printer, pair2, SCM_PRINTER_MODE_CLEAR);
  cut_assert_equal_string("((abc) . def)", scm_printer_buffer(printer));
}
