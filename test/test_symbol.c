#include <cutter.h>

#include "object.h"
#include "nil.h"
#include "symbol.h"

void
test_scm_symbol_construct(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  cut_assert_not_null(symbol);
}

void
test_scm_symbol_instance(void)
{
  ScmSymbol *symbol1 = scm_symbol_instance("foo");
  ScmSymbol *symbol2 = scm_symbol_instance("foo");

  cut_assert_not_null(symbol1);
  cut_assert_not_null(symbol2);
  cut_assert_equal_pointer(symbol1, symbol2);
}

void
test_scm_symbol_is_symbol(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");
  ScmNil *nil = scm_nil_construct();

  cut_assert_true(scm_symbol_is_symbol(SCM_OBJ(symbol)));
  cut_assert_false(scm_symbol_is_symbol(SCM_OBJ(nil)));
}

void
test_scm_symbol_name(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  cut_assert_equal_string("foo", scm_symbol_name(symbol));
}

void
test_scm_symbol_length(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  cut_assert_equal_int(3, scm_symbol_length(symbol));
}
