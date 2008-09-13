#include <cutter.h>

#include "parser.h"

void
test_parser_construct(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct_from_string("");
  ScmLexer *lexer = scm_lexer_construct(ibuffer);
  ScmParser *parser = scm_parser_construct(lexer);

  cut_assert_not_null(parser);
}
