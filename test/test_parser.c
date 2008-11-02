#include <cutter.h>

#include "parser.h"
#include "port.h"

void
test_parser_construct(void)
{
  ScmPort *port = scm_port_construct_input_string_port("", 0);
  ScmIBuffer *ibuffer = scm_ibuffer_construct(port);
  ScmLexer *lexer = scm_lexer_construct(ibuffer);
  ScmParser *parser = scm_parser_construct(lexer);

  cut_assert_not_null(parser);
}
