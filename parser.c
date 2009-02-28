#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "memory.h"
#include "basiclist.h"
#include "ibuffer.h"
#include "parser.h"
#include "object.h"
#include "bool.h"
#include "char.h"
#include "integer.h"
#include "miscobjects.h"
#include "nil.h"
#include "pair.h"
#include "string.h"
#include "symbol.h"
#include "vector.h"

extern int isblank(int c);

struct ScmParserRec {
  ScmLexer* lexer;
};

struct ScmLexerRec {
  ScmIBuffer *ibuffer;
  char *buffer;
  size_t buf_size;
  size_t buf_used;
  SCM_TOKEN_TYPE_T token_type;
  ScmBasicList *tokens;
  SCM_LEXER_ERR_TYPE_T error_type;
  int error_line;
  int error_column;
};


#define SCM_LEXER_INITIAL_BUFFER_SIZE 256

#define IDENTIFIER_START_CHARS "abcdefghijklmnopqrstuvwxyz!$%&*/:<=>?~_^"
#define IDENTIFIER_FOLLOW_ON_CHARS \
  IDENTIFIER_START_CHARS "0123456789.+-"
#define NUMERIC_START_CHAR "+-0123456789"
#define NUMERIC_INDEX_MARKER_CHARS "esfdl"

#define BINARY_DIGIT_CHARSET      "01#"
#define OCTAL_DIGIT_CHARSET       "01234567#"
#define DECIMAL_DIGIT_CHARSET     "0123456789#"
#define HEXADECIMAL_DIGIT_CHARSET "123456789abcdef#"

#define IS_LINE_FEED(c) ((c) == '\n')

#define IS_IDENTIFIER_START_CHAR(c) \
  (strchr(IDENTIFIER_START_CHARS, tolower(c)) != NULL)

#define IS_IDENTIFIER_FOLLOW_ON_CHAR(c) \
  (strchr(IDENTIFIER_FOLLOW_ON_CHARS, tolower(c)) != NULL)

#define IS_NUMERIC_START_CHAR(c) \
  (strchr(NUMERIC_START_CHAR, tolower(c)) != NULL)

typedef enum {
  LEXER_STATE_DONE,
  LEXER_STATE_INIT,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
  LEXER_STATE_DOT,
  LEXER_STATE_NUMERIC,
  LEXER_STATE_COMMENT,
  LEXER_STATE_NUMBER_SIGN,
  LEXER_STATE_STRING,
  LEXER_STATE_CHAR,
  LEXER_STATE_ERROR
} LEXER_STATE_T;



static void
scm_lexer_expand_buffer_if_needed(ScmLexer *lexer, size_t needed_size)
{
  size_t new_size;
  char *new_buffer = NULL;

  assert(lexer != NULL);

  if (lexer->buffer == NULL)
    lexer->buf_size = SCM_LEXER_INITIAL_BUFFER_SIZE;

  for (new_size = lexer->buf_size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > lexer->buf_size || lexer->buffer == NULL) {
    new_buffer = (char *)(scm_memory_allocate(new_size));
    if (lexer->buffer != NULL) {
      memcpy(new_buffer, lexer->buffer, lexer->buf_used + 1);
      scm_memory_release(lexer->buffer);
    }
    else {
      new_buffer[0] = '\0';
    }
    lexer->buffer = new_buffer;
    lexer->buf_size = new_size;
  }
}

static void
scm_lexer_push_char(ScmLexer *lexer, char c)
{
  assert(lexer != NULL);

  scm_lexer_expand_buffer_if_needed(lexer, lexer->buf_used + 2);
                                   /* 2 = pushd char + '\0' */
  lexer->buffer[lexer->buf_used++] = c;
  lexer->buffer[lexer->buf_used] = '\0';
}

static void
scm_lexer_set_token_type(ScmLexer *lexer, SCM_TOKEN_TYPE_T type)
{
  assert(lexer != NULL);
  lexer->token_type = type;
}

static SCM_TOKEN_TYPE_T
scm_lexer_token_type(ScmLexer *lexer)
{
  assert(lexer != NULL);
  return lexer->token_type;
}

static char *
scm_lexer_buffer(ScmLexer *lexer)
{
  assert(lexer != NULL);

  scm_lexer_expand_buffer_if_needed(lexer, SCM_LEXER_INITIAL_BUFFER_SIZE);
  return lexer->buffer;
}

static void
scm_lexer_setup_error_state(ScmLexer *lexer, SCM_LEXER_ERR_TYPE_T error)
{
  assert(lexer != NULL);

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_TOKENIZE_ERR);
  lexer->error_type = error;
  lexer->error_line = scm_ibuffer_current_line_num(lexer->ibuffer);
  lexer->error_column = scm_ibuffer_current_column_num(lexer->ibuffer);
}

static int
scm_lexer_tokenize_init(ScmLexer *lexer)
{
  const char *one_char_token_chars = "()'`";
  const SCM_TOKEN_TYPE_T one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  int current, state;
  char *p;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (current == EOF) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    state = LEXER_STATE_DONE;
  }
  else if (isspace(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    state = LEXER_STATE_DISREGARD;
  }
  else if ((p = strchr(one_char_token_chars, current))) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    scm_lexer_set_token_type(lexer,
			     one_char_token_types[p - one_char_token_chars]);
    state = LEXER_STATE_DONE;
  }
  else if (IS_IDENTIFIER_START_CHAR(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    state = LEXER_STATE_IDENTIFIER;
  }
  else if (IS_NUMERIC_START_CHAR(current)) {
    state = LEXER_STATE_NUMERIC;
  }
  else {
    switch (current) {
    case '.':
      scm_ibuffer_shift_char(lexer->ibuffer);
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_DOT;
      break;
    case ';':
      scm_ibuffer_shift_char(lexer->ibuffer);
      state = LEXER_STATE_COMMENT;
      break;
    case ',':
      scm_ibuffer_shift_char(lexer->ibuffer);
      scm_lexer_push_char(lexer, current);
      if (scm_ibuffer_head_char(lexer->ibuffer) == '@') {
	scm_ibuffer_shift_char(lexer->ibuffer);
	scm_lexer_push_char(lexer, '@');
	scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE_SPLICING);
      }
      else {
	scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
      }
      state = LEXER_STATE_DONE;
      break;
    case '#':
      scm_ibuffer_shift_char(lexer->ibuffer);
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_NUMBER_SIGN;
      break;
    case '"':
      scm_ibuffer_shift_char(lexer->ibuffer);
      /* scm_lexer_push_char(lexer, current); */
      state = LEXER_STATE_STRING;
      break;
    default:
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      state = LEXER_STATE_ERROR;
      break;
    }
  }

  return state;
}

static int
scm_lexer_tokenize_identifier(ScmLexer *lexer)
{
  int current;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (IS_IDENTIFIER_FOLLOW_ON_CHAR(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    return LEXER_STATE_IDENTIFIER;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
    return LEXER_STATE_DONE;
  }
}

static int
scm_lexer_tokenize_dot(ScmLexer *lexer)
{
  int current;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (IS_IDENTIFIER_FOLLOW_ON_CHAR(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    return LEXER_STATE_IDENTIFIER;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }
}

static int
scm_lexer_tokenize_comment(ScmLexer *lexer)
{
  int current;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);
  scm_ibuffer_shift_char(lexer->ibuffer);

  if (IS_LINE_FEED(current)) {
    return LEXER_STATE_DISREGARD;
  }
  else if (current == EOF) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    return LEXER_STATE_DONE;
  }

  return LEXER_STATE_COMMENT;
}

static int
scm_lexer_tokenize_number_sign(ScmLexer *lexer)
{
  int current, state;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (current == EOF) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  switch (tolower(current)) {
  case 't':
  case 'f':
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BOOL);
    state = LEXER_STATE_DONE;
    break;
  case '\\':
    state = LEXER_STATE_CHAR;
    break;
  case '(':
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_VECTOR_START);
    state = LEXER_STATE_DONE;
    break;
  default:
    state = LEXER_STATE_NUMERIC;
    break;
  }

  if (state != LEXER_STATE_NUMERIC) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
  }

  return state;
}

static int
scm_lexer_tokenize_string_escaped(ScmLexer *lexer)
{
  const char *control_chars_escape = "abtnvfr";
  const char *control_chars = "\a\b\t\n\v\f\r";
  int current;
  char *p;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);
  scm_ibuffer_shift_char(lexer->ibuffer);

  if ((p = strchr(control_chars_escape, current)))
    scm_lexer_push_char(lexer, control_chars[p - control_chars_escape]);
  else
    scm_lexer_push_char(lexer, current);

  return LEXER_STATE_STRING;
}

static int
scm_lexer_tokenize_string(ScmLexer *lexer)
{
  int current;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (current == EOF) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  scm_ibuffer_shift_char(lexer->ibuffer);

  if (current == '"') {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
    return LEXER_STATE_DONE;
  }
  else if (current == '\\')
    return scm_lexer_tokenize_string_escaped(lexer);

  scm_lexer_push_char(lexer, current);

  return LEXER_STATE_STRING;
}

static int
scm_lexer_tokenize_char(ScmLexer *lexer)
{
  const char *terminations = "()#'\"";
  int current;
  
  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);
  scm_ibuffer_shift_char(lexer->ibuffer);
  scm_lexer_push_char(lexer, current);

  current = scm_ibuffer_head_char(lexer->ibuffer);
  while (strchr(terminations, current) == NULL &&
         !isspace(current) && current != EOF ) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    current = scm_ibuffer_head_char(lexer->ibuffer);
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);

  return LEXER_STATE_DONE;
}


/* enum { BINARY = 0, OCTET = 1, DECIMAL = 2, HEX = 3 }; */
/* #define CARDINAL2NUMBER_CHAR_SET(rslt, cardinal) \ */
/*   do {						 \ */
/*     switch (cardinal) {				 \ */
/*     case 'b':  (rslt) = BINARY;  break;		 \ */
/*     case 'o':  (rslt) = OCTET;   break;		 \ */
/*     case 'd':  (rslt) = DECIMAL; break;		 \ */
/*     case 'x':  (rslt) = HEX;     break;		 \ */
/*     case '\0': (rslt) = DECIMAL; break;		 \ */
/*     default: break;				 \ */
/*     }						 \ */
/*   } while (0)  */

/* #define IS_VALID_NUMBER_CHAR_SET(specified, actual)  ((actual) <= (specified)) */

/* static int */
/* scm_lexer_tokenize_numeric_unsigned_integer(ScmLexer *lexer, */
/*                                             const char **charset) */
/* { */
/*   int current; */
/*   int nr_shift; */

/*   assert(lexer != NULL); */
/*   assert(charset != NULL && *charset != NULL); */

/*   nr_shift = 0; */
/*   current = scm_ibuffer_head_char(lexer->ibuffer); */
/*   while (strchr(*charset, current) != NULL) { */
/*     if (strchr(NUMERIC_INDEX_MARKER_CHARS, current) != NULL && */
/*         strchr("+-", scm_ibuffer_forecast(lexer->ibuffer, 1)) != NULL) */
/*       return nr_shift; */

/*     if (current == '#') *charset = "#"; */
/*     scm_lexer_push_char(lexer, current); */
/*     scm_ibuffer_shift_char(lexer->ibuffer); */
/*     nr_shift++; */
/*   } */

/*   return nr_shift; */
/* } */

/* static int */
/* scm_lexer_tokenize_numeric_unsigned_real(ScmLexer *lexer, */
/*                                          const char* charset) */
/* { */
/*   unsigned char current; */
/*   int nr_shift; */

/*   assert(lexer != NULL); */
/*   assert(charset != NULL); */

/*   /\* integer part  *\/ */

/*   nr_shift = scm_lexer_tokenize_numeric_unsigned_integer(lexer, &charset); */

/*   current = scm_ibuffer_head_char(lexer->ibuffer); */
/*   if (current == '/' && nr_shift) { */
/*   } */
/*   if (current == '.') { */
    
/*     // small number */
/*   } */
/*   else if (current == '/') { */
/*     // ratinal  */
/*   } */
/* } */

/* static int */
/* scm_lexer_tokenize_numeric_complex(ScmLexer *lexer, const char *charset) */
/* { */
/*   unsigned char current, sign; */

/*   assert(lexer != NULL); */

/*   current = scm_ibuffer_head_char(lexer->ibuffer); */

/*   sign = '\0'; */
/*   if (strchr("+-", current) != NULL) { */
/*     scm_ibuffer_shift_char(lexer->ibuffer); */
/*     scm_lexer_push_char(lexer, current); */
/*     sign = current; */
/*   } */

/*   scm_lexer_tokenize_numeric_unsigned_real(lexer, charset); */
/* } */

/* const char * */
/* cardinal2charset(unsigned char cardinal) */
/* { */
/*   const char *charset; */

/*   switch (cardinal) { */
/*   case 'b': */
/*     charset = BINARY_DIGIT_CHARSET; */
/*     break; */
/*   case 'o': */
/*     charset = OCTAL_DIGIT_CHARSET; */
/*     break; */
/*   case 'd': */
/*     charset = DECIMAL_DIGIT_CHARSET; */
/*     break; */
/*   case 'x': */
/*     charset = HEXADECIMAL_DIGIT_CHARSET; */
/*     break; */
/*   default: */
/*     charset = DECIMAL_DIGIT_CHARSET; */
/*   } */

/*   return charset; */
/* } */

/* static int */
/* scm_lexer_tokenize_numeric_start(ScmLexer *lexer, */
/* 				 unsigned char cardinal, unsigned char strict) */
/* { */
  
/*   unsigned char current; */

/*   assert(lexer != NULL); */
/*   assert(cardinal == '\0' || strchr("bodx", cardinal) != NULL); */
/*   assert(strict == '\0' || strchr("ie", strict) != NULL); */
  
/*   current = scm_ibuffer_head_char(lexer->ibuffer); */

/*   if (strchr("bodx", current) != NULL) { // cardinal */
/*     if (cardinal == '\0') { */
/*       scm_ibuffer_shift_char(lexer->ibuffer); */
/*       scm_lexer_push_char(lexer, current); */
/*       cardinal = current; */
/*       current = scm_ibuffer_head_char(lexer->ibuffer);   */
/*     } */
/*     else */
/*       goto not_numeric; */
/*   } */
/*   else if (strchr("ie", current) != NULL) { // strictness */
/*     if (strict == '\0') { */
/*       scm_ibuffer_shift_char(lexer->ibuffer); */
/*       scm_lexer_push_char(lexer, current); */
/*       strict = current; */
/*       current = scm_ibuffer_head_char(lexer->ibuffer);   */
/*     } */
/*     else */
/*       goto not_numeric; */
/*   } */

/*   if (current == '#') { */
/*     scm_ibuffer_shift_char(lexer->ibuffer); */
/*     scm_lexer_push_char(lexer, current); */
/*     return scm_lexer_tokenize_numeric_start(lexer, cardinal, strict); */
/*   } */
/*   else */
/*     return scm_lexer_tokenize_numeric_complex(lexer, */
/* 					      cardinal2charset(cardinal)); */

/*  not_numeric: */
/*   scm_ibuffer_shift_char(lexer->ibuffer); */
/*   scm_lexer_push_char(lexer, current); */
/*   lexer->token_type = SCM_TOKEN_TYPE_IDENTIFIER; */
/*   return LEXER_STATE_IDENTIFIER; */
/* } */

static int
scm_lexer_tokenize_numeric(ScmLexer *lexer)
{
  /* TODO: write appropriate implementation  */

  int current;

  assert(lexer != NULL);
  //  return scm_lexer_tokenize_number(lexer, '\0', '\0');

  current = scm_ibuffer_head_char(lexer->ibuffer);
  if (IS_NUMERIC_START_CHAR(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);

    if (!isdigit(scm_ibuffer_head_char(lexer->ibuffer)))
      return LEXER_STATE_IDENTIFIER;
  }

  current = scm_ibuffer_head_char(lexer->ibuffer);
  while (isdigit(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    current = scm_ibuffer_head_char(lexer->ibuffer);
  }

  if (IS_IDENTIFIER_FOLLOW_ON_CHAR(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    return LEXER_STATE_IDENTIFIER;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
    return LEXER_STATE_DONE;
  }
}

static void
scm_lexer_state_clear(ScmLexer *lexer)
{
  assert(lexer != NULL);

  lexer->buf_used = 0;
  if (lexer->buffer != NULL) lexer->buffer[0] = '\0';
  lexer->token_type = SCM_TOKEN_TYPE_NONE;
}

static void
scm_lexer_push_token(ScmLexer *lexer, ScmToken *token)
{
  assert(lexer != NULL);
  assert(token != NULL);

  scm_basic_list_push(lexer->tokens, SCM_BASIC_LIST_VALUE(token));
}

static void
scm_lexer_tokenize(ScmLexer *lexer)
{
  int state;

  assert(lexer != NULL);
  assert(lexer->buf_used == 0);
  assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);

  if (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE)
    return;

  state = LEXER_STATE_INIT;
  while (state != LEXER_STATE_DONE && state != LEXER_STATE_ERROR) {
    switch (state) {
    case LEXER_STATE_INIT:
    case LEXER_STATE_DISREGARD:
      state = scm_lexer_tokenize_init(lexer);
      break;
    case LEXER_STATE_IDENTIFIER:
      state = scm_lexer_tokenize_identifier(lexer);
      break;
    case LEXER_STATE_DOT:
      state = scm_lexer_tokenize_dot(lexer);
      break;
    case LEXER_STATE_COMMENT:
      state = scm_lexer_tokenize_comment(lexer);
      break;
    case LEXER_STATE_NUMBER_SIGN:
      state = scm_lexer_tokenize_number_sign(lexer);
      break;
    case LEXER_STATE_STRING:
      state = scm_lexer_tokenize_string(lexer);
      break;
    case LEXER_STATE_CHAR:
      state = scm_lexer_tokenize_char(lexer);
      break;
    case LEXER_STATE_NUMERIC:
      state = scm_lexer_tokenize_numeric(lexer);
      break;
    default:
      assert(false); // must not happen
      break;
    }
  }

  scm_lexer_push_token(lexer,
                       scm_token_construct(scm_lexer_token_type(lexer),
                                           scm_lexer_buffer(lexer)));

  scm_lexer_state_clear(lexer);
}


ScmLexer *
scm_lexer_construct(ScmIBuffer *ibuffer)
{
  ScmLexer *lexer;

  assert(ibuffer != NULL);

  lexer = scm_memory_allocate(sizeof(ScmLexer));
  lexer->ibuffer = ibuffer;
  lexer->tokens = scm_basic_list_construct();
  lexer->buffer = NULL;
  lexer->buf_size = 0;

  scm_lexer_state_clear(lexer);
  scm_lexer_error_state_clear(lexer);

  return lexer;
}

ScmLexer *
scm_lexer_destruct(ScmLexer *lexer)
{
  assert(lexer != lexer);

  while (scm_basic_list_head(lexer->tokens) != NULL)
    scm_lexer_shift_token(lexer);
  scm_basic_list_destruct(lexer->tokens);

  if (lexer->buffer != NULL)
    scm_memory_release(lexer->buffer);

  scm_memory_release(lexer);
}

ScmToken *
scm_lexer_head_token(ScmLexer *lexer)
{
  ScmBasicListEntry *entry;

  assert(lexer != NULL);

  if (scm_basic_list_length(lexer->tokens) == 0)
    scm_lexer_tokenize(lexer);

  entry = scm_basic_list_head(lexer->tokens);

  assert(entry != NULL);

  return SCM_BASIC_LIST_ENTRY_VALUE(entry);
}

void
scm_lexer_shift_token(ScmLexer *lexer)
{
  ScmBasicListEntry *entry;

  assert(lexer != NULL);

  if ((entry = scm_basic_list_head(lexer->tokens))) {
    ScmToken *token = SCM_TOKEN(SCM_BASIC_LIST_ENTRY_VALUE(entry));
    if (!scm_lexer_has_error(lexer)
        || SCM_TOKEN_TYPE(token) != SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      scm_basic_list_shift(lexer->tokens);
      scm_token_destruct(token);
    }
  }
}

void
scm_lexer_error_state_clear(ScmLexer *lexer)
{
  assert(lexer != NULL);

  lexer->error_type = SCM_LEXER_ERR_TYPE_NONE;
  lexer->error_line = 0;
  lexer->error_column = 0;
}

bool
scm_lexer_has_error(ScmLexer *lexer)
{
  assert(lexer != NULL);
  return (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE);
}

SCM_LEXER_ERR_TYPE_T
scm_lexer_error_type(ScmLexer *lexer)
{
  assert(lexer != NULL);
  return lexer->error_type;
}

int
scm_lexer_error_line(ScmLexer *lexer)
{
  assert(lexer != NULL);
  return lexer->error_line;
}

int
scm_lexer_error_column(ScmLexer *lexer)
{
  assert(lexer != NULL);
  return lexer->error_column;
}

ScmToken *
scm_token_construct(SCM_TOKEN_TYPE_T type, char *string)
{
  ScmToken *token;

  assert(string != NULL);

  token = scm_memory_allocate(sizeof(ScmToken));
  token->type = type;

  token->string = scm_memory_allocate(strlen((char *)string) + 1);
  strcpy((char *)token->string, (char *)string);

  return token;
}

void
scm_token_destruct(ScmToken *token)
{
  assert(token != NULL);
  scm_memory_release(token->string);
  scm_memory_release(token);
}

static ScmObj
scm_parser_parse_list(ScmParser *parser)
{
  ScmObj car, cdr;

  assert(parser != NULL);

  if (SCM_TOKEN_TYPE(scm_lexer_head_token(parser->lexer))
      == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ(scm_nil_instance());
  }

  car = scm_parser_parse_expression(parser);

  /* TODO: error handling */
  switch (scm_obj_type(car)) {
  case SCM_OBJ_TYPE_EOF:
    break;
  default:
    break;
  }

  if (SCM_TOKEN_TYPE(scm_lexer_head_token(parser->lexer))
      == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(parser->lexer);

    cdr = scm_parser_parse_expression(parser);

    if (SCM_TOKEN_TYPE(scm_lexer_head_token(parser->lexer))
        != SCM_TOKEN_TYPE_RPAREN) {
      /* TODO: error handling */
      return SCM_OBJ(scm_nil_instance());
    }

    scm_lexer_shift_token(parser->lexer);
  }
  else 
    cdr = scm_parser_parse_list(parser);

  /* TODO: error handling */
  switch (scm_obj_type(cdr)) {
  case SCM_OBJ_TYPE_EOF:
    break;
  default:
    break;
  }

  return SCM_OBJ(scm_pair_construct(car, cdr));
}

static ScmObj
scm_parser_parse_quote(ScmParser *parser)
{
  ScmObj quote;
  ScmObj quoted;

  assert(parser != NULL);

  switch (SCM_TOKEN_TYPE(scm_lexer_head_token(parser->lexer))) {
  case SCM_TOKEN_TYPE_QUOTE:
    quote = SCM_OBJ(scm_symbol_instance("quote"));
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    quote = SCM_OBJ(scm_symbol_instance("quasiquote"));
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    quote = SCM_OBJ(scm_symbol_instance("unquote"));
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    quote = SCM_OBJ(scm_symbol_instance("unquote-splicing"));
    break;
  default:
    return SCM_OBJ(scm_nil_instance()); // dummy;
    /* TODO: error handling */
    break;
  }

  scm_lexer_shift_token(parser->lexer);
  quoted = scm_parser_parse_expression(parser);

  /* TODO: error handling */
  switch (scm_obj_type(quoted)) {
  case SCM_OBJ_TYPE_EOF:
    break;
  default:
    break;
  }

  quoted = SCM_OBJ(scm_pair_construct(quoted,
                                      SCM_OBJ(scm_nil_instance())));
  return SCM_OBJ(scm_pair_construct(quote, quoted));
}

static ScmObj
scm_parser_parse_string(ScmParser *parser)
{
  char *p;
  ScmObj obj;

  assert(parser != NULL);

  p = SCM_TOKEN_STRING(scm_lexer_head_token(parser->lexer));
  obj = SCM_OBJ(scm_string_construct(p, strlen(p), SCM_ENCODING_ASCII));
  scm_lexer_shift_token(parser->lexer);
  return obj;
}

static ScmObj
scm_parser_parse_identifier(ScmParser *parser)
{
  ScmObj obj;

  assert(parser != NULL);

  obj = SCM_OBJ(scm_symbol_instance(SCM_TOKEN_STRING(scm_lexer_head_token(parser->lexer))));
  scm_lexer_shift_token(parser->lexer);

  return obj;  
}

static ScmObj
scm_parser_parse_numeric(ScmParser *parser)
{
  ScmObj obj;
  long long num;

  assert(parser != NULL);

  sscanf(SCM_TOKEN_STRING(scm_lexer_head_token(parser->lexer)),
         "%lld", &num);
  obj = SCM_OBJ(scm_integer_construct(num));
  scm_lexer_shift_token(parser->lexer);

  return obj;
}

static ScmObj
scm_parser_parse_bool(ScmParser *parser)
{
  ScmObj obj;

  assert(parser != NULL);

  if (strcasecmp("#t", SCM_TOKEN_STRING(scm_lexer_head_token(parser->lexer)))
      == 0)
    obj = SCM_OBJ(scm_bool_construct(true));
  else
    obj = SCM_OBJ(scm_bool_construct(false));

  scm_lexer_shift_token(parser->lexer);

  return obj;
}

static ScmObj
scm_parser_parse_vector_aux(ScmParser *parser, size_t *len)
{
  ScmObj car, cdr;

  assert(parser != NULL);
  assert(len != NULL);

  scm_lexer_shift_token(parser->lexer);

  if (SCM_TOKEN_TYPE(scm_lexer_head_token(parser->lexer))
      == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ(scm_nil_instance());
  }

  car = scm_parser_parse_expression(parser);

  /* TODO: error handling */
  switch (scm_obj_type(car)) {
  case SCM_OBJ_TYPE_EOF:
    break;
  default:
    break;
  }

  scm_lexer_shift_token(parser->lexer);
  *len += 1;
  cdr = scm_parser_parse_vector_aux(parser, len);
  
  /* TODO: error handling */
  switch (scm_obj_type(cdr)) {
  case SCM_OBJ_TYPE_EOF:
    break;
  default:
    break;
  }

  return SCM_OBJ(scm_pair_construct(car, cdr));
}

static ScmObj
scm_parser_parse_vector(ScmParser *parser)
{
  ScmVector *vector;
  ScmObj elements;
  size_t len, idx;

  assert(parser != NULL);

  len = 0;
  elements = scm_parser_parse_vector_aux(parser, &len);
  vector = scm_vector_construct(len);

  for (idx = 0; idx < len; idx++) {
    scm_vector_set(vector, idx, scm_pair_car(SCM_PAIR(elements)));
    elements = scm_pair_cdr(SCM_PAIR(elements));
  }

  return SCM_OBJ(vector);
}


static ScmObj
scm_parser_parse_char(ScmParser *parser)
{
  char *p;
  ScmObj obj;

  assert(parser != NULL);

  p = SCM_TOKEN_STRING(scm_lexer_head_token(parser->lexer));
  if (strcasecmp("#\\newline", p) == 0)
    obj = SCM_OBJ(scm_char_construct('\n'));
  else if (strcasecmp("#\\space", p) == 0)
    obj = SCM_OBJ(scm_char_construct(' '));
  else if (strlen(p) == 3)
    obj = SCM_OBJ(scm_char_construct(p[2]));
  else if (strncasecmp("#\\0x", p, 4) == 0) {
    unsigned int v;
    sscanf(p + 4, "%x", &v);
    obj = SCM_OBJ(scm_char_construct(v));
  }
  else {
    // TODO: error handling
    obj = SCM_OBJ(scm_nil_instance()); // dummy
  }
      
  scm_lexer_shift_token(parser->lexer);
  return obj;
}

static ScmObj
scm_parser_parse_eof(ScmParser *parser)
{
  assert(parser != NULL);

  scm_lexer_shift_token(parser->lexer);
  return SCM_OBJ(scm_eof_instance());
}

ScmParser *
scm_parser_construct(ScmLexer *lexer)
{
  ScmParser *parser;

  assert(lexer != NULL);

  parser = scm_memory_allocate(sizeof(ScmParser));
  parser->lexer = lexer;

  return parser;
}

ScmObj
scm_parser_parse_expression(ScmParser *parser)
{
  ScmObj result;
  ScmToken *token;

  assert(parser != NULL);

  token = scm_lexer_head_token(parser->lexer);
  switch (SCM_TOKEN_TYPE(token)) {
  case SCM_TOKEN_TYPE_LPAREN:
    scm_lexer_shift_token(parser->lexer);  
    result = scm_parser_parse_list(parser);
    break;
  case SCM_TOKEN_TYPE_RPAREN:
    // error
    result = SCM_OBJ(scm_nil_instance()); // dummy;
    break;
  case SCM_TOKEN_TYPE_DOT:
    // error
    result = SCM_OBJ(scm_nil_instance()); // dummy;
    break;
  case SCM_TOKEN_TYPE_QUOTE:
    result = scm_parser_parse_quote(parser);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    result = scm_parser_parse_quote(parser);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    result = scm_parser_parse_quote(parser);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    result = scm_parser_parse_quote(parser);
    break;
  case SCM_TOKEN_TYPE_STRING:
    result = scm_parser_parse_string(parser);
    break;
  case SCM_TOKEN_TYPE_IDENTIFIER:
    result = scm_parser_parse_identifier(parser);
    break;
  case SCM_TOKEN_TYPE_NUMERIC:
    result = scm_parser_parse_numeric(parser);
    break;
  case SCM_TOKEN_TYPE_BOOL:
    result = scm_parser_parse_bool(parser);
    break;
  case SCM_TOKEN_TYPE_VECTOR_START:
    result = scm_parser_parse_vector(parser);
    break;
  case SCM_TOKEN_TYPE_CHAR:
    result = scm_parser_parse_char(parser);
    break;
  case SCM_TOKEN_TYPE_EOF:
    result = scm_parser_parse_eof(parser);
    break;
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:
    // error
    result = SCM_OBJ(scm_nil_instance()); // dummy;
    break;
  default:
    assert(false); /* must not happen */
    break;
  }

  return result;
}
