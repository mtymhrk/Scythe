#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "memory.h"
#include "basiclist.h"
#include "ibuffer.h"
#include "parser.h"

extern int isblank(int c);

struct ScmParserRec {
  int dummy;
};

struct ScmLexerRec {
  ScmIBuffer *ibuffer;
  unsigned char *buffer;
  size_t buf_size;
  size_t buf_used;
  SCM_TOKEN_TYPE_T token_type;
  ScmBasicList *tokens;
};

struct ScmTokenRec {
  SCM_TOKEN_TYPE_T type;
  unsigned char *string;
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
  LEXER_STATE_KICK_OFF,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
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
  unsigned char *new_buffer = NULL;

  assert(lexer != NULL);

  if (lexer->buffer == NULL)
    lexer->buf_size = SCM_LEXER_INITIAL_BUFFER_SIZE;

  for (new_size = lexer->buf_size; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > lexer->buf_size || lexer->buffer == NULL) {
    new_buffer = (unsigned char *)(scm_memory_allocate(new_size));
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
scm_lexer_push_char(ScmLexer *lexer, unsigned char c)
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

static unsigned char *
scm_lexer_buffer(ScmLexer *lexer)
{
  assert(lexer != NULL);

  scm_lexer_expand_buffer_if_needed(lexer, SCM_LEXER_INITIAL_BUFFER_SIZE);
  return lexer->buffer;
}

static int
scm_lexer_tokenize_kick_off(ScmLexer *lexer)
{
  const char *one_char_token_chars = "().'`";
  const SCM_TOKEN_TYPE_T one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN, SCM_TOKEN_TYPE_DOT,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  int current, state;
  char *p;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);

  if (current == EOF) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    state = LEXER_STATE_DONE;
  }
  else if (isblank(current)) {
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
  else if (IS_LINE_FEED(current)) {
    /* TODO: write process for line feed */
    state = LEXER_STATE_DISREGARD;
  }
  else {
    switch (current) {
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
scm_lexer_tokenize_comment(ScmLexer *lexer)
{
  int current;

  assert(lexer != NULL);

  current = scm_ibuffer_head_char(lexer->ibuffer);
  scm_ibuffer_shift_char(lexer->ibuffer);

  if (IS_LINE_FEED(current)) {
    /* TODO: write process for line feed */
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
  case EOF:
    state = LEXER_STATE_ERROR;
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
  scm_ibuffer_shift_char(lexer->ibuffer);

  if (current == '"') {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
    return LEXER_STATE_DONE;
  }
  else if (current == EOF) {
    return LEXER_STATE_ERROR;
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
  while (strchr(terminations, current) != NULL &&
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
  while (isdigit(current)) {
    scm_ibuffer_shift_char(lexer->ibuffer);
    scm_lexer_push_char(lexer, current);
    current = scm_ibuffer_head_char(lexer->ibuffer);
  }

  if (current == EOF || isspace(current)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
    return LEXER_STATE_DONE;
  }
  else {
    return LEXER_STATE_IDENTIFIER;
  }
}

static void
scm_lexer_state_clear(ScmLexer *lexer)
{
  assert(lexer != NULL);

  lexer->buffer = NULL;
  lexer->buf_size = 0;
  lexer->buf_used = 0;
  lexer->token_type = SCM_TOKEN_TYPE_NONE;
}

static void
scm_lexer_tokenize(ScmLexer *lexer)
{
  int state;

  assert(lexer != NULL);
  assert(lexer->buffer == NULL);
  assert(lexer->buf_size == 0);
  assert(lexer->buf_used == 0);
  assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);

  state = LEXER_STATE_KICK_OFF;
  while (state != LEXER_STATE_DONE && state != LEXER_STATE_ERROR) {
    switch (state) {
    case LEXER_STATE_KICK_OFF:
    case LEXER_STATE_DISREGARD:
      state = scm_lexer_tokenize_kick_off(lexer);
      break;
    case LEXER_STATE_IDENTIFIER:
      state = scm_lexer_tokenize_identifier(lexer);
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
      break;
    }
  }

  if (state == LEXER_STATE_DONE) {
    scm_lexer_push_token(lexer,
			 scm_token_construct(scm_lexer_token_type(lexer),
					     scm_lexer_buffer(lexer)));
    scm_lexer_state_clear(lexer);
  }
  else {
    // TODO : process for Error 
  }
}


ScmLexer *
scm_lexer_construct(ScmIBuffer *ibuffer)
{
  ScmLexer *lexer;

  assert(ibuffer != NULL);

  lexer = scm_memory_allocate(sizeof(ScmLexer));
  lexer->ibuffer = ibuffer;
  lexer->tokens = scm_basic_list_construct();
  scm_lexer_state_clear(lexer);

  return lexer;
}

ScmToken *
scm_lexer_head_token(ScmLexer *lexer)
{
  ScmBasicListEntry *entry;

  assert(lexer != NULL);

  if (scm_basic_list_length(lexer->tokens) == 0)
    scm_lexer_tokenize(lexer);

  entry = scm_basic_list_head(lexer->tokens);
  return (entry != NULL) ? scm_basic_list_entry_value(entry) : NULL;
}

void
scm_lexer_push_token(ScmLexer *lexer, ScmToken *token)
{
  assert(lexer != NULL);
  assert(token != NULL);

  scm_basic_list_push(lexer->tokens, SCM_BASIC_LIST_VALUE(token));
}

void
scm_lexer_shift_token(ScmLexer *lexer)
{
  assert(lexer != NULL);

  scm_basic_list_shift(lexer->tokens);
}

void
scm_lexer_unshift_token(ScmLexer *lexer, ScmToken *token)
{
  assert(lexer != NULL);
  assert(token != NULL);

  scm_basic_list_unshift(lexer->tokens, SCM_BASIC_LIST_VALUE(token));
}

ScmToken *
scm_token_construct(SCM_TOKEN_TYPE_T type, unsigned char *string)
{
  ScmToken *token;

  assert(string != NULL);

  token = scm_memory_allocate(sizeof(ScmToken));
  token->type = type;
  token->string = string;

  return token;
}

SCM_TOKEN_TYPE_T
scm_token_type(ScmToken *token)
{
  assert(token != NULL);
  return token->type;
}

const unsigned char *
scm_token_string(ScmToken *token)
{
  assert(token != NULL);
  return token->string;
}
