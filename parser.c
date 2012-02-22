#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "impl_utils.h"
#include "parser.h"

#define SCM_LEXER_INITIAL_BUFFER_SIZE 16

#define IDENTIFIER_START_CHARS "abcdefghijklmnopqrstuvwxyz!$%&*/:<=>?~_^"
#define IDENTIFIER_FOLLOW_ON_CHARS \
  IDENTIFIER_START_CHARS "0123456789.+-"
#define IDENTIFIER_DELIMITER "()'`,\"[] \t\n\r;"
#define NUMERIC_START_CHAR "+-0123456789"

#define IS_LINE_FEED(c) ((c) == '\n')

#define IS_IDENTIFIER_START_CHAR(c) \
  (strchr(IDENTIFIER_START_CHARS, tolower(c)) != NULL)

#define IS_IDENTIFIER_FOLLOW_ON_CHAR(c) \
  (strchr(IDENTIFIER_FOLLOW_ON_CHARS, tolower(c)) != NULL)

#define IDENTIFIER_DELIMITER_P(c) \
  (strchr(IDENTIFIER_DELIMITER, tolower(c)) != NULL)

#define IS_NUMERIC_START_CHAR(c) \
  (strchr(NUMERIC_START_CHAR, tolower(c)) != NULL)

typedef enum {
  LEXER_STATE_DONE,
  LEXER_STATE_INIT,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
  LEXER_STATE_DOT,
  LEXER_STATE_UNQUOTE,
  LEXER_STATE_REFERENCE_DECL_OR_USE,
  LEXER_STATE_NUMERIC,
  LEXER_STATE_COMMENT,
  LEXER_STATE_NUMBER_SIGN,
  LEXER_STATE_STRING,
  LEXER_STATE_CHAR,
  LEXER_STATE_ERROR
} LEXER_STATE_T;



static int
scm_lexer_expand_buffer_if_needed(ScmLexer *lexer, size_t needed_size)
{
  size_t new_size;
  char *new_buffer = NULL;

  scm_assert(lexer != NULL);

  if (lexer->buffer == NULL)
    lexer->buf_capacity = SCM_LEXER_INITIAL_BUFFER_SIZE;

  for (new_size = lexer->buf_capacity; new_size < needed_size; new_size *= 2)
    ;

  if (new_size > lexer->buf_capacity || lexer->buffer == NULL) {
    new_buffer = scm_capi_realloc(lexer->buffer, new_size);
    if (new_buffer == NULL)
      return -1;
    lexer->buffer = new_buffer;
    lexer->buf_capacity = new_size;
  }

  return 0;
}

static int
scm_lexer_push_char(ScmLexer *lexer, char c)
{
  int rslt;

  scm_assert(lexer != NULL);

  rslt = scm_lexer_expand_buffer_if_needed(lexer, lexer->buf_used + 2);
  if (rslt < 0) return -1;
                                   /* 2 = pushd char + '\0' */
  lexer->buffer[lexer->buf_used++] = c;
  lexer->buffer[lexer->buf_used] = '\0';

  return 0;
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

  return lexer->buffer;
}

static size_t
scm_lexer_buffer_size(ScmLexer *lexer)
{
  assert(lexer != NULL);

  return lexer->buf_used;
}

static void
scm_lexer_setup_error_state(ScmLexer *lexer, SCM_LEXER_ERR_TYPE_T error)
{
  assert(lexer != NULL);

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_TOKENIZE_ERR);
  lexer->error_type = error;
}

static int
scm_lexer_tokenize_init(ScmLexer *lexer, ScmObj port)
{
  const char *one_char_token_chars = "()[]'`";
  const SCM_TOKEN_TYPE_T one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  int state;
  char current, dummy;
  ssize_t n;
  char *p;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_peek_raw(port, &current, sizeof(current));

  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    state = LEXER_STATE_DONE;
  }
  else if (n == 0) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    state = LEXER_STATE_DONE;
  }
  else if (isspace(current)) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    state = LEXER_STATE_DISREGARD;
  }
  else if ((p = strchr(one_char_token_chars, current))) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, current);
    scm_lexer_set_token_type(lexer,
                             one_char_token_types[p - one_char_token_chars]);
    state = LEXER_STATE_DONE;
  }
  else if (IS_NUMERIC_START_CHAR(current)) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, current);
    state = LEXER_STATE_NUMERIC;
  }
  else {
    switch (current) {
    case '.':
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_DOT;
      break;
    case ';':
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      state = LEXER_STATE_COMMENT;
      break;
    case ',':
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_UNQUOTE;
      break;
    case '#':
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_NUMBER_SIGN;
      break;
    case '"':
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      state = LEXER_STATE_STRING;
      break;
    default:
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      state = LEXER_STATE_IDENTIFIER;
      break;
    }
  }

  return state;
}

static int
scm_lexer_tokenize_identifier(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  while (1) {
    ssize_t n = scm_capi_peek_raw(port, &current, sizeof(current));
    if (n < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (n == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      return LEXER_STATE_DONE;
    }
    else if (IDENTIFIER_DELIMITER_P(current)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      return LEXER_STATE_DONE;
    }
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, current);
  }
}

static int
scm_lexer_tokenize_dot(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;
  ssize_t n;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_peek_raw(port, &current, sizeof(current));
  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (n == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }
  else if (IDENTIFIER_DELIMITER_P(current)){
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }
  else {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_IDENTIFIER;
  }
}

static int
scm_lexer_tokenize_comment(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  while (1) {
    ssize_t n = scm_capi_peek_raw(port, &current, sizeof(current));
    if (n < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (n == 0) {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
      return LEXER_STATE_DONE;
    }
    else if (IS_LINE_FEED(current)) {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      return LEXER_STATE_DISREGARD;
    }
    else {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
    }
  }
}

static int
scm_lexer_tokenize_unquote(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;
  ssize_t n;
  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_peek_raw(port, &current, sizeof(current));
  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (n == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }
  else if (current == '@') {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, '@');
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE_SPLICING);
    return LEXER_STATE_DONE;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }
}

static int
scm_lexer_tokenize_number_sign(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;
  ssize_t n;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_peek_raw(port, &current, sizeof(current));
  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (n == 0) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }
  else {
    int state;
    switch (current) {
    case 't':
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BOOL_TRUE);
      state = LEXER_STATE_DONE;
      break;
    case 'f':
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BOOL_FALSE);
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
      if (strchr("0123456789", current) != NULL) {
        state = LEXER_STATE_REFERENCE_DECL_OR_USE;
      }
      else if (strchr("bodxie", tolower(current)) != NULL) {
        state = LEXER_STATE_NUMERIC;
      }
      else {
        scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
        state = LEXER_STATE_ERROR;
      }
      break;
    }
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, current);
    return state;
  }
}

static int
scm_lexer_tokenize_string_escaped(ScmLexer *lexer, ScmObj port)
{
  const char *control_chars_escape = "abtnvfr";
  const char *control_chars = "\a\b\t\n\v\f\r";
  char current;
  char *p;
  ssize_t n;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_read_raw(port, &current, sizeof(current));
  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (n == 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }
  else if ((p = strchr(control_chars_escape, current))) {
    scm_lexer_push_char(lexer, control_chars[p - control_chars_escape]);
    return LEXER_STATE_STRING;
  }
  else {
    scm_lexer_push_char(lexer, current);
    return LEXER_STATE_STRING;
  }
}

static int
scm_lexer_tokenize_string(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  while (1) {
    ssize_t n = scm_capi_peek_raw(port, &current, sizeof(current));
    if (n < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (n == 0) {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (current == '"') {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
      return LEXER_STATE_DONE;
    }
    else if (current == '\\') {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      int st = scm_lexer_tokenize_string_escaped(lexer, port);
      if (st != LEXER_STATE_STRING)
        return st;
    }
    else {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
    }
  }
}

static int
scm_lexer_tokenize_char(ScmLexer *lexer, ScmObj port)
{
  const char *delimiters = "()[]#'\" \t\n\r";
  char current, dummy;
  ssize_t n;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  n = scm_capi_peek_raw(port, &current, sizeof(current));
  if (n < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (n == 0) {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }
  else {
    scm_capi_read_raw(port, &dummy, sizeof(dummy));
    scm_lexer_push_char(lexer, current);
    while (1) {
      n = scm_capi_peek_raw(port, &current, sizeof(current));
      if (n < 0) {
        scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
        return LEXER_STATE_ERROR;
      }
      else if (n == 0) {
        scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
        return LEXER_STATE_DONE;
      }
      else if (strchr(delimiters, current) != NULL) {
        scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
        return LEXER_STATE_DONE;
      }
      else {
        scm_capi_read_raw(port, &dummy, sizeof(dummy));
        scm_lexer_push_char(lexer, current);
      }
    }
  }
}

static int
scm_lexer_tokenize_reference(ScmLexer *lexer, ScmObj port)
{
  char current, dummy;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  while (1) {
    ssize_t n = scm_capi_peek_raw(port, &current, sizeof(current));
    if (n < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (n == 0) {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (current == '#') {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_USE);;
      return LEXER_STATE_DONE;
    }
    else if (current == '=') {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_DECL);;
      return LEXER_STATE_DONE;
    }
    else if (isdigit(current)) {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
    }
    else {
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_numeric(ScmLexer *lexer, ScmObj port)
{
  /* provisional implementation */
  char current, dummy;
  int state;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));

  state = LEXER_STATE_IDENTIFIER;
  while (1) {
    ssize_t n = scm_capi_peek_raw(port, &current, sizeof(current));
    if (n < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (n == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }
    else if (strchr("0123456789", current) != NULL) {
      state = LEXER_STATE_NUMERIC;
      scm_capi_read_raw(port, &dummy, sizeof(dummy));
      scm_lexer_push_char(lexer, current);
    }
    else if (strchr("()[]#'`,\" \t\n\r;", tolower(current)) != NULL) {
      if (state == LEXER_STATE_IDENTIFIER)
        scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      else
        scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }
    else {
      return LEXER_STATE_IDENTIFIER;
    }
  }
}

static void
scm_lexer_state_clear(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  lexer->buf_used = 0;
  if (lexer->buffer != NULL) lexer->buffer[0] = '\0';
  lexer->token_type = SCM_TOKEN_TYPE_NONE;
}

static void
scm_lexer_push_token(ScmLexer *lexer, ScmToken *token)
{
  assert(lexer != NULL);
  assert(token != NULL);

  if (lexer->tokens_head == NULL)
    lexer->tokens_head = token;
  else
    lexer->tokens_tail->next = token;

  lexer->tokens_tail = token;
  token->next = NULL;
}

static int
scm_lexer_tokenize(ScmLexer *lexer, ScmObj port)
{
  int state;
  ScmToken *token;

  scm_assert(lexer != NULL);
  scm_assert(lexer->buf_used == 0);
  scm_assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);
  scm_assert(scm_capi_input_port_p(port));

  if (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE)
    return -1;

  state = LEXER_STATE_INIT;
  while (state != LEXER_STATE_DONE && state != LEXER_STATE_ERROR) {
    switch (state) {
    case LEXER_STATE_INIT:
    case LEXER_STATE_DISREGARD:
      state = scm_lexer_tokenize_init(lexer, port);
      break;
    case LEXER_STATE_IDENTIFIER:
      state = scm_lexer_tokenize_identifier(lexer, port);
      break;
    case LEXER_STATE_DOT:
      state = scm_lexer_tokenize_dot(lexer, port);
      break;
    case LEXER_STATE_COMMENT:
      state = scm_lexer_tokenize_comment(lexer, port);
      break;
    case LEXER_STATE_UNQUOTE:
      state = scm_lexer_tokenize_unquote(lexer, port);
      break;
    case LEXER_STATE_NUMBER_SIGN:
      state = scm_lexer_tokenize_number_sign(lexer, port);
      break;
    case LEXER_STATE_STRING:
      state = scm_lexer_tokenize_string(lexer, port);
      break;
    case LEXER_STATE_CHAR:
      state = scm_lexer_tokenize_char(lexer, port);
      break;
    case LEXER_STATE_REFERENCE_DECL_OR_USE:
      state = scm_lexer_tokenize_reference(lexer, port);
      break;
    case LEXER_STATE_NUMERIC:
      state = scm_lexer_tokenize_numeric(lexer, port);
      break;
    default:
      assert(false); // must not happen
      break;
    }
  }

  token = scm_token_new(scm_lexer_token_type(lexer),
                        scm_lexer_buffer(lexer),
                        scm_lexer_buffer_size(lexer));
  if (token == NULL) return -1;

  scm_lexer_push_token(lexer, token);
  scm_lexer_state_clear(lexer);

  return 0;
}


ScmLexer *
scm_lexer_new(void)
{
  ScmLexer *lexer;

  lexer = scm_capi_malloc(sizeof(ScmLexer));
  if (lexer == NULL) return NULL;

  lexer->buffer = NULL;
  lexer->buf_capacity = 0;
  lexer->tokens_head = NULL;
  lexer->tokens_tail = NULL;

  scm_lexer_state_clear(lexer);
  scm_lexer_error_state_clear(lexer);

  return lexer;
}

ScmLexer *
scm_lexer_end(ScmLexer *lexer)
{
  assert(lexer != lexer);

  while (lexer->tokens_head != NULL)
    scm_lexer_shift_token(lexer);

  if (lexer->buffer != NULL)
    scm_capi_free(lexer->buffer);

  scm_capi_free(lexer);
}

ScmToken *
scm_lexer_head_token(ScmLexer *lexer, ScmObj port)
{
  ScmToken *token;

  scm_assert(lexer != NULL);

  if (lexer->tokens_head == NULL)
    scm_lexer_tokenize(lexer, port);

  token = lexer->tokens_head;

  scm_assert(token != NULL);

  return token;
}

void
scm_lexer_shift_token(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  if (lexer->tokens_head != NULL) {
    ScmToken *token = lexer->tokens_head;
    if (!scm_lexer_error_p(lexer)
        || SCM_TOKEN_TYPE(token) != SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      lexer->tokens_head = token->next;
      if (lexer->tokens_head == NULL)
        lexer->tokens_tail = NULL;
      scm_token_end(token);
    }
  }
}

void
scm_lexer_error_state_clear(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  lexer->error_type = SCM_LEXER_ERR_TYPE_NONE;
}

bool
scm_lexer_error_p(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  return (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE);
}

SCM_LEXER_ERR_TYPE_T
scm_lexer_error_type(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  return lexer->error_type;
}

ScmToken *
scm_token_new(SCM_TOKEN_TYPE_T type, const char *string, size_t size)
{
  ScmToken *token;

  token = scm_capi_malloc(sizeof(ScmToken));
  if (token == NULL) return NULL;

  if (string != NULL && size > 0) {
    token->string = scm_capi_malloc(size);
    if (token->string == NULL) {
      scm_capi_free(token);
      return NULL;
    }
    memcpy(token->string, string, size);
  }
  else
    token->string = NULL;

  token->type = type;
  token->size = size;

  return token;
}

void
scm_token_end(ScmToken *token)
{
  scm_assert(token != NULL);

  scm_capi_free(token->string);
  scm_capi_free(token);
}

static ScmObj
scm_parser_parse_list(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &car, &cdr);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return scm_api_nil();
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car))
    return SCM_OBJ_NULL;

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  if (token->type == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(parser->lexer);

    cdr = scm_parser_parse_expression(parser, port);
    if (scm_capi_null_value_p(cdr))
      return SCM_OBJ_NULL;

    token = scm_lexer_head_token(parser->lexer, port);
    if (token == NULL) {
      /* TODO: error handling */
      return SCM_OBJ_NULL;
    }

    if (token->type != SCM_TOKEN_TYPE_RPAREN) {
      /* TODO: error handling */
      return SCM_OBJ_NULL;
    }

    scm_lexer_shift_token(parser->lexer);
  }
  else {
    cdr = scm_parser_parse_list(parser, port);
    if (scm_capi_null_value_p(cdr))
      return SCM_OBJ_NULL;
  }

  return scm_api_cons(car, cdr);
}

static ScmObj
scm_parser_parse_quote(ScmParser *parser, ScmObj port)
{
  const char *quote_str = "quote";
  const char *quasiquote_str = "quasiquote";
  const char *unquote_str = "unquote";
  const char *unquote_splicing_str = "unquote-splicing";

  ScmObj quote;
  ScmObj quoted;
  ScmToken *token;

  SCM_STACK_FRAME_PUSH(&port, &quote, &quoted);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  switch (token->type) {
  case SCM_TOKEN_TYPE_QUOTE:
    quote = scm_capi_make_symbol_from_cstr(quote_str);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    quote = scm_capi_make_symbol_from_cstr(quasiquote_str);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    quote = scm_capi_make_symbol_from_cstr(unquote_str);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    quote = scm_capi_make_symbol_from_cstr(unquote_splicing_str);
    break;
  case SCM_TOKEN_TYPE_NONE:               /* fall through */
  case SCM_TOKEN_TYPE_LPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_RPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_DOT:                /* fall through */
  case SCM_TOKEN_TYPE_STRING:             /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER:         /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_DECL:     /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_USE:      /* fall through */
  case SCM_TOKEN_TYPE_NUMERIC:            /* fall through */
  case SCM_TOKEN_TYPE_BOOL_TRUE:          /* fall through */
  case SCM_TOKEN_TYPE_BOOL_FALSE:         /* fall through */
  case SCM_TOKEN_TYPE_VECTOR_START:       /* fall through */
  case SCM_TOKEN_TYPE_CHAR:               /* fall through */
  case SCM_TOKEN_TYPE_EOF:                /* fall through */
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:       /* fall through */
  default:
    scm_assert(false);   /* must not happen */
    break;
  }

  if (scm_capi_null_value_p(quote)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(parser->lexer);

  quoted = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL;

  quoted = scm_api_cons(quoted, scm_api_nil());
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL;

  return scm_api_cons(quote, quoted);
}

static ScmObj
scm_parser_parse_string(ScmParser *parser, ScmObj port)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmToken *token;

  SCM_STACK_FRAME_PUSH(&port, &str);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  if (token->string == NULL)
    str = scm_capi_make_string_from_cstr("");
  else
    str = scm_capi_make_string_from_bin(token->string, token->size);

  scm_lexer_shift_token(parser->lexer);

  if (scm_capi_null_value_p(str)) return SCM_OBJ_NULL;

  return str;
}

static ScmObj
scm_parser_parse_identifier(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &sym);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  sym = scm_capi_make_symbol_from_bin(token->string, token->size);
  scm_lexer_shift_token(parser->lexer);

  return sym;
}

static ScmObj
scm_parser_parse_numeric(ScmParser *parser, ScmObj port)
{
  /* provisional implementation */

  long long num;
  ScmToken *token;
  char *str;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL || token->string == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  str = scm_capi_malloc(token->size + 1);
  if (str == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }
  memcpy(str, token->string, token->size);
  str[token->size] = '\0';

  sscanf(str, "%lld", &num);

  scm_lexer_shift_token(parser->lexer);

  return scm_capi_make_fixnum(num);
}

static ScmObj
scm_parser_parse_bool(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  SCM_TOKEN_TYPE_T type;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  type = token->type;
  scm_lexer_shift_token(parser->lexer);

  if (type == SCM_TOKEN_TYPE_BOOL_TRUE)
    return scm_api_bool_true();
  else
    return scm_api_bool_false();
}

static ScmObj
scm_parser_parse_vector_aux(ScmParser *parser, ScmObj port, size_t *len)
{
  ScmToken *token;
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &car, &cdr);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(len != NULL);

  scm_lexer_shift_token(parser->lexer);

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return scm_api_nil();
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car)) return SCM_OBJ_NULL;

  *len += 1;

  cdr = scm_parser_parse_vector_aux(parser, port, len);
  if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL;

  return scm_api_cons(car, cdr);
}

static ScmObj
scm_parser_parse_vector(ScmParser *parser, ScmObj port)
{
  ScmObj vec = SCM_OBJ_INIT, elms = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t len, idx;

  SCM_STACK_FRAME_PUSH(&port, &vec, &elms, &elm);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  len = 0;
  elms = scm_parser_parse_vector_aux(parser, port, &len);
  if (scm_capi_null_value_p(elms)) return SCM_OBJ_NULL;

  vec = scm_capi_make_vector(len);
  if (scm_capi_null_value_p(vec)) return SCM_OBJ_NULL;

  for (idx = 0; idx < len; idx++) {
    elm = scm_api_car(elms);
    scm_capi_vector_set(vec, idx, elm);
    elms = scm_api_cdr(elms);
  }

  return vec;
}


static ScmObj
scm_parser_parse_char(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj chr = SCM_OBJ_INIT;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL || token->string == NULL) {
    /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  if (token->size == sizeof("#\\newline") - 1 &&
      memcmp("#\\newline", token->string, token->size) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_newline();
  }
  else if (token->size == sizeof("#\\space") - 1 &&
           memcmp("#\\space", token->string, token->size) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_space();
  }
  else if (token->size == 3) {
    scm_char_t c;
    c.ascii = (scm_char_ascii_t)token->string[2];
    chr = scm_capi_make_char(c);
  }
  else if (token->size == 5 &&
           memcmp("#\\0x", token->string, 3) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    scm_char_t c;

    for (int i = 0; i < 2; i++) {
      if (0x30 <= token->string[3 + i] && token->string[3 + i] <= 0x39)
        c.bytes[0] = (uint8_t)("0123456789"[token->string[3 + i] - 0x30]);
      else if (0x61 <= token->string[3 + i] && token->string[3 + i] <= 0x66)
        c.bytes[0] = (uint8_t)("abcdef"[token->string[3 + i] - 0x61]);
      else {
        /* TODO: error handling */
        scm_lexer_shift_token(parser->lexer);
        return SCM_OBJ_NULL;
      }
      c.bytes[0] = (uint8_t)(c.bytes[0] << 4);
    }
    chr = scm_capi_make_char(c);
  }
  else {
    // TODO: error handling
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;
  }

  scm_lexer_shift_token(parser->lexer);

  return chr;
}

static ScmObj
scm_parser_parse_eof(ScmParser *parser, ScmObj port)
{
  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  scm_lexer_shift_token(parser->lexer);
  return scm_api_eof();
}

ScmParser *
scm_parser_new(ScmLexer *lexer)
{
  ScmParser *parser;

  scm_assert(lexer != NULL);

  parser = scm_capi_malloc(sizeof(ScmParser));
  parser->lexer = lexer;

  return parser;
}

ScmObj
scm_parser_parse_expression(ScmParser *parser, ScmObj port)
{
  ScmObj rslt = SCM_OBJ_INIT;
  ScmToken *token;

  SCM_STACK_FRAME_PUSH(port, &rslt);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL;

  switch (SCM_TOKEN_TYPE(token)) {
  case SCM_TOKEN_TYPE_LPAREN:
    scm_lexer_shift_token(parser->lexer);
    rslt = scm_parser_parse_list(parser, port);
    break;
  case SCM_TOKEN_TYPE_RPAREN:
    /* error */
    /* TODO: error handling */
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_DOT:
    /* error */
    /* TODO: error handling */
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_QUOTE:
    rslt = scm_parser_parse_quote(parser, port);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    rslt = scm_parser_parse_quote(parser, port);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    rslt = scm_parser_parse_quote(parser, port);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    rslt = scm_parser_parse_quote(parser, port);
    break;
  case SCM_TOKEN_TYPE_STRING:
    rslt = scm_parser_parse_string(parser, port);
    break;
  case SCM_TOKEN_TYPE_IDENTIFIER:
    rslt = scm_parser_parse_identifier(parser, port);
    break;
  case SCM_TOKEN_TYPE_REFERENCE_DECL:
    /* do not supported */
    /* TODO: error handling */
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_REFERENCE_USE:
    /* do not supported */
    /* TODO: error handling */
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_NUMERIC:
    rslt = scm_parser_parse_numeric(parser, port);
    break;
  case SCM_TOKEN_TYPE_BOOL_TRUE: /* fall through */
  case SCM_TOKEN_TYPE_BOOL_FALSE:
    rslt = scm_parser_parse_bool(parser, port);
    break;
  case SCM_TOKEN_TYPE_VECTOR_START:
    rslt = scm_parser_parse_vector(parser, port);
    break;
  case SCM_TOKEN_TYPE_CHAR:
    rslt = scm_parser_parse_char(parser, port);
    break;
  case SCM_TOKEN_TYPE_EOF:
    rslt = scm_parser_parse_eof(parser, port);
    break;
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:
    /* error */
    /* TODO: error handling */
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_NONE:     /* fall through */
  default:
    scm_assert(false); /* must not happen */
    break;
  }

  return rslt;
}
