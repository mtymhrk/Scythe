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


static ScmToken *
scm_token_new(SCM_TOKEN_TYPE_T type,
              void *raw_str, size_t raw_size, size_t raw_len,
              char *ascii_str, size_t ascii_len)
{
  ScmToken *token;

  token = scm_capi_malloc(sizeof(ScmToken));
  if (token == NULL) return NULL;

  token->type = type;
  token->raw.str = raw_str;
  token->raw.size = raw_size;
  token->raw.len = raw_len;
  token->ascii.str = ascii_str;
  token->ascii.len = ascii_len;

  return token;
}

static void
scm_token_end(ScmToken *token)
{
  scm_assert(token != NULL);

  scm_capi_free(token->raw.str);
  scm_capi_free(token->ascii.str);
  scm_capi_free(token);
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

static void
scm_lexer_setup_error_state(ScmLexer *lexer, SCM_LEXER_ERR_TYPE_T error)
{
  assert(lexer != NULL);

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_TOKENIZE_ERR);
  lexer->error_type = error;
}

static void
scm_lexer_clear_state(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  if (lexer->buf.head != NULL)
    lexer->buf.head = scm_capi_free(lexer->buf.head);
  lexer->buf.capacity = 0;
  lexer->buf.used = 0;
  lexer->buf.len = 0;

  if (lexer->ascii.head != NULL)
    lexer->ascii.head = scm_capi_free(lexer->ascii.head);
  lexer->ascii.capacity = 0;
  lexer->ascii.len = 0;

  lexer->token_type = SCM_TOKEN_TYPE_NONE;
}

static int
scm_lexer_expand_buffer_if_needed(ScmLexer *lexer,
                                  size_t needed_size,
                                  size_t needed_ascii_size)
{
  scm_assert(lexer != NULL);

  if (needed_size > 0) {
    size_t new_size;

    if (lexer->buf.head == NULL)
      lexer->buf.capacity = SCM_LEXER_INITIAL_BUFFER_SIZE;

    for (new_size = lexer->buf.capacity; new_size < needed_size; new_size *= 2)
      ;

    if (new_size > lexer->buf.capacity || lexer->buf.head == NULL) {
      void *new_buffer = scm_capi_realloc(lexer->buf.head, new_size);
      if (new_buffer == NULL)
        return -1;
      lexer->buf.head = new_buffer;
      lexer->buf.capacity = new_size;
    }
  }

  if (needed_ascii_size > 0) {
    size_t new_size;

    if (lexer->ascii.head == NULL)
      lexer->ascii.capacity = SCM_LEXER_INITIAL_BUFFER_SIZE;

    for (new_size = lexer->ascii.capacity;
         new_size < needed_ascii_size;
         new_size *= 2)
      ;

    if (new_size > lexer->ascii.capacity || lexer->ascii.head == NULL) {
      void *new_buffer = scm_capi_realloc(lexer->ascii.head, new_size);
      if (new_buffer == NULL)
        return -1;
      lexer->ascii.head = new_buffer;
      lexer->ascii.capacity = new_size;
    }
  }

  return 0;
}

static int
scm_lexer_push_char(ScmLexer *lexer, void *chr, size_t w, char ascii)
{
  int rslt;

  scm_assert(lexer != NULL);

  rslt =
    scm_lexer_expand_buffer_if_needed(lexer,
                                      (chr == NULL) ? 0 : lexer->buf.used + w,
                                      (ascii == '\0') ? 0 : lexer->ascii.len + 2);
  if (rslt < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_MEM_ERR);
    return -1;
  }

  if (chr != NULL) {
    memcpy((uint8_t *)lexer->buf.head + lexer->buf.used, chr, w);
    lexer->buf.used += w;
    lexer->buf.len++;
  }

  if (ascii != '\0') {
    lexer->ascii.head[lexer->ascii.len] = ascii;
    lexer->ascii.head[lexer->ascii.len + 1] = '\0';
    lexer->ascii.len++;
  }

  return 0;
}

ScmToken *
scm_lexer_new_token(ScmLexer *lexer)
{
  ScmToken *token;

  token = scm_token_new(lexer->token_type,
                        lexer->buf.head, lexer->buf.used, lexer->buf.len,
                        lexer->ascii.head, lexer->ascii.len);
  if (token != NULL) {
    lexer->buf.head = NULL;
    lexer->ascii.head = NULL;
    scm_lexer_clear_state(lexer);
  }
  else {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_MEM_ERR);
  }

  return token;
}

static int
scm_lexer_tokenize_init(ScmLexer *lexer, ScmObj port,
                        const ScmEncVirtualFunc *vf)
{
  const char *one_char_token_chars = "()[]'`";
  const SCM_TOKEN_TYPE_T one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  scm_char_t current, dummy;
  int ascii;
  ssize_t width;
  char *p;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(port, &current);

  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(port, dummy.bytes, 1);
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    return LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii < 0) {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
  else if (isspace(ascii)) {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    return LEXER_STATE_DISREGARD;
  }
  else if ((p = strchr(one_char_token_chars, ascii))) {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes,
                               (size_t)width, (char)ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_set_token_type(lexer,
                             one_char_token_types[p - one_char_token_chars]);
    return LEXER_STATE_DONE;
  }
  else if (IS_NUMERIC_START_CHAR(ascii)) {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes,
                               (size_t)width, (char)ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_NUMERIC;
  }
  else {
    switch (ascii) {
    case '.':
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_DOT;
    case ';':
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      return LEXER_STATE_COMMENT;
    case ',':
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_UNQUOTE;
    case '#':
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_NUMBER_SIGN;
    case '"':
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      return LEXER_STATE_STRING;
    default:
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_IDENTIFIER;
    }
  }
}

static int
scm_lexer_tokenize_identifier(ScmLexer *lexer, ScmObj port,
                              const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    ssize_t width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (ascii >= 0 && IDENTIFIER_DELIMITER_P(ascii)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      return LEXER_STATE_DONE;
    }
    else {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_dot(ScmLexer *lexer, ScmObj port,
                       const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii, rslt;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(port, &current);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii >= 0 && IDENTIFIER_DELIMITER_P(ascii)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }
  else {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
    if (rslt < 0) return LEXER_STATE_ERROR;
    return  LEXER_STATE_IDENTIFIER;
  }
}

static int
scm_lexer_tokenize_comment(ScmLexer *lexer, ScmObj port,
                           const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    ssize_t width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (ascii >= 0 && IS_LINE_FEED(ascii)) {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      return LEXER_STATE_DISREGARD;
    }
    else {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    }
  }
}

static int
scm_lexer_tokenize_unquote(ScmLexer *lexer, ScmObj port,
                           const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii, rslt;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(port, &current);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii == '@') {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes,
                               (size_t)width, (char)ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE_SPLICING);
    return LEXER_STATE_DONE;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }
}

static int
scm_lexer_tokenize_number_sign(ScmLexer *lexer, ScmObj port,
                               const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int state, ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(port, &current);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(port, dummy.bytes, 1);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  ascii = vf->to_ascii(current);
  if (ascii < 0) {
    scm_capi_read_raw(port, dummy.bytes, (size_t)width);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  switch (tolower(ascii)) {
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
    if (strchr("0123456789", ascii) != NULL) {
      state = LEXER_STATE_REFERENCE_DECL_OR_USE;
    }
    else if (strchr("bodxie", tolower(ascii)) != NULL) {
      state = LEXER_STATE_NUMERIC;
    }
    else {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      state = LEXER_STATE_ERROR;
    }
    break;
  }

  scm_capi_read_raw(port, dummy.bytes, (size_t)width);
  rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, (char)ascii);
  if (rslt < 0) return LEXER_STATE_ERROR;

  return state;
}

static int
scm_lexer_tokenize_string_escaped(ScmLexer *lexer, ScmObj port,
                                  const ScmEncVirtualFunc *vf)
{
  const char *control_chars_escape = "abtnvfr";
  const char *control_chars = "\a\b\t\n\v\f\r";
  scm_char_t current;
  ssize_t width;
  int ascii, rslt;
  char *p;


  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_read_char(port, &current);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  ascii = vf->to_ascii(current);
  if (ascii > 0 && (p = strchr(control_chars_escape, ascii))) {
    scm_char_t ctrl;
    width = vf->ascii_to(control_chars[p - control_chars_escape], &ctrl);
    rslt = scm_lexer_push_char(lexer, ctrl.bytes, (size_t)width, '\0');
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_STRING;
  }
  else {
    scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
    return LEXER_STATE_STRING;
  }
}

static int
scm_lexer_tokenize_string(ScmLexer *lexer, ScmObj port,
                          const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(port, dummy.bytes, 1);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (ascii < 0) {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
    else if (ascii == '"') {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
      return LEXER_STATE_DONE;
    }
    else if (ascii == '\\') {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      int st = scm_lexer_tokenize_string_escaped(lexer, port, vf);
      if (st != LEXER_STATE_STRING)
        return st;
    }
    else {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, '\0');
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_char(ScmLexer *lexer, ScmObj port,
                        const ScmEncVirtualFunc *vf)
{
  const char *delimiters = "()[]#'\" \t\n\r";
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(port, &current);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(port, dummy.bytes, 1);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  ascii = vf->to_ascii(current);
  scm_capi_read_raw(port, dummy.bytes, (size_t)width);
  rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width,
                             (char)((ascii < 0) ? '\0' : ascii));
  if (rslt < 0) return LEXER_STATE_ERROR;

  while (1) {
    width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (ascii >= 0 && strchr(delimiters, ascii) != NULL) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }
    else {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width,
                                 (char)((ascii < 0) ? '\0' : ascii));
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_reference(ScmLexer *lexer, ScmObj port,
                             const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(port, dummy.bytes, 1);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (ascii < 0) {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }
    else if (ascii == '#') {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_USE);;
      return LEXER_STATE_DONE;
    }
    else if (ascii == '=') {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_DECL);;
      return LEXER_STATE_DONE;
    }
    else if (isdigit(ascii)) {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
    else {
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_numeric(ScmLexer *lexer, ScmObj port,
                           const ScmEncVirtualFunc *vf)
{
  /* provisional implementation */
  scm_char_t current, dummy;
  ssize_t width;
  int state, ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  state = LEXER_STATE_IDENTIFIER;
  while (1) {
    width = scm_capi_peek_char(port, &current);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (ascii < 0) {
      return LEXER_STATE_IDENTIFIER;
    }
    else if (strchr("0123456789", ascii) != NULL) {
      state = LEXER_STATE_NUMERIC;
      scm_capi_read_raw(port, dummy.bytes, (size_t)width);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
    else if (strchr("()[]#'`,\" \t\n\r;", ascii) != NULL) {
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
  const ScmEncVirtualFunc *vf;
  SCM_ENC_T enc;
  int state;

  scm_assert(lexer != NULL);
  scm_assert(lexer->buf.used == 0);
  scm_assert(lexer->ascii.len == 0);
  scm_assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);
  scm_assert(scm_capi_input_port_p(port));

  if (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE)
    return -1;

  if (scm_capi_port_encoding(port, &enc) < 0)
    return -1;                           /* TODO: error handling */

  vf = SCM_ENCODING_VFUNC(enc);

  state = LEXER_STATE_INIT;
  while (state != LEXER_STATE_DONE && state != LEXER_STATE_ERROR) {
    switch (state) {
    case LEXER_STATE_INIT:
    case LEXER_STATE_DISREGARD:
      state = scm_lexer_tokenize_init(lexer, port, vf);
      break;
    case LEXER_STATE_IDENTIFIER:
      state = scm_lexer_tokenize_identifier(lexer, port, vf);
      break;
    case LEXER_STATE_DOT:
      state = scm_lexer_tokenize_dot(lexer, port, vf);
      break;
    case LEXER_STATE_COMMENT:
      state = scm_lexer_tokenize_comment(lexer, port, vf);
      break;
    case LEXER_STATE_UNQUOTE:
      state = scm_lexer_tokenize_unquote(lexer, port, vf);
      break;
    case LEXER_STATE_NUMBER_SIGN:
      state = scm_lexer_tokenize_number_sign(lexer, port, vf);
      break;
    case LEXER_STATE_STRING:
      state = scm_lexer_tokenize_string(lexer, port, vf);
      break;
    case LEXER_STATE_CHAR:
      state = scm_lexer_tokenize_char(lexer, port, vf);
      break;
    case LEXER_STATE_REFERENCE_DECL_OR_USE:
      state = scm_lexer_tokenize_reference(lexer, port, vf);
      break;
    case LEXER_STATE_NUMERIC:
      state = scm_lexer_tokenize_numeric(lexer, port, vf);
      break;
    default:
      assert(false); // must not happen
      break;
    }
  }

  if (lexer->error_type != SCM_LEXER_ERR_TYPE_PORT_ERR
      && lexer->error_type != SCM_LEXER_ERR_TYPE_MEM_ERR) {
    ScmToken *token = scm_lexer_new_token(lexer);
    if (token == NULL) return -1;

    scm_lexer_push_token(lexer, token);
  }

  return 0;
}


ScmLexer *
scm_lexer_new(void)
{
  ScmLexer *lexer;

  lexer = scm_capi_malloc(sizeof(ScmLexer));
  if (lexer == NULL) return NULL;

  lexer->buf.head = NULL;
  lexer->ascii.head = NULL;
  lexer->tokens_head = NULL;
  lexer->tokens_tail = NULL;

  scm_lexer_clear_state(lexer);
  scm_lexer_clear_error_state(lexer);

  return lexer;
}

ScmLexer *
scm_lexer_end(ScmLexer *lexer)
{
  assert(lexer != lexer);

  while (lexer->tokens_head != NULL)
    scm_lexer_shift_token(lexer);

  scm_lexer_clear_state(lexer);

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
        || token->type != SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      lexer->tokens_head = token->next;
      if (lexer->tokens_head == NULL)
        lexer->tokens_tail = NULL;
      scm_token_end(token);
    }
  }
}

void
scm_lexer_clear_error_state(ScmLexer *lexer)
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

static ScmObj
scm_parser_parse_list(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &car, &cdr);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL)  return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return scm_api_nil();
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car)) return SCM_OBJ_NULL; /* [ERR]: [through]] */
  if (scm_capi_eof_object_p(car)) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected eof */

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->type == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(parser->lexer);

    cdr = scm_parser_parse_expression(parser, port);
    if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL; /* [ERR]: [through] */
    if (scm_capi_eof_object_p(car)) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected eof */

    token = scm_lexer_head_token(parser->lexer, port);
    if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

    if (token->type == SCM_TOKEN_TYPE_TOKENIZE_ERR) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected character */
    if (token->type != SCM_TOKEN_TYPE_RPAREN) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected token */

    scm_lexer_shift_token(parser->lexer);
  }
  else {
    cdr = scm_parser_parse_list(parser, port);
    if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL; /* [ERR]: [through] */
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
  SCM_ENC_T enc;

  SCM_STACK_FRAME_PUSH(&port, &quote, &quoted);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  if (scm_capi_port_encoding(port, &enc) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  switch (token->type) {
  case SCM_TOKEN_TYPE_QUOTE:
    quote = scm_capi_make_symbol_from_cstr(quote_str, enc);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    quote = scm_capi_make_symbol_from_cstr(quasiquote_str, enc);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    quote = scm_capi_make_symbol_from_cstr(unquote_str, enc);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    quote = scm_capi_make_symbol_from_cstr(unquote_splicing_str, enc);
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

  if (scm_capi_null_value_p(quote)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  scm_lexer_shift_token(parser->lexer);

  quoted = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  if (scm_capi_eof_object_p(quoted)) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected eof */

  quoted = scm_api_cons(quoted, scm_api_nil());
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_api_cons(quote, quoted);
}

static ScmObj
scm_parser_parse_string(ScmParser *parser, ScmObj port)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmToken *token;
  SCM_ENC_T enc;

  SCM_STACK_FRAME_PUSH(&port, &str);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  if (scm_capi_port_encoding(port, &enc) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->raw.str == NULL)
    str = scm_capi_make_string_from_cstr("", enc);
  else
    str = scm_capi_make_string_from_bin(token->raw.str, token->raw.size, enc);

  scm_lexer_shift_token(parser->lexer);

  if (scm_capi_null_value_p(str)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return str;
}

static ScmObj
scm_parser_parse_identifier(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj sym = SCM_OBJ_INIT;
  SCM_ENC_T enc;

  SCM_STACK_FRAME_PUSH(&port, &sym);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  if (scm_capi_port_encoding(port, &enc) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL)  return SCM_OBJ_NULL; /* [ERR]: [through] */

  sym = scm_capi_make_symbol_from_bin(token->raw.str, token->raw.size, enc);
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
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR: [through] */

  str = scm_capi_malloc(token->ascii.len + 1);
  if (str == NULL)  return SCM_OBJ_NULL; /* [ERR: [through] */

  memcpy(str, token->ascii.str, token->ascii.len);
  str[token->ascii.len] = '\0';

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
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

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

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL)  return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return scm_api_nil();
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  if (scm_capi_eof_object_p(car)) return SCM_OBJ_NULL; /* [ERR]: parser: unexpected eof */

  *len += 1;

  cdr = scm_parser_parse_vector_aux(parser, port, len);
  if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL; /* [ERR]: [through] */

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

  scm_lexer_shift_token(parser->lexer);

  len = 0;
  elms = scm_parser_parse_vector_aux(parser, port, &len);
  if (scm_capi_null_value_p(elms)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  vec = scm_capi_make_vector(len);
  if (scm_capi_null_value_p(vec)) return SCM_OBJ_NULL; /* [ERR]: [through] */

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
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->ascii.len == sizeof("#\\newline") - 1 &&
      memcmp("#\\newline", token->ascii.str, token->ascii.len) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_newline();
  }
  else if (token->ascii.len == sizeof("#\\space") - 1 &&
           memcmp("#\\space", token->raw.str, token->raw.len) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_space();
  }
  else if (token->raw.len == 3) {
    SCM_ENC_T enc;
    const ScmEncVirtualFunc *vf;
    ScmStrItr itr;
    scm_char_t c;

    if (scm_capi_port_encoding(port, &enc) < 0) {
      /* TODO: error handling */
      scm_lexer_shift_token(parser->lexer);
      return SCM_OBJ_NULL;      /* [ERR]: [through] */
    }

    vf = SCM_ENCODING_VFUNC(enc);
    itr = vf->index2iter(token->raw.str, token->raw.size, 2);
    if (SCM_STR_ITR_IS_ERR(&itr)) {
      /* TODO: error handling */
      scm_lexer_shift_token(parser->lexer);
      return SCM_OBJ_NULL;      /* [ERR]: [through] */
    }

    memcpy(c.bytes, SCM_STR_ITR_PTR(&itr), (size_t)SCM_STR_ITR_WIDTH(&itr));
    chr = scm_capi_make_char(c);
  }
  else {
    // TODO: error handling
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;        /* [ERR]: parser: unknown character name */
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
  if (parser == NULL) return NULL; /* [ERR]: [through] */

  parser->lexer = lexer;

  return parser;
}

ScmObj
scm_parser_parse_expression(ScmParser *parser, ScmObj port)
{
  ScmObj rslt = SCM_OBJ_INIT;
  ScmToken *token;

  SCM_STACK_FRAME_PUSH(&port, &rslt);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  switch (token->type) {
  case SCM_TOKEN_TYPE_LPAREN:
    scm_lexer_shift_token(parser->lexer);
    rslt = scm_parser_parse_list(parser, port);
    break;
  case SCM_TOKEN_TYPE_RPAREN:
    /* error */
    /* TODO: error handling */
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;        /* [ERR]: parser: unexpected right paren */
    break;
  case SCM_TOKEN_TYPE_DOT:
    /* error */
    /* TODO: error handling */
    scm_lexer_shift_token(parser->lexer);
    return SCM_OBJ_NULL;        /* [ERR]: parser: unexpected dot */
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
    return SCM_OBJ_NULL;        /* [ERR]: parser: reference not supported */
    break;
  case SCM_TOKEN_TYPE_REFERENCE_USE:
    /* do not supported */
    /* TODO: error handling */
    return SCM_OBJ_NULL;        /* [ERR]: parser: reference not supported */
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
    return SCM_OBJ_NULL;        /* [ERR]: parser: unepected character after */
    break;
  case SCM_TOKEN_TYPE_NONE:     /* fall through */
  default:
    scm_assert(false); /* must not happen */
    break;
  }

  return rslt;
}
