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

#define NONASCII ((char)0xff)

#define WHITESPACE " \t\n\r"
#define DELIMITER (WHITESPACE "()[]\";|")

inline bool
whitespace_p(int c)
{
  return (strchr(WHITESPACE, c) != NULL);
}

inline bool
intraline_whitespace_p(int c)
{
  return (c == ' ' || c == '\t');
}

inline bool
delimiter_p(int c)
{
  return (strchr(DELIMITER, c) != NULL);
}

inline bool
sign_p(int c)
{
  return (c == '-' || c == '+');
}

inline bool
dec_digit_p(int c)
{
  return ('0' <= c && c <= '9');
}

typedef enum {
  LEXER_STATE_DONE,
  LEXER_STATE_INIT,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
  LEXER_STATE_IDENTIFIER_VBAR,
  LEXER_STATE_DOT,
  LEXER_STATE_SIGN,
  LEXER_STATE_UNQUOTE,
  LEXER_STATE_COMMENT,
  LEXER_STATE_NUMBER_SIGN,
  LEXER_STATE_BOOL_TRUE,
  LEXER_STATE_BOOL_FALSE,
  LEXER_STATE_STRING,
  LEXER_STATE_CHAR,
  LEXER_STATE_REFERENCE_DECL_OR_USE,
  LEXER_STATE_NUMERIC,
  LEXER_STATE_BYTEVECTOR_START,
  LEXER_STATE_ERROR
} LEXER_STATE_T;


static ScmToken *
scm_token_new(SCM_TOKEN_TYPE_T type,
              void *raw_str, size_t raw_size, size_t raw_len,
              char *ascii_str, size_t ascii_len)
{
  ScmToken *token;

  token = scm_capi_malloc(sizeof(ScmToken));
  if (token == NULL) return NULL; /* [ERR]: [thgourh] */

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
  scm_assert(lexer != NULL);
  lexer->token_type = type;
}

static void
scm_lexer_setup_error_state(ScmLexer *lexer, SCM_LEXER_ERR_TYPE_T error)
{
  scm_assert(lexer != NULL);

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
      if (new_buffer == NULL) return -1; /* [ERR]: [thorugh] */
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
      if (new_buffer == NULL) return -1; /* [ERR]: [through] */
      lexer->ascii.head = new_buffer;
      lexer->ascii.capacity = new_size;
    }
  }

  return 0;
}

static int
scm_lexer_push_char(ScmLexer *lexer, void *chr, size_t w, int ascii)
{
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(chr != NULL);

  rslt =
    scm_lexer_expand_buffer_if_needed(lexer,
                                      lexer->buf.used + w,
                                      lexer->ascii.len + 2);
  if (rslt < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_MEM_ERR);
    return -1;
  }

  memcpy((uint8_t *)lexer->buf.head + lexer->buf.used, chr, w);
  lexer->buf.used += w;
  lexer->buf.len++;

  if (ascii <= 0x7f)
    lexer->ascii.head[lexer->ascii.len] = (char)ascii;
  else
    lexer->ascii.head[lexer->ascii.len] = NONASCII;
  lexer->ascii.head[lexer->ascii.len + 1] = '\0';
  lexer->ascii.len++;

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

  width = scm_capi_peek_char(&current, port);

  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    return LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii < 0) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
  else if (whitespace_p(ascii)) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    return LEXER_STATE_DISREGARD;
  }
  else if ((p = strchr(one_char_token_chars, ascii))) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_set_token_type(lexer,
                             one_char_token_types[p - one_char_token_chars]);
    return LEXER_STATE_DONE;
  }
  else if (dec_digit_p(ascii)) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_NUMERIC;
  }
  else if (sign_p(ascii)) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_SIGN;
  }
  else {
    switch (ascii) {
    case ',':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_UNQUOTE;
    case '.':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_DOT;
    case ';':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      return LEXER_STATE_COMMENT;
    case '#':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_NUMBER_SIGN;
    case '"':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      return LEXER_STATE_STRING;
    case '|':
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      return LEXER_STATE_IDENTIFIER_VBAR;
    default:
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      return LEXER_STATE_IDENTIFIER;
    }
  }
}

static int
scm_lexer_tokenize_identifier(ScmLexer *lexer, ScmObj port,
                               const ScmEncVirtualFunc *vf)
{
  const char *exceptions[] = { "-i", "+i", "+inf.0", "-inf.0", "+nan.0", NULL };
  scm_char_t current, dummy;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    ssize_t width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      break;
    }

    ascii = vf->to_ascii(current);
    if (ascii >= 0 && delimiter_p(ascii)) {
      break;
    }
    else {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }

  for (const char **p = exceptions; *p != NULL; p++) {
    if (strcasecmp(*p, lexer->ascii.head) == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
  return LEXER_STATE_DONE;
}

static int
scm_lexer_tokenize_ident_vbar(ScmLexer *lexer, ScmObj port,
                              const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    ssize_t width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (ascii == '|') {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER_VBAR);
      return LEXER_STATE_DONE;
    }
    else {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }
  return 0;
}

static int
scm_lexer_tokenize_dot(ScmLexer *lexer, ScmObj port,
                       const ScmEncVirtualFunc *vf)
{
  scm_char_t current;
  int ascii;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii < 0) {
    return LEXER_STATE_IDENTIFIER;
  }
  else if (whitespace_p(ascii) || delimiter_p(ascii)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return LEXER_STATE_DONE;
  }
  else if (dec_digit_p(ascii)){
    return LEXER_STATE_NUMERIC;
  }
  else {
    return LEXER_STATE_IDENTIFIER;
  }
}

static int
scm_lexer_tokenize_sign(ScmLexer *lexer, ScmObj port,
                        const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  int ascii, rslt;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
    return  LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (ascii == '.') {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;

    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
      return  LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
  }

  if (dec_digit_p(ascii))
    return LEXER_STATE_NUMERIC;
  else
    return LEXER_STATE_IDENTIFIER;
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
    ssize_t width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (ascii == '\n' || ascii == '\r') {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      return LEXER_STATE_DISREGARD;
    }
    else {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
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

  width = scm_capi_peek_char(&current, port);
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
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
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

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  ascii = vf->to_ascii(current);
  if (ascii < 0) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  switch (tolower(ascii)) {
  case 't':                     /* fall through */
  case 'T':
    state = LEXER_STATE_BOOL_TRUE;
    break;
  case 'f':                     /* fall through */
  case 'F':
    state = LEXER_STATE_BOOL_FALSE;
    break;
  case 'u':
  case 'U':                     /* fall through */
    state = LEXER_STATE_BYTEVECTOR_START;
    break;
  case '\\':
    state = LEXER_STATE_CHAR;
    break;
  case '(':
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_VECTOR_START);
    state = LEXER_STATE_DONE;
    break;
  default:
    if (dec_digit_p(ascii)) {
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

  scm_capi_read_raw(dummy.bytes, (size_t)width, port);
  rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
  if (rslt < 0) return LEXER_STATE_ERROR;

  return state;
}

static int
scm_lexer_tokenize_bool_true_or_false(ScmLexer *lexer, ScmObj port,
                                      const ScmEncVirtualFunc *vf,
                                      const char *str,
                                      SCM_TOKEN_TYPE_T ttype)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;
  int idx;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);
  scm_assert(str != NULL);

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  idx = 0;

  ascii = vf->to_ascii(current);
  if (delimiter_p(ascii)) {
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else if (ascii < 0 || str[idx] != tolower(ascii)) {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  scm_capi_read_raw(dummy.bytes, (size_t)width, port);
  rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
  if (rslt < 0) return LEXER_STATE_ERROR;

  idx++;

  while (str[idx] != '\0') {
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (ascii < 0 || str[idx] != tolower(ascii)) {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;

    idx++;
  }

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (delimiter_p(ascii)) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else {
    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }
}

static int
scm_lexer_tokenize_bool_true(ScmLexer *lexer, ScmObj port,
                             const ScmEncVirtualFunc *vf)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, vf,
                                               "rue",
                                               SCM_TOKEN_TYPE_BOOL_TRUE);
}

static int
scm_lexer_tokenize_bool_false(ScmLexer *lexer, ScmObj port,
                              const ScmEncVirtualFunc *vf)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, vf,
                                               "alse",
                                               SCM_TOKEN_TYPE_BOOL_FALSE);
}

static int
scm_lexer_tokenize_string(ScmLexer *lexer, ScmObj port,
                          const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;
  bool escaped_p;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  escaped_p = false;
  while (1) {
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (escaped_p) {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = false;
    }
    else  {
      if (ascii == '"') {
        scm_capi_read_raw(dummy.bytes, (size_t)width, port);
        scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
        return LEXER_STATE_DONE;
      }
      else if (ascii == '\\') {
        scm_capi_read_raw(dummy.bytes, (size_t)width, port);
        rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
        if (rslt < 0) return LEXER_STATE_ERROR;
        escaped_p = true;
      }
      else {
        scm_capi_read_raw(dummy.bytes, (size_t)width, port);
        rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
        if (rslt < 0) return LEXER_STATE_ERROR;
      }
    }
  }
}

static int
scm_lexer_tokenize_char(ScmLexer *lexer, ScmObj port,
                        const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  ascii = vf->to_ascii(current);
  scm_capi_read_raw(dummy.bytes, (size_t)width, port);
  rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
  if (rslt < 0) return LEXER_STATE_ERROR;

  while (1) {
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (delimiter_p(ascii)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }
    else {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
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
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (ascii < 0) {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }
    else if (ascii == '#') {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_USE);;
      return LEXER_STATE_DONE;
    }
    else if (ascii == '=') {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_DECL);;
      return LEXER_STATE_DONE;
    }
    else if (dec_digit_p(ascii)) {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
      rslt = scm_lexer_push_char(lexer, current.bytes,
                                 (size_t)width, (char)ascii);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
    else {
      scm_capi_read_raw(dummy.bytes, (size_t)width, port);
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
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  while (1) {
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }

    ascii = vf->to_ascii(current);
    if (delimiter_p(ascii)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
      return LEXER_STATE_DONE;
    }

    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }
}

static int
scm_lexer_tokenize_bv_start(ScmLexer *lexer, ScmObj port,
                            const ScmEncVirtualFunc *vf)
{
  scm_char_t current, dummy;
  ssize_t width;
  int ascii, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(vf != NULL);

  for (const char *p = "8("; *p != '\0'; p++) {
    width = scm_capi_peek_char(&current, port);
    if (width < 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_raw(dummy.bytes, 1, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    ascii = vf->to_ascii(current);
    if (*p != ascii) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    scm_capi_read_raw(dummy.bytes, (size_t)width, port);
    rslt = scm_lexer_push_char(lexer, current.bytes, (size_t)width, ascii);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }

  width = scm_capi_peek_char(&current, port);
  if (width < 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_PORT_ERR);
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_raw(dummy.bytes, 1, port);
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BYTEVECTOR_START);
    return LEXER_STATE_DONE;
  }

  ascii = vf->to_ascii(current);
  if (!delimiter_p(ascii)) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BYTEVECTOR_START);
  return LEXER_STATE_DONE;
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
  LEXER_STATE_T state;

  scm_assert(lexer != NULL);
  scm_assert(lexer->buf.used == 0);
  scm_assert(lexer->ascii.len == 0);
  scm_assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);
  scm_assert(scm_capi_input_port_p(port));

  if (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE)
    return -1;

  enc = scm_capi_system_encoding();
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
    case LEXER_STATE_IDENTIFIER_VBAR:
      state = scm_lexer_tokenize_ident_vbar(lexer, port, vf);
      break;
    case LEXER_STATE_DOT:
      state = scm_lexer_tokenize_dot(lexer, port, vf);
      break;
    case LEXER_STATE_SIGN:
      state = scm_lexer_tokenize_sign(lexer, port, vf);
      break;
    case LEXER_STATE_UNQUOTE:
      state = scm_lexer_tokenize_unquote(lexer, port, vf);
      break;
    case LEXER_STATE_COMMENT:
      state = scm_lexer_tokenize_comment(lexer, port, vf);
      break;
    case LEXER_STATE_NUMBER_SIGN:
      state = scm_lexer_tokenize_number_sign(lexer, port, vf);
      break;
    case LEXER_STATE_BOOL_TRUE:
      state = scm_lexer_tokenize_bool_true(lexer, port, vf);
      break;
    case LEXER_STATE_BOOL_FALSE:
      state = scm_lexer_tokenize_bool_false(lexer, port, vf);
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
    case LEXER_STATE_BYTEVECTOR_START:
      state = scm_lexer_tokenize_bv_start(lexer, port, vf);
      break;
    case LEXER_STATE_DONE:      /* fall through */
    case LEXER_STATE_ERROR:     /* fall through */
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
  if (scm_capi_eof_object_p(car)) {
    scm_capi_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->type == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(parser->lexer);

    cdr = scm_parser_parse_expression(parser, port);
    if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL; /* [ERR]: [through] */
    if (scm_capi_eof_object_p(car)) {
      scm_capi_error("Parser: unexpected eof", 0);
      return SCM_OBJ_NULL;
    }

    token = scm_lexer_head_token(parser->lexer, port);
    if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

    if (token->type == SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      if (scm_lexer_error_type(parser->lexer)
          == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
        scm_capi_error("unexpected character", 0);
      }
      else {
        scm_capi_error("unexpected eof", 0);
      }
      return SCM_OBJ_NULL; /* [ERR]: parser: unexpected character */
    }
    else if (token->type != SCM_TOKEN_TYPE_RPAREN) {
      /* TODO: change error message */
      scm_capi_error("Parser: unexpected token", 0);
      return SCM_OBJ_NULL;
    }

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

  SCM_STACK_FRAME_PUSH(&port, &quote, &quoted);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  switch (token->type) {
  case SCM_TOKEN_TYPE_QUOTE:
    quote = scm_capi_make_symbol_from_cstr(quote_str, SCM_ENC_ASCII);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    quote = scm_capi_make_symbol_from_cstr(quasiquote_str, SCM_ENC_ASCII);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    quote = scm_capi_make_symbol_from_cstr(unquote_str, SCM_ENC_ASCII);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    quote = scm_capi_make_symbol_from_cstr(unquote_splicing_str, SCM_ENC_ASCII);
    break;
  case SCM_TOKEN_TYPE_NONE:               /* fall through */
  case SCM_TOKEN_TYPE_LPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_RPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_DOT:                /* fall through */
  case SCM_TOKEN_TYPE_STRING:             /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER:         /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER_VBAR:    /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_DECL:     /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_USE:      /* fall through */
  case SCM_TOKEN_TYPE_NUMERIC:            /* fall through */
  case SCM_TOKEN_TYPE_BOOL_TRUE:          /* fall through */
  case SCM_TOKEN_TYPE_BOOL_FALSE:         /* fall through */
  case SCM_TOKEN_TYPE_VECTOR_START:       /* fall through */
  case SCM_TOKEN_TYPE_BYTEVECTOR_START:   /* fall through */
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
  if (scm_capi_eof_object_p(quoted)) {
    scm_capi_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  quoted = scm_api_cons(quoted, scm_api_nil());
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_api_cons(quote, quoted);
}

static int
scm_parser_parse_inline_hex_escape(const char *str, size_t size,
                                   long long *scalar)
{
  const char *hex = "0123456789abcdefABCDEF";
  size_t i;

  scm_assert(str != NULL);
  scm_assert(scalar != NULL);

  if (size < 4 || str[0] != '\\' || str[1] != 'x') {
    scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
    return -1;
  }

  for (i = 2; i < size && str[i] != ';'; i++) {
    const char *p = strchr(hex, str[i]);
    if (p == NULL) {
      scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
      return -1;
    }

    if (*scalar > (LLONG_MAX >> 4)) {
      scm_capi_error("Paraser: too big scalar value", 0);
      return -1;
    }

    *scalar = *scalar << 4;
    if (p - hex < 0xf)
      *scalar |= (p - hex);
    else
      *scalar |= ((p - hex) - 6);
  }

  if (i == 2 || i >= size) {
    scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
    return -1;
  }

  return 0;
}

static ssize_t
scm_parser_parse_str_fold_eec_seq(const char *str, size_t size)
{
  size_t i;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (size < 3 || str[0] == '\\' || str[1] == ' ') {
    scm_capi_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  for (i = 2; i < size && str[i] != ' '; i++)
    ;

  if (i >= size) {
    scm_capi_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  if (str[i] == '\r') {
    i++;
    if (i < size && str[i] == '\n') i++;
  }
  else if (str[i] == '\n') {
    i++;
  }
  else {
    scm_capi_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  for (; i < size && str[i] != ' '; i++)
    ;

  return (ssize_t)i;
}

static ssize_t
scm_parser_parse_string_esc_seq(const char *str, size_t size, SCM_ENC_T enc,
                                scm_char_t *chr, size_t *skip)
{
  long long hv;
  int rslt;
  ssize_t width, sk;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(chr != NULL);
  scm_assert(skip != NULL);

  if (size < 2 || str[0] != '\\') {
    scm_capi_error("Parser: invalid escpase sequence", 0);
    return -1;
  }

  switch (str[1]) {
  case 'a':
    *chr = SCM_ENCODING_CONST_BELL_CHR(enc);
    *skip = 2;
    width = (ssize_t)SCM_ENCODING_CONST_BELL_WIDTH(enc);
    break;
  case 'b':
    *chr = SCM_ENCODING_CONST_BS_CHR(enc);
    *skip = 2;
    width = (ssize_t)SCM_ENCODING_CONST_BS_WIDTH(enc);
    break;
  case 't':
    *chr = SCM_ENCODING_CONST_TAB_CHR(enc);
    *skip = 2;
    width = (ssize_t)SCM_ENCODING_CONST_TAB_WIDTH(enc);
    break;
  case 'n':
    *chr = SCM_ENCODING_CONST_LF_CHR(enc);
    *skip = 2;
    width = (ssize_t)SCM_ENCODING_CONST_LF_WIDTH(enc);
    break;
  case 'r':
    *chr = SCM_ENCODING_CONST_CR_CHR(enc);
    *skip = 2;
    width = (ssize_t)SCM_ENCODING_CONST_CR_WIDTH(enc);
    break;
  case ' ':
    sk = scm_parser_parse_str_fold_eec_seq(str, size);
    if (sk < 0) return -1;  /* [ERR]:  [through] */
    *skip = (size_t)sk;
    width = 0;
    break;
  case 'x':
    rslt = scm_parser_parse_inline_hex_escape(str, size, &hv);
    if (rslt < 0) return -1;  /* [ERR]:  [through] */
    *skip = (size_t)rslt;
    width = SCM_ENCODING_VFUNC_SCALAR_TO(enc)(hv, chr);
    if (width < 0) {
      scm_capi_error("Parser: invalid scalar value", 0);
      return -1;
    }
    break;
  default:
    *skip = 1;
    width = 0;
    break;
  }

  return width;
}

int
scm_parser_unescape_string(ScmToken *token, ScmObj str,
                           const ScmEncVirtualFunc *vf)
{
  size_t idx, skip;
  ssize_t width;
  SCM_ENC_T enc;
  int rslt;
  scm_char_t chr;
  ScmStrItr iter;

  scm_assert(token != NULL);
  scm_assert(scm_capi_string_p(str));
  scm_assert(vf != NULL);

  enc = scm_capi_system_encoding();

  iter = scm_str_itr_begin(token->raw.str, token->raw.size, vf->char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  idx = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    if (token->ascii.str[idx] == '\\') {
      width = scm_parser_parse_string_esc_seq(token->ascii.str + idx,
                                              token->ascii.len - idx,
                                              enc, &chr, &skip);
      if (width < 0) return -1;  /* [ERR]:  [through] */
    }
    else {
      width = SCM_STR_ITR_WIDTH(&iter);
      scm_assert(width > 0);

      memcpy(chr.bytes, SCM_STR_ITR_PTR(&iter), (size_t)width);
      skip = 1;
    }

    if (width > 0) {
      rslt = scm_capi_string_push(str, chr, enc);
      if (rslt < 0) return -1;   /* [ERR]: [through] */
    }

    idx += skip;
    for (size_t i = 0; i < skip; i++) {
      scm_str_itr_next(&iter);
      scm_assert(!SCM_STR_ITR_IS_ERR(&iter));
    }
  }

  return 0;
}

static ScmObj
scm_parser_parse_string(ScmParser *parser, ScmObj port)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmToken *token;
  SCM_ENC_T enc;
  int rslt;
  const ScmEncVirtualFunc *vf;

  SCM_STACK_FRAME_PUSH(&port, &str);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  enc = scm_capi_system_encoding();
  vf = SCM_ENCODING_VFUNC(enc);

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  str = scm_capi_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  if (token->raw.str != NULL) {
    rslt = scm_parser_unescape_string(token, str, vf);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */
  }

  scm_lexer_shift_token(parser->lexer);

  return str;
}

static int
scm_parser_unescape_ident(ScmToken *token, ScmObj str,
                          const ScmEncVirtualFunc *vf)
{
  ScmStrItr iter;
  size_t idx;
  long long hv;
  int rslt;
  scm_char_t chr;
  ssize_t width, skip;
  SCM_ENC_T enc;

  scm_assert(token != NULL);
  scm_assert(scm_capi_string_p(str));
  scm_assert(vf != NULL);

  enc = scm_capi_system_encoding();

  iter = scm_str_itr_begin(token->raw.str, token->raw.size, vf->char_width);
  if (SCM_STR_ITR_IS_ERR(&iter)) return -1;

  idx = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    if (token->ascii.str[idx] == '\\') {
      skip = scm_parser_parse_inline_hex_escape(token->ascii.str + idx,
                                                token->ascii.len - idx,
                                                &hv);
      if (skip < 0) return -1;  /* [ERR]:  [through] */

      width = vf->scalar_to(hv, &chr);
      if (width < 0) {
        scm_capi_error("Parser: invalid scalar value", 0);
        return -1;
      }
    }
    else {
      width = SCM_STR_ITR_WIDTH(&iter);
      scm_assert(width > 0);

      memcpy(chr.bytes, SCM_STR_ITR_PTR(&iter), (size_t)width);
      skip = 1;
    }

    rslt = scm_capi_string_push(str, chr, enc);
    if (rslt < 0) return -1;   /* [ERR]: [through] */

    idx += (size_t)skip;
    for (int i = 0; i < skip; i++) {
      scm_str_itr_next(&iter);
      scm_assert(!SCM_STR_ITR_IS_ERR(&iter));
    }
  }

  return 0;
}

static ScmObj
scm_parser_parse_identifier(ScmParser *parser, ScmObj port)
{
  ScmToken *token;
  ScmObj str = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  SCM_ENC_T enc;
  int rslt;
  const ScmEncVirtualFunc *vf;

  SCM_STACK_FRAME_PUSH(&port, &str, &sym);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  enc = scm_capi_system_encoding();
  vf = SCM_ENCODING_VFUNC(enc);

  str = scm_capi_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL; /* [ERR: [through] */

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL)  return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->raw.str != NULL) {
    rslt = scm_parser_unescape_ident(token, str, vf);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR: [through] */
  }

  sym = scm_api_string_to_symbol(str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  scm_lexer_shift_token(parser->lexer);

  return sym;
}

static ScmObj
scm_parser_parse_numeric(ScmParser *parser, ScmObj port)
{
  ScmObj num;
  ScmToken *token;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  num = scm_capi_make_number_from_literal(token->ascii.str, token->ascii.len);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  scm_lexer_shift_token(parser->lexer);

  return num;
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
    return scm_api_true();
  else
    return scm_api_false();
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
  if (scm_capi_eof_object_p(car)) {
    scm_capi_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

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

  vec = scm_capi_make_vector(len, SCM_OBJ_NULL);
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
  SCM_ENC_T enc;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  enc = scm_capi_system_encoding();

  token = scm_lexer_head_token(parser->lexer, port);
  if (token == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (token->ascii.len == sizeof("#\\newline") - 1 &&
      memcmp("#\\newline", token->ascii.str, token->ascii.len) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_newline(enc);
  }
  else if (token->ascii.len == sizeof("#\\space") - 1 &&
           memcmp("#\\space", token->raw.str, token->raw.len) == 0) {
    /* TODO: 現状、小文字しか受け付けていないが、case insensitive にする */
    chr = scm_api_make_char_space(enc);
  }
  else if (token->raw.len == 3) {
    const ScmEncVirtualFunc *vf;
    ScmStrItr itr;
    scm_char_t c;

    vf = SCM_ENCODING_VFUNC(enc);
    itr = vf->index2iter(token->raw.str, token->raw.size, 2);
    if (SCM_STR_ITR_IS_ERR(&itr)) {
      /* TODO: error handling */
      scm_lexer_shift_token(parser->lexer);
      return SCM_OBJ_NULL;      /* [ERR]: [through] */
    }

    memcpy(c.bytes, SCM_STR_ITR_PTR(&itr), (size_t)SCM_STR_ITR_WIDTH(&itr));
    chr = scm_capi_make_char(c, enc);
  }
  else {
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unknown character name", 0);
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
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unexpeted right paren", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_DOT:
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unexpeted dot", 0);
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
  case SCM_TOKEN_TYPE_IDENTIFIER_VBAR: /* fall through */
    rslt = scm_parser_parse_identifier(parser, port);
    break;
  case SCM_TOKEN_TYPE_REFERENCE_DECL:
    /* do not supported */
    scm_capi_error("Parser: reference not supported", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_REFERENCE_USE:
    /* do not supported */
    scm_capi_error("Parser: reference not supported", 0);
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
  case SCM_TOKEN_TYPE_BYTEVECTOR_START:
    scm_capi_error("Parser: byte vector  not supported", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_CHAR:
    rslt = scm_parser_parse_char(parser, port);
    break;
  case SCM_TOKEN_TYPE_EOF:
    rslt = scm_parser_parse_eof(parser, port);
    break;
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:
    scm_lexer_shift_token(parser->lexer);
    if (scm_lexer_error_type(parser->lexer)
        == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
      scm_capi_error("Parser: unexpected char", 0);
    }
    else {
      scm_capi_error("Parser: unexpected eof", 0);
    }
    return SCM_OBJ_NULL;        /* [ERR]: parser: unepected character after */
    break;
  case SCM_TOKEN_TYPE_NONE:     /* fall through */
  default:
    scm_assert(false); /* must not happen */
    break;
  }

  return rslt;
}
