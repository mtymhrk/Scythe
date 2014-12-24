#ifndef INCLUDE_FCD_PORT_H__
#define INCLUDE_FCD_PORT_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"

/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

bool scm_fcd_port_p(ScmObj obj);
ScmObj scm_fcd_port_P(ScmObj obj);
bool scm_fcd_file_port_p(ScmObj obj);
bool scm_fcd_string_port_p(ScmObj obj);
bool scm_fcd_input_port_p(ScmObj obj);
ScmObj scm_fcd_input_port_P(ScmObj obj);
bool scm_fcd_output_port_p(ScmObj obj);
ScmObj scm_fcd_output_port_P(ScmObj obj);
bool scm_fcd_textual_port_p(ScmObj obj);
ScmObj scm_fcd_textual_port_P(ScmObj obj);
bool scm_fcd_binary_port_p(ScmObj obj);
ScmObj scm_fcd_binary_port_P(ScmObj obj);
bool scm_fcd_input_port_open_p(ScmObj port);
ScmObj scm_fcd_input_port_open_P(ScmObj port);
bool scm_fcd_output_port_open_p(ScmObj port);
ScmObj scm_fcd_output_port_open_P(ScmObj port);
ScmObj scm_fcd_open_input_fd(int fd, const char *enc);
ScmObj scm_fcd_open_binary_input_fd(int fd);
ScmObj scm_fcd_open_output_fd(int fd, const char *enc);
ScmObj scm_fcd_open_binary_output_fd(int fd);
ScmObj scm_fcd_open_input_file(const char *path, const char *enc);
ScmObj scm_fcd_open_binary_input_file(const char *path);
ScmObj scm_fcd_open_output_file(const char *path, const char *enc);
ScmObj scm_fcd_open_binary_output_file(const char *path);
int scm_fcd_close_port(ScmObj port);
int scm_fcd_close_input_port(ScmObj port);
int scm_fcd_close_output_port(ScmObj port);
ScmObj scm_fcd_open_input_string_cstr(const char *str, const char *enc);
ScmObj scm_fcd_open_input_string(ScmObj str);
ScmObj scm_fcd_open_output_string(void);
ScmObj scm_fcd_get_output_string(ScmObj port);
ScmObj scm_fcd_open_input_bytevector_cbytes(const void *bytes, size_t size);
ScmObj scm_fcd_open_input_bytevector(ScmObj vec);
ScmObj scm_fcd_open_output_bytevector(void);
ScmObj scm_fcd_get_output_bytevector(ScmObj port);
off_t scm_fcd_port_seek(ScmObj port, off_t offset, int whence);
off_t scm_fcd_port_pos(ScmObj port);
const char *scm_fcd_port_encoding(ScmObj port);
ScmEncoding *scm_fcd_port_internal_encoding(ScmObj port);


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

ScmObj scm_fcd_read(ScmObj port);
ssize_t scm_fcd_read_cchr(scm_char_t *chr, ScmObj port);
ScmObj scm_fcd_read_char(ScmObj port);
ssize_t scm_fcd_peek_cchr(scm_char_t *chr, ScmObj port);
ScmObj scm_fcd_peek_char(ScmObj port);
ScmObj scm_fcd_read_line(ScmObj port);
int scm_fcd_char_ready(ScmObj port, bool *rslt);
ScmObj scm_fcd_char_ready_P(ScmObj port);
ScmObj scm_fcd_read_string(size_t n, ScmObj port);
ssize_t scm_fcd_read_cbytes(void *buf, size_t size, ScmObj port);


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

int scm_fcd_write_simple(ScmObj obj, ScmObj port);
int scm_fcd_write(ScmObj obj, ScmObj port);
int scm_fcd_display(ScmObj obj, ScmObj port);
int scm_fcd_newline(ScmObj port);
int scm_fcd_write_cchr(scm_char_t chr, ScmEncoding *enc, ScmObj port);
int scm_fcd_write_char(ScmObj chr, ScmObj port);
int scm_fcd_write_cstr(const char *str, ScmEncoding *enc, ScmObj port);
int scm_fcd_write_string(ScmObj str, ScmObj port, ssize_t start, ssize_t end);
int scm_fcd_write_cbytes(const void *bytes, size_t size, ScmObj port);
int scm_fcd_flush_output_port(ScmObj port);


#endif /* INCLUDE_FCD_PORT_H__ */
