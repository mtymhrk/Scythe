#ifndef INCLUDE_PORT_H__
#define INCLUDE_PORT_H__

#include <sys/types.h>
#include <unistd.h>
#include <iconv.h>

typedef struct ScmIORec ScmIO;
typedef struct ScmFileIORec ScmFileIO;
typedef struct ScmStringIORec ScmStringIO;
typedef struct ScmBufferedIORec ScmBufferedIO;
typedef struct ScmCharConvIORec ScmCharConvIO;
typedef struct ScmPortRec ScmPort;

#define SCM_PORT(obj) ((ScmPort *)(obj))

#include "object.h"
#include "encoding.h"
#include "api_enum.h"


/***************************************************************************/
/*  ScmIO                                                                  */
/***************************************************************************/

typedef enum {
  SCM_IO_MODE_READ,
  SCM_IO_MODE_WRITE
} SCM_IO_MODE_T;

typedef void (*ScmIOFinFunc)(ScmIO *io);
typedef ssize_t (*ScmIOReadFunc)(ScmIO *io, void *buf, size_t size);
typedef ssize_t (*ScmIOWriteFunc)(ScmIO *io, const void *buf, size_t size);
typedef off_t (*ScmIOSeekFunc)(ScmIO *io, off_t offset, int whence);
typedef int (*ScmIOCloseFunc)(ScmIO *io);
typedef int (*ScmIOReadyPFunc)(ScmIO *io);
typedef int (*ScmIOBuffModeFunc)(ScmIO *io, SCM_IO_MODE_T im,
                                 SCM_PORT_BUF_T *mode);
typedef ssize_t (*ScmIOBlkSizeFunc)(ScmIO *io);
typedef int (*ScmIOFlushFunc)(ScmIO *io);
typedef int (*ScmIOClearFunc)(ScmIO *io);

struct ScmIORec {
  ScmIOFinFunc fin_func;
  ScmIOReadFunc read_func;
  ScmIOWriteFunc write_func;
  ScmIOSeekFunc seek_func;
  ScmIOCloseFunc close_func;
  ScmIOReadyPFunc ready_p_func;
  ScmIOBuffModeFunc default_buf_mode_func;
  ScmIOBlkSizeFunc blk_size_func;
  ScmIOFlushFunc flush_func;
  ScmIOClearFunc clear_func;
};

struct ScmFileIORec {
  ScmIO io_base;
  int fd;
};

struct ScmStringIORec {
  ScmIO io_base;
  char *string;
  size_t capacity;
  size_t length;
  size_t pos;
};

struct ScmBufferedIORec {
  ScmIO io_base;
  ScmIO *io;
  char *buffer;
  size_t capacity;
  size_t pos;
  size_t used;
  bool eof_received_p;
};

struct ScmCharConvIORec {
  ScmIO io_base;
  ScmIO *io;
  iconv_t icd;
  char *unconverted;
  size_t uc_capacity;
  size_t uc_used;
  size_t uc_pos;
  bool uc_incomplete_p;
  char *converted;
  size_t co_capacity;
  size_t co_used;
  size_t co_pos;
  bool eof_received_p;
};

#ifdef SCM_UNIT_TEST

int scm_bufferedio_init_buffer(ScmBufferedIO *bufio, ScmIO *source);
int scm_port_init_encode(ScmObj port);

int scm_charconvio_init(ScmCharConvIO *ccio,
                        const char *tocode, const char *fromcode);
ssize_t scm_charconvio_read_from_io(ScmCharConvIO *ccio);
ssize_t scm_charconvio_conv_read(ScmCharConvIO *ccio,
                                 void *buf, size_t size, int *err);

#endif

void scm_io_initialize(ScmIO *io,
                       ScmIOFinFunc fin,
                       ScmIOReadFunc read,
                       ScmIOWriteFunc write,
                       ScmIOSeekFunc seek,
                       ScmIOCloseFunc close,
                       ScmIOReadyPFunc readyp,
                       ScmIOBuffModeFunc buff_mode,
                       ScmIOBlkSizeFunc blk_size,
                       ScmIOFlushFunc flush,
                       ScmIOClearFunc clear);
void scm_io_end(ScmIO *io);
ssize_t scm_io_read(ScmIO *io, void *buf, size_t size);
ssize_t scm_io_write(ScmIO *io, const void *buf, size_t size);
off_t scm_io_seek(ScmIO *io, off_t offset, int whence);
int scm_io_close(ScmIO *io);
int scm_io_ready_p(ScmIO *io);
int scm_io_buffer_mode(ScmIO *io, SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode);
ssize_t scm_io_block_size(ScmIO *io);
int scm_io_flush(ScmIO *io);
int scm_io_clear(ScmIO *io);

ScmFileIO *scm_fileio_new(int fd);
void scm_fileio_end(ScmFileIO *fileio);
ScmFileIO *scm_fileio_open(const char *pathname, int flags, mode_t mode);
ssize_t scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size);
ssize_t scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size);
int scm_fileio_ready_p(ScmFileIO *fileio);
off_t scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence);
int scm_fileio_close(ScmFileIO *fileio);
int scm_fileio_buffer_mode(ScmFileIO *fileio, SCM_IO_MODE_T im,
                           SCM_PORT_BUF_T *mode);
ssize_t scm_fileio_block_size(ScmFileIO *fileio);

ScmStringIO *scm_stringio_new(const char *str, size_t len);
void scm_stringio_end(ScmStringIO *strio);
ssize_t scm_stringio_read(ScmStringIO *strio, void *buf, size_t size);
ssize_t scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size);
int scm_stringio_ready_p(ScmStringIO *strio);
off_t scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence);
int scm_stringio_close(ScmStringIO *strio);
int scm_stringio_buffer_mode(ScmStringIO *strio, SCM_IO_MODE_T im,
                             SCM_PORT_BUF_T *mode);
char *scm_stringio_buffer(ScmStringIO *strio);
size_t scm_stringio_length(ScmStringIO *strio);

ScmBufferedIO *scm_bufferedio_new(ScmIO *io);
void scm_bufferedio_end(ScmBufferedIO *bufio);
ssize_t scm_bufferedio_read(ScmBufferedIO *bufio, void *buf, size_t size);
ssize_t scm_bufferedio_write(ScmBufferedIO *bufio, void *buf, size_t size);
int scm_bufferedio_ready_p(ScmBufferedIO *bufio);
off_t scm_bufferedio_seek(ScmBufferedIO *bufio, off_t offset, int whence);
int scm_bufferedio_close(ScmBufferedIO *bufio);
int scm_bufferedio_buffer_mode(ScmBufferedIO *bufio,
                               SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode);
ssize_t scm_bufferedio_block_size(ScmBufferedIO *bufio);
int scm_bufferedio_flush(ScmBufferedIO *bufio);
int scm_bufferedio_clear(ScmBufferedIO *bufio);

ScmCharConvIO *scm_charconvio_new(ScmIO *io,
                                  const char *tocode, const char *fromcode);
void scm_charconvio_end(ScmCharConvIO *ccio);
ssize_t scm_charconvio_read(ScmCharConvIO *ccio, void *buf, size_t size);
ssize_t scm_charconvio_write(ScmCharConvIO *ccio, const void *buf, size_t size);
int scm_charconvio_ready_p(ScmCharConvIO *ccio);
int scm_charconvio_close(ScmCharConvIO *ccio);
int scm_charconvio_buffer_mode(ScmCharConvIO *ccio,
                               SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode);
int scm_charconvio_flush(ScmCharConvIO *ccio);
int scm_charconvio_clear(ScmCharConvIO *ccio);


/***************************************************************************/
/*  ScmPort                                                                */
/***************************************************************************/

#define SCM_PORT_PUSHBACK_BUFF_SIZE 32 /* かなりテキトーな値 */

typedef enum {
  SCM_PORT_ATTR_INPUT       = 0x0001,
  SCM_PORT_ATTR_OUTPUT      = 0x0002,
  SCM_PORT_ATTR_BINARY      = 0x0004,
  SCM_PORT_ATTR_TEXTUAL     = 0x0008,
  SCM_PORT_ATTR_FILE        = 0x0010,
  SCM_PORT_ATTR_STRING      = 0x0020,
  SCM_PORT_ATTR_BUFFERED    = 0x0040,
  SCM_PORT_ATTR_CHARCONV    = 0x0080,
} SCM_PORT_ATTR;

struct ScmPortRec {
  ScmObjHeader header;
  ScmIO *io;
  SCM_PORT_BUF_T buf_mode;
  SCM_PORT_ATTR attr;
  bool closed_p;
  bool eof_received_p;
  uint8_t pushback[SCM_PORT_PUSHBACK_BUFF_SIZE];
  size_t pb_used;
  ScmEncoding *encoding;
};

extern ScmTypeInfo SCM_PORT_TYPE_INFO;

#ifdef SCM_UNIT_TEST

int scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_T buf_mode);
ssize_t scm_port_size_up_to_rearmost_lf(ScmObj port,
                                        const void *buf, size_t size);
uint8_t *scm_port_pushback_buff_head(ScmObj port);
size_t scm_port_pushback_buff_unused(ScmObj port);
ssize_t scm_port_read_from_pushback_buf(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_char_from_pushback_buf(ScmObj port, scm_char_t *chr);
ssize_t scm_port_read_from_io(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_buf(ScmObj port,
                          void *buf, size_t size, int mode);
ssize_t scm_port_read_into_pushback_buf(ScmObj port, size_t size);
ssize_t scm_port_read(ScmObj port, void *buf, size_t size);
ssize_t scm_port_write(ScmObj port, const void *buf, size_t size);

#endif

int scm_port_initialize(ScmObj port, ScmIO *io,
                        SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                        ScmEncoding *enc);
void scm_port_finalize(ScmObj port);
ScmObj scm_port_new(SCM_MEM_TYPE_T mtype,
                    ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                    ScmEncoding *enc);
ScmObj scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr,
                           SCM_PORT_BUF_T buf_mode, ScmEncoding *enc);
ScmObj scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr,
                            SCM_PORT_BUF_T buf_mode, ScmEncoding *enc);
ScmObj scm_port_open_input_fd(int fd,
                              SCM_PORT_BUF_T buf_mode, ScmEncoding *enc);
ScmObj scm_port_open_output_fd(int fd,
                               SCM_PORT_BUF_T buf_mode, ScmEncoding *enc);
ScmObj scm_port_open_input_file(const char *path, SCM_PORT_BUF_T buf_mode,
                                ScmEncoding *enc);
ScmObj scm_port_open_output_file(const char *path, SCM_PORT_BUF_T buf_mode,
                                 ScmEncoding *enc);
ScmObj scm_port_open_input_string(const void *string,
                                  size_t size, ScmEncoding *enc);
ScmObj scm_port_open_output_string(ScmEncoding *enc);
bool scm_port_input_port_p(ScmObj port);
bool scm_port_output_port_p(ScmObj port);
bool scm_port_textual_port_p(ScmObj port);
bool scm_port_binary_port_p(ScmObj port);
bool scm_port_buffered_port_p(ScmObj port);
bool scm_port_code_converted_port_p(ScmObj port);
bool scm_port_file_port_p(ScmObj port);
bool scm_port_string_port_p(ScmObj port);
bool scm_port_closed_p(ScmObj port);
bool scm_port_ready_p(ScmObj port);
ScmEncoding *scm_port_encoding(ScmObj port);
int scm_port_flush(ScmObj port);
int scm_port_close(ScmObj port);
ssize_t scm_port_read_bytes(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_line(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_pushback_bytes(ScmObj port, const void *buf, size_t size);
ssize_t scm_port_pushback_char(ScmObj port, const scm_char_t *chr);
ssize_t scm_port_peek_bytes(ScmObj port, void *buf, size_t size);
ssize_t scm_port_peek_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_write_bytes(ScmObj port, const void *buf, size_t size);
ssize_t scm_port_write_char(ScmObj port, scm_char_t chr);
int scm_port_seek(ScmObj port, off_t offset, int whence);
const void *scm_port_string_buffer(ScmObj port);
ssize_t scm_port_string_buffer_length(ScmObj port);
void scm_port_gc_initialize(ScmObj obj, ScmObj mem);
void scm_port_gc_finalize(ScmObj obj);

#endif /*  INCLUDE_PORT_H__ */
