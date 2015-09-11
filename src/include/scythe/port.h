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

typedef enum scm_port_buf scm_port_buf_t;
typedef enum scm_io_mode scm_io_mode_t;

#define SCM_PORT(obj) ((ScmPort *)(obj))

#include "scythe/object.h"
#include "scythe/encoding.h"


enum scm_port_buf {
  SCM_PORT_BUF_FULL,
  SCM_PORT_BUF_LINE,
  SCM_PORT_BUF_MODEST,
  SCM_PORT_BUF_NONE,
  SCM_PORT_BUF_DEFAULT,
};

#define SCM_PORT_NR_BUF_MODE (SCM_PORT_BUF_DEFAULT + 1)


/***************************************************************************/
/*  ScmIO                                                                  */
/***************************************************************************/

enum scm_io_mode {
  SCM_IO_MODE_READ,
  SCM_IO_MODE_WRITE
};

typedef void (*ScmIOFinFunc)(ScmIO *io);
typedef ssize_t (*ScmIOReadFunc)(ScmIO *io, void *buf, size_t size);
typedef ssize_t (*ScmIOWriteFunc)(ScmIO *io, const void *buf, size_t size);
typedef off_t (*ScmIOSeekFunc)(ScmIO *io, off_t offset, int whence);
typedef off_t (*ScmIOPosFunc)(ScmIO *io);
typedef int (*ScmIOCloseFunc)(ScmIO *io);
typedef int (*ScmIOReadyPFunc)(ScmIO *io);
typedef int (*ScmIOBuffModeFunc)(ScmIO *io, scm_io_mode_t im,
                                 scm_port_buf_t *mode);
typedef ssize_t (*ScmIOBlkSizeFunc)(ScmIO *io);
typedef int (*ScmIOFlushFunc)(ScmIO *io);
typedef int (*ScmIOClearFunc)(ScmIO *io);
typedef ScmIO *(*ScmIOLowerFunc)(ScmIO *io);

struct ScmIORec {
  ScmIOFinFunc fin_func;
  ScmIOReadFunc read_func;
  ScmIOWriteFunc write_func;
  ScmIOSeekFunc seek_func;
  ScmIOPosFunc pos_func;
  ScmIOCloseFunc close_func;
  ScmIOReadyPFunc ready_p_func;
  ScmIOBuffModeFunc default_buf_mode_func;
  ScmIOBlkSizeFunc blk_size_func;
  ScmIOFlushFunc flush_func;
  ScmIOClearFunc clear_func;
  ScmIOLowerFunc lower_func;
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

enum { SCM_BUFFEREDIO_ST_NONE,
       SCM_BUFFEREDIO_ST_READ,
       SCM_BUFFEREDIO_ST_WRITE };

struct ScmBufferedIORec {
  ScmIO io_base;
  ScmIO *io;
  char *buffer;
  size_t capacity;
  size_t pos;
  size_t used;
  bool eof_received_p;
  int stat;
};

struct ScmCharConvIORec {
  ScmIO io_base;
  ScmIO *io;
  scm_io_mode_t im;
  ScmEncCnv cnv;
  char *unconverted;
  bool uc_incomplete_p;
  char *save;
  size_t sv_pos;
  size_t sv_size;
  bool eof_received_p;
};


void scm_io_initialize(ScmIO *io,
                       ScmIOFinFunc fin,
                       ScmIOReadFunc read,
                       ScmIOWriteFunc write,
                       ScmIOSeekFunc seek,
                       ScmIOPosFunc pos,
                       ScmIOCloseFunc close,
                       ScmIOReadyPFunc readyp,
                       ScmIOBuffModeFunc buff_mode,
                       ScmIOBlkSizeFunc blk_size,
                       ScmIOFlushFunc flush,
                       ScmIOClearFunc clear,
                       ScmIOLowerFunc lower);
void scm_io_end(ScmIO *io);
ssize_t scm_io_read(ScmIO *io, void *buf, size_t size);
ssize_t scm_io_write(ScmIO *io, const void *buf, size_t size);
off_t scm_io_seek(ScmIO *io, off_t offset, int whence);
off_t scm_io_pos(ScmIO *io);
int scm_io_close(ScmIO *io);
int scm_io_ready_p(ScmIO *io);
int scm_io_buffer_mode(ScmIO *io, scm_io_mode_t im, scm_port_buf_t *mode);
ssize_t scm_io_block_size(ScmIO *io);
int scm_io_flush(ScmIO *io);
int scm_io_clear(ScmIO *io);
ScmIO *scm_io_lower(ScmIO *io);

ScmFileIO *scm_fileio_new(int fd);
void scm_fileio_end(ScmFileIO *fileio);
ScmFileIO *scm_fileio_open(const char *pathname, int flags, mode_t mode);
ssize_t scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size);
ssize_t scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size);
int scm_fileio_ready_p(ScmFileIO *fileio);
off_t scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence);
off_t scm_fileio_pos(ScmFileIO *fileio);
int scm_fileio_close(ScmFileIO *fileio);
int scm_fileio_buffer_mode(ScmFileIO *fileio, scm_io_mode_t im,
                           scm_port_buf_t *mode);
ssize_t scm_fileio_block_size(ScmFileIO *fileio);

ScmStringIO *scm_stringio_new(const char *str, size_t len);
void scm_stringio_end(ScmStringIO *strio);
ssize_t scm_stringio_read(ScmStringIO *strio, void *buf, size_t size);
ssize_t scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size);
int scm_stringio_ready_p(ScmStringIO *strio);
off_t scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence);
off_t scm_stringio_pos(ScmStringIO *strio);
int scm_stringio_close(ScmStringIO *strio);
int scm_stringio_buffer_mode(ScmStringIO *strio, scm_io_mode_t im,
                             scm_port_buf_t *mode);
char *scm_stringio_buffer(ScmStringIO *strio);
size_t scm_stringio_length(ScmStringIO *strio);
char *scm_stringio_chuck_buffer(ScmStringIO *strio);

ScmBufferedIO *scm_bufferedio_new(ScmIO *io);
void scm_bufferedio_end(ScmBufferedIO *bufio);
ssize_t scm_bufferedio_read(ScmBufferedIO *bufio, void *buf, size_t size);
ssize_t scm_bufferedio_write(ScmBufferedIO *bufio, void *buf, size_t size);
int scm_bufferedio_ready_p(ScmBufferedIO *bufio);
off_t scm_bufferedio_seek(ScmBufferedIO *bufio, off_t offset, int whence);
off_t scm_bufferedio_pos(ScmBufferedIO *bufio);
int scm_bufferedio_close(ScmBufferedIO *bufio);
int scm_bufferedio_buffer_mode(ScmBufferedIO *bufio,
                               scm_io_mode_t im, scm_port_buf_t *mode);
ssize_t scm_bufferedio_block_size(ScmBufferedIO *bufio);
int scm_bufferedio_flush(ScmBufferedIO *bufio);
int scm_bufferedio_clear(ScmBufferedIO *bufio);
ScmIO *scm_bufferedio_lower(ScmBufferedIO *bufio);

ScmCharConvIO *scm_charconvio_new(ScmIO *io, scm_io_mode_t mode,
                                  const char *incode, const char *extcode);
void scm_charconvio_end(ScmCharConvIO *ccio);
ssize_t scm_charconvio_read(ScmCharConvIO *ccio, void *buf, size_t size);
ssize_t scm_charconvio_write(ScmCharConvIO *ccio, const void *buf, size_t size);
int scm_charconvio_ready_p(ScmCharConvIO *ccio);
int scm_charconvio_close(ScmCharConvIO *ccio);
int scm_charconvio_buffer_mode(ScmCharConvIO *ccio,
                               scm_io_mode_t im, scm_port_buf_t *mode);
int scm_charconvio_flush(ScmCharConvIO *ccio);
int scm_charconvio_clear(ScmCharConvIO *ccio);
ScmIO *scm_charconvio_lower(ScmCharConvIO *ccio);


/***************************************************************************/
/*  ScmPort                                                                */
/***************************************************************************/

#define SCM_PORT_PUSHBACK_BUFF_SIZE 32 /* かなりテキトーな値 */
#define SCM_PORT_ENCODING_NAME_SIZE 64

enum {
  SCM_PORT_ATTR_INPUT       = 0x0001,
  SCM_PORT_ATTR_OUTPUT      = 0x0002,
  SCM_PORT_ATTR_BINARY      = 0x0004,
  SCM_PORT_ATTR_TEXTUAL     = 0x0008,
  SCM_PORT_ATTR_FILE        = 0x0010,
  SCM_PORT_ATTR_STRING      = 0x0020,
  SCM_PORT_ATTR_BUFFERED    = 0x0040,
  SCM_PORT_ATTR_CHARCONV    = 0x0080,
};

enum {
  SCM_PORT_OFLG_TRUNC  = 0x0001,
  SCM_PORT_OFLG_APPEND = 0x0002,
  SCM_PORT_OFLG_CREATE = 0x0004,
  SCM_PORT_OFLG_EXCL   = 0x0008,
};

struct ScmPortRec {
  ScmObjHeader header;
  ScmIO *io;
  scm_port_buf_t buf_mode;
  unsigned int attr;
  bool closed_p;
  bool eof_received_p;
  uint8_t pushback[SCM_PORT_PUSHBACK_BUFF_SIZE];
  size_t pb_used;
  ScmEncoding *inn_enc;
  char encoding[SCM_PORT_ENCODING_NAME_SIZE];
};

extern ScmTypeInfo SCM_PORT_TYPE_INFO;


int scm_port_initialize(ScmObj port, ScmIO *io,
                        unsigned int attr, scm_port_buf_t buf_mode,
                        ScmEncoding *inn_enc, const char *enc);
void scm_port_finalize(ScmObj port);
ScmObj scm_port_new(scm_mem_type_t mtype,
                    ScmIO *io, unsigned int attr, scm_port_buf_t buf_mode,
                    ScmEncoding *inn_enc, const char *enc);
ScmObj scm_port_open_fd_inter(int fd, unsigned int attr,
                              scm_port_buf_t buf_mode,
                              ScmEncoding *inn_enc, const char *enc);
ScmObj scm_port_open_fd(int fd, const char *mode, scm_port_buf_t buf_mode,
                        ScmEncoding *inn_enc, const char *enc);
ScmObj scm_port_open_file_inter(const char *path, unsigned int attr,
                                unsigned int oflg, scm_port_buf_t buf_mode,
                                mode_t perm,
                                ScmEncoding *inn_enc, const char *enc);
ScmObj scm_port_open_file(const char *path, const char *mode,
                          scm_port_buf_t buf_mode, mode_t perm,
                          ScmEncoding *inn_enc, const char *enc);
ScmObj scm_port_open_string(const void *string, size_t size, const char *mode,
                            ScmEncoding *inn_enc, const char *enc);
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
int scm_port_char_ready(ScmObj port, bool *rslt);
ScmEncoding *scm_port_internal_enc(ScmObj port);
const char *scm_port_external_enc(ScmObj port);
int scm_port_flush(ScmObj port);
int scm_port_close(ScmObj port);
ssize_t scm_port_read_bytes(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_read_line(ScmObj port, ScmIO *io);
ssize_t scm_port_read_string(size_t n, ScmObj port, ScmIO *io);
ssize_t scm_port_pushback_bytes(ScmObj port, const void *buf, size_t size);
ssize_t scm_port_pushback_char(ScmObj port, const scm_char_t *chr);
ssize_t scm_port_peek_bytes(ScmObj port, void *buf, size_t size);
ssize_t scm_port_peek_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_write_bytes(ScmObj port, const void *buf, size_t size);
ssize_t scm_port_write_char(ScmObj port, scm_char_t chr);
off_t scm_port_seek(ScmObj port, off_t offset, int whence);
off_t scm_port_pos(ScmObj port);
const void *scm_port_string_buffer(ScmObj port);
ssize_t scm_port_string_buffer_length(ScmObj port);
void scm_port_gc_initialize(ScmObj obj);
void scm_port_gc_finalize(ScmObj obj);

#endif /*  INCLUDE_PORT_H__ */
