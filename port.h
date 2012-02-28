#ifndef INCLUDE_PORT_H__
#define INCLUDE_PORT_H__

#include <sys/types.h>
#include <unistd.h>

typedef struct ScmIORec ScmIO;
typedef struct ScmFileIORec ScmFileIO;
typedef struct ScmStringIORec ScmStringIO;
typedef struct ScmCharConvIORec ScmCharConvIO;
typedef struct ScmPortRec ScmPort;

#define SCM_PORT(obj) ((ScmPort *)(obj))

typedef enum {
  SCM_PORT_ATTR_READABLE    = 0x0001,
  SCM_PORT_ATTR_WRITABLE    = 0x0002,
  SCM_PORT_ATTR_FILE        = 0x0004,
  SCM_PORT_ATTR_STRING      = 0x0008,
} SCM_PORT_ATTR;


#include "object.h"
#include "encoding.h"
#include "api_enum.h"
#include "api.h"


/***************************************************************************/
/*  ScmIO                                                                  */
/***************************************************************************/

typedef void (*ScmIOFinFunc)(ScmIO *io);
typedef ssize_t (*ScmIOReadFunc)(ScmIO *io, void *buf, size_t size);
typedef ssize_t (*ScmIOWriteFunc)(ScmIO *io, const void *buf, size_t size);
typedef off_t (*ScmIOSeekFunc)(ScmIO *io, off_t offset, int whence);
typedef int (*ScmIOCloseFunc)(ScmIO *io);
typedef int (*ScmIOReadyPFunc)(ScmIO *io);
typedef SCM_PORT_BUF_T (*ScmIOBuffModeFunc)(ScmIO *io);
typedef ssize_t (*ScmIOBlkSizeFunc)(ScmIO *io);


struct ScmIORec {
  ScmIOFinFunc fin_func;
  ScmIOReadFunc read_func;
  ScmIOWriteFunc write_func;
  ScmIOSeekFunc seek_func;
  ScmIOCloseFunc close_func;
  ScmIOReadyPFunc ready_p_func;
  ScmIOBuffModeFunc default_buf_mode_func;
  ScmIOBlkSizeFunc blk_size_func;
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

void scm_io_initialize(ScmIO *io,
                       ScmIOFinFunc fin,
                       ScmIOReadFunc read,
                       ScmIOWriteFunc write,
                       ScmIOSeekFunc seek,
                       ScmIOCloseFunc close,
                       ScmIOReadyPFunc readyp,
                       ScmIOBuffModeFunc buff_mode,
                       ScmIOBlkSizeFunc blk_size);
void scm_io_end(ScmIO *io);
ssize_t scm_io_read(ScmIO *io, void *buf, size_t size);
ssize_t scm_io_write(ScmIO *io, const void *buf, size_t size);
bool scm_io_seek(ScmIO *io, off_t offset, int whence);
int scm_io_close(ScmIO *io);
int scm_io_ready_p(ScmIO *io);
SCM_PORT_BUF_T scm_io_default_buffer_mode(ScmIO *io);
ssize_t scm_io_block_size(ScmIO *io);

ScmFileIO *scm_fileio_new(int fd);
void scm_fileio_end(ScmFileIO *fileio);
ScmFileIO *scm_fileio_open(const char *pathname, int flags, mode_t mode);
ssize_t scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size);
ssize_t scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size);
int scm_fileio_ready_p(ScmFileIO *fileio);
off_t scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence);
int scm_fileio_close(ScmFileIO *fileio);
SCM_PORT_BUF_T scm_fileio_default_buffer_mode(ScmFileIO *fileio);
ssize_t scm_fileio_block_size(ScmFileIO *fileio);

ScmStringIO *scm_stringio_new(const char *str, size_t len);
void scm_stringio_end(ScmStringIO *strio);
ssize_t scm_stringio_read(ScmStringIO *strio, void *buf, size_t size);
ssize_t scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size);
int scm_stringio_ready_p(ScmStringIO *strio);
off_t scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence);
int scm_stringio_close(ScmStringIO *strio);
SCM_PORT_BUF_T scm_stringio_default_buffer_mode(ScmStringIO *strio);
char *scm_stringio_buffer(ScmStringIO *strio);
size_t scm_stringio_length(ScmStringIO *strio);


/***************************************************************************/
/*  ScmPort                                                                */
/***************************************************************************/

#define SCM_PORT_PUSHBACK_BUFF_SIZE 32 /* かなりテキトーな値 */

extern ScmTypeInfo SCM_PORT_TYPE_INFO;

struct ScmPortRec {
  ScmObjHeader header;
  ScmIO *io;
  SCM_PORT_BUF_T buffer_mode;
  SCM_PORT_ATTR attr;
  char *buffer;
  size_t capacity;
  size_t pos;
  size_t used;
  bool closed_p;
  bool eof_received_p;
  uint8_t pushback[SCM_PORT_PUSHBACK_BUFF_SIZE];
  size_t pb_used;
  SCM_ENC_T encoding;
};

#ifdef SCM_UNIT_TEST

int scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_T buf_mode);
bool scm_port_buffer_empty_p(ScmObj port);
ssize_t scm_port_read_from_buffer(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_into_buffer(ScmObj port);
ssize_t scm_port_size_up_to_lf(ScmObj port, const void *buf, size_t size);
uint8_t *scm_port_pushback_buff_head(ScmObj port);
size_t scm_port_pushback_buff_unused(ScmObj port);
ssize_t scm_port_read_from_pushback_buf(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_line_from_pushback_buf(ScmObj port,
                                             void *buf, size_t size,
                                             bool *lf_exists_p);
ssize_t scm_port_read_char_from_pushback_buf(ScmObj port, scm_char_t *chr);
ssize_t scm_port_read_buf(ScmObj port,
                          void *buf, size_t size, int mode);
ssize_t scm_port_read_line_nonbuf(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_nonbuf(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_into_pushback_buf(ScmObj port, size_t size);
ssize_t scm_port_write_buf(ScmObj port,
                           const void *buf, size_t size);

#endif

void scm_port_initialize(ScmObj port, ScmIO *io,
                         SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                         SCM_ENC_T enc);
void scm_port_finalize(ScmObj port);
ScmObj scm_port_new(SCM_CAPI_MEM_TYPE_T mtype,
                    ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                    SCM_ENC_T enc);
ScmObj scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr,
                           SCM_PORT_BUF_T buf_mode, SCM_ENC_T enc);
ScmObj scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr,
                            SCM_PORT_BUF_T buf_mode, SCM_ENC_T enc);
ScmObj scm_port_open_input_fd(int fd,
                              SCM_PORT_BUF_T buf_mode, SCM_ENC_T enc);
ScmObj scm_port_open_output_fd(int fd,
                               SCM_PORT_BUF_T buf_mode, SCM_ENC_T enc);
ScmObj scm_port_open_input_file(const char *path, SCM_PORT_BUF_T buf_mode,
                                SCM_ENC_T enc);
ScmObj scm_port_open_output_file(const char *path, SCM_PORT_BUF_T buf_mode,
                                 SCM_ENC_T enc);
ScmObj scm_port_open_input_string(const void *string,
                                  size_t size, SCM_ENC_T enc);
ScmObj scm_port_open_output_string(SCM_ENC_T enc);
bool scm_port_readable_p(ScmObj port);
bool scm_port_writable_p(ScmObj port);
bool scm_port_file_port_p(ScmObj port);
bool scm_port_string_port_p(ScmObj port);
bool scm_port_closed_p(ScmObj port);
bool scm_port_ready_p(ScmObj port);
SCM_ENC_T scm_port_encoding(ScmObj port);
int scm_port_flush(ScmObj port);
int scm_port_close(ScmObj port);
ssize_t scm_port_read(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_line(ScmObj port, void *buf, size_t size);
ssize_t scm_port_read_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_pushback(ScmObj port, const void *buf, size_t size);
ssize_t scm_port_pushback_char(ScmObj port, const scm_char_t *chr);
ssize_t scm_port_peek(ScmObj port, void *buf, size_t size);
ssize_t scm_port_peek_char(ScmObj port, scm_char_t *chr);
ssize_t scm_port_write(ScmObj port, const void *buf, size_t size);
int scm_port_seek(ScmObj port, off_t offset, int whence);
void *scm_port_string_buffer(ScmObj port);
ssize_t scm_port_string_buffer_length(ScmObj port);
void scm_port_gc_finalize(ScmObj obj);

#endif /*  INCLUDE_PORT_H__ */
