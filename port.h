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

#include "object.h"

extern ScmTypeInfo SCM_PORT_TYPE_INFO;

typedef enum {
  SCM_PORT_BUF_FULL,
  SCM_PORT_BUF_LINE,
  SCM_PORT_BUF_MODEST,
  SCM_PORT_BUF_NONE,
  SCM_PORT_BUF_DEFAULT,
} SCM_PORT_BUF_MODE;

enum { SCM_PORT_NR_BUF_MODE = SCM_PORT_BUF_DEFAULT + 1 };

typedef enum {
  SCM_PORT_ATTR_READABLE    = 0x0001,
  SCM_PORT_ATTR_WRITABLE    = 0x0002,
  SCM_PORT_ATTR_FILE        = 0x0004,
  SCM_PORT_ATTR_STRING      = 0x0008,
  SCM_PORT_ATTR_DESTRUCT_IO = 0x0010,
} SCM_PORT_ATTR;


struct ScmPortRec {
  ScmObjHeader header;
  ScmIO *io;
  SCM_PORT_BUF_MODE buffer_mode;
  SCM_PORT_ATTR attr;
  char *buffer;
  size_t capacity;
  size_t pos;
  size_t used;
};

#define SCM_PORT_IO(obj) (SCM_PORT(obj)->io)
#define SCM_PORT_BUFFER_MODE(obj) (SCM_PORT(obj)->buffer_mode)
#define SCM_PORT_ATTR(obj) (SCM_PORT(obj)->attr)
#define SCM_PORT_BUFFER(obj) (SCM_PORT(obj)->buffer)
#define SCM_PORT_CAPACITY(obj) (SCM_PORT(obj)->capacity)
#define SCM_PORT_POS(obj) (SCM_PORT(obj)->pos)
#define SCM_PORT_USED(obj) (SCM_PORT(obj)->used)

void scm_io_referred(ScmIO *io);
void scm_io_end(ScmIO *io);
ssize_t scm_io_read(ScmIO *io, void *buf, size_t size);
ssize_t scm_io_write(ScmIO *io, const void *buf, size_t size);
bool scm_io_seek(ScmIO *io, off_t offset, int whence);
int scm_io_close(ScmIO *io);
void scm_io_clear_error(ScmIO *io);
bool scm_io_is_ready(ScmIO *io);
bool scm_io_is_closed(ScmIO *io);
bool scm_io_is_eof(ScmIO *fileio);
SCM_PORT_BUF_MODE scm_io_default_buffer_mode(ScmIO *io);
ssize_t scm_io_block_size(ScmIO *io);
bool scm_io_has_error(ScmIO *io);
int scm_io_errno(ScmIO *io);

ScmFileIO *scm_fileio_new(int fd);
void scm_fileio_end(ScmFileIO *fileio);
ScmFileIO *scm_fileio_open(const char *pathname, int flags, mode_t mode);
ssize_t scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size);
ssize_t scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size);
bool scm_fileio_is_ready(ScmFileIO *fileio);
off_t scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence);
int scm_fileio_close(ScmFileIO *fileio);
bool scm_fileio_is_closed(ScmFileIO *fileio);
bool scm_fileio_is_eof(ScmFileIO *strio);
void scm_fileio_clear_error(ScmFileIO *fileio);
SCM_PORT_BUF_MODE scm_fileio_default_buffer_mode(ScmFileIO *fileio);
ssize_t scm_fileio_block_size(ScmFileIO *fileio);
bool scm_fileio_has_error(ScmFileIO *fileio);
int scm_fileio_errno(ScmFileIO *fileio);

ScmStringIO *scm_stringio_new(const char *str, size_t len);
void scm_stringio_end(ScmStringIO *strio);
size_t scm_stringio_read(ScmStringIO *strio, void *buf, size_t size);
size_t scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size);
bool scm_stringio_is_ready(ScmStringIO *strio);
off_t scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence);
int scm_stringio_close(ScmStringIO *strio);
bool scm_stringio_is_closed(ScmStringIO *strio);
bool scm_stringio_is_eof(ScmStringIO *strio);
void scm_stringio_clear_error(ScmStringIO *strio);
SCM_PORT_BUF_MODE scm_stringio_default_buffer_mode(ScmStringIO *strio);
bool scm_stringio_has_error(ScmStringIO *strio);
int scm_stringio_errno(ScmStringIO *strio);
char *scm_stringio_buffer(ScmStringIO *strio);
size_t scm_stringio_length(ScmStringIO *strio);

ScmCharConvIO *scm_charconvio_new(ScmIO *io,
                                        const char *internal_encode,
                                        const char *external_encode,
                                        bool owner);
void scm_charconvio_end(ScmCharConvIO *convio);
ssize_t scm_charconvio_read(ScmCharConvIO *convio, void *buf, size_t size);
ssize_t scm_charconvio_write(ScmCharConvIO *convio,
                             const void *buf, size_t size);
bool scm_charconvio_is_ready(ScmCharConvIO *convio);
off_t scm_charconvio_seek(ScmCharConvIO *convio, off_t offset, int whence);
int scm_charconvio_close(ScmCharConvIO *convio);
bool scm_charconvio_is_closed(ScmCharConvIO *convio);
bool scm_charconvio_is_eof(ScmCharConvIO *convio);
void scm_charconvio_clear_error(ScmCharConvIO *convio);
SCM_PORT_BUF_MODE scm_charconvio_default_buffer_mode(ScmCharConvIO *convio);
int scm_charconvio_block_size(ScmCharConvIO *convio);
bool scm_charconvio_has_error(ScmCharConvIO *convio);
int scm_charconvio_errno(ScmCharConvIO *convio);


void scm_port_initialize(ScmObj port, ScmIO *io,
                         SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode);
void scm_port_finalize(ScmObj port);
ScmObj scm_port_new(ScmIO *io,
                          SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode);
ScmObj scm_port_open_input(ScmIO *io,
                           SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode);
ScmObj scm_port_open_output(ScmIO *io,
                            SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode);
ScmObj scm_port_open_input_file_with_charconv(const char *path,
                                              SCM_PORT_BUF_MODE buf_mode,
                                              const char *internal_encode,
                                              const char *external_encode);
ScmObj scm_port_open_output_file_with_charconv(const char *path,
                                               SCM_PORT_BUF_MODE buf_mode,
                                               const char *internal_encode,
                                               const char *external_encode);
ScmObj scm_port_open_input_file(const char *path,
                                SCM_PORT_BUF_MODE buf_mode);
ScmObj scm_port_open_output_file(const char *path,
                                 SCM_PORT_BUF_MODE buf_mode);
ScmObj scm_port_open_input_string(const void *string, size_t size);
ScmObj scm_port_open_output_string(void);
bool scm_port_is_readable(ScmObj port);
bool scm_port_is_writable(ScmObj port);
bool scm_port_is_file_port(ScmObj port);
bool scm_port_is_string_port(ScmObj port);
bool scm_port_is_port(ScmObj obj);
bool scm_port_is_closed(ScmObj port);
bool scm_port_is_ready(ScmObj port);
bool scm_port_is_eof(ScmObj port);
int scm_port_flush(ScmObj port);
int scm_port_close(ScmObj port);
ssize_t scm_port_read_prim(ScmObj port, void *buf, size_t size);
ssize_t scm_port_write_prim(ScmObj port, const void *buf, size_t size);
int scm_port_seek(ScmObj port, off_t offset, int whence);
int scm_port_errno(ScmObj port);
bool scm_port_has_error(ScmObj port);
void *scm_port_string_buffer(ScmObj port);
ssize_t scm_port_string_buffer_length(ScmObj port);
void scm_port_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_port_gc_finalize(ScmObj obj);

#endif /*  INCLUDE_PORT_H__ */
