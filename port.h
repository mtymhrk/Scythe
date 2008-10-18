#ifndef INCLUDE_PORT_H__
#define INCLUDE_PORT_H__

typedef struct ScmIORec ScmIO;
typedef struct ScmFileIORec ScmFileIO;
typedef struct ScmStringIORec ScmStringIO;
typedef struct ScmPortRec ScmPort;

#define SCM_PORT(obj) ((ScmPort *)(obj))

#include "object.h"

typedef enum {
  SCM_PORT_BUF_FULL,
  SCM_PORT_BUF_LINE,
  SCM_PORT_BUF_NONE
} SCM_PORT_BUF_MODE;

typedef enum {
  SCM_PORT_ATTR_READABLE = 0x0001,
  SCM_PORT_ATTR_WRITABLE = 0x0002,
  SCM_PORT_ATTR_FILE     = 0x0004,
  SCM_PORT_ATTR_STRING   = 0x0008,
} SCM_PORT_ATTR;

void scm_io_destruct(ScmIO *io);
ssize_t scm_io_read(ScmIO *io, void *buf, size_t size);
ssize_t scm_io_write(ScmIO *io, const void *buf, size_t size);
bool scm_io_seek(ScmIO *io, off_t offset, int whence);
void scm_io_close(ScmIO *io);
void scm_io_clear_error(ScmIO *io);
bool scm_io_is_ready(ScmIO *io);
bool scm_io_is_closed(ScmIO *io);
bool scm_io_is_eof(ScmIO *fileio);
SCM_PORT_BUF_MODE scm_io_default_buffer_mode(ScmIO *io);
int scm_io_block_size(ScmIO *io);
bool scm_io_has_error(ScmIO *io);
int scm_io_errno(ScmIO *io);

ScmFileIO *scm_fileio_construct(int fd);
void scm_fileio_destruct(ScmFileIO *fileio);
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

ScmStringIO *scm_stringio_construct(const char *str, size_t len);
void scm_stringio_destruct(ScmStringIO *strio);
size_t scm_stringio_read(ScmStringIO *strio, void *buf, size_t size);
size_t scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size);
bool scm_stringio_is_ready(ScmStringIO *strio);
off_t scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence);
int scm_stringio_close(ScmStringIO *strio);
bool scm_stringio_is_closed(ScmStringIO *strio);
bool scm_stringio_is_eof(ScmStringIO *strio);
void scm_stringio_clear_error(ScmStringIO *strio);
bool scm_stringio_has_error(ScmStringIO *strio);
int scm_stringio_errno(ScmStringIO *strio);

bool scm_port_is_readable(ScmPort *port);
bool scm_port_is_writable(ScmPort *port);
bool scm_port_is_file_port(ScmPort *port);
bool scm_port_is_string_port(ScmPort *port);
bool scm_port_is_port(ScmObj obj);
ScmPort *scm_port_construct_input_port(const char *path);
ScmPort *scm_port_construct_output_port(const char *path);
bool scm_port_is_closed(ScmPort *port);
bool scm_port_is_ready(ScmPort *port);
bool scm_port_is_eof(ScmPort *port);
int scm_port_flush(ScmPort *port);
ssize_t scm_port_read_prim(ScmPort *port, void *buf, size_t size);
ssize_t scm_port_write_prim(ScmPort *port, const void *buf, size_t size);
int scm_port_seek(ScmPort *port, off_t offset, int whence);

#endif /*  INCLUDE_PORT_H__ */
