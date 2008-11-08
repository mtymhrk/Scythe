#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "printer.h"
#include "port.h"

#define SCM_SYSCALL(result, expr)                 \
  do {                                            \
    (result) = (expr);                            \
  } while (result < 0 && errno == EINTR)

#define BIT_IS_SETED(val, bit) (((val) & (bit)) ? true : false)

#define SCM_STRINGIO_INIT_BUF_SIZE 64
#define SCM_PORT_DEFAULT_BUF_SIZE 256

typedef void (*DestructFunc)(ScmIO *io);
typedef ssize_t (*ReadFunc)(ScmIO *io, void *buf, size_t size);
typedef ssize_t (*WriteFunc)(ScmIO *io, const void *buf, size_t size);
typedef off_t (*SeekFunc)(ScmIO *io, off_t offset, int whence);
typedef int (*CloseFunc)(ScmIO *io);
typedef void (*ClearErrorFunc)(ScmIO *io);
typedef bool (*IsReadyFunc)(ScmIO *io);
typedef bool (*IsClosedFunc)(ScmIO *io);
typedef bool (*IsEOFFunc)(ScmIO *io);
typedef SCM_PORT_BUF_MODE (*BufferModeFunc)(ScmIO *io);
typedef int (*BlockSizeFunc)(ScmIO *io);
typedef bool (*HasErrorFunc)(ScmIO *io);
typedef int (*ErrnoFunc)(ScmIO *io);

struct ScmIORec {
  DestructFunc destruct;
  ReadFunc read;
  WriteFunc write;
  SeekFunc seek;
  CloseFunc close;
  ClearErrorFunc clear_error;
  IsReadyFunc is_ready;
  IsClosedFunc is_closed;
  IsEOFFunc is_eof;
  BufferModeFunc default_buffer_mode;
  BlockSizeFunc block_size;
  HasErrorFunc has_error;
  ErrnoFunc error_no;
};

struct ScmFileIORec {
  ScmIO io_base;
  int fd;
  int error_no;
  bool is_eof;
  bool is_closed;
};

struct ScmStringIORec {
  ScmIO io_base;
  char *string;
  size_t capacity;
  size_t length;
  size_t pos;
  bool is_closed;
};

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

static void
scm_io_initialize(ScmIO *io,
                  DestructFunc destruct,
                  ReadFunc read,
                  WriteFunc write,
                  SeekFunc seek,
                  CloseFunc close,
                  ClearErrorFunc clear_error,
                  IsReadyFunc is_ready,
                  IsClosedFunc is_closed,
                  IsEOFFunc is_eof,
                  BufferModeFunc default_buffer_mode,
                  BlockSizeFunc block_size,
                  HasErrorFunc has_error,
                  ErrnoFunc error_no)
{
  assert(io != NULL);

  io->destruct = destruct;
  io->read = read;
  io->write = write;
  io->seek = seek;
  io->close = close;
  io->clear_error = clear_error;
  io->is_ready = is_ready;
  io->is_closed = is_closed;
  io->is_eof = is_eof;
  io->default_buffer_mode = default_buffer_mode;
  io->block_size = block_size;
  io->has_error = has_error;
  io->error_no = error_no;
}

void
scm_io_destruct(ScmIO *io)
{
  assert(io != NULL);
  if (io->destruct != NULL)
    io->destruct(io);
}

ssize_t
scm_io_read(ScmIO *io, void *buf, size_t size)
{
  assert(io != NULL);
  return (io->read != NULL) ? io->read(io, buf, size) : -1;
}

ssize_t
scm_io_write(ScmIO *io, const void *buf, size_t size)
{
  assert(io != NULL);
  return (io->write != NULL) ? io->write(io, buf, size) : -1;
}

bool
scm_io_seek(ScmIO *io, off_t offset, int whence)
{
  assert(io != NULL);
  return (io->seek != NULL) ? io->seek(io, offset, whence) : -1;
}

int
scm_io_close(ScmIO *io)
{
  assert(io != NULL);
  return (io->close != NULL) ? io->close(io) : -1;
}

void
scm_io_clear_error(ScmIO *io)
{
  assert(io != NULL);
  if (io->clear_error != NULL)
    io->clear_error(io);
}

bool
scm_io_is_ready(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_ready != NULL) ? io->is_ready(io) : true;
}

bool
scm_io_is_closed(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_closed != NULL) ? io->is_closed(io) : false;
}

bool
scm_io_is_eof(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_eof != NULL) ? io->is_eof(io) : false;
}

SCM_PORT_BUF_MODE
scm_io_default_buffer_mode(ScmIO *io)
{
  assert(io != NULL);
  if (io->default_buffer_mode != NULL)
    return io->default_buffer_mode(io);
  else
    return SCM_PORT_BUF_FULL;
}

int
scm_io_block_size(ScmIO *io)
{
  assert(io != NULL);
  return (io->block_size != NULL) ? io->block_size(io) : -1;
}

bool
scm_io_has_error(ScmIO *io)
{
  assert(io != NULL);
  return (io->has_error != NULL) ? io->has_error(io) : false;
}

int
scm_io_errno(ScmIO *io)
{
  assert(io != NULL);
  return (io->error_no != NULL) ? io->error_no(io) : 0;
}

ScmFileIO *
scm_fileio_construct(int fd)
{
  ScmFileIO *fileio;

  fileio = scm_memory_allocate(sizeof(ScmFileIO));
  scm_io_initialize((ScmIO *)fileio,
                    (DestructFunc)scm_fileio_destruct,
                    (ReadFunc)scm_fileio_read,
                    (WriteFunc)scm_fileio_write,
                    (SeekFunc)scm_fileio_seek,
                    (CloseFunc)scm_fileio_close,
                    (ClearErrorFunc)scm_fileio_clear_error,
                    (IsReadyFunc)scm_fileio_is_ready,
                    (IsClosedFunc)scm_fileio_is_closed,
                    (IsEOFFunc)scm_fileio_is_eof,
                    (BufferModeFunc)scm_fileio_default_buffer_mode,
                    (BlockSizeFunc)scm_fileio_block_size,
                    (HasErrorFunc)scm_fileio_has_error,
                    (ErrnoFunc)scm_fileio_errno);
  fileio->fd = fd;
  fileio->error_no = 0;
  fileio->is_eof = false;
  fileio->is_closed = false;

  return fileio;
}

void
scm_fileio_destruct(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  
  scm_fileio_close(fileio);
  scm_memory_release(fileio);
}

ScmFileIO *
scm_fileio_open(const char *pathname, int flags, mode_t mode)
{
  int fd;

  assert(pathname != NULL);

  SCM_SYSCALL(fd, open(pathname, flags, mode));
  if (fd < 0) return NULL;

  return scm_fileio_construct(fd);
}

ssize_t
scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size)
{
  ssize_t n;

  assert(fileio != NULL);
  assert(buf != NULL);

  if (fileio->is_eof) return 0;
  else if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, read(fileio->fd, buf, size));
  if (n == 0)
    fileio->is_eof = true;
  if (n < 0)
    fileio->error_no = errno;

  return n;
}

ssize_t
scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size)
{
  ssize_t n;

  assert(fileio != NULL);
  assert(buf != NULL);

  if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, write(fileio->fd, buf, size));
  if (n < 0)
    fileio->error_no = errno;
  
  return n;
}

bool
scm_fileio_is_ready(ScmFileIO *fileio)
{
  int n;
  fd_set readfds;
  struct timeval tout;

  assert(fileio != NULL);

  if (fileio->is_eof) return false;
  else if (fileio->is_closed) return false;
  else if (fileio->error_no != 0) return false;

  FD_ZERO(&readfds);
  FD_SET(fileio->fd, &readfds);
  tout.tv_sec = 0;
  tout.tv_usec = 0;

  SCM_SYSCALL(n, select(fileio->fd + 1, &readfds, NULL, NULL, &tout));
  if (n < 0)
    fileio->error_no = errno;
  
  if (n > 0)
    return true;
  else
    return false;
}

off_t
scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence)
{
  off_t n;

  assert(fileio != NULL);

  if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, lseek(fileio->fd, offset, whence));
  if (n < 0)
    fileio->error_no = errno;

  fileio->is_eof = false;

  return n;
}

int
scm_fileio_close(ScmFileIO *fileio)
{
  int n;

  assert(fileio != NULL);

  if (!fileio->is_closed) {
    SCM_SYSCALL(n, close(fileio->fd));
    if (n < 0)
      fileio->error_no = errno;
    else
      fileio->is_closed = true;
    return n;
  }
  else
    return 0;
}

bool
scm_fileio_is_closed(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->is_closed;
}

bool
scm_fileio_is_eof(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->is_eof;
}

void
scm_fileio_clear_error(ScmFileIO *fileio)
{
  assert(fileio != NULL);

  fileio->error_no = 0;
}

SCM_PORT_BUF_MODE
scm_fileio_default_buffer_mode(ScmFileIO *fileio)
{
  struct stat st;
  int ret;

  assert(fileio != NULL);

  if (isatty(fileio->fd))
    return SCM_PORT_BUF_MODEST;

  SCM_SYSCALL(ret, fstat(fileio->fd, &st));
  if (ret < 0) {
    fileio->error_no = errno;
    return SCM_PORT_BUF_FULL;
  }

  if (S_ISFIFO(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else if (S_ISSOCK(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else if (S_ISCHR(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else
    return SCM_PORT_BUF_FULL;
}

ssize_t
scm_fileio_block_size(ScmFileIO *fileio)
{
  int rslt;
  struct stat stat;

  assert(fileio != NULL);

  SCM_SYSCALL(rslt, fstat(fileio->fd, &stat));
  if (rslt < 0)
    return -1;
  else
    return stat.st_blksize;
}

bool
scm_fileio_has_error(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->error_no != 0;
}

int
scm_fileio_errno(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->error_no;
}

void
scm_stringio_expand_buffer(ScmStringIO *strio, size_t needed_size)
{
  size_t new_size;

  assert(strio != NULL);

  for (new_size = SCM_STRINGIO_INIT_BUF_SIZE;
       new_size < needed_size;
       new_size *= 2)
    ;

  if (strio->string == NULL) {
    strio->string = scm_memory_allocate(new_size);
    strio->capacity = new_size;
  }
  else if (new_size > strio->capacity) {
    char *new_buffer = scm_memory_allocate(new_size);
    memset(new_buffer, 0, new_size);
    memcpy(new_buffer, strio->string, strio->length);
    scm_memory_release(strio->string);
    strio->string = new_buffer;
    strio->capacity = new_size;
  }
}


ScmStringIO *
scm_stringio_construct(const char *str, size_t len)
{
  ScmStringIO *strio;

  strio = scm_memory_allocate(sizeof(ScmStringIO));
  scm_io_initialize((ScmIO *)strio,
                    (DestructFunc)scm_stringio_destruct,
                    (ReadFunc)scm_stringio_read,
                    (WriteFunc)scm_stringio_write,
                    (SeekFunc)scm_stringio_seek,
                    (CloseFunc)scm_stringio_close,
                    (ClearErrorFunc)scm_stringio_clear_error,
                    (IsReadyFunc)scm_stringio_is_ready,
                    (IsClosedFunc)scm_stringio_is_closed,
                    (IsEOFFunc)scm_stringio_is_eof,
                    (BufferModeFunc)scm_stringio_default_buffer_mode,
                    (BlockSizeFunc)NULL,
                    (HasErrorFunc)scm_stringio_has_error,
                    (ErrnoFunc)scm_stringio_errno);
  strio->string = NULL;
  strio->capacity = 0;
  strio->length = 0;
  strio->pos = 0;
  strio->is_closed = false;

  if (str != NULL) {
    scm_stringio_expand_buffer(strio, len);
    memcpy(strio->string, str, len);
    strio->length = len;
  }
  else
    scm_stringio_expand_buffer(strio, 0);

  return strio;
}

void
scm_stringio_destruct(ScmStringIO *strio)
{
  assert(strio != NULL);
  scm_memory_release(strio->string);
  scm_memory_release(strio);
}

size_t
scm_stringio_read(ScmStringIO *strio, void *buf, size_t size)
{
  size_t n;
  size_t rest;

  assert(strio != NULL);
  assert(buf != NULL);

  if (strio->is_closed) return -1;

  rest = strio->length - strio->pos;
  if (rest <= 0) return 0;

  n = (size <= rest) ? size : rest;
  memcpy(buf, strio->string + strio->pos, n);
  strio->pos += n;

  return n;
}

size_t
scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size)
{
  assert(strio != NULL);
  assert(buf != NULL);

  if (strio->is_closed) return -1;

  scm_stringio_expand_buffer(strio, strio->pos + size);
  memcpy(strio->string + strio->pos, buf, size);
  strio->pos += size;
  if (strio->pos > strio->length)
    strio->length = strio->pos;

  return size;
}


bool
scm_stringio_is_ready(ScmStringIO *strio)
{
  assert(strio != NULL);

  if (strio->is_closed) return false;

  return (strio->pos < strio->length);
}

off_t
scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence)
{
  off_t pos;

  assert(strio != NULL);

  if (strio->is_closed) return -1;

  switch (whence) {
  case SEEK_SET:
    pos = offset;
    break;
  case SEEK_CUR:
    pos = strio->pos + offset;
    break;
  case SEEK_END:
    pos = strio->length + offset;
    break;
  default:
    return -1;
  }

  scm_stringio_expand_buffer(strio, pos);
  strio->pos = (size_t)pos;

  return pos;
}

int
scm_stringio_close(ScmStringIO *strio)
{
  assert(strio != NULL);
  strio->is_closed = true;
  return 0;
}

bool
scm_stringio_is_closed(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->is_closed;
}

bool
scm_stringio_is_eof(ScmStringIO *strio)
{
  assert(strio != NULL);  
  return (strio->pos >= strio->length);
}

void
scm_stringio_clear_error(ScmStringIO *strio)
{
  assert(strio != NULL);
}

SCM_PORT_BUF_MODE
scm_stringio_default_buffer_mode(ScmStringIO *strio)
{
  assert(strio != NULL);
  return SCM_PORT_BUF_NONE;
}

bool
scm_stringio_has_error(ScmStringIO *strio)
{
  assert(strio != NULL);
  return false;
}

int
scm_stringio_errno(ScmStringIO *strio)
{
  assert(strio != NULL);
  return 0;
}

char *
scm_stringio_buffer(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->string;
}

size_t
scm_stringio_length(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->length;
}

static void
scm_port_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  ScmPort *port;

  assert(obj != NULL); assert(scm_port_is_port(obj));
  assert(printer != NULL);

  port = SCM_PORT(obj);

  scm_printer_concatenate_string(printer, "#<port:");
  if (scm_port_is_readable(port))
    scm_printer_concatenate_string(printer, " readable");
  if (scm_port_is_writable(port))
    scm_printer_concatenate_string(printer, " writable");
  if (scm_port_is_file_port(port))
    scm_printer_concatenate_string(printer, " file");
  if (scm_port_is_string_port(port))
    scm_printer_concatenate_string(printer, " string");
  scm_printer_concatenate_char(printer, '>');
}

static void
scm_port_init_buffer(ScmPort *port, SCM_PORT_BUF_MODE buf_mode)
{
  assert(port != NULL);
  assert(buf_mode >= 0 && buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT)
    port->buffer_mode = scm_io_default_buffer_mode(port->io);
  else
    port->buffer_mode = buf_mode;

  port->pos = 0;
  port->used = 0;
  switch (port->buffer_mode) {
  case SCM_PORT_BUF_FULL:   /* fall through */
  case SCM_PORT_BUF_LINE:   /* fall through */
  case SCM_PORT_BUF_MODEST: /* fall through */
    port->capacity = scm_io_block_size(port->io);
    if (port->capacity < 0) 
      port->capacity = SCM_PORT_DEFAULT_BUF_SIZE;
    break;
  case SCM_PORT_BUF_NONE:
    port->capacity = 0;
    break;
  default:
    port->capacity = 0; /* must not happen */
    break;
  }

  if (port->capacity == 0) {
    port->buffer = NULL;
    return;
  }

  port->buffer = scm_memory_allocate(port->capacity);
}

static void
scm_port_initialize(ScmPort *port,
                    SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  assert(port != NULL);

  scm_obj_init(SCM_OBJ(port), SCM_OBJ_TYPE_PORT, scm_port_pretty_print);
  port->attr = attr;
  port->buffer_mode = SCM_PORT_BUF_NONE;
  port->buffer = NULL;
  port->capacity = 0;
  port->pos = 0;
  port->used = 0;

  scm_port_init_buffer(port, buf_mode);
}

ScmPort *
scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  ScmPort *port;

  assert(io != NULL);

  port = scm_memory_allocate(sizeof(ScmPort));
  port->io = io;
  scm_port_initialize(port, attr | SCM_PORT_ATTR_READABLE, buf_mode);

  return port;
}

ScmPort *
scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  ScmPort *port;

  assert(io != NULL);

  port = scm_memory_allocate(sizeof(ScmPort));
  port->io = io;
  scm_port_initialize(port, attr | SCM_PORT_ATTR_WRITABLE, buf_mode);

  return port;
}

ScmPort *
scm_port_open_input_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_RDONLY, 0);
  if (io == NULL) return NULL;

  return scm_port_open_input(io, SCM_PORT_ATTR_FILE, buf_mode);
}

ScmPort *
scm_port_open_output_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_WRONLY | O_CREAT, 00644);
  if (io == NULL) return NULL;

  return scm_port_open_output(io, SCM_PORT_ATTR_FILE, buf_mode);
}

ScmPort *
scm_port_open_input_string(const void *string, size_t size)
{
  ScmIO *io;

  assert(string != NULL);

  io = (ScmIO *)scm_stringio_construct(string, size);
  if (io == NULL) return NULL;

  return scm_port_open_input(io, SCM_PORT_ATTR_STRING, SCM_PORT_BUF_DEFAULT);
}

ScmPort *
scm_port_open_output_string(void)
{
  ScmIO *io;

  io = (ScmIO *)scm_stringio_construct(NULL, 0);
  if (io == NULL) return NULL;

  return scm_port_open_output(io, SCM_PORT_ATTR_STRING, SCM_PORT_BUF_DEFAULT);
}

void
scm_port_destruct(ScmPort *port)
{
  assert(port != NULL);

  scm_port_close(port);
  scm_memory_release(port->buffer);
  scm_io_destruct(port->io);
  scm_memory_release(port);
}

bool
scm_port_is_readable(ScmPort *port)
{
  assert(port != NULL);

  return BIT_IS_SETED(port->attr, SCM_PORT_ATTR_READABLE);
}

bool
scm_port_is_writable(ScmPort *port)
{
  assert(port != NULL);

  return BIT_IS_SETED(port->attr, SCM_PORT_ATTR_WRITABLE);
}

bool
scm_port_is_file_port(ScmPort *port)
{
  assert(port != NULL);

  return BIT_IS_SETED(port->attr, SCM_PORT_ATTR_FILE);
}

bool
scm_port_is_string_port(ScmPort *port)
{
  assert(port != NULL);

  return BIT_IS_SETED(port->attr, SCM_PORT_ATTR_STRING);  
}

bool
scm_port_is_port(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_PORT);
}

bool
scm_port_is_closed(ScmPort *port)
{
  assert(port != NULL);

  return scm_io_is_closed(port->io);
}

static bool
scm_port_is_buffer_empty(ScmPort *port)
{
  assert(port != NULL);
  return (port->pos == port->used);
}

bool
scm_port_is_ready(ScmPort *port)
{
  assert(port != NULL);

  if (scm_port_is_buffer_empty(port))
    return scm_io_is_ready(port->io);
  else
    return true;
}

bool
scm_port_is_eof(ScmPort *port)
{
  assert(port != NULL);

  return (scm_port_is_buffer_empty(port) && scm_io_is_eof(port->io));
}

int
scm_port_flush(ScmPort *port)
{
  assert(port != NULL);

  if (!scm_port_is_writable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  if (port->used > 0) {
    scm_io_write(port->io, port->buffer, port->used);
    port->used = 0;
    port->pos = 0;
  }

  return 0;
}

int
scm_port_close(ScmPort *port)
{
  assert(port != NULL);

  if (scm_port_is_closed(port)) return 0;

  if (scm_port_is_writable(port))
    scm_port_flush(port);

  return scm_io_close(port->io);
}

static void
scm_port_clear_buffer(ScmPort *port)
{
  assert(port != NULL);

  port->used = 0;
  port->pos = 0;
}

static ssize_t
scm_port_read_from_buffer(ScmPort *port, void *buf, size_t size)
{
  ssize_t n;

  assert(port != NULL);
  assert(buf != NULL);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(port->buffer_mode != SCM_PORT_BUF_NONE);

  n = port->used - port->pos;
  n = (n < size) ? n : size;

  memcpy(buf, port->buffer + port->pos, n);
  port->pos += n;

  return n;
}

static ssize_t
scm_port_read_into_buffer(ScmPort *port)
{
  ssize_t n;

  assert(port != NULL);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(port->buffer_mode != SCM_PORT_BUF_NONE);

  n = scm_io_read(port->io, port->buffer, port->capacity);
  if (n < 0) return -1; // error

  port->pos = 0;
  port->used = n;

  return n;
}

static ssize_t
scm_port_read_prim_buf(ScmPort *port, void *buf, size_t size, bool wait_all)
{
  ssize_t p;

  assert(port != NULL);
  assert(buf != NULL);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(port->buffer_mode != SCM_PORT_BUF_NONE);

  p = 0;
  do {
    ssize_t n;

    if (scm_port_is_buffer_empty(port)) {
      n = scm_port_read_into_buffer(port);
      if (n < 0) return -1; // error
      if (n == 0) break;
    }

    n = scm_port_read_from_buffer(port, buf + p, size - p);
    if (n < 0) return -1; // error
    p += n;
    
    if (p >= size) break;

  } while (wait_all || (!wait_all && scm_port_is_ready(port)));

  return p;
}

ssize_t
scm_port_read_prim(ScmPort *port, void *buf, size_t size)
{
  const bool wait_all = true;
  const bool dont_wait = false;

  assert(port != NULL);
  assert(buf != NULL);

  if (!scm_port_is_readable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  switch (port->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE:
    return scm_port_read_prim_buf(port, buf, size, wait_all);
    break;
  case SCM_PORT_BUF_MODEST:
    return scm_port_read_prim_buf(port, buf, size, dont_wait);
    break;
  case SCM_PORT_BUF_NONE:
    return scm_io_read(port->io, buf, size);
    break;
  default:
    return -1; /* must not happen */
    break;
  }
}

static ssize_t
scm_port_write_prim_buf_full(ScmPort *port, const void *buf, size_t size)
{
  int n, i;

  assert(port != NULL);
  assert(buf != NULL);
  assert(scm_port_is_writable(port));
  assert(!scm_port_is_closed(port));
  
  for (i = 0; i < size; i += n) {
    n = port->capacity - port->pos;
    n = (n > size) ? size : n;

    memcpy(port->buffer + port->pos, buf + i, n);
    port->pos += n;
    if (port->pos > port->used)
      port->used = port->pos;

    if (port->used >= port->capacity)
      scm_port_flush(port);
  }

  return i;
}

/* static ssize_t */
/* scm_port_write_prim_buf_line(ScmPort *port, const void *buf, size_t size) */
/* { */
/*   int i, n; */
  
/*   assert(port != NULL); */
/*   assert(buf != NULL); */
/*   assert(scm_port_is_writable(port)); */
/*   assert(!scm_port_is_closed(port)); */

/*   for (i = 0; i < size; i++) { */
/*     n = scm_port_write_prim_buf_full(port, buf + i, 1); */
/*     if (n < 0) return -1; */
/*     if (((const char *)buf)[i] == '\n') scm_port_flush(port); */
/*   } */

/*   return i; */
/* } */

ssize_t
scm_port_write_prim(ScmPort *port, const void *buf, size_t size)
{
  assert(port != NULL);
  assert(buf != NULL);

  if (!scm_port_is_writable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  switch (port->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE: /* fall through */
  case SCM_PORT_BUF_MODEST:
    return scm_port_write_prim_buf_full(port, buf, size);
    break;
  case SCM_PORT_BUF_NONE:
    return scm_io_write(port->io, buf, size);
    break;
  default:
    return -1; /* must not happen */
    break;
  }
}

int
scm_port_seek(ScmPort *port, off_t offset, int whence)
{
  off_t off, result;

  assert(port != NULL);

  if (scm_port_is_closed(port)) return -1;

  if (scm_port_is_writable(port))
    scm_port_flush(port);

  switch (whence) {
  case SEEK_SET:
    result = scm_io_seek(port->io, offset, whence);
    break;
  case SEEK_CUR:
    off = offset - (port->used - port->pos);
    result = scm_io_seek(port->io, off, whence);
    break;
  case SEEK_END:
    result = scm_io_seek(port->io, offset, whence);
    break;
  default:
    return -1;
    break;
  }

  if (!scm_port_is_writable(port))
    scm_port_clear_buffer(port);

  return (result < 0) ? result : 0;
}

bool
scm_port_has_error(ScmPort *port)
{
  assert(port != NULL);

  return scm_io_has_error(port->io);
}

int
scm_port_errno(ScmPort *port)
{
  assert(port != NULL);

  return scm_io_errno(port->io);
}

void *
scm_port_string_buffer(ScmPort *port)
{
  assert(port != NULL);

  if (!scm_port_is_string_port(port)) return NULL;

  return (void *)scm_stringio_buffer((ScmStringIO *)port->io);
}

ssize_t
scm_port_string_buffer_length(ScmPort *port)
{
  assert(port != NULL);

  if (!scm_port_is_string_port(port)) return -1;

  return scm_stringio_length((ScmStringIO *)port->io);
}
