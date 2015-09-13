#ifndef INCLUDE_FORMAT_H__
#define INCLUDE_FORMAT_H__

#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"

int scm_pformat_lst(ScmObj port, ScmObj fmt, ScmObj lst);
ScmObj scm_format_lst(ScmObj fmt, ScmObj lst);
int scm_pformat_cv(ScmObj port, ScmObj fmt, ScmObj *obj, size_t n);
ScmObj scm_format_cv(ScmObj fmt, ScmObj *obj, size_t n);
int scm_pformat_va(ScmObj port, ScmObj fmt, va_list arg);
int scm_pformat(ScmObj port, ScmObj fmt, ...);
ScmObj scm_format_va(ScmObj fmt, va_list arg);
ScmObj scm_format(ScmObj fmt, ...);
int scm_pformat_cstr_va(ScmObj port, const char *fmt, va_list arg);
int scm_pformat_cstr(ScmObj port, const char *fmt, ...);
ScmObj scm_format_cstr_va(const char *fmt, va_list arg);
ScmObj scm_format_cstr(const char *fmt, ...);

#endif /* INCLUDE_FORMAT_H__ */
