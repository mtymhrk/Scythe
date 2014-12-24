#ifndef INCLUDE_FCD_FORMAT_H__
#define INCLUDE_FCD_FORMAT_H__

#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"

int scm_fcd_pformat_lst(ScmObj port, ScmObj fmt, ScmObj lst);
ScmObj scm_fcd_format_lst(ScmObj fmt, ScmObj lst);
int scm_fcd_pformat_cv(ScmObj port, ScmObj fmt, ScmObj *obj, size_t n);
ScmObj scm_fcd_format_cv(ScmObj fmt, ScmObj *obj, size_t n);
int scm_fcd_pformat_va(ScmObj port, ScmObj fmt, va_list arg);
int scm_fcd_pformat(ScmObj port, ScmObj fmt, ...);
ScmObj scm_fcd_format_va(ScmObj fmt, va_list arg);
ScmObj scm_fcd_format(ScmObj fmt, ...);
int scm_fcd_pformat_cstr_va(ScmObj port, const char *fmt, va_list arg);
int scm_fcd_pformat_cstr(ScmObj port, const char *fmt, ...);
ScmObj scm_fcd_format_cstr_va(const char *fmt, va_list arg);
ScmObj scm_fcd_format_cstr(const char *fmt, ...);

#endif /* INCLUDE_FCD_FORMAT_H__ */
