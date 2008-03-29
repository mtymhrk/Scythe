#ifndef INCLUDE_PRINTER_H__
#define INCLUDE_PRINTER_H__

#include <stdio.h>

typedef struct ScmPrinterRec ScmPrinter;

typedef enum {
  SCM_PRINTER_MODE_CLEAR = 0x01,
  SCM_PRINTER_MODE_FLUSH = 0x02,
} SCM_PINTER_MODE_T;

void scm_printer_concatenate_string(ScmPrinter *printer, const char *str);
void scm_printer_concatenate_char(ScmPrinter *printer, char c);
void scm_printer_truncate_buffer(ScmPrinter *printer, int size);
void scm_printer_clear(ScmPrinter *printer);
void scm_printer_flush(ScmPrinter *printer);
void scm_printer_line_feed(ScmPrinter *printer);
void scm_printer_pretty_print_scm_obj(ScmPrinter *printer,
				      ScmObj obj, unsigned int mode);
size_t scm_printer_length(ScmPrinter *printer);
void scm_printer_copy_buffer(ScmPrinter *printer, char *dst, size_t len);
char *scm_printer_buffer(ScmPrinter *printer);
ScmPrinter *scm_printer_construct(FILE *output);

#endif /* INCLUDE_PRINTER_H__ */
