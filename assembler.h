#ifndef INCLUDE_ASSEMBLER_H__
#define INCLUDE_ASSEMBLER_H__

#include <stdint.h>
#include <stdbool.h>

#include "object.h"
#include "earray.h"

/* pseudo-instructions */
enum {
  SCM_ASM_PI_LABEL = 0x10000000,  /* define a label */
  SCM_ASM_PI_ASM,                 /* make ScmISeq object and set it to VAL register */
};

#define SCM_ISEQ_LABEL_NAME_MAX 256

typedef struct {
  char label[SCM_ISEQ_LABEL_NAME_MAX];
  EArray ref;
  size_t idx;
  bool defined_p;
} ScmLabelInfo;

ScmObj scm_asm_assemble(ScmObj lst);

#endif /* INCLUDE_ASSEMBLER_H__ */
