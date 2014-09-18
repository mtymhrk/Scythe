#ifndef INCLUDE_ASSEMBLER_H__
#define INCLUDE_ASSEMBLER_H__

#include <stdint.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/earray.h"

#define SCM_ISEQ_LABEL_NAME_MAX 256

#define SCM_ASM_PI_START 0x10000

enum {
  SCM_ASM_PI_LABEL = SCM_ASM_PI_START,  /* define a label */
  SCM_ASM_PI_ASM,                       /* make ScmISeq object
                                           and set it to VAL register */
  SCM_ASM_PI_ASM_CLOSE,                 /* assemble, make a closure,
                                           and set it to VAL register */
};

typedef struct {
  char label[SCM_ISEQ_LABEL_NAME_MAX];
  EArray ref;
  size_t offset;
  bool defined_p;
} ScmLabelInfo;

int scm_asm_mnemonic2opcode(const char *mne);
const char *scm_asm_opcode2mnemonic(int code);
ScmObj scm_asm_mnemonic(int opcode);

ScmObj scm_asm_assemble(ScmObj lst, ScmObj iseq);

#endif /* INCLUDE_ASSEMBLER_H__ */
