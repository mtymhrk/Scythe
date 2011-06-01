#ifndef INCLUDE_INST_H__
#define INCLUDE_INST_H__

#include <stdint.h>

typedef uint32_t scm_iword_t;

#include "object.h"

typedef enum {
  SCM_OPCODE_NOP,                 /* no operation */
  SCM_OPCODE_CALL,                /* function call */
  SCM_OPCODE_RET,                 /* return from function */
  SCM_OPCODE_FRAME,               /* create stack frame */
  SCM_OPCODE_IMMVAL,              /* copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* push value of val register */
  SCM_OPCODE_PUSH_IMMVAL,         /* push immediate value */
  SCM_OPCODE_PUSH_PRIMVAL,        /* push primitive value of host language */
  /* SCM_OPCODE_GREF,                /\* refere global variable *\/ */
  /* SCM_OPCODE_SREF,                /\* refere value in stack *\/ */
  /* SCM_OPCODE_CREF                 /\* refere value in closure *\/ */
} SCM_OPCODE_T;

typedef union {
  scm_iword_t iword;
  struct {
    uint8_t op;
    int imm_idx : 24;
  } immv;
  struct {
    uint8_t op;
    int primval : 24;
  } primv;
} scm_inst_t;

#define SCM_INST_IMMVAL_MAX   8388607
#define SCM_INST_IMMVAL_MIN  -8388608
#define SCM_INST_PRIMVAL_MAX  8388607
#define SCM_INST_PRIMVAL_MIN -8388608

#endif /* INCLUDE_INST_H__ */
