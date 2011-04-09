#ifndef INCLUDE_INST_H__
#define INCLUDE_INST_H__

#include <stdint.h>

typedef uint32_t scm_iword_t;

#include "object.h"

typedef enum {
  SCM_INST_NOP,                 /* no operation */
  SCM_INST_CALL,                /* function call */
  SCM_INST_RET,                 /* return from function */
  SCM_INST_FRAME,               /* create stack frame */
  SCM_INST_IMMVAL,              /* copy immediate value to val register */
  SCM_INST_PUSH,                /* push value of val register */
  SCM_INST_PUSH_IMMVAL,         /* push immediate value */
  SCM_INST_PUSH_PRIMVAL,        /* push primitive value of host language */
  /* SCM_INST_GREF,                /\* refere global variable *\/ */
  /* SCM_INST_SREF,                /\* refere value in stack *\/ */
  /* SCM_INST_CREF                 /\* refere value in closure *\/ */
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

#endif /* INCLUDE_INST_H__ */
