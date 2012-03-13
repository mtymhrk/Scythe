#ifndef INCLUDE_API_TYPE_H__
#define INCLUDE_API_TYPE_H__

#include <stdint.h>

#include "object.h"


/*******************************************************************/
/*  VM                                                             */
/*******************************************************************/

typedef scm_uword_t scm_vm_stack_val_t;


/*******************************************************************/
/*  VM Instractions                                                */
/*******************************************************************/

typedef uint32_t scm_iword_t;

typedef union {
  scm_iword_t iword;
  struct {
    uint8_t op;
    int arg : 24;
  } plain;
  struct {
    uint8_t op;
    int imm_idx : 24;
  } immv1;
  struct {
    uint8_t op;
    int primval : 24;
  } primv;
} scm_inst_t;

#define SCM_INST_IMMVAL_MAX   8388607
#define SCM_INST_IMMVAL_MIN  -8388608
#define SCM_INST_PRIMVAL_MAX  8388607
#define SCM_INST_PRIMVAL_MIN -8388608


#endif /* INCLUDE_API_TYPE_H__ */
