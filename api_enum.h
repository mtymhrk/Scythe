#ifndef INCLUDE_API_CONST_H__
#define INCLUDE_API_CONST_H__

/*******************************************************************/
/*  Encoding                                                       */
/*******************************************************************/

typedef enum {
  SCM_ENC_ASCII,
  SCM_ENC_BIN,
  SCM_ENC_UCS4,
  SCM_ENC_UTF8,
  SCM_ENC_EUCJP,
  SCM_ENC_SJIS,
  SCM_ENC_SYS,
} SCM_ENC_T;

#define SCM_ENC_NR_ENC SCM_ENC_SYS


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

typedef enum {
  SCM_MEM_HEAP,
  SCM_MEM_ROOT,
} SCM_MEM_TYPE_T;


/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

typedef enum {
  SCM_PORT_BUF_FULL,
  SCM_PORT_BUF_LINE,
  SCM_PORT_BUF_MODEST,
  SCM_PORT_BUF_NONE,
  SCM_PORT_BUF_DEFAULT,
} SCM_PORT_BUF_T;

#define SCM_PORT_NR_BUF_MODE (SCM_PORT_BUF_DEFAULT + 1)


/*******************************************************************/
/*  VM Instractions                                                */
/*******************************************************************/

typedef enum {
  SCM_OPCODE_NOP,                 /* no operation */
  SCM_OPCODE_STOP,                /* stop calculation */
  SCM_OPCODE_CALL,                /* function call */
  SCM_OPCODE_TAIL_CALL,           /* function tail call */
  SCM_OPCODE_RETURN,              /* return from function */
  SCM_OPCODE_FRAME,               /* create stack frame */
  SCM_OPCODE_IMMVAL,              /* copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* push value of val register */
  SCM_OPCODE_PUSH_PRIMVAL,        /* push primitive value of host language */
  SCM_OPCODE_GREF,                /* refere global variable */
  SCM_OPCODE_GDEF,                /* define global variable */
  SCM_OPCODE_GSET,                /* update global variable */
  /* SCM_OPCODE_SREF,                /\* refere value in stack *\/ */
  /* SCM_OPCODE_CREF                 /\* refere value in closure *\/ */
  SCM_OPCODE_JMP,                  /* jump */
} SCM_OPCODE_T;


#endif /* INCLUDE_API_CONST_H__ */
