#ifndef INCLUDE_API_CONST_H__
#define INCLUDE_API_CONST_H__

/*******************************************************************/
/*  Encoding                                                       */
/*******************************************************************/

typedef enum {
  SCM_ENC_ASCII,
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
  SCM_OPCODE_NOP = 0x0000,        /* 0: no operation */
  SCM_OPCODE_HALT,                /* 1:stop calculation */
  SCM_OPCODE_CALL,                /* 2: function call */
  SCM_OPCODE_TAIL_CALL,           /* 3: function tail call */
  SCM_OPCODE_RETURN,              /* 4: return from function */
  SCM_OPCODE_FRAME,               /* 5; create stack frame */
  SCM_OPCODE_IMMVAL,              /* 6: copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* 7: push value of val register */
  SCM_OPCODE_GREF,                /* 8: refere global variable */
  SCM_OPCODE_GDEF,                /* 9: define global variable */
  SCM_OPCODE_GSET,                /* 10: update global variable */
  /* SCM_OPCODE_SREF,                /\* refere value in stack *\/ */
  /* SCM_OPCODE_CREF                 /\* refere value in closure *\/ */
  SCM_OPCODE_JMP,                  /* 11: jump */
  SCM_OPCODE_RAISE                 /* 12: exception handler */
} SCM_OPCODE_T;


/*******************************************************************/
/*  Assembler Pseudo-Instructions                                  */
/*******************************************************************/

enum {
  SCM_ASM_PI_LABEL = 0x10000,  /* define a label */
  SCM_ASM_PI_ASM,              /* make ScmISeq object
                                  and set it to VAL register */
};



#endif /* INCLUDE_API_CONST_H__ */
