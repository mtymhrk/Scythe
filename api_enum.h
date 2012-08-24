
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
  SCM_OPCODE_HALT,                /* 1: stop calculation */
  SCM_OPCODE_CALL,                /* 2: function call */
  SCM_OPCODE_TAIL_CALL,           /* 3: function tail call */
  SCM_OPCODE_RETURN,              /* 4: return from function */
  SCM_OPCODE_FRAME,               /* 5; create continuation frame */
                                  /*    and envrionment frame */
  SCM_OPCODE_CFRAME,              /* 6; create continuation frame */
  SCM_OPCODE_EFRAME,              /* 7; create environment frame */
  SCM_OPCODE_ECOMMIT,             /* 8; commit environment frame */
  SCM_OPCODE_EPOP,                /* 9; pop environment frame */
  SCM_OPCODE_IMMVAL,              /* 10: copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* 11: push value of val register */
  SCM_OPCODE_GREF,                /* 12: refere global variable */
  SCM_OPCODE_GDEF,                /* 13: define global variable */
  SCM_OPCODE_GSET,                /* 14: update global variable */
  SCM_OPCODE_SREF,                /* 15: refere value in stack */
  SCM_OPCODE_SSET,                /* 16: update value in stack */
  SCM_OPCODE_JMP,                 /* 17: jump */
  SCM_OPCODE_JMPF,                /* 18: jump if false */
  SCM_OPCODE_RAISE,               /* 19: exception handler */
  SCM_OPCODE_BOX,                 /* 20: boxing */
  SCM_OPCODE_CLOSE,               /* 21: make closure */
} SCM_OPCODE_T;

typedef enum {
  SCM_OPFMT_NOOPD = 0,          /* op(16)  */
  SCM_OPFMT_OBJ,                /* op(16) || index_to_scmobj(32) */
  SCM_OPFMT_SI,                 /* op(16) || signed_int(32) */
  SCM_OPFMT_SI_SI,              /* op(16) || signed_int(32) || signed_int(32) */
  SCM_OPFMT_SI_OBJ,             /* op(16) || signed_int(32) || index_to_scmobj(32) */
  SCM_OPFMT_IOF,                /* op(16) || offset_to_dst_ip(32) */
} SCM_OPFMT_T;


/*******************************************************************/
/*  Assembler Pseudo-Instructions                                  */
/*******************************************************************/

#define SCM_ASM_PI_START 0x10000

enum {
  SCM_ASM_PI_LABEL = SCM_ASM_PI_START,  /* define a label */
  SCM_ASM_PI_ASM,                       /* make ScmISeq object
                                           and set it to VAL register */
  SCM_ASM_PI_ASM_CLOSE,                 /* assemble, make a closure,
                                           and set it to VAL register */
};



#endif /* INCLUDE_API_CONST_H__ */
