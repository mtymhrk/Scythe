
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
  SCM_OPCODE_UNDEF,               /* 2: update val register to undefined */
                                  /*    value */
  SCM_OPCODE_CALL,                /* 3: function call */
  SCM_OPCODE_TAIL_CALL,           /* 4: function tail call */
  SCM_OPCODE_RETURN,              /* 5: return from function */
  SCM_OPCODE_FRAME,               /* 6; create continuation frame */
                                  /*    and envrionment frame */
  SCM_OPCODE_CFRAME,              /* 7; create continuation frame */
  SCM_OPCODE_EFRAME,              /* 8; create environment frame */
  SCM_OPCODE_ECOMMIT,             /* 9; commit environment frame */
  SCM_OPCODE_EPOP,                /* 10; pop environment frame */
  SCM_OPCODE_IMMVAL,              /* 11: copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* 12: push value of val register */
  SCM_OPCODE_GREF,                /* 13: refere global variable */
  SCM_OPCODE_GDEF,                /* 14: define global variable */
  SCM_OPCODE_GSET,                /* 15: update global variable */
  SCM_OPCODE_SREF,                /* 16: refere value in stack */
  SCM_OPCODE_SSET,                /* 17: update value in stack */
  SCM_OPCODE_JMP,                 /* 18: jump */
  SCM_OPCODE_JMPF,                /* 19: jump if false */
  SCM_OPCODE_RAISE,               /* 20: exception handler */
  SCM_OPCODE_BOX,                 /* 21: boxing */
  SCM_OPCODE_CLOSE,               /* 22: make closure */
  SCM_OPCODE_DEMINE,              /* 23: demine variable */
  SCM_OPCODE_EMINE,               /* 24: make enviroment frame */
                                  /*     and make it mine field */
  SCM_OPCODE_EDEMINE,             /* 25: demine enviromnet frame with */
                                  /*     incomplete enviromnet frame as */
                                  /*     initial value */
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
