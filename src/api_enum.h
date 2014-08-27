
#ifndef INCLUDE_API_CONST_H__
#define INCLUDE_API_CONST_H__


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
/*  Procedure                                                      */
/*******************************************************************/

typedef enum {
  SCM_PROC_ADJ_UNWISHED = 0x0001,
} SCM_PROC_FLG_T;


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
  SCM_OPCODE_EREBIND,             /* 12; rebind variables */
  SCM_OPCODE_IMMVAL,              /* 12: copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* 13: push value of val register */
  SCM_OPCODE_MVPUSH,              /* 14: push value of val register */
  SCM_OPCODE_GREF,                /* 15: refere global variable */
  SCM_OPCODE_GDEF,                /* 16: define global variable */
  SCM_OPCODE_GSET,                /* 17: update global variable */
  SCM_OPCODE_SREF,                /* 18: refere value in stack */
  SCM_OPCODE_SSET,                /* 19: update value in stack */
  SCM_OPCODE_JMP,                 /* 20: jump */
  SCM_OPCODE_JMPT,                /* 21: jump if true */
  SCM_OPCODE_JMPF,                /* 22: jump if false */
  SCM_OPCODE_BOX,                 /* 23: boxing */
  SCM_OPCODE_CLOSE,               /* 24: make closure */
  SCM_OPCODE_DEMINE,              /* 25: demine variable */
  SCM_OPCODE_EMINE,               /* 26: make enviroment frame */
                                  /*     and make it mine field */
  SCM_OPCODE_EDEMINE,             /* 27: demine enviromnet frame with */
                                  /*     incomplete enviromnet frame as */
                                  /*     initial value */
  SCM_OPCODE_MRVC,                /* 29: Multiple-Return-Value Check */
  SCM_OPCODE_MRVE,                /* 30: Multiple-Return-Value Error */
} SCM_OPCODE_T;

typedef enum {
  SCM_OPFMT_NOOPD = 0,          /* op(16)  */
  SCM_OPFMT_OBJ,                /* op(16) || scmobj(1 word) */
  SCM_OPFMT_OBJ_OBJ,            /* op(16) || scmobj(1 word) || scmobj(1 word) */
  SCM_OPFMT_SI,                 /* op(16) || signed_int(32) */
  SCM_OPFMT_SI_SI,              /* op(16) || signed_int(32) || signed_int(32) */
  SCM_OPFMT_SI_SI_OBJ,          /* op(16) || signed_int(32) || signed_int(32) || scmobj(1 word) */
  SCM_OPFMT_IOF,                /* op(16) || offset_to_dst_ip(32) */
} SCM_OPFMT_T;

#define SCM_OPSIZE 2

#define SCM_OPFMT_INST_SZ_NOOPD      SCM_OPSIZE
#define SCM_OPFMT_INST_SZ_OBJ        (SCM_OPSIZE + sizeof(ScmObj))
#define SCM_OPFMT_INST_SZ_OBJ_OBJ    (SCM_OPSIZE + sizeof(ScmObj) * 2)
#define SCM_OPFMT_INST_SZ_SI         (SCM_OPSIZE + sizeof(int))
#define SCM_OPFMT_INST_SZ_SI_SI      (SCM_OPSIZE + sizeof(int) * 2)
#define SCM_OPFMT_INST_SZ_SI_SI_OBJ  (SCM_OPSIZE + sizeof(int) * 2 + sizeof(ScmObj))
#define SCM_OPFMT_INST_SZ_IOF        (SCM_OPSIZE + sizeof(int))

#define SCM_INST_SZ_NOP         SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_HALT        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_UNDEF       SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_CALL        SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_TAIL_CALL   SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_RETURN      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_FRAME       SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_CFRAME      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_EFRAME      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_ECOMMIT     SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_EPOP        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_EREBIND     SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_IMMVAL      SCM_OPFMT_INST_SZ_OBJ
#define SCM_INST_SZ_PUSH        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_MVPUSH      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_GREF        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_GDEF        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_GSET        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_SREF        SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_SSET        SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_JMP         SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_JMPT        SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_JMPF        SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_BOX         SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_CLOSE       SCM_OPFMT_INST_SZ_SI_SI_OBJ
#define SCM_INST_SZ_DEMINE      SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_EMINE       SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_EDEMINE     SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_MRVC        SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_MRVE        SCM_OPFMT_INST_SZ_NOOPD


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


/*******************************************************************/
/*  Cached Global Variables                                        */
/*******************************************************************/

enum {
  SCM_CACHED_GV_COMPILE,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
};


#endif /* INCLUDE_API_CONST_H__ */
