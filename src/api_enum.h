
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
