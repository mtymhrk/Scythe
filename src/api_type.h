#ifndef INCLUDE_API_TYPE_H__
#define INCLUDE_API_TYPE_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmForwardRec ScmForward;

typedef struct ScmRefStackBlockRec ScmRefStackBlock;
typedef struct ScmRefStackInfoRec ScmRefStackInfo;

typedef struct ScmEvaluatorRec ScmEvaluator;

#include "object.h"


/*******************************************************************/
/* Forward Object                                                  */
/*******************************************************************/

/* ScmForward オブジェクトの構造体定義を memory.h から移動。ScmEFBox オブジェ
 * クトが ScmForward オブジェクトのサイズを必要とするため。
 */

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

typedef enum {
  SCM_MEM_HEAP,
  SCM_MEM_ROOT,
} SCM_MEM_TYPE_T;


/*******************************************************************/
/*  RefStack                                                       */
/*******************************************************************/

enum { SCM_REFSTACK_RARY, SCM_REFSTACK_ARY };

struct ScmRefStackBlockRec {
  ScmRefStackBlock *next;
  int type;
  union {
    ScmObj **rary;
    struct {
      ScmObj *head;
      size_t n;
    } ary;
  } ref;
};

struct ScmRefStackInfoRec {
  ScmRefStackBlock *stack;
};


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


typedef int (*ScmSubrFunc)(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Cached Global Variables                                        */
/*******************************************************************/

enum {
  SCM_CACHED_GV_COMPILE,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
};


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

struct ScmEvaluatorRec {
  void *bedrock;
  ScmObj vm;
  ScmObj stack;
};


#endif /* INCLUDE_API_TYPE_H__ */
