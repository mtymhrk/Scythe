#ifndef INCLUDE_API_TYPE_H__
#define INCLUDE_API_TYPE_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmForwardRec ScmForward;

typedef struct ScmRefStackBlockRec ScmRefStackBlock;
typedef struct ScmRefStackInfoRec ScmRefStackInfo;

typedef struct ScmEvaluatorRec ScmEvaluator;

#include "object.h"


/****************************************************************************/
/* Forward Object                                                           */
/****************************************************************************/

/* ScmForward オブジェクトの構造体定義を memory.h から移動。ScmEFBox オブジェ
 * クトが ScmForward オブジェクトのサイズを必要とするため。
 */

struct ScmForwardRec {
  ScmObjHeader header;
  ScmObj forward;
};


/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

typedef int (*ScmSubrFunc)(ScmObj subr, int argc, const ScmObj *argv);


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
/*  Facade                                                         */
/*******************************************************************/

struct ScmEvaluatorRec {
  void *bedrock;
  ScmObj vm;
  ScmObj stack;
};



#endif /* INCLUDE_API_TYPE_H__ */
