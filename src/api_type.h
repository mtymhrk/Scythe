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
/*  Syntax                                                         */
/*******************************************************************/

typedef ScmObj (*ScmSyntaxHandlerFunc)(ScmObj cmpl, ScmObj exp, ScmObj env,
                                       ScmObj next, int arity,
                                       bool tail_p, bool toplevel_p,
                                       ssize_t *rdepth);


/*******************************************************************/
/*  RefStack                                                       */
/*******************************************************************/

struct ScmRefStackBlockRec {
  ScmRefStackBlock *next;
  ScmRefStackBlock *prev;
  size_t size;
  ScmRef *sp;
  ScmRef stack[0];
};

struct ScmRefStackInfoRec {
  ScmRefStackBlock *current;
  ScmRef *sp;
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
