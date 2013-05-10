#ifndef INCLUDE_API_TYPE_H__
#define INCLUDE_API_TYPE_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmForwardRec ScmForward;

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

typedef int (*ScmSubrFunc)(int argc, const ScmObj *argv);


/*******************************************************************/
/*  Syntax                                                         */
/*******************************************************************/

typedef ScmObj (*ScmSyntaxHandlerFunc)(ScmObj cmpl, ScmObj exp, ScmObj env,
                                       ScmObj next, int arity,
                                       bool tail_p, bool toplevel_p,
                                       ssize_t *rdepth);


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

struct ScmEvaluatorRec {
  ScmObj vm;
};



#endif /* INCLUDE_API_TYPE_H__ */
