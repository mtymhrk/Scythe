#ifndef INCLUDE_API_TYPE_H__
#define INCLUDE_API_TYPE_H__

#include <stdint.h>

typedef struct ScmEnvFrameRec ScmEnvFrame;
typedef struct ScmCntFrameRec ScmCntFrame;

typedef struct ScmEvaluatorRec ScmEvaluator;

#include "object.h"

/*******************************************************************/
/*  VM Continuation Frame, Environment Frame                       */
/*******************************************************************/

/* XXX: 構造体サイズが sizeof(ScmObj) の倍数になるよう調整が必要 */
struct ScmEnvFrameRec {
  ScmEnvFrame * out;
  size_t len;
  ScmObj arg[0];
};

/* XXX: 構造体サイズが sizeof(ScmObj) の倍数になるよう調整が必要 */
struct ScmCntFrameRec {
  ScmCntFrame *cfp;
  ScmEnvFrame *efp;
  ScmObj cp;
  ScmObj isp;
  uint8_t *ip;
  ScmObj dummy[0];  /* 構造体サイズを sizeof(ScmObj) の倍数にするためメンバ*/
};


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

struct ScmEvaluatorRec {
  ScmObj vm;
};



#endif /* INCLUDE_API_TYPE_H__ */
