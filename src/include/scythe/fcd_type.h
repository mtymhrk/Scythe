#ifndef INCLUDE_FCD_TYPE_H__
#define INCLUDE_FCD_TYPE_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmForwardRec ScmForward;

typedef struct ScmEvaluatorRec ScmEvaluator;

#include "scythe/object.h"
#include "scythe/vminst.h"


/*******************************************************************/
/*  ScmBedrock                                                     */
/*******************************************************************/

typedef struct ScmBedrockRec ScmBedrock;

extern ScmBedrock *scm_fcd__current_br;
extern ScmObj scm_fcd__current_vm;
extern ScmObj scm_fcd__current_ref_stack;

static inline ScmBedrock *
scm_fcd_current_br(void)
{
  return scm_fcd__current_br;
}

static inline ScmObj
scm_fcd_current_vm(void)
{
  return scm_fcd__current_vm;
}

static inline ScmObj
scm_fcd_current_ref_stack(void)
{
  return scm_fcd__current_ref_stack;
}

static inline void
scm_fcd_chg_current_br(ScmBedrock *br)
{
  scm_fcd__current_br = br;
}

static inline void
scm_fcd_chg_current_vm(ScmObj vm)
{
  scm_fcd__current_vm = vm;
}

static inline void
scm_fcd_chg_current_ref_stack(ScmObj stack)
{
  scm_fcd__current_ref_stack = stack;
}


/*******************************************************************/
/*  Memory                                                         */
/*******************************************************************/

typedef enum {
  SCM_MEM_HEAP,
  SCM_MEM_ROOT,
} SCM_MEM_TYPE_T;

typedef struct ScmMemRec ScmMem;


/*******************************************************************/
/*  RefStack                                                       */
/*******************************************************************/

enum { SCM_REFSTACK_RARY, SCM_REFSTACK_ARY };

typedef struct ScmRefStackBlockRec ScmRefStackBlock;
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

typedef struct ScmRefStackInfoRec ScmRefStackInfo;
struct ScmRefStackInfoRec {
  ScmRefStackBlock *stack;
};

typedef struct ScmRefStackRec ScmRefStack;
struct ScmRefStackRec {
  ScmObjHeader *header;
  ScmRefStackBlock *stack;
};

#define SCM_REFSTACK(obj) ((ScmRefStack *)(obj))


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
  SCM_CACHED_GV_COMPILE = 0,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
  SCM_CACHED_GV_LOAD_PATH,
};

#define SCM_CACHED_GV_NR 5


/*******************************************************************/
/*  Cached Symbols                                                 */
/*******************************************************************/

enum {
  SCM_CACHED_SYM_QUOTE = 0,
  SCM_CACHED_SYM_QUASIQUOTE,
  SCM_CACHED_SYM_UNQUOTE,
  SCM_CACHED_SYM_UNQUOTE_SPLICING,
};

#define SCM_CACHED_SYM_NR 4


/*******************************************************************/
/*  Bignum                                                         */
/*******************************************************************/

#if SIZEOF_LONG > SIZEOF_INT

typedef unsigned int scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UINT_MAX + 1)

#elif SIZEOF_LLONG > SIZEOF_INT

typedef unsigned int scm_bignum_d_t;
typedef unsigned long long scm_bignum_c_t;
typedef long long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UINT_MAX + 1)

#elif SIZEOF_LONG > SIZEOF_SHORT

typedef unsigned short scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)USHORT_MAX + 1)

#else

typedef unsigned char scm_bignum_d_t;
typedef unsigned long scm_bignum_c_t;
typedef long scm_bignum_sc_t;

#define SCM_BIGNUM_BASE ((scm_bignum_c_t)UCHAR_MAX + 1)

#endif


/*******************************************************************/
/* Assemble/Disassemble                                            */
/*******************************************************************/

enum {
  SCM_DISASM_TK_INST,
  SCM_DISASM_TK_LABEL,
  SCM_DISASM_TK_END,
};

typedef struct ScmDisasmTokenRec ScmDisasmToken;

struct ScmDisasmTokenRec {
  int type;
  struct {
    int fmt;
    union {
      scm_opcode_t op;
      struct scm_vm_inst_noopd     noopd;
      struct scm_vm_inst_obj       obj;
      struct scm_vm_inst_obj_obj   obj_obj;
      struct scm_vm_inst_si        si;
      struct scm_vm_inst_si_si     si_si;
      struct scm_vm_inst_si_si_obj si_si_obj;
      struct scm_vm_inst_iof       iof;
    } i;
  } inst;
  size_t label_id;
};


/*******************************************************************/
/*  Facade                                                         */
/*******************************************************************/

struct ScmEvaluatorRec {
  void *bedrock;
  ScmObj vm;
  ScmObj stack;
};


#endif /* INCLUDE_FCD_TYPE_H__ */
