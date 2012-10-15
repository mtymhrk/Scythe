#ifndef INCLUDE_VMSTACK_H__
#define INCLUDE_VMSTACK_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmEnvFrameRec ScmEnvFrame;
typedef struct ScmCntFrameRec ScmCntFrame;

typedef struct ScmEFBoxRec ScmEFBox;

typedef struct ScmVMStckSgRec ScmVMStckSg;
typedef struct ScmVMStckRcRec ScmVMStckRc;

typedef struct ScmContCapRec ScmContCap;

#define SCM_EFBOX(obj) ((ScmEFBox *)(obj))

#define SCM_VMSTCKSG(obj) ((ScmVMStckSg*)(obj))
#define SCM_VMSTCKRC(obj) ((ScmVMStckRc*)(obj))

#define SCM_CONTCAP(obj) ((ScmContCap *)(obj))

#include "object.h"


/*******************************************************************/
/*  VM Continuation Frame, Environment Frame                       */
/*******************************************************************/

/*
 * 継続フレームと環境フレームの環実装上の制約:
 * 1. 構造体のアライメントサイズが ScmObj のアライメントサイズと同じでないと
 *    いけない
 *    --> VM スタック操作ではアライメントを考慮したな処理を行っていないため
 * 2. 構造体のアライメントサイズが SCM_VM_FRAME_MIN_ALIGN_SIZE(4) 以上でない
 *    といけない
 *    --> ポインタ値の下位 2bit をフラグとして使用しているため
 *
 */

#define SCM_VM_FRAME_MIN_ALIGN_SIZE 4


/* out メンバの下位 2bit をフラグとして使用する
 *   0 bit: 作りかけの環境フレームの直上に作られたフレームはこの bit をセット
 *          する。continuation 関連の操作に使用する予定。
 *   1 bit: box 化してヒープにコピーされた環境フレームはこの bit をセットする。
 *          GC 時のフレームのトレース等に使用する。
 */
struct ScmEnvFrameRec {
  uintptr_t out;
  size_t len;
  ScmObj arg[0];
};

/* offset メンバの下位 2bit をフラグとして使用する
 *   0 bit: 作りかけの環境フレームの直上に作られたフレームはこの bit をセット
 *          する。continuation 関連の操作に使用する予定。
 *   1 bit: 継続フレームを作成し、対応する call 命令を実行する前に新たに継続フ
 *          レームを作成した場合、新しく作成したフレームのこの bit をセットす
 *          る。バックトレース情報取得に使用する予定。
 */
struct ScmCntFrameRec {
  ptrdiff_t offset;
  ScmEnvFrame *efp;
  ScmObj cp;
  scm_byte_t *ip;
};

int scm_vm_ef_gc_accept(ScmObj owner, ScmEnvFrame **efp,
                        ScmObj mem, ScmGCRefHandlerFunc handler);
int scm_vm_cf_gc_accept(ScmObj owner, ScmCntFrame *cfp,
                        ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmCntFrame *
scm_vm_cf_next(ScmCntFrame *cfp) {
  scm_assert(cfp != NULL);
  return (ScmCntFrame *)((scm_byte_t *)cfp + (cfp->offset & ~0x03));
}

inline ScmEnvFrame *
scm_vm_ef_outer(ScmEnvFrame *efp)
{
  scm_assert(efp != NULL);
  return (ScmEnvFrame *)((uintptr_t)efp->out & ~0x3u);
}

inline bool
scm_vm_cf_maked_on_pef_p(ScmCntFrame *cfp)
{
  scm_assert(cfp != NULL);
  return ((cfp->offset & 0x01) == 0) ? false : true;
}

inline bool
scm_vm_cf_maked_on_pcf_p(ScmCntFrame *cfp)
{
  scm_assert(cfp != NULL);
  return ((cfp->offset & 0x02) == 0) ? false : true;
}

inline bool
scm_vm_ef_maked_on_pef_p(ScmEnvFrame *efp)
{
  scm_assert(efp != NULL);
  return ((efp->out & 0x01u) == 0) ? false : true;
}

inline bool
scm_vm_ef_boxed_p(ScmEnvFrame *efp)
{
  scm_assert(efp != NULL);
  return ((efp->out & 0x02u) == 0) ? false : true;
}

inline bool
scm_vm_ef_in_stack_p(ScmEnvFrame *efp)
{
  return (efp == NULL || scm_vm_ef_boxed_p(efp)) ? false : true;
}

inline void
scm_vm_cf_init(ScmCntFrame *cfp,
               ScmCntFrame *cur_cfp, ScmEnvFrame *efp, ScmObj cp,
               scm_byte_t *ip, bool pef, bool pcf)
{
  scm_assert(SCM_ALIGNOF(ScmCntFrame) == SCM_ALIGNOF(ScmObj));
  scm_assert(SCM_ALIGNOF(ScmCntFrame) >= SCM_VM_FRAME_MIN_ALIGN_SIZE);

  scm_assert(cfp != NULL);
  scm_assert(((uintptr_t)cfp & 0x03) == 0);
  scm_assert(((uintptr_t)cur_cfp & 0x03) == 0);

  cfp->offset = (scm_byte_t *)cur_cfp - (scm_byte_t *)cfp;
  cfp->efp = efp;
  cfp->cp = cp;
  cfp->ip = ip;

  if (pef) cfp->offset |= 0x01;
  if (pcf) cfp->offset |= 0x02;
}

inline void
scm_vm_ef_init(ScmEnvFrame *efp, ScmEnvFrame *out, size_t len, bool pef)
{
  scm_assert(SCM_ALIGNOF(ScmEnvFrame) == SCM_ALIGNOF(ScmObj));
  scm_assert(SCM_ALIGNOF(ScmEnvFrame) >= SCM_VM_FRAME_MIN_ALIGN_SIZE);

  scm_assert(efp != NULL);
  scm_assert(((uintptr_t)efp & 0x03) == 0);
  scm_assert(((uintptr_t)out & 0x03) == 0);

  efp->out = (uintptr_t)out;
  efp->len = len;
  if (pef) efp->out |= 0x01u;
}

inline void
scm_vm_ef_replace_outer(ScmEnvFrame *efp, ScmEnvFrame *out)
{
  scm_assert(((uintptr_t)out & 0x03) == 0);

  efp->out = (uintptr_t)out | (efp->out & 0x03);
}

inline void
scm_vm_ef_boxed(ScmEnvFrame *efp)
{
  efp->out |= 0x02;
}


/***************************************************************************/
/*  ScmEnvFrameBox                                                         */
/***************************************************************************/

extern ScmTypeInfo SCM_EFBOX_TYPE_INFO;

/* GC 時、copying gc のコピー元のオブジェクトはコピー後に ScmForward オブ
 * ジェクトで上書きされる。GC 時に frame メンバのフラグ情報を利用する必要
 * があるが、frame メンバが上書きされると、コピー元をまだ参照している変数
 * を処理する際にその情報を得ることができなくなってしまう。上書きされても
 * frame メンバの情報を読みとれるよう、ScmFoward オブジェクトのサイズ以降
 * に frame メンバを配置するために dummy メンバを追加している。
 */
struct ScmEFBoxRec {
  ScmObjHeader header;
  scm_byte_t dummy[sizeof(ScmForward) - sizeof(ScmObjHeader)];
  ScmEnvFrame frame;
};

int scm_efbox_initialize(ScmObj efb, ScmEnvFrame *ef);
ScmObj scm_efbox_new(SCM_MEM_TYPE_T mtype, ScmEnvFrame *ef);
void scm_efbox_gc_initialize(ScmObj obj, ScmObj mem);
int scm_efbox_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmEnvFrame *
scm_efbox_to_efp(ScmObj efb)
{
  scm_assert_obj_type_accept_null(efb, &SCM_EFBOX_TYPE_INFO);

  if (scm_obj_null_p(efb))
    return NULL;
  else
    return &(SCM_EFBOX(efb)->frame);
}

inline ScmObj
scm_efbox_efp_to_efbox(ScmEnvFrame *efp)
{
  if (efp == NULL)
    return SCM_OBJ_NULL;
  else
    return SCM_OBJ((scm_byte_t *)efp - offsetof(ScmEFBox, frame));
}

inline void
scm_efbox_update_outer(ScmObj efb, ScmObj outer)
{
  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert_obj_type_accept_null(outer, &SCM_EFBOX_TYPE_INFO);

  if (scm_obj_null_p(outer))
    scm_vm_ef_replace_outer(&SCM_EFBOX(efb)->frame, NULL);
  else
    SCM_WB_EXP(efb,
               scm_vm_ef_replace_outer(&SCM_EFBOX(efb)->frame,
                                       scm_efbox_to_efp(outer)));
}


/***************************************************************************/
/*  ScmVMStckSg ScmVMStckRc                                                */
/***************************************************************************/

extern ScmTypeInfo SCM_VMSTCKSG_TYPE_INFO;
extern ScmTypeInfo SCM_VMSTCKRC_TYPE_INFO;

struct ScmVMStckSgRec {
  ScmObjHeader header;
  scm_byte_t *stack;
  size_t capacity;
};

struct ScmVMStckRcRec {
  ScmObjHeader header;
  ScmObj segment;
  scm_byte_t *base;
  size_t size;
  struct {
    ScmCntFrame *cfp;
    ScmEnvFrame *efp;
    ScmEnvFrame *pefp;
    bool pcf;
    bool pef;
  } reg;
  ScmObj next;
  ScmCntFrame *next_cf;
};

int scm_vmss_initialize(ScmObj vmss, size_t size);
ScmObj scm_vmss_new(SCM_MEM_TYPE_T mtype, size_t size);
void scm_vmss_gc_finalize(ScmObj obj);

int scm_vmsr_initialize(ScmObj vmsr, ScmObj segment,
                        scm_byte_t *base, ScmObj next);
ScmObj scm_vmsr_new(SCM_MEM_TYPE_T mtype,
                    ScmObj segment, scm_byte_t *base, ScmObj next);
void scm_vmsr_rec(ScmObj vmsr, scm_byte_t *ceil,
                  ScmCntFrame *cfp, ScmEnvFrame *efp, ScmEnvFrame *pefp,
                  bool pcf, bool pef);
void scm_vmsr_clear(ScmObj vmsr);
void scm_vmsr_relink(ScmObj vmsr, ScmObj next, ScmCntFrame *cfp);
void scm_vmsr_relink_cf(ScmObj vmsr, ScmCntFrame *cfp);
void scm_vmsr_gc_initialize(ScmObj obj, ScmObj mem);
int scm_vmsr_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline scm_byte_t *
scm_vmss_base(ScmObj vmss)
{
  scm_assert_obj_type(vmss, &SCM_VMSTCKSG_TYPE_INFO);

  return SCM_VMSTCKSG(vmss)->stack;
}

inline size_t
scm_vmss_capacity(ScmObj vmss)
{
  scm_assert_obj_type(vmss, &SCM_VMSTCKSG_TYPE_INFO);

  return SCM_VMSTCKSG(vmss)->capacity;
}

inline scm_byte_t *
scm_vmss_ceiling(ScmObj vmss)
{
  scm_assert_obj_type(vmss, &SCM_VMSTCKSG_TYPE_INFO);

  return SCM_VMSTCKSG(vmss)->stack + SCM_VMSTCKSG(vmss)->capacity;
}

inline ScmObj
scm_vmsr_segment(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->segment;
}

inline scm_byte_t *
scm_vmsr_base(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->base;
}

inline scm_byte_t *
scm_vmsr_ceiling(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->base + SCM_VMSTCKRC(vmsr)->size;
}

inline ScmCntFrame *
scm_vmsr_cfp(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->reg.cfp;
}

inline ScmEnvFrame *
scm_vmsr_efp(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->reg.efp;
}

inline ScmEnvFrame *
scm_vmsr_pefp(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->reg.pefp;
}

inline ScmObj
scm_vmsr_next(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->next;
}

inline ScmCntFrame *
scm_vmsr_next_cf(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->next_cf;
}

inline bool
scm_vmsr_pcf_p(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->reg.pcf;
}

inline bool
scm_vmsr_pef_p(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return SCM_VMSTCKRC(vmsr)->reg.pef;
}

inline bool
scm_vmsr_overflow_p(ScmObj vmsr, scm_byte_t *sp)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return ((sp > SCM_VMSTCKRC(vmsr)->base + SCM_VMSTCKRC(vmsr)->size) ?
          true : false);
}

inline bool
scm_vmsr_ceiling_p(ScmObj vmsr, scm_byte_t *sp)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return ((sp == SCM_VMSTCKRC(vmsr)->base + SCM_VMSTCKRC(vmsr)->size) ?
          true : false);
}

inline bool
scm_vmsr_reach_to_ceiling_p(ScmObj vmsr, scm_byte_t *sp)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return ((sp >= SCM_VMSTCKRC(vmsr)->base + SCM_VMSTCKRC(vmsr)->size) ?
          true : false);
}

inline bool
scm_vmsr_underflow_p(ScmObj vmsr, scm_byte_t *sp)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  return ((sp < SCM_VMSTCKRC(vmsr)->base) ? true : false);
}

inline bool
scm_vmsr_include_p(ScmObj vmsr, scm_byte_t *sp)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  if (scm_vmsr_underflow_p(vmsr, sp) || scm_vmsr_reach_to_ceiling_p(vmsr, sp))
    return false;
  else
    return true;
}


#endif /* INCLUDE_VMSTACK_H__ */
