#ifndef INCLUDE_VMSTACK_H__
#define INCLUDE_VMSTACK_H__

#include <stdint.h>
#include <stddef.h>

typedef struct ScmEnvFrameRec ScmEnvFrame;
typedef struct ScmCntFrameRec ScmCntFrame;

typedef struct ScmEFBoxRec ScmEFBox;

#define SCM_EFBOX(obj) ((ScmEFBox *)(obj))

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
  uint8_t *ip;
};


inline ScmCntFrame *
scm_vm_cf_next(ScmCntFrame *cfp) {
  scm_assert(cfp != NULL);
  return (ScmCntFrame *)((uint8_t *)cfp + (cfp->offset & ~0x03));
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
  return ((efp->out & 0x01) == 0) ? false : true;
}

inline bool
scm_vm_ef_boxed_p(ScmEnvFrame *efp)
{
  scm_assert(efp != NULL);
  return ((efp->out & 0x02) == 0) ? false : true;
}

inline bool
scm_vm_ef_in_stack_p(ScmEnvFrame *efp)
{
  return (efp == NULL || scm_vm_ef_boxed_p(efp)) ? false : true;
}

inline void
scm_vm_cf_init(ScmCntFrame *cfp,
               ScmCntFrame *cur_cfp, ScmEnvFrame *efp, ScmObj cp, uint8_t *ip,
               bool pef, bool pcf)
{
  scm_assert(SCM_ALIGNOF(ScmCntFrame) == SCM_ALIGNOF(ScmObj));
  scm_assert(SCM_ALIGNOF(ScmCntFrame) >= SCM_VM_FRAME_MIN_ALIGN_SIZE);

  scm_assert(cfp != NULL);
  scm_assert(((uintptr_t)cfp & 0x03) == 0);
  scm_assert(((uintptr_t)cur_cfp & 0x03) == 0);

  cfp->offset = (uint8_t *)cur_cfp - (uint8_t *)cfp;;
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
  if (pef) efp->out |= 0x01;
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
  uint8_t dummy[sizeof(ScmForward) - sizeof(ScmObjHeader)];
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
    return SCM_OBJ((uint8_t *)efp - offsetof(ScmEFBox, frame));
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


#endif /* INCLUDE_VMSTACK_H__ */
