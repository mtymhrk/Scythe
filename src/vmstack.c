#include <stdint.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/impl_utils.h"
#include "scythe/vmstack.h"


/*******************************************************************/
/*  VM Continuation Frame, Environment Frame                       */
/*******************************************************************/

int
scm_vm_ef_gc_accept(ScmObj owner, ScmEnvFrame *efp, ScmGCRefHandler handler)
{
  ScmObj *partial, *values;
  ScmEnvFrame *ef;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_not_null_p(owner));

  for (ef = efp; ef != NULL; ef = scm_vm_ef_outer(ef)) {
    if (scm_obj_type_p(owner, &SCM_EFBOX_TYPE_INFO)) {
      if (!scm_efbox_include_p(owner, ef))
        break;
    }
    else if (scm_vm_ef_boxed_p(ef)){
      break;
    }

    partial = scm_vm_ef_partial_base(ef);
    for (int i = 0; i < ef->partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, partial[i]);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    values = scm_vm_ef_values(ef);
    for (int i = 0; i < ef->len; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, values[i]);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  if (ef != NULL && scm_vm_ef_boxed_p(ef)) {
    ScmObj efb = scm_efbox_efp_to_owner(ef);
    rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, efb);
  }

  return rslt;
}

int
scm_vm_cf_gc_accept(ScmObj owner, ScmCntFrame *cfp, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_not_null_p(owner));

  while (cfp != NULL) {
    ScmObj *partial = scm_vm_cf_partial_base(cfp);

    for (int i = 0; i < cfp->partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, partial[i]);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    rslt = scm_vm_ef_gc_accept(owner, cfp->efp, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, cfp->cp);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    cfp = scm_vm_cf_next(cfp);
  }

  return rslt;
}


/***************************************************************************/
/*  ScmEnvFrameBox                                                         */
/***************************************************************************/

ScmTypeInfo SCM_EFBOX_TYPE_INFO = {
  .name                         = "efbox",
  .flags                        = SCM_TYPE_FLG_MMO,
  .obj_print_func               = NULL,
  .obj_size                     = sizeof(ScmEFBox),
  .gc_ini_func                  = scm_efbox_gc_initialize,
  .gc_fin_func                  = scm_efbox_gc_finalize,
  .gc_accept_func               = scm_efbox_gc_accept,
  .gc_accept_func_weak          = NULL,
  .extra                        = NULL,
};

static ssize_t
scm_efbox_calc_data_size(ScmEnvFrame *efp, size_t depth)
{
  ScmEnvFrame *e;
  size_t total;

  scm_assert(efp != NULL);
  scm_assert(depth > 0);

  total = 0;
  e = efp;

  for (size_t i = 0; i < depth; i++) {
    if (e == NULL) {
      scm_fcd_error("invalid access to environment frame: out of range", 0);
      return -1;
    }

    if (scm_vm_ef_boxed_p(e))
      break;

    /* + 1 しているのは partial 領域用 */
    total += sizeof(ScmEnvFrame) + sizeof(ScmObj) * ((size_t)e->len + 1);

    e = scm_vm_ef_outer(e);
  }

  return (ssize_t)total;
}

static ScmEnvFrame *
scm_efbox_copy_ef(ScmObj efb, ScmEnvFrame *efp, size_t depth)
{
  ScmEnvFrame *head, *prev, *copy, *e;
  scm_byte_t *p;

  scm_assert(efp != NULL);
  scm_assert(depth > 0);

  e = efp;
  p = SCM_EFBOX(efb)->data;
  head = prev = NULL;
  for (size_t i = 0; i < depth; i++) {
    scm_assert(e != NULL);

    if (scm_vm_ef_boxed_p(e))
      return head;

    /* 自分自身 (efb) を環境フレームの partial 領域に保持する */
    *(ScmObj *)p = efb;
    p += sizeof(ScmObj);

    for (int j = 0; j < e->len; j++) {
      SCM_WB_EXP(efb,
                 *(ScmObj *)p = scm_vm_ef_values(e)[j]);
      p += sizeof(ScmObj);
    }

    copy = (ScmEnvFrame *)p;
    p += sizeof(ScmEnvFrame);

    *copy = *e;
    copy->partial = 1;
    scm_vm_ef_boxed(copy);

    if (prev != NULL)
      scm_vm_ef_replace_outer(prev, copy);

    if (head == NULL)
      head = copy;

    prev = copy;
    e = scm_vm_ef_outer(e);
  }

  scm_vm_ef_replace_outer(copy, NULL);

  return head;
}

int
scm_efbox_initialize(ScmObj efb, ScmEnvFrame *efp, size_t depth)
{
  ssize_t data_size;

  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert(efp != NULL);
  scm_assert(depth > 0 );

  data_size = scm_efbox_calc_data_size(efp, depth);
  if (data_size < 0) return -1;

  if (data_size == 0) {
    SCM_EFBOX(efb)->size = 0;
    SCM_EFBOX(efb)->data = NULL;
    SCM_EFBOX(efb)->efp = efp;
    return 0;
  }

  SCM_EFBOX(efb)->size = (size_t)data_size;
  SCM_EFBOX(efb)->data = scm_fcd_malloc((size_t)data_size);
  if (SCM_EFBOX(efb)->data == NULL) return -1;

  SCM_EFBOX(efb)->efp = scm_efbox_copy_ef(efb, efp, depth);
  if (SCM_EFBOX(efb)->efp == NULL) return -1;

  return 0;
}

void
scm_efbox_finalize(ScmObj efb)
{
  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);

  if (SCM_EFBOX(efb)->data == NULL)
    return;

  scm_fcd_free(SCM_EFBOX(efb)->data);
  SCM_EFBOX(efb)->data = NULL;
}

ScmObj
scm_efbox_new(scm_mem_type_t mtype, ScmEnvFrame *efp, size_t depth)
{
  ScmObj efb = SCM_OBJ_INIT;
  int rslt;

  scm_assert(efp != NULL);
  scm_assert(depth > 0);

  efb = scm_fcd_mem_alloc(&SCM_EFBOX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(efb)) return SCM_OBJ_NULL;

  rslt = scm_efbox_initialize(efb, efp, depth);
  if (rslt < 0) return SCM_OBJ_NULL;

  return efb;
}

void
scm_efbox_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);

  SCM_EFBOX(obj)->efp = NULL;
  SCM_EFBOX(obj)->data = NULL;
  SCM_EFBOX(obj)->size = 0;
}

void
scm_efbox_gc_finalize(ScmObj obj)
{
  scm_efbox_finalize(obj);
}

int
scm_efbox_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);

  if (SCM_EFBOX(obj)->data != NULL) {
    return scm_vm_ef_gc_accept(obj, SCM_EFBOX(obj)->efp, handler);
  }
  else {
    ScmObj holder = scm_efbox_efp_to_owner(SCM_EFBOX(obj)->efp);
    return SCM_GC_CALL_REF_HANDLER(handler, obj, holder);
  }
}


/***************************************************************************/
/*  ScmVMStckSg ScmVMStckRc                                                */
/***************************************************************************/

ScmTypeInfo SCM_VMSTCKSG_TYPE_INFO = {
  .name                            = "vmstcksg",
  .flags                           = SCM_TYPE_FLG_MMO,
  .obj_print_func                  = NULL,
  .obj_size                        = sizeof(ScmVMStckSg),
  .gc_ini_func                     = NULL,
  .gc_fin_func                     = scm_vmss_gc_finalize,
  .gc_accept_func                  = NULL,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

ScmTypeInfo SCM_VMSTCKRC_TYPE_INFO = {
  .name                            = "vmstckrc",
  .flags                           = SCM_TYPE_FLG_MMO,
  .obj_print_func                  = NULL,
  .obj_size                        = sizeof(ScmVMStckRc),
  .gc_ini_func                     = scm_vmsr_gc_initialize,
  .gc_fin_func                     = NULL,
  .gc_accept_func                  = scm_vmsr_gc_accept,
  .gc_accept_func_weak             = NULL,
  .extra                           = NULL,
};

int
scm_vmss_initialize(ScmObj vmss, size_t size)
{
  scm_assert_obj_type(vmss, &SCM_VMSTCKSG_TYPE_INFO);

  SCM_VMSTCKSG(vmss)->stack = scm_fcd_malloc(size);
  if (SCM_VMSTCKSG(vmss)->stack == NULL) return -1;

  SCM_VMSTCKSG(vmss)->capacity = size;

  return 0;
}

ScmObj
scm_vmss_new(scm_mem_type_t mtype, size_t size)
{
  ScmObj vmss = SCM_OBJ_INIT;

  vmss = scm_fcd_mem_alloc(&SCM_VMSTCKSG_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vmss)) return SCM_OBJ_NULL;

  if (scm_vmss_initialize(vmss, size) < 0)
    return SCM_OBJ_NULL;

  return vmss;
}

void
scm_vmss_gc_finalize(ScmObj obj)
{
  scm_fcd_free(SCM_VMSTCKSG(obj)->stack);
}

int
scm_vmsr_initialize(ScmObj vmsr, ScmObj segment, scm_byte_t *base, ScmObj next)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert_obj_type(segment, &SCM_VMSTCKSG_TYPE_INFO);
  scm_assert(scm_vmss_base(segment) <= base);
  scm_assert(base <= scm_vmss_ceiling(segment));
  scm_assert_obj_type_accept_null(next, &SCM_VMSTCKRC_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVMStckRc, vmsr, segment, segment);
  SCM_VMSTCKRC(vmsr)->base = base;
  SCM_VMSTCKRC(vmsr)->size = (size_t)(scm_vmss_ceiling(segment) - base);
  SCM_VMSTCKRC(vmsr)->reg.cfp = NULL;
  SCM_VMSTCKRC(vmsr)->reg.efp = NULL;
  SCM_VMSTCKRC(vmsr)->reg.partial = 0;
  SCM_VMSTCKRC(vmsr)->reg.ucf = false;
  SCM_SLOT_SETQ(ScmVMStckRc, vmsr, next, next);
  if (scm_obj_not_null_p(next)) {
    SCM_VMSTCKRC(vmsr)->next_cf = SCM_VMSTCKRC(next)->reg.cfp;
    SCM_VMSTCKRC(vmsr)->next_cf_ucf = SCM_VMSTCKRC(next)->reg.ucf;
  }
  else {
    SCM_VMSTCKRC(vmsr)->next_cf = NULL;
    SCM_VMSTCKRC(vmsr)->next_cf_ucf = false;
  }

  return 0;
}

ScmObj
scm_vmsr_new(scm_mem_type_t mtype, ScmObj stack, scm_byte_t *base, ScmObj next)
{
  ScmObj vmsr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&stack, &next,
                      &vmsr);

  scm_assert_obj_type(stack, &SCM_VMSTCKSG_TYPE_INFO);
  scm_assert(base != NULL);
  scm_assert_obj_type_accept_null(next, &SCM_VMSTCKRC_TYPE_INFO);

  vmsr = scm_fcd_mem_alloc(&SCM_VMSTCKRC_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vmsr)) return SCM_OBJ_NULL;

  if (scm_vmsr_initialize(vmsr, stack, base, next) < 0)
    return SCM_OBJ_NULL;

  return vmsr;
}

void
scm_vmsr_rec(ScmObj vmsr, scm_byte_t *ceil,
             ScmCntFrame *cfp, ScmEnvFrame *efp, int partial, bool ucf)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert(SCM_VMSTCKRC(vmsr)->base <= ceil);
  scm_assert(ceil <= scm_vmss_ceiling(SCM_VMSTCKRC(vmsr)->segment));

  SCM_VMSTCKRC(vmsr)->size = (size_t)(ceil - SCM_VMSTCKRC(vmsr)->base);
  SCM_VMSTCKRC(vmsr)->reg.cfp = cfp;
  SCM_VMSTCKRC(vmsr)->reg.efp = efp;
  SCM_VMSTCKRC(vmsr)->reg.partial = partial;
  SCM_VMSTCKRC(vmsr)->reg.ucf = ucf;
}

void
scm_vmsr_clear(ScmObj vmsr)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);

  SCM_VMSTCKRC(vmsr)->reg.cfp = NULL;
  SCM_VMSTCKRC(vmsr)->reg.efp = NULL;
  SCM_VMSTCKRC(vmsr)->reg.partial = 0;
  SCM_VMSTCKRC(vmsr)->reg.ucf = false;
}

void
scm_vmsr_relink(ScmObj vmsr, ScmObj next, ScmCntFrame *cfp, bool ucf)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert_obj_type_accept_null(next, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert((scm_obj_null_p(next) && cfp == NULL)
             || (scm_obj_not_null_p(next)
                 && (cfp == NULL
                     || scm_vmsr_include_p(next, (scm_byte_t *)cfp))));

  SCM_SLOT_SETQ(ScmVMStckRc, vmsr, next, next);
  SCM_VMSTCKRC(vmsr)->next_cf = cfp;
  SCM_VMSTCKRC(vmsr)->next_cf_ucf = ucf;
}

void
scm_vmsr_relink_cf(ScmObj vmsr, ScmCntFrame *cfp, bool ucf)
{
  scm_assert_obj_type(vmsr, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert((scm_obj_null_p(SCM_VMSTCKRC(vmsr)->next) && cfp == NULL)
             || (scm_obj_not_null_p(SCM_VMSTCKRC(vmsr)->next)
                 && (cfp == NULL
                     || scm_vmsr_include_p(SCM_VMSTCKRC(vmsr)->next, (scm_byte_t *)cfp))));

  SCM_VMSTCKRC(vmsr)->next_cf = cfp;
  SCM_VMSTCKRC(vmsr)->next_cf_ucf = ucf;
}

void
scm_vmsr_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_VMSTCKRC_TYPE_INFO);

  SCM_VMSTCKRC(obj)->segment = SCM_OBJ_NULL;
  SCM_VMSTCKRC(obj)->reg.cfp = NULL;
  SCM_VMSTCKRC(obj)->reg.efp = NULL;
  SCM_VMSTCKRC(obj)->next = SCM_OBJ_NULL;
}

int
scm_vmsr_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  ScmObj *partial;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_VMSTCKRC_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VMSTCKRC(obj)->segment);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VMSTCKRC(obj)->next);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  partial = scm_vmsr_partial_base(obj);
  for (int i = 0; i < SCM_VMSTCKRC(obj)->reg.partial; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, partial[i]);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = scm_vm_cf_gc_accept(obj, SCM_VMSTCKRC(obj)->reg.cfp, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(obj, SCM_VMSTCKRC(obj)->reg.efp, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
