#include <stdint.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/api.h"
#include "scythe/impl_utils.h"
#include "scythe/vmstack.h"


/*******************************************************************/
/*  VM Continuation Frame, Environment Frame                       */
/*******************************************************************/

int
scm_vm_ef_gc_accept(ScmObj owner, ScmEnvFrame **efp,
                    ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj efb = SCM_OBJ_INIT;
  ScmEnvFrame *prv_ef, *ef;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_not_null_p(owner));
  scm_assert(efp != NULL);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  prv_ef = NULL;
  ef = *efp;
  while (scm_vm_ef_in_stack_p(ef)) {
    ScmObj *partial = scm_vm_ef_partial_base(ef);

    for (int i = 0; i < ef->len; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     owner, scm_vm_ef_values(ef)[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    for (int i = 0; i < ef->partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, partial[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    prv_ef = ef;
    ef = scm_vm_ef_outer(ef);
  }

  if (ef != NULL && scm_vm_ef_boxed_p(ef)) {
    efb = scm_efbox_efp_to_efbox(ef);
    rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, efb, mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    if (prv_ef == NULL)
      *efp = scm_efbox_to_efp(efb);
    else
      scm_vm_ef_replace_outer(prv_ef, scm_efbox_to_efp(efb));
  }

  return rslt;
}

int
scm_vm_cf_gc_accept(ScmObj owner, ScmCntFrame *cfp,
                    ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_obj_not_null_p(owner));
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  while (cfp != NULL) {
    ScmObj *partial = scm_vm_cf_partial_base(cfp);

    for (int i = 0; i < cfp->partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, partial[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    rslt = scm_vm_ef_gc_accept(owner, &cfp->efp, mem, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, cfp->cp, mem);
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
  .gc_fin_func                  = NULL,
  .gc_accept_func               = scm_efbox_gc_accept,
  .gc_accept_func_weak          = NULL,
  .extra                        = NULL,
};

int
scm_efbox_initialize(ScmObj efb, ScmEnvFrame *ef)
{
  ScmEnvFrame *out, *efp;

  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert(ef != NULL);
  scm_assert(!scm_vm_ef_boxed_p(ef));

  SCM_EFBOX(efb)->offset = (ptrdiff_t)(sizeof(ScmObj) * (size_t)ef->len);

  efp = scm_efbox_to_efp(efb);
  efp->partial = 0;
  efp->len = ef->len;

  for (int i = 0; i < ef->len; i++) {
    SCM_WB_EXP(efb,
               scm_vm_ef_value_set(efp, i, scm_vm_ef_value_ref(ef, i)));
  }

  out = scm_vm_ef_outer(ef);
  if (out != NULL && scm_vm_ef_boxed_p(out))
    scm_vm_ef_replace_outer(efp, out);
  else
    scm_vm_ef_replace_outer(efp, NULL);

  scm_vm_ef_boxed(efp);

  return 0;
}

ScmObj
scm_efbox_new(SCM_MEM_TYPE_T mtype, ScmEnvFrame *ef)
{
  ScmObj efb = SCM_OBJ_INIT;
  int rslt;

  scm_assert(ef != NULL);
  scm_assert(!scm_vm_ef_boxed_p(ef));

  efb = scm_capi_mem_alloc(&SCM_EFBOX_TYPE_INFO,
                           sizeof(ScmEnvFrame) + sizeof(ScmObj) * (size_t)ef->len,
                           mtype);
  if (scm_obj_null_p(efb)) return SCM_OBJ_NULL;

  rslt = scm_efbox_initialize(efb, ef);
  if (rslt < 0) return SCM_OBJ_NULL;

  return efb;
}

void
scm_efbox_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);

  SCM_EFBOX(obj)->offset = -1;
}

int
scm_efbox_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj outer = SCM_OBJ_INIT;
  ScmEnvFrame *efp;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (SCM_EFBOX(obj)->offset == -1)
    return rslt;

  efp = scm_efbox_to_efp(obj);

  /* XXX: EFBox 内の frame.out は必ずボックス化された frame を指す */
  /*      (VM stack 上の enrioment frame を指すことはない)         */
  outer = scm_efbox_efp_to_efbox(scm_vm_ef_outer(efp));
  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, outer, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  scm_vm_ef_replace_outer(efp, scm_efbox_to_efp(outer));

  for (int i = 0; i < efp->len; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, scm_vm_ef_values(efp)[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
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

  SCM_VMSTCKSG(vmss)->stack = scm_capi_malloc(size);
  if (SCM_VMSTCKSG(vmss)->stack == NULL) return -1;

  SCM_VMSTCKSG(vmss)->capacity = size;

  return 0;
}

ScmObj
scm_vmss_new(SCM_MEM_TYPE_T mtype, size_t size)
{
  ScmObj vmss = SCM_OBJ_INIT;

  vmss = scm_capi_mem_alloc(&SCM_VMSTCKSG_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(vmss)) return SCM_OBJ_NULL;

  if (scm_vmss_initialize(vmss, size) < 0)
    return SCM_OBJ_NULL;

  return vmss;
}

void
scm_vmss_gc_finalize(ScmObj obj)
{
  scm_capi_free(SCM_VMSTCKSG(obj)->stack);
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
scm_vmsr_new(SCM_MEM_TYPE_T mtype, ScmObj stack, scm_byte_t *base, ScmObj next)
{
  ScmObj vmsr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&stack, &next,
                      &vmsr);

  scm_assert_obj_type(stack, &SCM_VMSTCKSG_TYPE_INFO);
  scm_assert(base != NULL);
  scm_assert_obj_type_accept_null(next, &SCM_VMSTCKRC_TYPE_INFO);

  vmsr = scm_capi_mem_alloc(&SCM_VMSTCKRC_TYPE_INFO, 0, mtype);
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
scm_vmsr_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VMSTCKRC_TYPE_INFO);

  SCM_VMSTCKRC(obj)->segment = SCM_OBJ_NULL;
  SCM_VMSTCKRC(obj)->reg.cfp = NULL;
  SCM_VMSTCKRC(obj)->reg.efp = NULL;
  SCM_VMSTCKRC(obj)->next = SCM_OBJ_NULL;
}

int
scm_vmsr_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj *partial;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VMSTCKRC(obj)->segment, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VMSTCKRC(obj)->next, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  partial = scm_vmsr_partial_base(obj);
  for (int i = 0; i < SCM_VMSTCKRC(obj)->reg.partial; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, partial[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = scm_vm_cf_gc_accept(obj, SCM_VMSTCKRC(obj)->reg.cfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(obj, &SCM_VMSTCKRC(obj)->reg.efp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
