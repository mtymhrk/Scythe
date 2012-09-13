#include <stdint.h>
#include <string.h>

#include "object.h"
#include "api.h"
#include "impl_utils.h"
#include "vmstack.h"


/***************************************************************************/
/*  ScmEnvFrameBox                                                         */
/***************************************************************************/

ScmTypeInfo SCM_EFBOX_TYPE_INFO = {
  .name = "efbox",
  .flags = SCM_TYPE_FLG_MMO,
  .pp_func = NULL,
  .obj_size = sizeof(ScmEFBox),
  .gc_ini_func = scm_efbox_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_efbox_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

int
scm_efbox_initialize(ScmObj efb, ScmEnvFrame *ef)
{
  ScmEnvFrame *out;

  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert(ef != NULL);
  scm_assert(!scm_vm_ef_boxed_p(ef));

  memcpy(&SCM_EFBOX(efb)->frame, ef, sizeof(*ef) + sizeof(ScmObj) * ef->len);

  out = scm_vm_ef_outer(ef);
  if (out != NULL && scm_vm_ef_boxed_p(out))
    scm_vm_ef_replace_outer(&SCM_EFBOX(efb)->frame, out);
  else
    scm_vm_ef_replace_outer(&SCM_EFBOX(efb)->frame, NULL);

  scm_vm_ef_boxed(&SCM_EFBOX(efb)->frame);

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
                           sizeof(ScmObj) * ef->len, mtype);
  if (scm_obj_null_p(efb)) return SCM_OBJ_NULL;

  rslt = scm_efbox_initialize(efb, ef);
  if (rslt < 0) return SCM_OBJ_NULL;

  return efb;
}

void
scm_efbox_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);

  SCM_EFBOX(obj)->frame.out = 0;
  SCM_EFBOX(obj)->frame.len = 0;
}

int
scm_efbox_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj outer = SCM_OBJ_INIT;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  /* XXX: EFBox 内の frame.out は必ずボックス化された frame を指す */
  /*      (VM stack 上の enrioment frame を指すことはない)         */
  outer = scm_efbox_efp_to_efbox(scm_vm_ef_outer(&SCM_EFBOX(obj)->frame));
  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, outer, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  scm_vm_ef_replace_outer(&SCM_EFBOX(obj)->frame, scm_efbox_to_efp(outer));

  for (size_t i = 0; i < SCM_EFBOX(obj)->frame.len; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_EFBOX(obj)->frame.arg[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
