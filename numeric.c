#include <string.h>
#include <limits.h>

#include "object.h"
#include "earray.h"
#include "memory.h"
#include "vm.h"
#include "reference.h"
#include "numeric.h"

/***************************************************************************/
/*  Fixnum                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_FIXNUM_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = 0,
  .gc_ini_func = NULL,
  .gc_fin_func = NULL,
  .gc_accept_func = NULL,
  .gc_accept_func_weak = NULL,
};

#if 0

/***************************************************************************/
/*  Bignum                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_BIGNUM_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmBignum),
  .gc_ini_func = scm_bignum_gc_initialize,
  .gc_fin_func = scm_bignum_gc_finalize,
  .gc_accept_func = NULL,
  .gc_accept_func_weak = NULL,
};

void
scm_bignum_initialize(ScmObj bignum, int sign,
                      size_t nr_digit, const scm_bignum_d_t *digits)
{
  int rslt;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(sign == SCM_BIGNUM_SIGN_PLUS || sign == SCM_BIGNUM_SIGN_MINUS);

  SCM_BIGNUM(bignum)->sign = sign;
  SCM_BIGNUM(bignum)->nr_digit = nr_digit;
  rslt = eary_init(&SCM_BIGNUM(bignum)->digits,
                   sizeof(scm_bignum_d_t), nr_digit);
  if (rslt != 0)
    ;                         /* TODO: error handling */

  for (size_t i = 0; i < nr_digit; i++) {
    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, i, digits[i], rslt);
    if (rslt != 0)
      ;                         /* TODO: error handling */
  }
}

void
scm_bignum_finalize(ScmObj bignum)
{
  eary_fin(&SCM_BIGNUM(bignum)->digits);
}



void
scm_bignum_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_BIGNUM_TYPE_INFO);

  eary_init(&SCM_BIGNUM(obj)->digits, 0, 0);
}

void
scm_bignum_gc_finalize(ScmObj obj)
{
  scm_bignum_finalize(obj);
}

#endif
