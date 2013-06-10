#include <stddef.h>
#include <limits.h>

#include "number_common.h"
#include "fixnum.h"
#include "bignum.h"

int
scm_num_calc_base_and_place_for_ary_of_digits(int radix,
                                              scm_bignum_c_t *base,
                                              int *place)
{
  static int place_tab[] = { 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0};
  static scm_bignum_c_t base_tab[] = { 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0 };
  scm_assert(radix > 0);
  scm_assert(base != NULL);
  scm_assert(place != NULL);

  if ((size_t)radix >= sizeof(place_tab)/sizeof(place_tab[0]))
    return -1;

  if (place_tab[radix] > 0) {
    *place = place_tab[radix];
    *base = base_tab[radix];
  }
  else {
    *place = (int)scm_log_ul((unsigned long)radix, SCM_BIGNUM_BASE);
    *base = (scm_bignum_c_t)scm_pow_ul((unsigned long)radix,
                                     (unsigned long)*place);
    place_tab[radix] = *place;
    base_tab[radix] = *base;
  }

  return 0;
}

ScmObj
scm_num_make_int_from_ary(char sign, scm_bignum_d_t *ary, size_t size,
                          scm_bignum_c_t base)
{
  scm_sword_t num, abs_max;

  scm_assert(ary != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(0 < base);

  if (sign == '+' || sign == '\0')
    abs_max = SCM_FIXNUM_MAX;
  else
    abs_max = -SCM_FIXNUM_MIN;

  num = 0;
  for (ssize_t i = (ssize_t)size - 1; i >= 0; i--) {
    if (num > (abs_max - (scm_sword_t)ary[i]) / (scm_sword_t)base) {
      return scm_bignum_new_from_ary(SCM_MEM_HEAP,
                                     (char)((sign == '\0') ? '+' : sign),
                                     ary, size, base);
    }
    num = num * (scm_sword_t)base + (scm_sword_t)ary[i];
  }

  if (sign == '+' || sign == '\0')
    return scm_fixnum_new(num);
  else
    return scm_fixnum_new(-num);
}
