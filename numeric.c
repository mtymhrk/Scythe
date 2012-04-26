#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "earray.h"
#include "numeric.h"

/***************************************************************************/
/*  Fixnum                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_FIXNUM_TYPE_INFO = {
  .pp_func             = scm_fixnum_pretty_print,
  .obj_size            = 0,
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};

ScmObj
scm_fixnum_plus(ScmObj fn1, ScmObj fn2)
{
  scm_sword_t v;

  scm_assert_obj_type(fn1, &SCM_FIXNUM_TYPE_INFO);
  scm_assert_obj_type(fn2, &SCM_FIXNUM_TYPE_INFO);

  v = scm_fixnum_value(fn1) + scm_fixnum_value(fn2);

  if (v < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < v)
    return scm_bignum_new_from_sword(SCM_MEM_HEAP, v);
  else
    return scm_fixnum_new(v);
}

ScmObj
scm_fixnum_minus(ScmObj fn1, ScmObj fn2)
{
  scm_sword_t v;

  scm_assert_obj_type(fn1, &SCM_FIXNUM_TYPE_INFO);
  scm_assert_obj_type(fn2, &SCM_FIXNUM_TYPE_INFO);

  v = scm_fixnum_value(fn1) - scm_fixnum_value(fn2);

  if (v < SCM_FIXNUM_MIN || SCM_FIXNUM_MAX < v)
    return scm_bignum_new_from_sword(SCM_MEM_HEAP, v);
  else
    return scm_fixnum_new(v);
}

int
scm_fixnum_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[32];
  int rslt;

  scm_assert_obj_type(obj, &SCM_FIXNUM_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "%lld",
           (long long)scm_rshift_arith_sword((scm_sword_t)obj,
                                             SCM_FIXNUM_SHIFT_BIT));

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}


/***************************************************************************/
/*  Bignum                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_BIGNUM_TYPE_INFO = {
  .pp_func             = scm_bignum_pretty_print,
  .obj_size            = sizeof(ScmBignum),
  .gc_ini_func         = scm_bignum_gc_initialize,
  .gc_fin_func         = scm_bignum_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};

static int
scm_bignum_calc_base_and_place_for_ary_of_digits(int radix,
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

static ssize_t
scm_bignum_base_conv(scm_bignum_d_t *digits, size_t len, scm_bignum_c_t fbase,
                     EArray *ary, scm_bignum_c_t tbase)
{
  scm_bignum_c_t d, val, c;
  int err;

  eary_truncate(ary);

  if (fbase == tbase) {
    size_t i;
    for (i = len; i > 0 && digits[i - 1] == 0; i--)
      ;
    for (; i > 0; i--) {
      EARY_SET(ary, scm_bignum_d_t, i - 1, digits[i - 1], err);
      if (err < 0) return -1;
    }
  }
  else {
    for (size_t i = len; i > 0; i--) {
      c = 0;
      for (size_t j = 0; j < EARY_SIZE(ary) || c > 0; j++) {
        if (j < EARY_SIZE(ary))
          EARY_GET(ary, scm_bignum_d_t, j, val);
        else
          val = 0;
        d = (val * fbase + c) % tbase;
        c = (val * fbase + c) / tbase;
        EARY_SET(ary, scm_bignum_d_t, j, d, err);
        if (err < 0) return -1;
      }

      c = (scm_bignum_c_t)digits[i - 1];

      for (size_t j = 0; c > 0; j++) {
        if (j < EARY_SIZE(ary))
          EARY_GET(ary, scm_bignum_d_t, j, val);
        else
          val = 0;
        d = (val + c) % tbase;
        c = (val + c) / tbase;
        EARY_SET(ary, scm_bignum_d_t, j, d, err);
        if (err < 0) return -1;
      }
    }
  }

  if (EARY_SIZE(ary) == 0) {
    EARY_SET(ary, scm_bignum_d_t, 0, 0, err);
    if (err < 0) return -1;
  }

  return (ssize_t)EARY_SIZE(ary);
}

static ScmObj
scm_bignum_adder(ScmObj bn1, bool comp1, ScmObj bn2, bool comp2)
{
  const scm_bignum_c_t mask = SCM_BIGNUM_BASE - 1;
  size_t place;

  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  place = SCM_BIGNUM(bn1)->nr_digits;
  if (place < SCM_BIGNUM(bn2)->nr_digits)
    place = SCM_BIGNUM(bn2)->nr_digits;
  place++;

  if (place == 0) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return SCM_OBJ_NULL;
  }

  {
    scm_bignum_c_t v, v1, v2, c, c1, c2;
    scm_bignum_d_t ary[place];
    size_t len;
    char sign;

    c = 0;
    len = 1;
    c1 = 1;
    c2 = 1;
    for (size_t i = 0; i < place; i++) {
      if (i < SCM_BIGNUM(bn1)->nr_digits)
        EARY_GET(&SCM_BIGNUM(bn1)->digits, scm_bignum_d_t, i, v1);
      else
        v1 = 0;

      if (i < SCM_BIGNUM(bn2)->nr_digits)
        EARY_GET(&SCM_BIGNUM(bn2)->digits, scm_bignum_d_t, i, v2);
      else
        v2 = 0;

      if (comp1) {
        v1 = (~v1 & mask) + c1;
        c1 = (v1 & ~mask) ? 1 : 0;
        v1 = v1 & mask;
      }

      if (comp2) {
        v2 = (~v2 & mask) + c2;
        c2 = (v2 & ~mask) ? 1 : 0;
        v2 = v2 & mask;
      }

      v = v1 + v2 + c;
      c = (v & ~mask) ? 1 : 0;
      v &= mask;

      ary[i] = (scm_bignum_d_t)v;
      if (v > 0) len = i + 1;
    }

    sign = (v & ~(mask >> 1)) ? '-' : '+';
    if (sign == '-') {
      c = 1;
      for (size_t i = 0; i < place; i++) {
        v = (scm_uword_t)ary[i];
        v = (~v & mask) + c;
        c = (v & ~mask) ? 1 : 0;
        v = v & mask;
        ary[i] = (scm_bignum_d_t)v;
        if (v > 0) len = i + 1;
      }
    }

    return scm_bignum_new_from_ary(SCM_MEM_HEAP, sign,
                                   ary, len, SCM_BIGNUM_BASE);
  }
}

int
scm_bignum_initialize_ary(ScmObj bignum,
                          char sign, scm_bignum_d_t *digits, size_t len,
                          scm_bignum_c_t base)
{
  ssize_t n;
  int rslt;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);
  scm_assert(sign == '+' || sign == '-');

  SCM_BIGNUM(bignum)->sign = sign;
  rslt = eary_init(&SCM_BIGNUM(bignum)->digits, sizeof(scm_bignum_d_t), 3);
  if (rslt != 0) return -1;     /* [ERR]: [through] */

  n = scm_bignum_base_conv(digits, len, base,
                           &SCM_BIGNUM(bignum)->digits, SCM_BIGNUM_BASE);
  if (n < 0)  return -1;        /* [ERR]: [through] */

  SCM_BIGNUM(bignum)->nr_digits = (size_t)n;

  return 0;
}

int
scm_bignum_initialize_sword(ScmObj bignum, scm_sword_t val)
{
  scm_uword_t uval, dg;
  int err, rslt;

  scm_assert_obj_type(bignum, &SCM_BIGNUM_TYPE_INFO);

  rslt = eary_init(&SCM_BIGNUM(bignum)->digits, sizeof(scm_bignum_d_t), 2);
  if (rslt != 0) return -1;     /* [ERR]: [through] */

  if (val >= 0) {
    SCM_BIGNUM(bignum)->sign = '+';
    uval = (scm_uword_t)val;
  }
  else {
    SCM_BIGNUM(bignum)->sign = '-';
    uval = (scm_uword_t)-val;
  }

  dg = uval % SCM_BIGNUM_BASE;
  EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 0, dg, err);
  if (err < 0) return -1;

  dg = uval / SCM_BIGNUM_BASE;
  if (dg > 0) {
    EARY_SET(&SCM_BIGNUM(bignum)->digits, scm_bignum_d_t, 1, dg, err);
    if (err < 0) return -1;
    SCM_BIGNUM(bignum)->nr_digits = 2;
  }
  else {
    SCM_BIGNUM(bignum)->nr_digits = 1;
  }

  return 0;
}

ScmObj
scm_bignum_new_from_ary(SCM_MEM_TYPE_T mtype, char sign,
                        scm_bignum_d_t *digits, size_t len, scm_bignum_c_t base)
{
  ScmObj bn = SCM_OBJ_INIT;

  bn = scm_capi_mem_alloc(&SCM_BIGNUM_TYPE_INFO, mtype);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_bignum_initialize_ary(bn, sign, digits, len, base) < 0)
    return SCM_OBJ_NULL;  /* [ERR]: [through] */

  return bn;
}

ScmObj
scm_bignum_new_from_sword(SCM_MEM_TYPE_T mtype, scm_sword_t val)
{
  ScmObj bn = SCM_OBJ_INIT;

  bn = scm_capi_mem_alloc(&SCM_BIGNUM_TYPE_INFO, mtype);
  if (scm_obj_null_p(bn)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_bignum_initialize_sword(bn, val) < 0)
    return SCM_OBJ_NULL;  /* [ERR]: [through] */

  return bn;
}

void
scm_bignum_finalize(ScmObj bignum)
{
  eary_fin(&SCM_BIGNUM(bignum)->digits);
}

ScmObj
scm_bignum_plus(ScmObj bn1, ScmObj bn2)
{
  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  return scm_bignum_adder(bn1, (SCM_BIGNUM(bn1)->sign == '-') ? true : false,
                          bn2, (SCM_BIGNUM(bn2)->sign == '-') ? true : false);
}

ScmObj
scm_bignum_minus(ScmObj bn1, ScmObj bn2)
{
  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  return scm_bignum_adder(bn1, (SCM_BIGNUM(bn1)->sign == '-') ? true : false,
                          bn2, (SCM_BIGNUM(bn2)->sign == '+') ? true : false);
}

ScmObj
scm_bignum_multi(ScmObj bn1, ScmObj bn2)
{
  char sign;
  size_t place;

  scm_assert_obj_type(bn1, &SCM_BIGNUM_TYPE_INFO);
  scm_assert_obj_type(bn2, &SCM_BIGNUM_TYPE_INFO);

  place = SCM_BIGNUM(bn1)->nr_digits + SCM_BIGNUM(bn2)->nr_digits;
  if (place < SCM_BIGNUM(bn1)->nr_digits
      || place < SCM_BIGNUM(bn2)->nr_digits) {
    scm_capi_error("number of digits of Integer overflow", 0);
    return SCM_OBJ_NULL;
  }

  {
    scm_bignum_c_t v, v1, v2, c, c_tmp;
    scm_bignum_d_t ary[place];
    size_t len;

    memset(ary, 0, sizeof(ary));
    len = 1;
    for (size_t i = 0; i < SCM_BIGNUM(bn2)->nr_digits; i++) {
      EARY_GET(&SCM_BIGNUM(bn2)->digits, scm_bignum_d_t, i, v2);
      c = 0;
      for (size_t j = 0; j < SCM_BIGNUM(bn1)->nr_digits || c > 0; j++) {
        if (j < SCM_BIGNUM(bn1)->nr_digits)
          EARY_GET(&SCM_BIGNUM(bn1)->digits, scm_bignum_d_t, j, v1);
        else
          v1 = 0;

        v = v1 * v2;
        c_tmp = v / SCM_BIGNUM_BASE;
        v = v % SCM_BIGNUM_BASE;
        v += ary[i + j] + c;
        c = c_tmp + v / SCM_BIGNUM_BASE;
        v = v % SCM_BIGNUM_BASE;
        ary[i + j] = (scm_bignum_d_t)v;
        if (v > 0) len = i + j + 1;
      }
    }

    if (SCM_BIGNUM(bn1)->sign == SCM_BIGNUM(bn2)->sign)
      sign = '+';
    else
      sign = '-';

    return scm_bignum_new_from_ary(SCM_MEM_HEAP, sign,
                                   ary, len, SCM_BIGNUM_BASE);

  }
}

int
scm_bignum_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  scm_bignum_c_t base;
  scm_bignum_d_t val;
  EArray ary;
  ssize_t sz;
  int rslt, place, width;
  char str[32];

  scm_assert_obj_type(obj, &SCM_BIGNUM_TYPE_INFO);

  rslt = scm_bignum_calc_base_and_place_for_ary_of_digits(10, &base, &place);
  if (rslt < 0) return -1;

  rslt = eary_init(&ary, sizeof(scm_bignum_d_t), SCM_BIGNUM(obj)->nr_digits);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  sz = scm_bignum_base_conv(EARY_HEAD(&SCM_BIGNUM(obj)->digits),
                            EARY_SIZE(&SCM_BIGNUM(obj)->digits),
                            SCM_BIGNUM_BASE,
                            &ary, base);
  if (sz < 0) goto err;       /* [ERR]: [through] */

  if (SCM_BIGNUM(obj)->sign == '-') {
    rslt = scm_capi_write_cstr("-", SCM_ENC_ASCII, port);
    if (rslt < 0) goto err;    /* [ERR]: [through] */
  }

  width = 0;
  for (ssize_t i = sz; i > 0; i--) {
    EARY_GET(&ary, scm_bignum_d_t, i - 1, val);
    snprintf(str, sizeof(str), "%0*u", width, val);
    rslt = scm_capi_write_cstr(str, SCM_ENC_ASCII, port);
    if (rslt < 0) goto err;    /* [ERR]: [through] */
    width = place;
  }

  eary_fin(&ary);
  return 0;

 err:
  eary_fin(&ary);
  return -1;
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


/***************************************************************************/
/*  Parser for Literal                                                     */
/***************************************************************************/

typedef struct ScmNumParseDataRec ScmNumParseData;
typedef struct ScmNumParseDataRealRec ScmNumParseDataReal;

enum { SCM_NUM_PARSE_NONE, SCM_NUM_PARSE_INF, SCM_NUM_PARSE_NAN };
enum { SCM_NUM_PARSE_CMPLX_ORTHOG, SCM_NUM_PARSE_CMPLX_POLAR };

struct ScmNumParseDataRealRec {
  char sign;
  int infinity;
  struct {
    const char *head;
    size_t size;
  } p1;
  char delim;
  struct {
    const char *head;
    size_t size;
  } p2;
  struct {
    char em;
    char sign;
    const char *head;
    size_t size;
  } sfx;
};

struct ScmNumParseDataRec {
  bool exact;
  int radix;
  struct {
    int form;
    union {
      struct {
        bool real_spc;
        ScmNumParseDataReal real;
        bool img_spc;
        ScmNumParseDataReal img;
      } orthog;
      struct {
        ScmNumParseDataReal abs;
        ScmNumParseDataReal arg;
      } polar;
    };
  } cmplx;
};

static ssize_t
scm_num_parse_prefix(const char *literal, size_t size, ScmNumParseData *data)
{
  bool radix, exact;
  size_t idx;

  scm_assert(literal != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(data != NULL);

  data->exact = true;
  data->radix = 10;
  radix = exact = false;
  idx = 0;

  while (!(exact && radix) && idx < size && literal[idx] == '#') {
    idx++;
    if (idx >= size) {
      scm_capi_error("number literal parse error: invalid format", 0);
      return -1;
    }

    switch (literal[idx]) {
    case 'i':                   /* fall through */
    case 'I':
      if (exact) {
        scm_capi_error("number literal parse error: duplicate excatness", 0);
        return -1;
      }
      data->exact = false;
      exact = true;
      break;
    case 'e':                   /* fall through */
    case 'E':
      if (exact) {
        scm_capi_error("number literal parse error: duplicate excatness", 0);
        return -1;
      }
      data->exact = true;
      exact = true;
      break;
    case 'b':                   /* fall through */
    case 'B':
      if (radix) {
        scm_capi_error("number literal parse error: duplicate radix", 0);
        return -1;
      }
      data->radix = 2;
      radix = true;
      break;
    case 'o':                   /* fall through */
    case 'O':
      if (radix) {
        scm_capi_error("number literal parse error: duplicate radix", 0);
        return -1;
      }
      data->radix = 8;
      radix = true;
      break;
    case 'd':                   /* fall through */
    case 'D':
      if (radix) {
        scm_capi_error("number literal parse error: duplicate radix", 0);
        return -1;
      }
      data->radix = 10;
      radix = true;
      break;
    case 'x':                   /* fall through */
    case 'X':
      if (radix) {
        scm_capi_error("number literal parse error: duplicate radix", 0);
        return -1;
      }
      data->radix = 16;
      radix = true;
      break;
    default:
      scm_capi_error("number literal parse error: unsupported prefix", 0);
      return -1;
      break;
    }
    idx++;
  }

  return (ssize_t)idx;
}

static size_t
scm_num_parse_digit(const char *literal, size_t size, const char *digits)
{
  size_t idx;
  for (idx = 0; idx < size && strchr(digits, literal[idx]) != NULL; idx++)
    ;
  return idx;
}

static char
scm_num_parse_sign(const char *literal, size_t size)
{
  if (size < 1) return '\0';
  else if (literal[0] == '+' || literal[0] == '-') return literal[0];
  else return '\0';
}

static const char *
scm_num_digits_string(int radix)
{
  static const char *digits_tbl[] = { NULL, NULL, "01", NULL,
                                      NULL, NULL, NULL, NULL,
                                      "01234567", NULL, "0123456789", NULL,
                                      NULL, NULL, NULL, NULL,
                                      "0123456789abcdefABCDEF" };
  if (radix < 0
      || (size_t)radix >= sizeof(digits_tbl)/sizeof(digits_tbl[0]))
    return NULL;

  return digits_tbl[radix];
}

static ssize_t
scm_num_parse_real(const char *literal, size_t size, int radix,
                   ScmNumParseDataReal *real)
{
  struct infinity_tbl {
    const char *str;
    size_t len;
    int type;
  } inf_tbl[] = { { "inf.0", 5, SCM_NUM_PARSE_INF },
                  { "nan.0", 5, SCM_NUM_PARSE_NAN },
                  { NULL, 0, SCM_NUM_PARSE_NONE } };
  const char *digit;
  size_t idx;

  scm_assert(literal != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(real != NULL);

  digit = scm_num_digits_string(radix);
  if (digit == NULL) {
    scm_capi_error("number literal parse error: unsupported radix", 0);
    return -1;
  }

  if (size < 1) {
    scm_capi_error("number literal parse error: invalid format", 0);
    return 0;
  }

  idx = 0;
  real->sign = scm_num_parse_sign(literal + idx, size - idx);
  if (real->sign != '\0') idx++;

  if (idx >= size) {
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }

  real->infinity = SCM_NUM_PARSE_NONE;

  real->p1.head = literal + idx;
  real->p1.size = scm_num_parse_digit(real->p1.head, size - idx, digit);
  idx += real->p1.size;

  if (idx >= size) {
    real->delim = '\0';
    real->sfx.em = '\0';
  }
  else if (literal[idx] == '/') {
    if (real->p1.size == 0) {
      scm_capi_error("number literal parse error: invalid format", 0);
      return -1;
    }

    real->delim = '/';

    idx++;
    real->p2.head = literal + idx;
    real->p2.size = scm_num_parse_digit(real->p2.head, size - idx, digit);
    idx += real->p2.size;

    if (real->p2.size == 0) {
      scm_capi_error("number literal parse error: invalid format", 0);
      return -1;
    }

    real->sfx.em = '\0';
  }
  else if (radix == 10) {
    if (literal[idx] == '.') {
      idx++;
      real->delim = '.';
      real->p2.head = literal + idx;
      real->p2.size = scm_num_parse_digit(real->p2.head, size - idx, digit);
      if (real->p1.size == 0 && real->p2.size == 0) {
        scm_capi_error("number literal parse error: invalid format", 0);
        return -1;
      }
      idx += real->p2.size;
    }
    else if (real->p1.size == 0) {
      scm_capi_error("number literal parse error: invalid format", 0);
      return -1;
    }
    else {
      real->delim = '\0';
    }

    if (idx < size && strchr("esfdlESFDL", literal[idx]) != NULL) {
      real->sfx.em = literal[idx++];
      if (idx >= size) {
        scm_capi_error("number literal parse error: invalid format", 0);
        return -1;
      }

      real->sfx.sign = scm_num_parse_sign(literal + idx, size - idx);
      if (real->sfx.sign != '\0') idx++;

      if (idx >= size) {
        scm_capi_error("number literal parse error: invalid format", 0);
        return -1;
      }

      real->sfx.head = literal + idx;
      real->sfx.size = scm_num_parse_digit(real->sfx.head, size - idx, digit);
      if (real->sfx.size == 0) return -1;
    }
  }
  else if (real->p1.size > 0) {
    real->delim = '\0';
    real->sfx.em = '\0';
  }
  else if (real->sign != '\0') {
    for (struct infinity_tbl *p = inf_tbl; p->str != NULL; p++) {
      if (size > p->len && strncmp(p->str, literal + idx, size) == 0) {
        idx += p->len;
        real->infinity = p->type;
        return (ssize_t)idx;
      }
    }
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }
  else {
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }

  return (ssize_t)idx;
}

static ssize_t
scm_num_parse_complex(const char *literal, size_t size, ScmNumParseData *data)
{
  ScmNumParseDataReal real;
  ssize_t idx;

  scm_assert(literal != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(data != NULL);

  idx = scm_num_parse_real(literal, size, data->radix, &real);
  if (idx < 0) return -1;       /* [ERR]:[through] */

  if ((size_t)idx == size) {
    data->cmplx.form = SCM_NUM_PARSE_CMPLX_ORTHOG;
    data->cmplx.orthog.real_spc = true;
    data->cmplx.orthog.real = real;
    data->cmplx.orthog.img_spc = false;
  }
  else if (literal[idx] == '@') {
    ssize_t i;

    data->cmplx.form = SCM_NUM_PARSE_CMPLX_POLAR;
    data->cmplx.polar.abs = real;

    i = scm_num_parse_real(literal + idx, size - (size_t)idx,
                           data->radix, &real);
    if (i < 0) return -1;       /* [ERR]: [through] */

    idx += i;
    data->cmplx.polar.arg = real;
  }
  else if (literal[idx] == '+' || literal[idx] == '-') {
    if ((size_t)idx + 1 >= size) {
      scm_capi_error("number literal parse error: invalid format", 0);
      return -1;
    }

    data->cmplx.form = SCM_NUM_PARSE_CMPLX_ORTHOG;
    data->cmplx.orthog.real_spc = true;
    data->cmplx.orthog.real = real;

    if ((literal[idx + 1] == 'i' || literal[idx + 1] == 'I')
        && (size_t)idx + 2 == size) {
      data->cmplx.orthog.img_spc = true;
      data->cmplx.orthog.img.sign = literal[idx];
      data->cmplx.orthog.img.infinity = SCM_NUM_PARSE_NONE;
      data->cmplx.orthog.img.p1.head = NULL;
      data->cmplx.orthog.img.p1.size = 0;
    }
    else {
      ssize_t i = scm_num_parse_real(literal + idx, size - (size_t)idx,
                                     data->radix, &real);
      if (i < 0) return -1;     /* [ERR]: [through] */

      idx += i;
      data->cmplx.orthog.img_spc = true;
      data->cmplx.orthog.img = real;
    }
  }
  else if (literal[idx] == 'i' || literal[idx] == 'I') {
    data->cmplx.form = SCM_NUM_PARSE_CMPLX_ORTHOG;
    data->cmplx.orthog.real_spc = false;
    data->cmplx.orthog.img_spc = true;
    data->cmplx.orthog.img = real;
    idx++;
  }
  else {
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }

  return idx;
}

static int
scm_num_parse_literal(const char *literal, size_t size, ScmNumParseData *data)
{
  ssize_t p_idx, c_idx;

  scm_assert(literal != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(data != NULL);

  p_idx = scm_num_parse_prefix(literal, size, data);
  if (p_idx < 0) return -1;        /* [ERR]: [through] */

  c_idx = scm_num_parse_complex(literal + p_idx, size - (size_t)p_idx, data);
  if (c_idx < 0) return -1;     /* [ERR]: [through] */

  if ((size_t)p_idx + (size_t)c_idx < size) {
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }

  return 0;
}

static int
scm_num_char_to_int(char c)
{
  const char *digits = "0123456789abcdefABCDEF";
  int v;

  char *p = strchr(digits, c);
  if (p == NULL) {
    scm_capi_error("number literal parse error: invalid format", 0);
    return -1;
  }

  v = (int)(p - digits);
  if (v > 15) return v -= 6;

  return v;
}

static ssize_t
scm_num_calc_len_of_ary_of_sword(size_t str_size, int radix)
{
  scm_bignum_c_t base;
  int place, rslt;

  scm_assert(str_size <= SSIZE_MAX);

  rslt = scm_bignum_calc_base_and_place_for_ary_of_digits(radix,
                                                          &base, &place);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return ((ssize_t)str_size / place
          + (((ssize_t)str_size % place) ? 1 : 0));
}

static scm_bignum_sc_t
scm_num_str_to_ary_of_sword(int radix, const char *str, size_t size,
                            scm_bignum_d_t *ary, size_t ary_sz)
{
  scm_bignum_c_t base;
  int place, rslt;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(ary != NULL);
  scm_assert(0 < ary_sz && ary_sz <= SSIZE_MAX);

  rslt = scm_bignum_calc_base_and_place_for_ary_of_digits(radix, &base, &place);

  if (rslt < 0) return -1;      /* [ERR]: [through] */
  for (ssize_t i = (ssize_t)size, idx = 0;
       i > 0;
       i -= place, idx++) {
    ary[idx] = 0;

    for (int j = (i >= place) ? -place : (int)-i; j < 0; j++) {
      int v = scm_num_char_to_int(str[i + j]);
      if (v < 0) return -1;     /* [ERR]: [through] */

      ary[idx] = ary[idx] * (scm_bignum_d_t)radix + (scm_bignum_d_t)v;
    }
  }

  return (scm_bignum_sc_t)base;
}

static ScmObj
scm_num_make_integer_from_ary_of_sword(char sign,
                                       scm_bignum_d_t *ary, size_t size,
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

static ScmObj
scm_num_make_real(int radix, ScmNumParseDataReal *real)
{
  ssize_t ary_sz;

  scm_assert(real != NULL);

  if (real->delim != '\0') {
    scm_capi_error("unsupported number literal", 0);
    return SCM_OBJ_NULL;
  }

  if (real->sfx.em != '\0') {
    return SCM_OBJ_NULL;
  }

  ary_sz = scm_num_calc_len_of_ary_of_sword(real->p1.size, radix);
  if (ary_sz < 0) return 0;     /* [ERR]: [through] */

  {
    scm_bignum_d_t ary[ary_sz];
    scm_bignum_sc_t base;

    base = scm_num_str_to_ary_of_sword(radix, real->p1.head, real->p1.size,
                                       ary, (size_t)ary_sz);
    if (base < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    return scm_num_make_integer_from_ary_of_sword(real->sign,
                                                  ary, (size_t)ary_sz,
                                                  (scm_bignum_c_t)base);
  }
}

ScmObj
scm_num_make_from_literal(const char *literal, size_t size)
{
  ScmNumParseData data;
  ssize_t rslt;

  scm_assert(literal != NULL);
  scm_assert(size <= SSIZE_MAX);

  rslt = scm_num_parse_literal(literal, size, &data);
  if (rslt < 0) return SCM_OBJ_NULL;      /* [ERR] */

  if (data.cmplx.form == SCM_NUM_PARSE_CMPLX_POLAR
      || data.cmplx.orthog.img_spc) {
    scm_capi_error("unsupported number literal", 0);
    return SCM_OBJ_NULL;
  }

  if (data.cmplx.orthog.real.infinity != SCM_NUM_PARSE_NONE) {
    scm_capi_error("unsupported number literal", 0);
    return SCM_OBJ_NULL;
  }

  return scm_num_make_real(data.radix, &data.cmplx.orthog.real);
}
