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


#if 0

/***************************************************************************/
/*  Bignum                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_BIGNUM_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmBignum),
  .gc_ini_func         = scm_bignum_gc_initialize,
  .gc_fin_func         = scm_bignum_gc_finalize,
  .gc_accept_func      = NULL,
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
    if (idx < size) return -1;
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
      return -1;              /* [ERR] */
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

static int
scm_num_calc_base_and_place_for_ary_of_sword(int radix,
                                             scm_sword_t *base, int *place)
{
  scm_assert(base != NULL);
  scm_assert(place != NULL);
#if SCM_SWORD_MAX >= 4611686018427387904

  switch (radix) {
  case 2:
    *base = 4611686018427387904; /* 2^62 */
    *place = 62;
    break;
  case 8:
    *base = 1152921504606846976; /* 2^60 */
    *place = 20;
    break;
  case 10:
    *base = 1000000000000000000;
    *place = 18;
    break;
  case 16:
    *base = 1152921504606846976; /* 2^60 */
    *place = 15;
    break;
  default:
    scm_capi_error("number literal parse error: unsupported radix", 0);
    return -1;
  }

#elif SCM_SWORD_MAX >= 1073741824

  switch (radix) {
  case 2:
    *base = 1073741824; /* 2^30 */
    *place = 30;
    break;
  case 8:
    *base = 1073741824; /* 2^30 */
    *place = 10;
    break;
  case 10:
    *base = 1000000000;
    *place = 9;
    break;
  case 16:
    *base = 268435456; /* 2^28 */
    *place = 7;
    break;
  default:
    scm_capi_error("number literal parse error: unsupported radix", 0);
    return -1;
  }

#else

  switch (radix) {
  case 2:
    *base = 16384; /* 2^14 */
    *digits = 14;
    break;
  case 8:
    *base = 4096; /* 2^12 */
    *digits = 4;
    break;
  case 10:
    *base = 10000;
    *digits = 4;
    break;
  case 16:
    *base = 4096; /* 2^12 */
    *digits = 3;
    break;
  default:
    scm_capi_error("number literal parse error: unsupported radix", 0);
    return -1;
  }

#endif

  return 0;
}

static ssize_t
scm_num_calc_len_of_ary_of_sword(size_t str_size, int radix)
{
  scm_sword_t base;
  int place, rslt;

  scm_assert(str_size <= SSIZE_MAX);

  rslt = scm_num_calc_base_and_place_for_ary_of_sword(radix,
                                                      &base, &place);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  return ((ssize_t)str_size / place
          + (((ssize_t)str_size % place) ? 1 : 0));
}

static scm_sword_t
scm_num_str_to_ary_of_sword(int radix, const char *str, size_t size,
                            scm_sword_t *ary, size_t ary_sz)
{
  scm_sword_t base;
  int place, rslt;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(ary != NULL);
  scm_assert(0 < ary_sz && ary_sz <= SSIZE_MAX);

  rslt = scm_num_calc_base_and_place_for_ary_of_sword(radix, &base, &place);
  if (rslt < 0) return -1;      /* [ERR]: [through] */
  for (ssize_t i = (ssize_t)size, idx = 0;
       i > 0;
       i -= place, idx++) {
    ary[idx] = 0;

    for (int j = (i >= place) ? -place : (int)-i; j < 0; j++) {
      int v = scm_num_char_to_int(str[i + j]);
      if (v < 0) return -1;     /* [ERR]: [through] */

      ary[idx] = ary[idx] * radix + v;
    }
  }

  return base;
}

static ScmObj
scm_num_make_integer_from_ary_of_sword(char sign,
                                       scm_sword_t *ary, size_t size,
                                       scm_sword_t base)
{
  scm_sword_t num, abs_max;

  scm_assert(ary != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (sign == '+' || sign == '\0')
    abs_max = SCM_FIXNUM_MAX;
  else
    abs_max = -SCM_FIXNUM_MIN;

  num = 0;
  for (ssize_t i = (ssize_t)size - 1; i >= 0; i--) {
    if (num > (abs_max - ary[i]) / base) {
      scm_capi_error("can not make integer: too big integer", 0);
      /* TODO: make bignum object */
      return SCM_OBJ_NULL;
    }
    num = num * base + ary[i];
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
    scm_sword_t ary[ary_sz];
    scm_sword_t base;

    base = scm_num_str_to_ary_of_sword(radix, real->p1.head, real->p1.size,
                                       ary, (size_t)ary_sz);
    if (base < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    return scm_num_make_integer_from_ary_of_sword(real->sign,
                                                  ary, (size_t)ary_sz,
                                                  base);
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
