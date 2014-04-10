#ifndef INCLUDE_NUMBER_COMMON_H__
#define INCLUDE_NUMBER_COMMON_H__

typedef struct ScmNumFuncRec ScmNumFunc;

#include "object.h"

struct ScmNumFuncRec {
  ScmObj (*coerce)(ScmObj obj, ScmObj num);
  ScmObj (*copy)(ScmObj num);
  bool (*complex_p)(ScmObj obj);
  bool (*real_p)(ScmObj obj);
  bool (*rational_p)(ScmObj obj);
  bool (*integer_p)(ScmObj obj);
  bool (*exact_p)(ScmObj obj);
  bool (*inexact_p)(ScmObj obj);
  bool (*finite_p)(ScmObj obj);
  bool (*infinite_p)(ScmObj obj);
  bool (*nan_p)(ScmObj obj);
  int (*cmp)(ScmObj x, ScmObj y, int *cmp);
  bool (*zero_p)(ScmObj num);
  bool (*positive_p)(ScmObj num);
  bool (*negative_p)(ScmObj num);
  bool (*odd_p)(ScmObj num);
  bool (*even_p)(ScmObj num);
  ScmObj (*invert_sign)(ScmObj num);
  ScmObj (*plus)(ScmObj aug, ScmObj add);
  ScmObj (*minus)(ScmObj min, ScmObj sub);
  ScmObj (*mul)(ScmObj mud, ScmObj mur);
  int (*floor_div)(ScmObj dvd, ScmObj dvr,
                   scm_csetter_t *quo, scm_csetter_t *rem);
  int (*ceiling_div)(ScmObj dvd, ScmObj dvr,
                     scm_csetter_t *quo, scm_csetter_t *rem);
  int (*truncate_div)(ScmObj dvd, ScmObj dvr,
                      scm_csetter_t *quo, scm_csetter_t *rem);
};


#define SCM_NUM_CALL_FUNC(obj, func, ...) \
  ((ScmNumFunc *)scm_obj_type_extra(obj))->func(obj, ## __VA_ARGS__)


#endif  /* INCLUDE_NUMBER_COMMON_H__ */

