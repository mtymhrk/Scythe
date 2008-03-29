#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "pair.h"
#include "nil.h"
#include "printer.h"


struct ScmPairRec {
  ScmObjHeader header;
  ScmObj car;
  ScmObj cdr;
};

static void
scm_pair_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  ScmPair *pair = NULL;

  assert(obj != NULL); assert(scm_pair_is_pair(obj));
  assert(printer != NULL);

  pair = SCM_PAIR(obj);

  scm_printer_concatenate_char(printer, '(');
  scm_obj_pretty_print(pair->car, printer);
  while (scm_pair_is_pair(pair->cdr)) {
    pair = SCM_PAIR(pair->cdr);
    scm_printer_concatenate_char(printer, ' ');
    scm_obj_pretty_print(pair->car, printer);
  }

  if (scm_nil_is_nil(pair->cdr))
    scm_printer_concatenate_char(printer, ')');
  else {
    scm_printer_concatenate_string(printer, " . ");
    scm_obj_pretty_print(pair->cdr, printer);
    scm_printer_concatenate_char(printer, ')');
  }
}

ScmPair *
scm_pair_construct(ScmObj car, ScmObj cdr)
{
  ScmPair *pair = NULL;

  assert(car != NULL); assert(cdr != NULL);

  pair = (ScmPair *)scm_memory_allocate(sizeof(ScmPair));
  scm_obj_init(SCM_OBJ(pair), SCM_OBJ_TYPE_PAIR, scm_pair_pretty_print);

  pair->car = car;
  pair->cdr = cdr;

  return pair;
}

ScmObj
scm_pair_car(const ScmPair *pair)
{
  assert(pair != NULL);

  return pair->car;
}

ScmObj
scm_pair_cdr(const ScmPair *pair)
{
  assert(pair != NULL);

  return pair->cdr;
}

bool
scm_pair_is_pair(const ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_PAIR);
}
