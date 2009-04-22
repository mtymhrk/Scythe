#include <cutter.h>

#include "object.h"
#include "symbol.h"
#include "binding.h"

void
test_scm_bind_ref_construct(void)
{
  ScmObj sym, val;
  ScmBindRef *ref;

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val = SCM_OBJ(scm_symbol_construct("val"));
  ref = scm_bind_ref_construct(sym, val);

  cut_assert_not_null(ref);
  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref)));
  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val, scm_bind_ref_value(ref)));

  scm_bind_ref_destruct(ref);
}

void
test_scm_bind_ref_bind(void)
{
  ScmObj sym, val1, val2;
  ScmBindRef *ref;

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val1 = SCM_OBJ(scm_symbol_construct("val1"));
  val2 = SCM_OBJ(scm_symbol_construct("val2"));
  ref = scm_bind_ref_construct(sym, val1);

  cut_assert_not_null(ref);
  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref)));
  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val1, scm_bind_ref_value(ref)));


  scm_bind_ref_bind(ref, val2);

  cut_assert_equal_int(0,
                       scm_obj_is_same_instance(val1, scm_bind_ref_value(ref)));
  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val2, scm_bind_ref_value(ref)));

  scm_bind_ref_destruct(ref);
}

void
test_scm_bind_tbl_construct(void)
{
  ScmBindTable *tbl;

  tbl = scm_bind_tbl_construct(1024);

  cut_assert_not_null(tbl);

  scm_bind_tbl_destruct(tbl);
}

void
test_scm_bind_tbl_bind(void)
{
  ScmBindTable *tbl;
  ScmBindRef *ref;
  ScmObj sym, val;

  tbl = scm_bind_tbl_construct(1024);

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val = SCM_OBJ(scm_symbol_construct("val"));

  ref = scm_bind_tbl_bind(tbl, sym, val);

  cut_assert_not_null(ref);

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref)));

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val, scm_bind_ref_value(ref)));

  scm_bind_tbl_destruct(tbl);
}

void
test_scm_bind_tbl_lookup_found(void)
{
  ScmBindTable *tbl;
  ScmBindRef *ref;
  ScmObj sym, val;

  tbl = scm_bind_tbl_construct(1024);

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val = SCM_OBJ(scm_symbol_construct("val"));

  scm_bind_tbl_bind(tbl, sym, val);

  ref = scm_bind_tbl_lookup(tbl, sym);

  cut_assert_not_null(ref);

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref)));

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val, scm_bind_ref_value(ref)));

  scm_bind_tbl_destruct(tbl);
}

void
test_scm_bind_tbl_lookup_not_found(void)
{
  ScmBindTable *tbl;
  ScmObj sym, other, val;

  tbl = scm_bind_tbl_construct(1024);

  sym = SCM_OBJ(scm_symbol_construct("var"));
  other = SCM_OBJ(scm_symbol_construct("other"));
  val = SCM_OBJ(scm_symbol_construct("val"));

  scm_bind_tbl_bind(tbl, sym, val);

  cut_assert_null(scm_bind_tbl_lookup(tbl, other));

  scm_bind_tbl_destruct(tbl);
}


void
test_scm_bind_tbl_rebind(void)
{
  ScmBindTable *tbl;
  ScmBindRef *ref;
  ScmObj sym, val1, val2;

  tbl = scm_bind_tbl_construct(1024);

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val1 = SCM_OBJ(scm_symbol_construct("val1"));
  val2 = SCM_OBJ(scm_symbol_construct("val2"));

  scm_bind_tbl_bind(tbl, sym, val1);
  scm_bind_tbl_bind(tbl, sym, val2);

  ref = scm_bind_tbl_lookup(tbl, sym);

  cut_assert_not_null(ref);

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref)));

  cut_assert_equal_int(1,
                       scm_obj_is_same_instance(val2, scm_bind_ref_value(ref)));

  scm_bind_tbl_destruct(tbl);
}

void
test_scm_bind_tbl_unbind(void)
{
  ScmBindTable *tbl;
  ScmObj sym, val;

  tbl = scm_bind_tbl_construct(1024);

  sym = SCM_OBJ(scm_symbol_construct("var"));
  val = SCM_OBJ(scm_symbol_construct("val"));

  scm_bind_tbl_bind(tbl, sym, val);
  scm_bind_tbl_unbind(tbl, sym);

  cut_assert_null(scm_bind_tbl_lookup(tbl, sym));

  scm_bind_tbl_destruct(tbl);
}
