#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "string.h"
#include "symbol.h"
#include "binding.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}

void
test_scm_bind_ref_new(void)
{
  ScmObj bref = SCM_OBJ_INIT, str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bref, &str1, &str2, &sym, &val);

  SCM_SETQ(str1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "var", sizeof("var") - 1,
                                      SCM_ENCODING_ASCII));
  SCM_SETQ(str2, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "val", sizeof("val") - 1,
                                      SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_instance(str1));
  SCM_SETQ(val, scm_symbol_instance(str2));

  SCM_SETQ(bref, scm_bind_ref_new(SCM_MEM_ALLOC_HEAP, sym, val));

  cut_assert(SCM_OBJ_IS_NOT_NULL(bref));
  cut_assert_true(scm_obj_is_same_instance(sym, scm_bind_ref_symbol(bref)));
  cut_assert_true(scm_obj_is_same_instance(val, scm_bind_ref_value(bref)));
}

void
test_scm_bind_ref_bind(void)
{
  ScmObj bref = SCM_OBJ_INIT;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT, str3 = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT, val1 = SCM_OBJ_INIT, val2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bref, &str1, &str2, &str3, &sym, &val1, &val2);

  SCM_SETQ(str1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "var", sizeof("var") - 1,
                                      SCM_ENCODING_ASCII));
  SCM_SETQ(str2, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "val1", sizeof("val1") - 1,
                                      SCM_ENCODING_ASCII));
  SCM_SETQ(str3, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                      "val2", sizeof("val2") - 1,
                                      SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_instance(str1));
  SCM_SETQ(val1, scm_symbol_instance(str2));
  SCM_SETQ(val2, scm_symbol_instance(str3));

  SCM_SETQ(bref, scm_bind_ref_new(SCM_MEM_ALLOC_HEAP, sym, val1));

  cut_assert(SCM_OBJ_IS_NOT_NULL(bref));
  cut_assert_true(scm_obj_is_same_instance(sym, scm_bind_ref_symbol(bref)));
  cut_assert_true(scm_obj_is_same_instance(val1, scm_bind_ref_value(bref)));

  scm_bind_ref_bind(bref, val2);

  cut_assert_false(scm_obj_is_same_instance(val1, scm_bind_ref_value(bref)));
  cut_assert_true(scm_obj_is_same_instance(val2, scm_bind_ref_value(bref)));
}

/* TODO: write test code for the Binding Table */
/* void */
/* test_scm_bind_tbl_new(void) */
/* { */
/*   ScmBindTable *tbl; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   cut_assert_not_null(tbl); */

/*   scm_bind_tbl_end(tbl); */
/* } */

/* void */
/* test_scm_bind_tbl_bind(void) */
/* { */
/*   ScmBindTable *tbl; */
/*   ScmBindRef *ref; */
/*   ScmObj sym, val; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   sym = SCM_OBJ(scm_symbol_new("var")); */
/*   val = SCM_OBJ(scm_symbol_new("val")); */

/*   ref = scm_bind_tbl_bind(tbl, sym, val); */

/*   cut_assert_not_null(ref); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref))); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(val, scm_bind_ref_value(ref))); */

/*   scm_bind_tbl_end(tbl); */
/* } */

/* void */
/* test_scm_bind_tbl_lookup_found(void) */
/* { */
/*   ScmBindTable *tbl; */
/*   ScmBindRef *ref; */
/*   ScmObj sym, val; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   sym = SCM_OBJ(scm_symbol_new("var")); */
/*   val = SCM_OBJ(scm_symbol_new("val")); */

/*   scm_bind_tbl_bind(tbl, sym, val); */

/*   ref = scm_bind_tbl_lookup(tbl, sym); */

/*   cut_assert_not_null(ref); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref))); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(val, scm_bind_ref_value(ref))); */

/*   scm_bind_tbl_end(tbl); */
/* } */

/* void */
/* test_scm_bind_tbl_lookup_not_found(void) */
/* { */
/*   ScmBindTable *tbl; */
/*   ScmObj sym, other, val; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   sym = SCM_OBJ(scm_symbol_new("var")); */
/*   other = SCM_OBJ(scm_symbol_new("other")); */
/*   val = SCM_OBJ(scm_symbol_new("val")); */

/*   scm_bind_tbl_bind(tbl, sym, val); */

/*   cut_assert_null(scm_bind_tbl_lookup(tbl, other)); */

/*   scm_bind_tbl_end(tbl); */
/* } */


/* void */
/* test_scm_bind_tbl_rebind(void) */
/* { */
/*   ScmBindTable *tbl; */
/*   ScmBindRef *ref; */
/*   ScmObj sym, val1, val2; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   sym = SCM_OBJ(scm_symbol_new("var")); */
/*   val1 = SCM_OBJ(scm_symbol_new("val1")); */
/*   val2 = SCM_OBJ(scm_symbol_new("val2")); */

/*   scm_bind_tbl_bind(tbl, sym, val1); */
/*   scm_bind_tbl_bind(tbl, sym, val2); */

/*   ref = scm_bind_tbl_lookup(tbl, sym); */

/*   cut_assert_not_null(ref); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(sym, scm_bind_ref_symbol(ref))); */

/*   cut_assert_equal_int(1, */
/*                        scm_obj_is_same_instance(val2, scm_bind_ref_value(ref))); */

/*   scm_bind_tbl_end(tbl); */
/* } */

/* void */
/* test_scm_bind_tbl_unbind(void) */
/* { */
/*   ScmBindTable *tbl; */
/*   ScmObj sym, val; */

/*   tbl = scm_bind_tbl_new(1024); */

/*   sym = SCM_OBJ(scm_symbol_new("var")); */
/*   val = SCM_OBJ(scm_symbol_new("val")); */

/*   scm_bind_tbl_bind(tbl, sym, val); */
/*   scm_bind_tbl_unbind(tbl, sym); */

/*   cut_assert_null(scm_bind_tbl_lookup(tbl, sym)); */

/*   scm_bind_tbl_end(tbl); */
/* } */
