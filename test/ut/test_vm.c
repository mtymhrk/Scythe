#include "vm.c"

#include "scythe/api.h"

#include "test.h"

TEST_GROUP(vm);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;
static ScmObj vm;

TEST_SETUP(vm)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  vm = scm_vm_current_vm();
  scm_capi_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(vm)
{
  scm_capi_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

TEST(vm, vm_run__op_immval)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq, &sym);

  /* preprocess */
  iseq = scm_api_make_iseq();
  sym = scm_capi_make_symbol_from_cstr("abc", SCM_ENC_SRC);

  scm_capi_iseq_push_inst(iseq, SCM_OPCODE_IMMVAL, sym);
  scm_capi_iseq_push_inst(iseq, SCM_OPCODE_HALT);

  /* action */
  scm_vm_run(vm, iseq);

  /* postconditin check */
  TEST_ASSERT_EQUAL_INT(1, SCM_VM(vm)->reg.vc);
  TEST_ASSERT_SCM_EQ(sym, SCM_VM(vm)->reg.val[0]);
}

static void
test_adjust_val_to_arity_aux(ScmObj val, size_t vl, int arity,
                             ScmObj expected, size_t el)
{
  ScmObj v[vl], e[el];

  SCM_REFSTK_INIT_REG(&val, &expected);
  SCM_REFSTK_REG_ARY(v, vl);
  SCM_REFSTK_REG_ARY(e, el);

  for (size_t i = 0; i < vl; i++)
    v[i] = scm_capi_vector_ref(val, i);

  for (size_t i = 0; i < el; i++)
    e[i] = scm_capi_vector_ref(expected, i);

  scm_vm_set_val_reg(vm, v, (int)vl);

  TEST_ASSERT_EQUAL_INT(abs(arity), scm_vm_adjust_val_to_arity(vm, arity));

  TEST_ASSERT_EQUAL_INT(abs(arity), SCM_VM(vm)->reg.vc);
  for (size_t i = 0; i < el; i++)
    TEST_ASSERT_SCM_EQUAL(e[i], SCM_VM(vm)->reg.val[i]);
}

static void
test_adjust_val_to_arity(const char *val, int arity, const char *expected)
{
  ScmObj v = SCM_OBJ_INIT, e = SCM_OBJ_INIT;
  ssize_t vl, el;

  SCM_REFSTK_INIT_REG(&v, &e);

  v = read_cstr(val);
  e = read_cstr(expected);

  vl = scm_capi_vector_length(v);
  el = scm_capi_vector_length(e);

  test_adjust_val_to_arity_aux(v, (size_t)vl, arity, e, (size_t)el);
}

TEST(vm, vm_adjust_val_to_arity__1)
{
  test_adjust_val_to_arity("#(1 2)",
                           2,
                           "#(1 2)");
}

TEST(vm, vm_adjust_val_to_arity__2)
{
  test_adjust_val_to_arity("#(1 2 3)",
                           -2,
                           "#(1 (2 3))");
}

TEST(vm, vm_adjust_val_to_arity__3)
{
  test_adjust_val_to_arity("#(1 2 3)",
                           -4,
                           "#(1 2 3 ())");
}

/* SCM_VM_NR_VAL_REG = 10 が前提のテスト記述
 */

TEST(vm, vm_adjust_val_to_arity__SCM_VM_NR_VAL_REG__1)
{
  test_adjust_val_to_arity("#(1 2 3 4 5 6 7 8 9 10)",
                           -10,
                           "#(1 2 3 4 5 6 7 8 9 (10))");
}

TEST(vm, vm_adjust_val_to_arity__SCM_VM_NR_VAL_REG__2)
{
  test_adjust_val_to_arity("#(1 2 3 4 5 6 7 8 9 10 11)",
                           -10,
                           "#(1 2 3 4 5 6 7 8 9 (10 11))");
}

TEST(vm, vm_adjust_val_to_arity__SCM_VM_NR_VAL_REG__3)
{
  test_adjust_val_to_arity("#(1 2 3 4 5 6 7 8 9 10 11 12)",
                           -11,
                           "#(1 2 3 4 5 6 7 8 9 #(10 (11 12) 12))");
}

TEST(vm, vm_adjust_val_to_arity__SCM_VM_NR_VAL_REG__4)
{
  test_adjust_val_to_arity("#(1 2 3 4 5 6 7 8 9 10)",
                           -11,
                           "#(1 2 3 4 5 6 7 8 9 #(10 ()))");
}

TEST(vm, vm_adjust_val_to_arity__SCM_VM_NR_VAL_REG__5)
{
  test_adjust_val_to_arity("#(1 2 3 4 5 6 7 8 9 10 11)",
                           -12,
                           "#(1 2 3 4 5 6 7 8 9 #(10 11 ()))");
}
