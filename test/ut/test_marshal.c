#include "marshal.c"

#include "scythe/api.h"

#include "test.h"

TEST_GROUP(marshal);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

TEST_SETUP(marshal)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(marshal)
{
  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

static void
test_marshal_unmarshal(ScmObj obj)
{
  ScmObj marshal = SCM_OBJ_INIT, unmarshal = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT;
  void *data;

  SCM_REFSTK_INIT_REG(&obj,
                      &marshal, &unmarshal,
                      &actual);

  marshal = scm_marshal_new(SCM_MEM_HEAP);

  TEST_ASSERT_EQUAL_INT(0, scm_marshal_push_obj(marshal, obj));

  data = scm_marshal_terminate(marshal, NULL);
  TEST_ASSERT(data != NULL);

  unmarshal = scm_unmarshal_new(SCM_MEM_HEAP, data);
  TEST_ASSERT_SCM_EQUAL(obj, scm_unmarshal_ref(unmarshal, 0));
}

TEST(marshal, test_marshal_unmarshal__eof)
{
  test_marshal_unmarshal(SCM_EOF_OBJ);
}

TEST(marshal, test_marshal_unmarshal__true)
{
  test_marshal_unmarshal(SCM_TRUE_OBJ);
}

TEST(marshal, test_marshal_unmarshal__false)
{
  test_marshal_unmarshal(SCM_FALSE_OBJ);
}

TEST(marshal, test_marshal_unmarshal__nil)
{
  test_marshal_unmarshal(SCM_NIL_OBJ);
}

TEST(marshal, test_marshal_unmarshal__undef)
{
  test_marshal_unmarshal(SCM_UNDEF_OBJ);
}

TEST(marshal, test_marshal_unmarshal__pair)
{
  ScmObj pair = SCM_OBJ_INIT;
  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  test_marshal_unmarshal(pair);
}

TEST(marshal, test_marshal_unmarshal__pair__cycle)
{
  ScmObj pair = SCM_OBJ_INIT;
  pair = scm_api_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  scm_api_set_cdr_i(pair, pair);
  test_marshal_unmarshal(pair);
}

TEST(marshal, test_marshal_unmarshal__integer_positive)
{
  ScmObj num = SCM_OBJ_INIT;
  num = scm_fcd_make_number_from_sword(INT32_MAX);
  test_marshal_unmarshal(num);
}

TEST(marshal, test_marshal_unmarshal__integer_negative)
{
  ScmObj num = SCM_OBJ_INIT;
  num = scm_fcd_make_number_from_sword(INT32_MIN);
  test_marshal_unmarshal(num);
}

TEST(marshal, test_marshal_unmarshal__integer_string)
{
  ScmObj num = SCM_OBJ_INIT;
  num = scm_fcd_make_number_from_sword((scm_sword_t)INT32_MAX + 1);
  test_marshal_unmarshal(num);
}

TEST(marshal, test_marshal_unmarshal__symbol)
{
  ScmObj sym = SCM_OBJ_INIT;
  sym = scm_fcd_make_symbol_from_cstr("abcdef", SCM_ENC_SRC);
  test_marshal_unmarshal(sym);
}

TEST(marshal, test_marshal_unmarshal__char)
{
  ScmObj chr = SCM_OBJ_INIT;
  chr = scm_fcd_make_char(&(scm_char_t){ .bytes = { 'a' }}, SCM_ENC_SRC);
  test_marshal_unmarshal(chr);
}

TEST(marshal, test_marshal_unmarshal__string)
{
  ScmObj str = SCM_OBJ_INIT;
  str = scm_fcd_make_string_from_cstr("foobarbaz", SCM_ENC_SRC);
  test_marshal_unmarshal(str);
}

TEST(marshal, test_marshal_unmarshal__vector)
{
  ScmObj vec = SCM_OBJ_INIT;
  vec = read_cstr("#(a b c d e)");
  test_marshal_unmarshal(vec);
}

TEST(marshal, test_marshal_unmarshal__vector__cycle)
{
  ScmObj vec = SCM_OBJ_INIT;
  vec = read_cstr("#(a b c d e)");
  scm_fcd_vector_set_i(vec, 4, vec);
  test_marshal_unmarshal(vec);
}

TEST(marshal, test_marshal_unmarshal__bytevector)
{
  ScmObj vec = SCM_OBJ_INIT;
  unsigned char b[] = { 1, 2, 3, 4, 5 };
  vec = scm_fcd_make_bytevector_from_cv(b, sizeof(b));
  test_marshal_unmarshal(vec);
}

static void
test_marshal_unmarshal_iseq(const char *asmbl)
{
  ScmObj lst, iseq;
  lst = read_cstr(asmbl);
  iseq = scm_api_assemble(lst, SCM_OBJ_NULL);
  test_marshal_unmarshal(iseq);
}

TEST(marshal, test_marshal_unmarshal__iseq__noopd)
{
  test_marshal_unmarshal_iseq("((push))");
}

TEST(marshal, test_marshal_unmarshal__iseq__obj)
{
  test_marshal_unmarshal_iseq("((immval a))");
}

TEST(marshal, test_marshal_unmarshal__iseq__obj_obj)
{
  test_marshal_unmarshal_iseq("((gref a (abc)))");
}

TEST(marshal, test_marshal_unmarshal__iseq__si)
{
  test_marshal_unmarshal_iseq("((call -100))");
}

TEST(marshal, test_marshal_unmarshal__iseq__si_si)
{
  test_marshal_unmarshal_iseq("((sref 10 -20))");
}

TEST(marshal, test_marshal_unmarshal__iseq__si_si_obj)
{
  test_marshal_unmarshal_iseq("((close 10 20 abc))");
}

TEST(marshal, test_marshal_unmarshal__iseq__iof)
{
  test_marshal_unmarshal_iseq("((label abc)(jmp abc))");
}
