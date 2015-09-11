#ifndef INCLUDE_OBJECT_H__
#define INCLUDE_OBJECT_H__

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>

#include "scythe/impl_utils.h"
#include "scythe/config.h"

typedef uintptr_t scm_word_t;
typedef uintptr_t scm_uword_t;
typedef intptr_t  scm_sword_t;

typedef unsigned char scm_byte_t;
typedef unsigned char scm_ubyte_t;
typedef signed char scm_sbyte_t;

#define SCM_UWORD_MAX UINTPTR_MAX
#define SCM_UWORD_NIN UINTPTR_MIN
#define SCM_SWORD_MAX INTPTR_MAX
#define SCM_SWORD_MIN INTPTR_MIN
#define SIZEOF_SCM_WORD_T SIZEOF_INTPTR_T

#define SCM_BYTE_MAX UCHAR_MAX
#define SCM_BYTE_MIN UCHAR_MIN
#define SCM_UBYTE_MAX UCHAR_MAX
#define SCM_UBYTE_MIN UCHAR_MIN
#define SCM_SBYTE_MAX SCHAR_MAX
#define SCM_SBYTE_MIN SCHAR_MIN
#define SIZEOF_SCM_BYTE_T 1


/** definition for ScmObj ****************************************************/

typedef struct ScmObjHeaderRec ScmObjHeader;
typedef struct ScmMMObjRec ScmMMObj;
typedef scm_word_t ScmObj;
typedef struct ScmTypeInfoRec ScmTypeInfo;
typedef ScmObj *ScmRef;
typedef const ScmObj *ScmCRef;

#define SCM_MMOBJ(obj) ((ScmMMObj *)(obj))
#define SCM_OBJ(obj) ((ScmObj)(obj))

#define SCM_OBJ_NULL 0u
#define SCM_OBJ_INIT SCM_OBJ_NULL
#define SCM_OBJ_TAG_MASK 0x07u
#define SCM_OBJ_TAG_NR_KIND 8

#define SCM_SLOT_SETQ(type, obj, slt, val) \
  do {                                     \
    ScmObj SCM_CONCAT_SYMBOL__(scm_slot_setq__inner_variable__, __LINE__) \
      = (val);                                                          \
    ((type *)(obj))->slt                                                \
      = SCM_CONCAT_SYMBOL__(scm_slot_setq__inner_variable__, __LINE__); \
  } while(0)

#define SCM_SLOT_REF_SETQ(type, obj, slt, val)                          \
  do {                                                                  \
    ScmObj SCM_CONCAT_SYMBOL__(scm_slot_ref_setq__inner_variable__, __LINE__) \
      = (val);                                                          \
    *(((type *)(obj))->slt)                                             \
      = SCM_CONCAT_SYMBOL__(scm_slot_ref_setq__inner_variable__, __LINE__); \
  } while(0)

#define SCM_WB_SETQ(owner, lval, rval)                                  \
  do {                                                                  \
    ScmObj SCM_CONCAT_SYMBOL__(scm_wb_setq__inner_variable__, __LINE__) \
      = (rval);                                                         \
    (lval) = SCM_CONCAT_SYMBOL__(scm_wb_setq__inner_variable__, __LINE__); \
  } while(0)

#define SCM_WB_EXP(owner, exp)                  \
  do {                                          \
    exp;                                        \
  } while (0)


/** definition for scm_csetter_t **********************************************/

/*
 * = scm_csetter_t の説明
 * 関数の引数に ScmRef 型 (つまり ScmObj * 型) の値を受け取り、何らかの結果を
 * そのポインタに設定して返したいケースで、ScmRef 型を使うのではなく、
 * scm_csetter_t * 型を使用する。
 *
 * scm_csetter_t * を引数に受け取る関数の caller は結果を格納してほしい場所が
 * ただの自動変数であれば、
 *
 *   func( ..., SCM_CSETTER_L(変数名), ... );
 *
 * といった形で SCM_CSETTER_L を使用して scm_csetter_t を生成すると同時に関数
 * を呼出す。結果を格納してほしい場所が、ある Memory Managed Object が管理して
 * いる領域であれば、
 *
 *   func( ..., SCM_CSETTER_O(object を指す変数, 格納場所を表す lvalue), ...);
 *
 * といった形で SCM_CSETTER_O を使用して生成する。 結果を格納してほしい場所が、
 * ある Memory Managed Object の構造体のメンバであれば、
 *
 *   func( ..., SCM_CSETTER_S(object の型,
 *                            object を指す変数、
 *                            格納場所を表す lvalue), ...);
 *
 * といった形で SCM_CSETTER_S を使用して生成する。
 *
 * calle 側では、scm_csetter_setq を使用して結果を格納する。
 *
 * SCM_CSETTER_O と SCM_CSETTER_S に分かれているのは、Memory Managed Object
 * 構造体のメンバの場合、scm_csetter_t 生成以降に GC が走るとメンバを指すポイ
 * ンタ値が変ってしまうため、変りにメンバのオフセット値を保持するようにするた
 * め。
 *
 * 導入の理由は、GC の write barrier を挿入するため (ただし今現在、世代別 GC
 * を実装していないため挿入する必要はない。単に将来的な話)。
 * Memory Managed Object が管理している領域への ScmObj 値の代入は write
 * barrier を挿入しなければならないが、自動変数への代入はその必要がない。
 * ScmRef 型を渡された callee 側ではそれが 自動変数を指しているのか、Memory
 * Managed Object の管理領域を指しているのかが判断できない。そのため、
 * scm_csetter_t で抽象化する。
 *
 * ScmRef 型の引数に指定できるのは自動変数のポインタのみとし、callee では一切
 * write barrier を挿入しないとすることもできるが、そのコーディング規約を自分
 * が守れる自信が無い。そため、試験的に scm_csetter_t を導入してみた。
 *
 */

typedef struct {
  char kind;
  union {
    ScmObj *l;
    struct {
      ScmObj *owner;
      ScmObj *ptr;
    } o;
    struct {
      ScmObj *owner;
      size_t offset;
    } s;
  } lval;
} scm_csetter_t;

#define SCM_CSETTER_L(lval) \
  &(scm_csetter_t){ 'L', { .l = &(lval) }}

#define SCM_CSETTER_O(owner, lval) \
  &(scm_csetter_t){ 'O', { .o = { &(owner), &(lval) }}}

#define SCM_CSETTER_S(type, obj, slt) \
  &(scm_csetter_t){ 'S', { .s = { &(obj), offsetof(type, slt) }}}

static inline void
scm_csetter_setq(scm_csetter_t *st, ScmObj val)
{
  switch (st->kind) {
  case 'L':
    *st->lval.l = val;
    break;
  case 'O':
    SCM_WB_SETQ(*st->lval.o.owner, *st->lval.o.ptr, val);
    break;
  case 'S':
    SCM_WB_SETQ(*st->lval.s.owner,
                *(ScmObj *)((scm_byte_t *)*st->lval.s.owner
                            + st->lval.s.offset),
                val);
    break;
  }
}

static inline ScmObj
scm_csetter_val(scm_csetter_t *st)
{
  switch (st->kind) {
  case 'L':
    return *st->lval.l;
  case 'O':
    return *st->lval.o.ptr;
  case 'S':
    return *(ScmObj *)((scm_byte_t *)*st->lval.s.owner
                       + st->lval.s.offset);
  default:
    return SCM_OBJ_NULL;        /* must not happen */
  }
}


/** definition for ScmRef ****************************************************/

#define SCM_REF_MAKE(obj) ((ScmRef)&(obj))
#define SCM_REF_MAKE_FROM_PTR(ptr) ((ScmRef)(ptr))
#define SCM_REF_TO_PTR(ref) ((ScmObj *)(ref))

#define SCM_REF_NULL ((ScmRef)NULL)

#define SCM_REF_DEREF(ref) (*((ScmObj *)(ref)))
#define SCM_CREF_DEREF(ref) (*((const ScmObj *)(ref)))
#define SCM_REF_UPDATE(ref, obj) (*((ScmObj *)(ref)) = SCM_OBJ(obj))
#define SCM_REF_SETQ(ref, obj) SCM_REF_UPDATE(ref, obj)


/** definition for function to print object **********************************/

enum {
  SCM_OBJ_PRINT_SIMPLE,
  SCM_OBJ_PRINT_SHARED,
  SCM_OBJ_PRINT_DISPLAY,
};

typedef struct ScmObjPrintHandlerBodyRec ScmObjPrintHandlerBody;
typedef ScmObjPrintHandlerBody *ScmObjPrintHandler;

struct ScmObjPrintHandlerBodyRec {
  bool (*interesting_p)(ScmObj obj);
  int (*print_obj)(ScmObjPrintHandler handler,
                   ScmObj obj, ScmObj port, int kind);
  int (*print_list)(ScmObjPrintHandler handler,
                    ScmObj obj, ScmObj port, int kind);
  ScmObj val;
};

#define SCM_OBJ_PRINT_MAKE_HANDLER(body) (&(body))
#define SCM_OBJ_PRINT_HANDLER_BODY(handler) (handler)

#define SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler, obj, port, kind) \
  ((handler)->print_obj(handler, obj, port, kind))

#define SCM_OBJ_PRINT_HANDLER_PRINT_LIST(handler, obj, port, kind) \
  ((handler)->print_list(handler, obj, port, kind))


typedef int (*ScmObjPrintFunc)(ScmObj obj, ScmObj port, int kind,
                               ScmObjPrintHandler handler);

int scm_obj_print_func_nameonly(ScmObj obj, ScmObj port, int kind,
                                ScmObjPrintHandler handker);
int scm_obj_default_print_func(ScmObj obj, ScmObj port, int kind,
                               ScmObjPrintHandler handler);


/** definition for GC ********************************************************/

typedef struct ScmGCRefHandlerBodyRec ScmGCRefHandlerBody;
typedef ScmGCRefHandlerBody *ScmGCRefHandler;

struct ScmGCRefHandlerBodyRec {
  ScmObj mem;
  int (*func)(ScmObj mem, ScmObj obj, ScmRef child);
};

#define SCM_GC_REF_HANDLER_MAKE(body) (&(body))

typedef void (*ScmGCInitializeFunc)(ScmObj obj);
typedef void (*ScmGCFinalizeFunc)(ScmObj obj);
typedef int (*ScmGCAcceptFunc)(ScmObj obj, ScmGCRefHandler handler);

#define SCM_GC_REF_HANDLER_VAL_INIT 0
#define SCM_GC_CALL_REF_HANDLER(handler, obj, child)            \
  (handler->func(handler->mem, obj, SCM_REF_MAKE(child)))

static inline bool
scm_gc_ref_handler_failure_p(int ret_val)
{
  return (ret_val < 0) ? true : false;
}


/****************************************************************************/
/** ScmTypeInfo                                                             */
/****************************************************************************/

struct ScmTypeInfoRec {
  const char *name;
  unsigned int flags;
  ScmObjPrintFunc obj_print_func;
  size_t obj_size;
  ScmGCInitializeFunc gc_ini_func;
  ScmGCFinalizeFunc gc_fin_func;
  ScmGCAcceptFunc gc_accept_func;
  ScmGCAcceptFunc gc_accept_func_weak;
  void *extra;
};

enum {
  SCM_TYPE_FLG_MMO  = 0x00000001,
  SCM_TYPE_FLG_NUM  = 0x00000002,
  SCM_TYPE_FLG_PROC = 0x00000004,
  SCM_TYPE_FLG_EXC  = 0x00000008,
};

static inline bool
scm_type_info_same_p(ScmTypeInfo *type1, ScmTypeInfo *type2)
{
  return (type1 == type2) ? true : false;
}

static inline const char *
scm_type_info_name(ScmTypeInfo *type)
{
  return type->name;
}

static inline bool
scm_type_info_flg_set_p(ScmTypeInfo *type, unsigned int flg)
{
  return ((type->flags & flg) == flg) ? true : false;
}

static inline size_t
scm_type_info_obj_size(ScmTypeInfo *type)
{
  return type->obj_size;
}

static inline bool
scm_type_info_has_print_func_p(ScmTypeInfo *type)
{
  return (type->obj_print_func != NULL) ? true : false;
}

static inline int
scm_type_info_call_print_func(ScmTypeInfo *type,
                              ScmObj obj, ScmObj port, int kind,
                              ScmObjPrintHandler handler)
{
  if (scm_type_info_has_print_func_p(type))
    return type->obj_print_func(obj, port, kind, handler);
  else
    return scm_obj_default_print_func(obj, port, kind, handler);
}

static inline bool
scm_type_info_has_gc_ini_func_p(ScmTypeInfo *type)
{
  return (type->gc_ini_func != NULL) ? true : false;
}

static inline void
scm_type_info_call_gc_ini_func(ScmTypeInfo *type, ScmObj obj)
{
  if (scm_type_info_has_gc_ini_func_p(type))
    type->gc_ini_func(obj);
}

static inline bool
scm_type_info_has_gc_fin_func_p(ScmTypeInfo *type)
{
  return (type->gc_fin_func != NULL) ? true : false;
}

static inline void
scm_type_info_call_gc_fin_func(ScmTypeInfo *type, ScmObj obj)
{
  if (scm_type_info_has_gc_fin_func_p(type))
    type->gc_fin_func(obj);
}

static inline bool
scm_type_info_has_gc_accept_func_p(ScmTypeInfo *type)
{
  return (type->gc_accept_func != NULL) ? true : false;
}

static inline int
scm_type_info_call_gc_accept_func(ScmTypeInfo *type, ScmObj obj,
                                  ScmGCRefHandler handler)
{
  if (scm_type_info_has_gc_accept_func_p(type))
    return type->gc_accept_func(obj, handler);
  else
    return SCM_GC_REF_HANDLER_VAL_INIT;
}

static inline bool
scm_type_info_has_instance_weak_ref_p(ScmTypeInfo *type)
{
  return (type->gc_accept_func_weak != NULL) ? true : false;
}

static inline int
scm_type_info_call_gc_accept_func_weak(ScmTypeInfo *type, ScmObj obj,
                                       ScmGCRefHandler handler)
{
  if (scm_type_info_has_instance_weak_ref_p(type))
    return type->gc_accept_func_weak(obj, handler);
  else
    return SCM_GC_REF_HANDLER_VAL_INIT;
}

static inline void *
scm_type_info_extra(ScmTypeInfo *type)
{
  return type->extra;
}


/****************************************************************************/
/** SCM_OBJ_NULL                                                            */
/****************************************************************************/

extern ScmTypeInfo SCM_NULL_OBJ_TYPE_INFO;


/****************************************************************************/
/** ScmObj                                                                  */
/****************************************************************************/

struct ScmObjHeaderRec {
  ScmTypeInfo *type;
};

struct ScmMMObjRec {
  ScmObjHeader header;
};

extern ScmTypeInfo *SCM_OBJ_TAG2TYPE_TBL[];

static inline bool
scm_obj_null_p(ScmObj obj)
{
  return (obj == SCM_OBJ_NULL) ? true : false;
}

static inline bool
scm_obj_not_null_p(ScmObj obj)
{
  return (obj != SCM_OBJ_NULL) ? true : false;
}

static inline bool
scm_obj_same_instance_p(ScmObj obj1, ScmObj obj2)
{
  return (scm_obj_not_null_p(obj1) && (obj1 == obj2)) ? true : false;
}

static inline unsigned int
scm_obj_tag(ScmObj obj)
{
  return (scm_uword_t)obj & SCM_OBJ_TAG_MASK;
}

static inline bool
scm_obj_mem_managed_p(ScmObj obj)
{
  return (scm_obj_not_null_p(obj) && scm_obj_tag(obj) == 0x00u) ? true : false;
}

static inline ScmTypeInfo *
scm_obj_type(ScmObj obj)
{
  ScmTypeInfo *t = SCM_OBJ_TAG2TYPE_TBL[scm_obj_tag(obj)];
  if (t != NULL)
    return t;
  else
    return (scm_obj_null_p(obj) ?
            &SCM_NULL_OBJ_TYPE_INFO : SCM_MMOBJ(obj)->header.type);
}

static inline bool
scm_obj_type_p(ScmObj obj, ScmTypeInfo *type)
{
  return scm_type_info_same_p(scm_obj_type(obj), type);
}

static inline const char *
scm_obj_type_name(ScmObj obj)
{
  return scm_type_info_name(scm_obj_type(obj));
}

static inline bool
scm_obj_type_flag_set_p(ScmObj obj, unsigned int flg)
{
  return scm_type_info_flg_set_p(scm_obj_type(obj), flg);
}

static inline size_t
scm_obj_size(ScmObj obj)
{
  return scm_type_info_obj_size(scm_obj_type(obj));
}

static inline bool
scm_obj_has_print_func_p(ScmObj obj)
{
  return scm_type_info_has_print_func_p(scm_obj_type(obj));
}

static inline int
scm_obj_call_print_func(ScmObj obj, ScmObj port, int kind,
                        ScmObjPrintHandler handler)
{
  return scm_type_info_call_print_func(scm_obj_type(obj),
                                       obj, port, kind, handler);
}

static inline bool
scm_obj_has_gc_ini_func_p(ScmObj obj)
{
  return scm_type_info_has_gc_ini_func_p(scm_obj_type(obj));
}

static inline void
scm_obj_call_gc_fin_func(ScmObj obj)
{
  scm_type_info_call_gc_fin_func(scm_obj_type(obj), obj);
}

static inline bool
scm_obj_has_gc_fin_func_p(ScmObj obj)
{
  return scm_type_info_has_gc_fin_func_p(scm_obj_type(obj));
}

static inline int
scm_obj_call_gc_accept_func(ScmObj obj, ScmGCRefHandler handler)
{
  return scm_type_info_call_gc_accept_func(scm_obj_type(obj),
                                           obj, handler);
}

static inline bool
scm_obj_has_gc_accpet_func_p(ScmObj obj)
{
  return scm_type_info_has_gc_accept_func_p(scm_obj_type(obj));
}


static inline int
scm_obj_call_gc_accept_func_weak(ScmObj obj, ScmGCRefHandler handler)
{
  return scm_type_info_call_gc_accept_func_weak(scm_obj_type(obj),
                                                obj, handler);
}

static inline int
scm_obj_has_weak_ref_p(ScmObj obj)
{
  return scm_type_info_has_instance_weak_ref_p(scm_obj_type(obj));
}

static inline void *
scm_obj_type_extra(ScmObj obj)
{
  return scm_type_info_extra(scm_obj_type(obj));
}


#define scm_assert_obj_type(obj, type)          \
  do {                                          \
    scm_assert(scm_obj_not_null_p(obj));        \
    scm_assert(scm_obj_type_p(obj, type));      \
  } while(0)


#define scm_assert_obj_type_accept_null(obj, type)      \
  scm_assert(scm_obj_null_p(obj) || scm_obj_type_p(obj, type))


static inline void
scm_obj_init(ScmObj obj, ScmTypeInfo *type)
{
  scm_assert(type != NULL);

  if (scm_obj_mem_managed_p(obj))
    SCM_MMOBJ(obj)->header.type = type;
}

#endif /* INCLUDE_OBJECT_H__ */
