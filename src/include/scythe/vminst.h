#ifndef INCLUDE_VMINST_H__
#define INCLUDE_VMINST_H__

#include "scythe/object.h"
#include "scythe/config.h"
#include "scythe/impl_utils.h"

#define SCM_VMINST_INST_ALIGN SIZEOF_SCM_WORD_T

typedef int scm_opcode_t;

struct scm_vm_inst_noopd {
  scm_opcode_t op;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_obj {
  scm_opcode_t op;
  ScmObj opd1;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_obj_obj {
  scm_opcode_t op;
  ScmObj opd1;
  ScmObj opd2;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_si {
  scm_opcode_t op;
  int opd1;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_si_si {
  scm_opcode_t op;
  int opd1;
  int opd2;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_si_si_obj {
  scm_opcode_t op;
  int opd1;
  int opd2;
  ScmObj opd3;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));

struct scm_vm_inst_iof {
  scm_opcode_t op;
  int opd1;
} __attribute((aligned(SCM_VMINST_INST_ALIGN)));


enum {
  SCM_OPCODE_NOP = 0x0000,        /*  0: no operation */
  SCM_OPCODE_HALT,                /*  1: stop calculation */
  SCM_OPCODE_INT,                 /*  2: interrupt */
  SCM_OPCODE_UNDEF,               /*  3: update val register to undefined */
                                  /*     value */
  SCM_OPCODE_UNINIT,              /*  4: update val register to uninitialized */
                                  /*     value */
  SCM_OPCODE_CFRAME,              /*  5; create a continuation frame */
  SCM_OPCODE_EFRAME,              /*  6; create a environment frame */
  SCM_OPCODE_EPOP,                /*  7; pop a environment frame */
  SCM_OPCODE_ESHIFT,              /*  8; shift environment frames */
  SCM_OPCODE_IMMVAL,              /*  9: copy immediate value to val register */
  SCM_OPCODE_PUSH,                /* 10: push value of val register */
  SCM_OPCODE_MVPUSH,              /* 11: push value of val register */
  SCM_OPCODE_RETURN,              /* 12: return from function */
  SCM_OPCODE_PCALL,               /* 13: primitive function call */
  SCM_OPCODE_CALL,                /* 14: function call */
  SCM_OPCODE_TAIL_CALL,           /* 15: function tail call */
  SCM_OPCODE_GREF,                /* 16: refere global variable */
  SCM_OPCODE_GDEF,                /* 17: define global variable */
  SCM_OPCODE_GSET,                /* 18: update global variable */
  SCM_OPCODE_SREF,                /* 19: refere value in stack */
  SCM_OPCODE_SSET,                /* 20: update value in stack */
  SCM_OPCODE_JMP,                 /* 21: jump */
  SCM_OPCODE_JMPT,                /* 22: jump if true */
  SCM_OPCODE_JMPF,                /* 23: jump if false */
  SCM_OPCODE_BOX,                 /* 24: boxing */
  SCM_OPCODE_CLOSE,               /* 25: make closure */
  SCM_OPCODE_DEMINE,              /* 26: demine variable */
  SCM_OPCODE_EMINE,               /* 27: make enviroment frame */
                                  /*     and make it mine field */
  SCM_OPCODE_EDEMINE,             /* 28: demine enviromnet frame with */
                                  /*     incomplete enviromnet frame as */
                                  /*     initial value */
  SCM_OPCODE_MRVC,                /* 29: Multiple-Return-Value Check */
  SCM_OPCODE_MRVE,                /* 30: Multiple-Return-Value Error */

  SCM_VMINST_NR_OP,
};

enum {
  SCM_OPFMT_NOOPD = 0,          /* OPCODE */
  SCM_OPFMT_OBJ,                /* OPCODE || ScmObj */
  SCM_OPFMT_OBJ_OBJ,            /* OPCODE || ScmObj || ScmObj */
  SCM_OPFMT_SI,                 /* OPCODE || INT */
  SCM_OPFMT_SI_SI,              /* OPCODE || INT || INT */
  SCM_OPFMT_SI_SI_OBJ,          /* OPCODE || INT || INT || ScmObj */
  SCM_OPFMT_IOF,                /* OPCODE || OFFSET(INT) */
};

#define SCM_OPSIZE sizeof(scm_opcode_t)

#define SCM_OPFMT_INST_SZ_NOOPD      sizeof(struct scm_vm_inst_noopd)
#define SCM_OPFMT_INST_SZ_OBJ        sizeof(struct scm_vm_inst_obj)
#define SCM_OPFMT_INST_SZ_OBJ_OBJ    sizeof(struct scm_vm_inst_obj_obj)
#define SCM_OPFMT_INST_SZ_SI         sizeof(struct scm_vm_inst_si)
#define SCM_OPFMT_INST_SZ_SI_SI      sizeof(struct scm_vm_inst_si_si)
#define SCM_OPFMT_INST_SZ_SI_SI_OBJ  sizeof(struct scm_vm_inst_si_si_obj)
#define SCM_OPFMT_INST_SZ_IOF        sizeof(struct scm_vm_inst_iof)

#define SCM_INST_SZ_NOP         SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_HALT        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_INT         SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_UNDEF       SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_UNINIT      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_CFRAME      SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_EFRAME      SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_EPOP        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_ESHIFT      SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_IMMVAL      SCM_OPFMT_INST_SZ_OBJ
#define SCM_INST_SZ_PUSH        SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_MVPUSH      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_RETURN      SCM_OPFMT_INST_SZ_NOOPD
#define SCM_INST_SZ_PCALL       SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_CALL        SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_TAIL_CALL   SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_GREF        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_GDEF        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_GSET        SCM_OPFMT_INST_SZ_OBJ_OBJ
#define SCM_INST_SZ_SREF        SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_SSET        SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_JMP         SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_JMPT        SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_JMPF        SCM_OPFMT_INST_SZ_IOF
#define SCM_INST_SZ_BOX         SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_CLOSE       SCM_OPFMT_INST_SZ_SI_SI_OBJ
#define SCM_INST_SZ_DEMINE      SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_EMINE       SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_EDEMINE     SCM_OPFMT_INST_SZ_SI_SI
#define SCM_INST_SZ_MRVC        SCM_OPFMT_INST_SZ_SI
#define SCM_INST_SZ_MRVE        SCM_OPFMT_INST_SZ_NOOPD

extern const int scm_opfmt_table[SCM_VMINST_NR_OP];

#define SCM_INST_OPD_OFFSET_OBJ_1 (offsetof(struct scm_vm_inst_obj, opd1))
#define SCM_INST_OPD_OFFSET_OBJ_OBJ_1 (offsetof(struct scm_vm_inst_obj_obj, opd1))
#define SCM_INST_OPD_OFFSET_OBJ_OBJ_2 (offsetof(struct scm_vm_inst_obj_obj, opd2))
#define SCM_INST_OPD_OFFSET_SI_1 (offsetof(struct scm_vm_inst_si, opd1))
#define SCM_INST_OPD_OFFSET_SI_SI_1 (offsetof(struct scm_vm_inst_si_si, opd1))
#define SCM_INST_OPD_OFFSET_SI_SI_2 (offsetof(struct scm_vm_inst_si_si, opd2))
#define SCM_INST_OPD_OFFSET_SI_SI_OBJ_1 (offsetof(struct scm_vm_inst_si_si_obj, opd1))
#define SCM_INST_OPD_OFFSET_SI_SI_OBJ_2 (offsetof(struct scm_vm_inst_si_si_obj, opd2))
#define SCM_INST_OPD_OFFSET_SI_SI_OBJ_3 (offsetof(struct scm_vm_inst_si_si_obj, opd3))
#define SCM_INST_OPD_OFFSET_IOF_1 (offsetof(struct scm_vm_inst_iof, opd1))


#define SCM_VMINST_GET_OP(ip) *(scm_opcode_t *)(ip)

#define SCM_VMINST_FETCH_OPD_NOOPD(ip)                                  \
  do {                                                                  \
    (ip) = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_NOOPD);  \
  } while (0)

#define SCM_VMINST_FETCH_OPD_OBJ(ip, o)                                 \
  do {                                                                  \
    (o) = ((struct scm_vm_inst_obj *)(ip))->opd1;                       \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_OBJ);      \
  } while (0)

#define SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip, o1, o2)                        \
  do {                                                                  \
    (o1) = ((struct scm_vm_inst_obj_obj *)(ip))->opd1;                  \
    (o2) = ((struct scm_vm_inst_obj_obj *)(ip))->opd2;                  \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_OBJ_OBJ);  \
  } while (0)

#define SCM_VMINST_FETCH_OPD_SI(ip, i)                                  \
  do {                                                                  \
    (i) = ((struct scm_vm_inst_si *)(ip))->opd1;                        \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_SI);       \
  } while (0)

#define SCM_VMINST_FETCH_OPD_SI_SI(ip, i1, i2)                          \
  do {                                                                  \
    (i1) = ((struct scm_vm_inst_si_si *)(ip))->opd1;                    \
    (i2) = ((struct scm_vm_inst_si_si *)(ip))->opd2;                    \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_SI_SI);    \
  } while (0)

#define SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip, i1, i2, o)                   \
  do {                                                                  \
    (i1) = ((struct scm_vm_inst_si_si_obj *)(ip))->opd1;                \
    (i2) = ((struct scm_vm_inst_si_si_obj *)(ip))->opd2;                \
    (o) = ((struct scm_vm_inst_si_si_obj *)(ip))->opd3;                 \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_SI_SI_OBJ); \
  } while (0)

#define SCM_VMINST_FETCH_OPD_IOF(ip, i)                                 \
  do {                                                                  \
    (i) = ((struct scm_vm_inst_iof *)(ip))->opd1;                       \
    ip = (typeof(ip))((scm_byte_t *)(ip) + SCM_OPFMT_INST_SZ_IOF);      \
  } while (0)



static inline void
scm_vminst_set_inst_noopd(scm_byte_t *ip, scm_opcode_t op)
{
  ((struct scm_vm_inst_obj *)ip)->op = op;
}

static inline void
scm_vminst_set_inst_obj(scm_byte_t *ip, scm_opcode_t op, ScmObj obj)
{
  ((struct scm_vm_inst_obj *)ip)->op = op;
  ((struct scm_vm_inst_obj *)ip)->opd1 = obj;
}

static inline void
scm_vminst_set_inst_obj_obj(scm_byte_t *ip,
                            scm_opcode_t op, ScmObj obj1, ScmObj obj2)
{
  ((struct scm_vm_inst_obj_obj *)ip)->op = op;
  ((struct scm_vm_inst_obj_obj *)ip)->opd1 = obj1;
  ((struct scm_vm_inst_obj_obj *)ip)->opd2 = obj2;
}

static inline void
scm_vminst_set_inst_si(scm_byte_t *ip, scm_opcode_t op, int si)
{
  ((struct scm_vm_inst_si *)ip)->op = op;
  ((struct scm_vm_inst_si *)ip)->opd1 = si;
}

static inline void
scm_vminst_set_inst_si_si(scm_byte_t *ip, scm_opcode_t op, int si1, int si2)
{
  ((struct scm_vm_inst_si_si *)ip)->op = op;
  ((struct scm_vm_inst_si_si *)ip)->opd1 = si1;
  ((struct scm_vm_inst_si_si *)ip)->opd2 = si2;
}

static inline void
scm_vminst_set_inst_si_si_obj(scm_byte_t *ip,
                              scm_opcode_t op, int si1, int si2, ScmObj obj)
{
  ((struct scm_vm_inst_si_si_obj *)ip)->op = op;
  ((struct scm_vm_inst_si_si_obj *)ip)->opd1 = si1;
  ((struct scm_vm_inst_si_si_obj *)ip)->opd2 = si2;
  ((struct scm_vm_inst_si_si_obj *)ip)->opd3 = obj;
}

static inline void
scm_vminst_set_inst_iof(scm_byte_t *ip, scm_opcode_t op, int iof)
{
  ((struct scm_vm_inst_iof *)ip)->op = op;
  ((struct scm_vm_inst_iof *)ip)->opd1 = iof;
}


#define SCM_VMINST_UPD_FLG_OPD1 0x01u
#define SCM_VMINST_UPD_FLG_OPD2 0x02u
#define SCM_VMINST_UPD_FLG_OPD3 0x04u

static inline void
scm_vminst_update_opd_obj(scm_byte_t *ip, ScmObj obj,
                          unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_obj *)ip)->opd1 = obj;
}

static inline void
scm_vminst_update_opd_obj_obj(scm_byte_t *ip, ScmObj obj1, ScmObj obj2,
                              unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_obj_obj *)ip)->opd1 = obj1;

  if (flg & SCM_VMINST_UPD_FLG_OPD2)
    ((struct scm_vm_inst_obj_obj *)ip)->opd2 = obj2;
}

static inline void
scm_vminst_update_opd_si(scm_byte_t *ip, int si, unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_si *)ip)->opd1 = si;
}

static inline void
scm_vminst_update_opd_si_si(scm_byte_t *ip, int si1, int si2, unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_si_si *)ip)->opd1 = si1;

  if (flg & SCM_VMINST_UPD_FLG_OPD2)
    ((struct scm_vm_inst_si_si *)ip)->opd2 = si2;
}

static inline void
scm_vminst_update_opd_si_si_obj(scm_byte_t *ip, int si1, int si2, ScmObj obj,
                                  unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_si_si_obj *)ip)->opd1 = si1;

  if (flg & SCM_VMINST_UPD_FLG_OPD2)
    ((struct scm_vm_inst_si_si_obj *)ip)->opd2 = si2;

  if (flg & SCM_VMINST_UPD_FLG_OPD3)
    ((struct scm_vm_inst_si_si_obj *)ip)->opd3 = obj;
}

static inline void
scm_vminst_update_opd_iof(scm_byte_t *ip, int iof, unsigned int flg)
{
  if (flg & SCM_VMINST_UPD_FLG_OPD1)
    ((struct scm_vm_inst_iof *)ip)->opd1 = iof;
}


#endif /* INCLUDE_VMINST_H__ */
