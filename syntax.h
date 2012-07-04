#ifndef INCLUDE_SYNTAX_H__
#define INCLUDE_SYNTAX_H__

#include <stdbool.h>

typedef struct ScmSyntaxRec ScmSyntax;

#define SCM_SYNTAX(obj) ((ScmSyntax *)(obj))

#include "object.h"
#include "api_enum.h"

#define SCM_SYNTAX_KEYWORD_LEN_MAX 64

struct ScmSyntaxRec {
  ScmObjHeader header;
  int id;
  char keyword[SCM_SYNTAX_KEYWORD_LEN_MAX];
};

extern ScmTypeInfo SCM_SYNTAX_TYPE_INFO;

ScmObj scm_syntax_new(SCM_MEM_TYPE_T mtype, int id, const char *keyword);
bool scm_syntax_id_eq_p(ScmObj syx, int id);

int scm_syntax_pretty_print(ScmObj obj, ScmObj port, bool write_p);

static int
scm_syntax_id(ScmObj syx)
{
  scm_assert_obj_type(syx, &SCM_SYNTAX_TYPE_INFO);

  return SCM_SYNTAX(syx)->id;
}

#endif /* INCLUDE_SYNTAX_H__ */
