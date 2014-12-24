#ifndef INCLUDE_FCD_VECTOR_H__
#define INCLUDE_FCD_VECTOR_H__

#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"


/*******************************************************************/
/*  Vectors                                                        */
/*******************************************************************/

bool scm_fcd_vector_p(ScmObj obj);
ScmObj scm_fcd_vector_P(ScmObj obj);
ScmObj scm_fcd_make_vector(size_t len, ScmObj fill);
ScmObj scm_fcd_vector_lst(ScmObj lst);
ScmObj scm_fcd_vector_cv(const ScmObj *elm, size_t n);
ScmObj scm_fcd_vector(size_t n, ...);
size_t scm_fcd_vector_length(ScmObj vec);
ScmObj scm_fcd_vector_ref(ScmObj vec, size_t idx);
void scm_fcd_vector_set_i(ScmObj vec, size_t idx, ScmObj obj);
ScmObj scm_fcd_vector_to_list(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_fcd_list_to_vector(ScmObj lst);
ScmObj scm_fcd_vector_to_string(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_fcd_string_to_vector(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_fcd_vector_copy(ScmObj vec, ssize_t start, ssize_t end);
int scm_fcd_vector_copy_i(ScmObj to, size_t at,
                          ScmObj from, ssize_t start, ssize_t end);
ScmObj scm_fcd_vector_append_lst(ScmObj lst);
ScmObj scm_fcd_vector_append_cv(ScmObj *ary, size_t n);
ScmObj scm_fcd_vector_append(size_t n, ...);
void scm_fcd_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end);
int scm_fcd_vector_push(ScmObj vec, ScmObj obj);


/*******************************************************************/
/*  Bytevectors                                                    */
/*******************************************************************/

bool scm_fcd_bytevector_p(ScmObj obj);
ScmObj scm_fcd_bytevector_P(ScmObj obj);
ScmObj scm_fcd_make_bytevector(size_t len, int fill);
ScmObj scm_fcd_make_bytevector_from_cv(const void *bytes, size_t length);
size_t scm_fcd_bytevector_length(ScmObj vec);
void scm_fcd_bytevector_u8_set_i(ScmObj vec, size_t idx, int val);
void *scm_fcd_bytevector_to_cv(ScmObj vec, void *buf, size_t size);

#endif /* INCLUDE_FCD_VECTOR_H__ */
