#ifndef INCLUDE_NUMBER_H__
#define INCLUDE_NUMBER_H__

#include <stddef.h>

#include "object.h"
#include "number_common.h"
#include "fixnum.h"
#include "bignum.h"

ScmObj scm_num_make_from_literal(const char *literal, size_t size);

#endif /* INCLUDE_NUMBER_H__ */
