#ifndef __ARRAY_INPLACE_HEADER__
#define __ARRAY_INPLACE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g void arrayinplace_get(addr pos, size_t index, struct array_value *str);
_g void arrayinplace_set(addr pos, size_t index, const struct array_value *value);

#endif

