#ifndef __ARRAY_INPLACE_HEADER__
#define __ARRAY_INPLACE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g int arrayinplace_get_(addr pos, size_t index, struct array_value *str);
_g int arrayinplace_set_(addr pos, size_t index, const struct array_value *str);

#endif

