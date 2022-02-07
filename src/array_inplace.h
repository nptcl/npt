#ifndef __ARRAY_INPLACE_HEADER__
#define __ARRAY_INPLACE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

#define arrayinplace_get_ _n(arrayinplace_get_)
#define arrayinplace_set_ _n(arrayinplace_set_)

int arrayinplace_get_(addr pos, size_t index, struct array_value *str);
int arrayinplace_set_(addr pos, size_t index, const struct array_value *str);

#endif

