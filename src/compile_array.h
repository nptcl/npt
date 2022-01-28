#ifndef __COMPILE_ARRAY_HEADER__
#define __COMPILE_ARRAY_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslwrite_value_array_ _n(faslwrite_value_array_)
#define faslread_value_array_ _n(faslread_value_array_)

int faslwrite_value_array_(Execute ptr, addr stream, addr pos);
int faslread_value_array_(Execute ptr, addr stream, addr *ret);

#endif

