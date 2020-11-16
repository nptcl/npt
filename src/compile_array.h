#ifndef __COMPILE_ARRAY_HEADER__
#define __COMPILE_ARRAY_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslwrite_value_array _n(faslwrite_value_array)
#define faslread_value_array _n(faslread_value_array)

int faslwrite_value_array(Execute ptr, addr stream, addr pos);
int faslread_value_array(Execute ptr, addr stream, addr *ret);

#endif

