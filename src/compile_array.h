#ifndef __COMPILE_ARRAY_HEADER__
#define __COMPILE_ARRAY_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslwrite_value_array _n(faslwrite_value_array)
#define faslread_value_array _n(faslread_value_array)

_g int faslwrite_value_array(Execute ptr, addr stream, addr pos);
_g int faslread_value_array(Execute ptr, addr stream, addr *ret);

#endif

