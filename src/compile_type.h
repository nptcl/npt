#ifndef __COMPILE_TYPE_HEADER__
#define __COMPILE_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int faslwrite_value_type(Execute ptr, addr stream, addr pos);
_g int faslread_value_type(Execute ptr, addr stream, addr *ret);

#endif

