#ifndef __COMPILE_CLOS_HEADER__
#define __COMPILE_CLOS_HEADER__

#include "execute.h"
#include "typedef.h"

_g int faslwrite_value_clos(Execute ptr, addr stream, addr pos);
_g int faslread_value_clos(Execute ptr, addr stream, addr *ret);

#endif

