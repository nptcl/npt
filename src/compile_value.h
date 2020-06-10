#ifndef __COMPILE_VALUE_HEADER__
#define __COMPILE_VALUE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int faslread_fixnum_code(Execute ptr, addr stream, addr *ret);
_g int faslread_bignum_code(Execute ptr, addr stream, addr *ret);
_g int faslread_ratio_code(Execute ptr, addr stream, addr *ret);

_g int faslwrite_value(Execute ptr, addr stream, addr pos);

#endif

