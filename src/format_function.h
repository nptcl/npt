#ifndef __FORMAT_FUNCTION_HEADER__
#define __FORMAT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int format_execute(Execute ptr, addr stream, addr format, addr args, addr *ret);
_g void init_format_function(void);

#endif

