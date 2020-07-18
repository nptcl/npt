#ifndef __COMPILE_READ_HEADER__
#define __COMPILE_READ_HEADER__

#include "execute.h"
#include "typedef.h"

_g int faslread_header_(addr input, int *ret);
_g int faslread_footer_(addr input, int *ret);
_g int faslread_value(Execute ptr, addr stream, addr *ret);
_g void init_compile_read(void);

#endif

