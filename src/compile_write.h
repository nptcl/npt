#ifndef __COMPILE_WRITE_HEADER__
#define __COMPILE_WRITE_HEADER__

#include "typedef.h"

_g void faslwrite_header(addr stream);
_g void faslwrite_footer(addr stream);
_g int faslwrite_value(Execute ptr, addr stream, addr pos);
_g void init_compile_write(void);

#endif

