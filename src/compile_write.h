#ifndef __COMPILE_WRITE_HEADER__
#define __COMPILE_WRITE_HEADER__

#include "typedef.h"

_g int faslwrite_header_(addr stream);
_g int faslwrite_footer_(addr stream);
_g int faslwrite_value(Execute ptr, addr stream, addr pos);
_g void init_compile_write(void);

#endif

