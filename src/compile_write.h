#ifndef __COMPILE_WRITE_HEADER__
#define __COMPILE_WRITE_HEADER__

#include "typedef.h"

#define faslwrite_header_ _n(faslwrite_header_)
#define faslwrite_footer_ _n(faslwrite_footer_)
#define faslwrite_value _n(faslwrite_value)
#define init_compile_write _n(init_compile_write)

_g int faslwrite_header_(addr stream);
_g int faslwrite_footer_(addr stream);
_g int faslwrite_value(Execute ptr, addr stream, addr pos);
_g void init_compile_write(void);

#endif

