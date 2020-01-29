#ifndef __FASL_HEADER__
#define __FASL_HEADER__

#include "execute.h"
#include "typedef.h"

_g void faslwrite_header(addr stream);
_g int faslread_stream(Execute ptr, addr stream);
_g void init_fasl(void);

#endif

