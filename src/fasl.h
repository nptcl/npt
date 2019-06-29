#ifndef __FASL_HEADER__
#define __FASL_HEADER__

#include "execute.h"
#include "typedef.h"

_g void init_fasl(void);
_g void build_fasl(void);
_g int fasl_stream(Execute ptr, addr stream);

#endif

