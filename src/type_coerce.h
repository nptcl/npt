#ifndef __TYPE_COERCE_HEADER__
#define __TYPE_COERCE_HEADER__

#include "define.h"
#include "execute.h"

_g void init_type_coerce(void);
_g int coerce_common(Execute ptr, addr pos, addr type, addr *ret);

#endif

