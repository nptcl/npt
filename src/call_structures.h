#ifndef __CALL_STRUCTURES_HEADER__
#define __CALL_STRUCTURES_HEADER__

#include "execute.h"
#include "typedef.h"

#define defstruct_common _n(defstruct_common)
_g int defstruct_common(Execute ptr, addr form, addr env, addr *ret);

#endif

