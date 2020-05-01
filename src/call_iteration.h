#ifndef __CALL_ITERATION_HEADER__
#define __CALL_ITERATION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int do_common(addr form, addr env, addr *ret);
_g int doa_common(addr form, addr env, addr *ret);
_g int dotimes_common(addr form, addr env, addr *ret);
_g int dolist_common(Execute ptr, addr form, addr env, addr *ret);

#endif

