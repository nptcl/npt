#ifndef __CALL_ITERATION_HEADER__
#define __CALL_ITERATION_HEADER__

#include "execute.h"
#include "typedef.h"

#define do_common _n(do_common)
#define doa_common _n(doa_common)
#define dotimes_common _n(dotimes_common)
#define dolist_common _n(dolist_common)

_g int do_common(addr form, addr env, addr *ret);
_g int doa_common(addr form, addr env, addr *ret);
_g int dotimes_common(addr form, addr env, addr *ret);
_g int dolist_common(Execute ptr, addr form, addr env, addr *ret);

#endif

