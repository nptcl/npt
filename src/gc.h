#ifndef __GC_HEADER__
#define __GC_HEADER__

#include "execute.h"

_g void gcexec(void);
_g void gcsync(Execute ptr);
_g void heap_check(void);

#endif

