#ifndef __GC_HEADER__
#define __GC_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "local.h"

_g void gcexec(void);
_g void gcsync(Execute ptr);
_g void heap_check(void);

_g void gchold_local(LocalRoot local, addr pos);
_g void gchold_va_local(LocalRoot local, ...);
_g void gchold_execute(Execute ptr, addr pos);
_g void gchold_va_execute(Execute ptr, ...);

#endif

