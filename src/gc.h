#ifndef __GC_HEADER__
#define __GC_HEADER__

#include "execute.h"
#include "typedef.h"

#define gcexec _n(gcexec)
#define gcsync _n(gcsync)

_g void gcexec(enum GcMode mode);
_g void gcsync(Execute ptr, enum GcMode mode);

#endif

