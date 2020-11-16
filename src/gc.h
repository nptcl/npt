#ifndef __GC_HEADER__
#define __GC_HEADER__

#include "execute.h"
#include "typedef.h"

#define gcexec _n(gcexec)
#define gcsync _n(gcsync)

void gcexec(enum GcMode mode);
void gcsync(Execute ptr, enum GcMode mode);

#endif

