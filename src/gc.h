#ifndef __GC_HEADER__
#define __GC_HEADER__

#include "execute.h"

void gcexec(void);
void gcsync(Execute ptr);
void heap_check(void);

#endif

