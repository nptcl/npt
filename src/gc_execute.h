#ifndef __GC_EXECUTE_HEADER__
#define __GC_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define gcexec_full _n(gcexec_full)
#define gcexec_partial _n(gcexec_partial)

_g void gcexec_full(void);
_g void gcexec_partial(void);

#endif

