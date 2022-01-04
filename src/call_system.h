#ifndef __CALL_SYSTEM_HEADER__
#define __CALL_SYSTEM_HEADER__

#include "execute.h"
#include "typedef.h"

#define load_common_ _n(load_common_)

int load_common_(Execute ptr, addr filespec, addr rest, int *ret);

#endif

