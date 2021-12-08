#ifndef __CALL_STRUCTURES_HEADER__
#define __CALL_STRUCTURES_HEADER__

#include "execute.h"
#include "typedef.h"

#define defstruct_common_ _n(defstruct_common_)
int defstruct_common_(Execute ptr, addr form, addr env, addr *ret);

#endif

