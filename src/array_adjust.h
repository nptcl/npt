#ifndef __ARRAY_ADJUST_HEADER__
#define __ARRAY_ADJUST_HEADER__

#include "typedef.h"

_g void array_adjust_array(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset);

#endif

