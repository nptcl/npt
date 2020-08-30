#ifndef __ARRAY_ADJUST_HEADER__
#define __ARRAY_ADJUST_HEADER__

#include "typedef.h"

#define array_adjust_array_ _n(array_adjust_array_)

_g int array_adjust_array_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset);

#endif

