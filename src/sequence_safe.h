#ifndef __SEQUENCE_SAFE_HEADER__
#define __SEQUENCE_SAFE_HEADER__

#include "typedef.h"

#define get_unsigned8_sequence _n(get_unsigned8_sequence)

_g int get_unsigned8_sequence(addr pos, size_t index, byte *ret);

#endif

