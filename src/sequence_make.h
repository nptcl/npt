#ifndef __SEQUENCE_MAKE_HEADER__
#define __SEQUENCE_MAKE_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_sequence_common_ _n(make_sequence_common_)

int make_sequence_common_(Execute ptr, addr *ret, addr type, addr size, addr rest);

#endif

