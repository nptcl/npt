#ifndef __SEQUENCE_MAP_HEADER__
#define __SEQUENCE_MAP_HEADER__

#include "execute.h"
#include "typedef.h"

#define map_common_ _n(map_common_)
#define map_into_common_ _n(map_into_common_)

int map_common_(Execute ptr, addr *ret, addr type, addr call, addr rest);
int map_into_common_(Execute ptr, addr var, addr call, addr rest);

#endif

