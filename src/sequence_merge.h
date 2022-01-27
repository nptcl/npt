#ifndef __SEQUENCE_MERGE_HEADER__
#define __SEQUENCE_MERGE_HEADER__

#include "execute.h"
#include "typedef.h"

#define merge_common_ _n(merge_common_)

int merge_common_(Execute ptr,
		addr type, addr pos1, addr pos2, addr call, addr rest, addr *ret);

#endif

