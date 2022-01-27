#ifndef __SEQUENCE_HEADER_FIND__
#define __SEQUENCE_HEADER_FIND__

#include "execute.h"
#include "typedef.h"

#define find_common_ _n(find_common_)
#define find_if_common_ _n(find_if_common_)
#define find_if_not_common_ _n(find_if_not_common_)
#define position_common_ _n(position_common_)
#define position_if_common_ _n(position_if_common_)
#define position_if_not_common_ _n(position_if_not_common_)
#define search_common_ _n(search_common_)
#define mismatch_common_ _n(mismatch_common_)

int find_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int find_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int find_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int position_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int position_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int position_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int search_common_(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
int mismatch_common_(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);

#endif

