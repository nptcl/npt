#ifndef __SEQUENCE_DUPLICATES_HEADER__
#define __SEQUENCE_DUPLICATES_HEADER__

#include "execute.h"
#include "typedef.h"

#define remove_duplicates_common_ _n(remove_duplicates_common_)
#define delete_duplicates_common_ _n(delete_duplicates_common_)

int remove_duplicates_common_(Execute ptr, addr *ret, addr pos, addr rest);
int delete_duplicates_common_(Execute ptr, addr *ret, addr pos, addr rest);

#endif

