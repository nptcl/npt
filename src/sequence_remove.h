#ifndef __SEQUENCE_REMOVE_HEADER__
#define __SEQUENCE_REMOVE_HEADER__

#include "execute.h"
#include "typedef.h"

#define remove_common_ _n(remove_common_)
#define remove_if_common_ _n(remove_if_common_)
#define remove_if_not_common_ _n(remove_if_not_common_)
#define delete_common_ _n(delete_common_)
#define delete_if_common_ _n(delete_if_common_)
#define delete_if_not_common_ _n(delete_if_not_common_)

int remove_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int remove_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int remove_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int delete_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int delete_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int delete_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);

#endif

