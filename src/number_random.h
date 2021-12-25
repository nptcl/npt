#ifndef __NUMBER_RANDOM_HEADER__
#define __NUMBER_RANDOM_HEADER__

#include "typedef.h"

#define random_number_common_ _n(random_number_common_)
int random_number_common_(LocalRoot local, addr limit, addr state, addr *ret);

#endif

