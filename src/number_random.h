#ifndef __NUMBER_RANDOM_HEADER__
#define __NUMBER_RANDOM_HEADER__

#include "typedef.h"

#define random_number_common _n(random_number_common)
_g int random_number_common(LocalRoot local, addr limit, addr state, addr *ret);

#endif

