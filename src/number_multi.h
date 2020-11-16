#ifndef __NUMBER_MULTI_HEADER__
#define __NUMBER_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

#define multi_number_heap_ _n(multi_number_heap_)
#define inverse_number_heap_ _n(inverse_number_heap_)
#define div_number_heap_ _n(div_number_heap_)

int multi_number_heap_(LocalRoot local, addr left, addr right, addr *ret);
int inverse_number_heap_(LocalRoot local, addr left, addr *ret);
int div_number_heap_(LocalRoot local, addr left, addr right, addr *ret);

#endif

