#ifndef __NUMBER_PLUS_HEADER__
#define __NUMBER_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define oneplus_number_common_ _n(oneplus_number_common_)
#define oneminus_number_common_ _n(oneminus_number_common_)
#define sign_reverse_number_common_ _n(sign_reverse_number_common_)
#define plus_number_heap_ _n(plus_number_heap_)
#define minus_number_heap_ _n(minus_number_heap_)

int oneplus_number_common_(LocalRoot local, addr value, addr *ret);
int oneminus_number_common_(LocalRoot local, addr value, addr *ret);
int sign_reverse_number_common_(addr left, addr *ret);

int plus_number_heap_(LocalRoot local, addr left, addr right, addr *ret);
int minus_number_heap_(LocalRoot local, addr left, addr right, addr *ret);

#endif

