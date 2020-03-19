#ifndef __NUMBER_COMMON_HEADER__
#define __NUMBER_COMMON_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

_g int number_equal_common(LocalRoot local, addr left, addr rest);
_g int number_not_equal_common(LocalRoot local, addr left, addr rest);
_g int number_less_common(LocalRoot local, addr left, addr rest);
_g int number_greater_common(LocalRoot local, addr left, addr rest);
_g int number_less_equal_common(LocalRoot local, addr left, addr rest);
_g int number_greater_equal_common(LocalRoot local, addr left, addr rest);
_g void max_common(LocalRoot local, addr left, addr rest, addr *ret);
_g void min_common(LocalRoot local, addr left, addr rest, addr *ret);
_g void plus_common(LocalRoot local, addr rest, addr *ret);
_g void minus_common(LocalRoot local, addr left, addr rest, addr *ret);
_g void asterisk_common(LocalRoot local, addr rest, addr *ret);
_g void slash_common(LocalRoot local, addr left, addr rest, addr *ret);
_g void incf_common(Execute ptr, addr form, addr env, addr *ret);
_g void decf_common(Execute ptr, addr form, addr env, addr *ret);
_g void random_common(Execute ptr, addr limit, addr state, addr *ret);
_g void conjugate_common(addr var, addr *ret);
_g void realpart_common(addr var, addr *ret);
_g void imagpart_common(addr var, addr *ret);
_g int parse_integer_common(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2);

#endif

