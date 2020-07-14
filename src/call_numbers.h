#ifndef __CALL_NUMBERS_HEADER__
#define __CALL_NUMBERS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

_g int number_equal_common(LocalRoot local, addr left, addr rest, int *ret);
_g int number_not_equal_common(LocalRoot local, addr left, addr rest, int *ret);
_g int number_less_common(LocalRoot local, addr left, addr rest, int *ret);
_g int number_greater_common(LocalRoot local, addr left, addr rest, int *ret);
_g int number_less_equal_common(LocalRoot local, addr left, addr rest, int *ret);
_g int number_greater_equal_common(LocalRoot local, addr left, addr rest, int *ret);
_g int max_common(LocalRoot local, addr left, addr rest, addr *ret);
_g int min_common(LocalRoot local, addr left, addr rest, addr *ret);
_g int plus_common(LocalRoot local, addr rest, addr *ret);
_g int minus_common(LocalRoot local, addr left, addr rest, addr *ret);
_g int asterisk_common(LocalRoot local, addr rest, addr *ret);
_g int slash_common(LocalRoot local, addr left, addr rest, addr *ret);
_g int incf_common(Execute ptr, addr form, addr env, addr *ret);
_g int decf_common(Execute ptr, addr form, addr env, addr *ret);
_g int random_common(Execute ptr, addr limit, addr state, addr *ret);
_g int conjugate_common(addr var, addr *ret);
_g void realpart_common(addr var, addr *ret);
_g void imagpart_common(addr var, addr *ret);
_g int parse_integer_common(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2);

#endif

