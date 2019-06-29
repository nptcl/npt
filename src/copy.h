#ifndef __COPY_HEADER__
#define __COPY_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

_g void copyhard_cons(LocalRoot local, addr *ret, addr pos);
_g void copyhard_vectorA2(LocalRoot local, addr *ret, addr left);
_g void copyhard_vectorA4(LocalRoot local, addr *ret, addr left);
#ifdef LISP_ARCH_64BIT
_g void copyhard_vectorA8(LocalRoot local, addr *ret, addr left);
#endif
_g void copyhard_vector(LocalRoot local, addr *ret, addr pos);
_g void copyhard_character(LocalRoot local, addr *ret, addr pos);
_g void copyhard_string(LocalRoot local, addr *ret, addr pos);
_g void copyhard_fixnum(LocalRoot local, addr *ret, addr pos);
_g void copyhard_bignum(LocalRoot local, addr *ret, addr pos);
_g void copyhard_ratio(LocalRoot local, addr *ret, addr pos);
_g void copyhard_float(LocalRoot local, addr *ret, addr pos);
_g void copyhard_double(LocalRoot local, addr *ret, addr pos);
_g void copyhard_long_double(LocalRoot local, addr *ret, addr pos);
_g void copyhard_complex(LocalRoot local, addr *ret, addr pos);
_g void copyhard_callname(LocalRoot local, addr *ret, addr pos);
_g void copyhard_random_state(LocalRoot local, addr *ret, addr pos);
_g void copyhard_pathname(LocalRoot local, addr *ret, addr pos);
_g void copyhard_object(LocalRoot local, addr *ret, addr pos);

_g int copylocalp(LocalRoot local, addr pos);
_g int copylocal_check(LocalRoot local, addr pos);
_g int copylocal_object(LocalRoot local, addr *ret, addr pos);
_g void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args);
_g int copyheap(addr *ret, addr pos);

_g void init_copy(void);

#endif

