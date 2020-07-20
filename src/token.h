#ifndef __TOKEN_HEADER__
#define __TOKEN_HEADER__

#include "build.h"

_g int getchar_digit(unsigned v, int upperp, unicode *ret);
_g int getvalue_digit(unsigned base, unicode c, unsigned *ret);
_g unicode checkchar_digit(unsigned v, int upperp);
_g unsigned checkvalue_digit(unsigned base, unicode c);

_g void maketoken_integer(LocalRoot local, addr queue, unsigned base, addr *ret);
_g int maketoken_float_(Execute ptr, addr queue, addr *ret);
_g void maketoken_ratio(LocalRoot local, addr queue, unsigned base, addr *ret);

#endif

