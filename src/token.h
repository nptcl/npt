#ifndef __TOKEN_HEADER__
#define __TOKEN_HEADER__

#include "build.h"

#define getchar_digit _n(getchar_digit)
#define getvalue_digit _n(getvalue_digit)
#define checkchar_digit _n(checkchar_digit)
#define checkvalue_digit _n(checkvalue_digit)
#define maketoken_integer _n(maketoken_integer)
#define maketoken_float_ _n(maketoken_float_)
#define maketoken_ratio _n(maketoken_ratio)

_g int getchar_digit(unsigned v, int upperp, unicode *ret);
_g int getvalue_digit(unsigned base, unicode c, unsigned *ret);
_g unicode checkchar_digit(unsigned v, int upperp);
_g unsigned checkvalue_digit(unsigned base, unicode c);

_g void maketoken_integer(LocalRoot local, addr queue, unsigned base, addr *ret);
_g int maketoken_float_(Execute ptr, addr queue, addr *ret);
_g void maketoken_ratio(LocalRoot local, addr queue, unsigned base, addr *ret);

#endif

