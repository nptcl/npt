#ifndef __PACKAGE_MAKE_HEADER__
#define __PACKAGE_MAKE_HEADER__

#include "execute.h"
#include "typedef.h"

#define nicknames_make_package_ _n(nicknames_make_package_)
#define make_package_ _n(make_package_)
#define init_package_make _n(init_package_make)

_g int nicknames_make_package_(Execute ptr, addr list, addr *ret);
_g int make_package_(Execute ptr, addr name, addr names, addr use, addr *ret);
_g void init_package_make(void);

#endif

