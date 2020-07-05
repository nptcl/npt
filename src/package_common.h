#ifndef __PACKAGE_COMMON_HEADER__
#define __PACKAGE_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

_g void make_gentemp(Execute ptr, addr prefix, addr package, addr *ret);
_g int defpackage_execute(Execute ptr, addr rest, addr *ret);
_g int do_symbols_package(Execute ptr, addr call, addr package);
_g int do_external_symbols_package(Execute ptr, addr call, addr package);
_g int do_all_symbols_package(Execute ptr, addr call);
_g void all_symbols_package(addr package, addr *ret);

_g void init_package_common(void);

#endif

