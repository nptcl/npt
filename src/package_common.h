#ifndef __PACKAGE_COMMON_HEADER__
#define __PACKAGE_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_gentemp_ _n(make_gentemp_)
#define defpackage_execute _n(defpackage_execute)
#define do_symbols_package _n(do_symbols_package)
#define do_external_symbols_package _n(do_external_symbols_package)
#define do_all_symbols_package_ _n(do_all_symbols_package_)
#define all_symbols_package_ _n(all_symbols_package_)
#define init_package_common _n(init_package_common)

_g int make_gentemp_(Execute ptr, addr prefix, addr package, addr *ret);
_g int defpackage_execute(Execute ptr, addr rest, addr *ret);
_g int do_symbols_package(Execute ptr, addr call, addr package);
_g int do_external_symbols_package(Execute ptr, addr call, addr package);
_g int do_all_symbols_package_(Execute ptr, addr call);
_g int all_symbols_package_(addr package, addr *ret);

_g void init_package_common(void);

#endif

